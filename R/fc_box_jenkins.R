# =============================================================================
# Forecasting > Box-Jenkins: AR(p), MA(q), ARMA(p, q), ARIMA(p, d, q)
# One module, used four times with a different 'type'.
# Shows ACF/PACF plots, parameter estimates, forecasts, MAD / MSE / MAPE.
# =============================================================================

bj_fit <- function(y, p, d, q, include_mean = TRUE) {
  tryCatch(
    stats::arima(y, order = c(p, d, q), include.mean = include_mean && d == 0,
                 method = "CSS-ML"),
    error = function(e) tryCatch(
      stats::arima(y, order = c(p, d, q), include.mean = include_mean && d == 0,
                   method = "ML"),
      error = function(e2) NULL))
}

bj_score <- function(fit, y, criterion, d) {
  if (is.null(fit)) return(Inf)
  if (criterion == "AIC") return(stats::AIC(fit))
  if (criterion == "BIC") return(stats::BIC(fit))
  fitted <- y - as.numeric(stats::residuals(fit))
  if (d > 0) fitted[seq_len(d)] <- NA
  criterion_value(y, fitted, criterion)
}

fc_bj_ui <- function(id, type = c("AR", "MA", "ARMA", "ARIMA")) {
  type <- match.arg(type)
  ns <- NS(id)
  title <- switch(type, AR = "Autoregressive model AR(p)", MA = "Moving average model MA(q)",
                  ARMA = "ARMA(p, q) model", ARIMA = "ARIMA(p, d, q) model")
  sidebarLayout(
    sidebarPanel(
      h4(title),
      fc_series_input(ns),
      radioButtons(ns("mode"), "How should the order be chosen?",
                   choices = c("I will enter it" = "manual", "Find the best order" = "search")),
      conditionalPanel("input.mode == 'search'", ns = ns,
        selectInput(ns("criterion"), "Criterion",
                    choices = c("AIC", "BIC", "MSE", "MAD", "MAPE")),
        numericInput(ns("maxord"), "Largest p / q to try", value = 3, min = 1, max = 5)),
      conditionalPanel("input.mode == 'manual'", ns = ns,
        if (type %in% c("AR", "ARMA", "ARIMA"))
          numericInput(ns("p"), "p (AR order)", value = if (type == "ARIMA") 1 else 1,
                       min = if (type == "ARIMA") 0 else 1, max = 8, step = 1),
        if (type %in% c("MA", "ARMA", "ARIMA"))
          numericInput(ns("q"), "q (MA order)", value = if (type == "ARIMA") 0 else 1,
                       min = if (type == "ARIMA") 0 else 1, max = 8, step = 1)),
      if (type == "ARIMA")
        numericInput(ns("d"), "d (order of differencing)", value = 1, min = 1, max = 2, step = 1),
      if (type != "ARIMA")
        checkboxInput(ns("mean"), "Include a constant (mean)", TRUE),
      fc_horizon_input(ns, value = 6),
      helpText("Parameters are estimated by maximum likelihood (stats::arima).",
               "Forecast bands are 95% prediction intervals.")
    ),
    mainPanel(
      h4(textOutput(ns("headline"))),
      plotOutput(ns("plot"), height = "340px"),
      h4("Autocorrelation (ACF) and partial autocorrelation (PACF) of the series"),
      plotOutput(ns("acf"), height = "260px"),
      uiOutput(ns("acf_diff_ui")),
      fluidRow(
        column(6, h4("Parameter estimates"), tableOutput(ns("coef"))),
        column(6, h4("Error measures"), tableOutput(ns("errors")),
               h4("Model information"), tableOutput(ns("info")))
      ),
      uiOutput(ns("search_ui")),
      h4("Forecasts"),
      tableOutput(ns("future")),
      h4("Residual diagnostics"),
      plotOutput(ns("resid"), height = "240px"),
      h4("Fitted values by period"),
      tableOutput(ns("table"))
    )
  )
}

fc_bj_server <- function(id, data, type = c("AR", "MA", "ARMA", "ARIMA")) {
  type <- match.arg(type)
  moduleServer(id, function(input, output, session) {
    y <- fc_series_server(input, session, data, "stationary")

    res <- reactive({
      y <- y(); n <- length(y)
      validate(need(n >= 8, "Box-Jenkins models need at least 8 observations."))
      d <- if (type == "ARIMA") as.integer(input$d) else 0L
      include_mean <- if (type == "ARIMA") FALSE else isTRUE(input$mean)
      search_table <- NULL
      if (input$mode == "search") {
        mo <- max(1, min(5, input$maxord))
        cand <- switch(type,
          AR    = expand.grid(p = 1:mo, q = 0),
          MA    = expand.grid(p = 0, q = 1:mo),
          ARMA  = expand.grid(p = 1:mo, q = 1:mo),
          ARIMA = expand.grid(p = 0:mo, q = 0:mo))
        scores <- mapply(function(p, q) {
          fit <- suppressWarnings(bj_fit(y, p, d, q, include_mean))
          bj_score(fit, y, input$criterion, d)
        }, cand$p, cand$q)
        validate(need(any(is.finite(scores)), "No model could be estimated."))
        ord <- order(scores)
        p <- cand$p[ord[1]]; q <- cand$q[ord[1]]
        top <- head(ord[is.finite(scores[ord])], 10)
        search_table <- data.frame(Rank = seq_along(top),
                                   Model = paste0(type, "(", cand$p[top],
                                                  if (type == "ARIMA") paste0(", ", d) else "",
                                                  if (type %in% c("ARMA", "ARIMA")) paste0(", ", cand$q[top]) else "",
                                                  ")"),
                                   Criterion = scores[top])
        if (type == "AR") search_table$Model <- paste0("AR(", cand$p[top], ")")
        if (type == "MA") search_table$Model <- paste0("MA(", cand$q[top], ")")
        names(search_table)[3] <- input$criterion
      } else {
        p <- if (type %in% c("AR", "ARMA", "ARIMA")) as.integer(input$p) else 0L
        q <- if (type %in% c("MA", "ARMA", "ARIMA")) as.integer(input$q) else 0L
        validate(need(!is.na(p) && !is.na(q), "Enter the model order."))
      }
      fit <- suppressWarnings(bj_fit(y, p, d, q, include_mean))
      validate(need(!is.null(fit), "The model could not be estimated for this series / order."))

      label <- switch(type, AR = paste0("AR(", p, ")"), MA = paste0("MA(", q, ")"),
                      ARMA = paste0("ARMA(", p, ", ", q, ")"),
                      ARIMA = paste0("ARIMA(", p, ", ", d, ", ", q, ")"))
      resid <- as.numeric(stats::residuals(fit))
      fitted <- y - resid
      if (d > 0) fitted[seq_len(d)] <- NA
      em <- error_measures(y, fitted)
      h <- max(1, input$h)
      pr <- stats::predict(fit, n.ahead = h)
      fut <- data.frame(Period = n + seq_len(h), Forecast = as.numeric(pr$pred),
                        `Std. error` = as.numeric(pr$se),
                        Lower = as.numeric(pr$pred - 1.96 * pr$se),
                        Upper = as.numeric(pr$pred + 1.96 * pr$se), check.names = FALSE)
      se <- sqrt(pmax(diag(fit$var.coef), 0))
      coefs <- if (length(fit$coef)) data.frame(
        Parameter = sub("intercept", "constant (mean)", names(fit$coef)),
        Estimate = as.numeric(fit$coef), `Std. error` = se,
        `t value` = as.numeric(fit$coef) / se, check.names = FALSE) else
        data.frame(Parameter = "(no parameters)", Estimate = NA)
      lb <- stats::Box.test(resid[(d + 1):n], lag = max(p + q + 1, min(10, n - d - 1)),
                            type = "Ljung-Box", fitdf = p + q)
      list(
        type = type, label = label, fit = fit, d = d, y = y, resid = resid,
        headline = paste0(label, " — next-period forecast: ", fmt_num(fut$Forecast[1])),
        plot_title = paste0("Actual demand, fitted values and ", label, " forecast"),
        actual = y, fitted = fitted, future = fut,
        errors = error_table(em), coef = coefs,
        info = data.frame(Item = c("sigma^2", "Log-likelihood", "AIC", "BIC",
                                   "Ljung-Box p-value (residuals)"),
                          Value = c(fit$sigma2, fit$loglik, stats::AIC(fit),
                                    stats::BIC(fit), lb$p.value)),
        search_table = search_table,
        table = data.frame(Period = seq_len(n), `Demand (D)` = y, `Fitted (F)` = fitted,
                           `Error (e = F - D)` = fitted - y, check.names = FALSE)
      )
    })

    output$headline <- renderText(res()$headline)
    output$plot <- renderPlot(fc_plot(res()))
    output$acf <- renderPlot({
      y <- res()$y
      par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
      stats::acf(y, main = "ACF of the series", lag.max = min(24, length(y) - 1))
      stats::pacf(y, main = "PACF of the series", lag.max = min(24, length(y) - 1))
    })
    output$acf_diff_ui <- renderUI({
      if (res()$d == 0) return(NULL)
      tagList(h4(paste0("ACF and PACF of the differenced series (d = ", res()$d, ")")),
              plotOutput(session$ns("acf_diff"), height = "260px"))
    })
    output$acf_diff <- renderPlot({
      r <- res(); z <- diff(r$y, differences = r$d)
      par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
      stats::acf(z, main = "ACF (differenced)", lag.max = min(24, length(z) - 1))
      stats::pacf(z, main = "PACF (differenced)", lag.max = min(24, length(z) - 1))
    })
    output$coef <- renderTable(res()$coef, digits = 4)
    output$errors <- renderTable(res()$errors, digits = 3)
    output$info <- renderTable(res()$info, digits = 4)
    output$future <- renderTable(res()$future, digits = 2)
    output$table <- renderTable(int_cols(res()$table), digits = 3, na = "")
    output$resid <- renderPlot({
      r <- res(); e <- r$resid[(r$d + 1):length(r$resid)]
      par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
      plot(e, type = "h", col = col_forecast, xlab = "Period", ylab = "Residual",
           main = "Residuals")
      abline(h = 0, col = "grey50")
      stats::acf(e, main = "ACF of residuals", lag.max = min(24, length(e) - 1))
    })
    output$search_ui <- renderUI({
      if (is.null(res()$search_table)) return(NULL)
      tagList(h4("Order search (best first)"), tableOutput(session$ns("search_table")))
    })
    output$search_table <- renderTable(res()$search_table, digits = 3)
  })
}
