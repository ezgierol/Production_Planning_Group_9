# =============================================================================
# Forecasting > Seasonal series > Seasonal decomposition using moving averages
#   1. Centred moving average (CMA) of length N
#   2. Ratio D_t / CMA_t
#   3. Seasonal factor = average ratio of like seasons, normalised to sum N
#   4. Deseasonalised series D_t / c_t, trend fitted by regression: a + b t
#   5. Forecast = (a + b t) x c_t
# =============================================================================

centered_ma <- function(y, N) {
  w <- if (N %% 2 == 1) rep(1 / N, N) else c(0.5, rep(1, N - 1), 0.5) / N
  as.numeric(stats::filter(y, w, sides = 2))
}

decomp_compute <- function(y, N) {
  n <- length(y)
  season <- ((seq_len(n) - 1) %% N) + 1
  cma <- centered_ma(y, N)
  ratio <- y / cma
  raw <- as.numeric(tapply(ratio, factor(season, levels = 1:N), mean, na.rm = TRUE))
  raw[is.na(raw)] <- 1
  c <- raw * N / sum(raw)
  deseason <- y / c[season]
  t <- seq_len(n)
  fit <- stats::lm(deseason ~ t)
  a <- unname(coef(fit)[1]); b <- unname(coef(fit)[2])
  trend <- a + b * t
  list(season = season, cma = cma, ratio = ratio, raw = raw, factors = c,
       deseason = deseason, a = a, b = b, trend = trend, fitted = trend * c[season])
}

fc_decomp_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fc_series_input(ns),
      fc_mode_input(ns, "the season length N"),
      conditionalPanel("input.mode == 'manual'", ns = ns,
        numericInput(ns("N"), "Season length N", value = 4, min = 2, step = 1)),
      fc_horizon_input(ns, value = 4),
      helpText("The deseasonalised series is forecast with a regression line,",
               "then multiplied by the seasonal factor.")
    ),
    mainPanel(
      fc_output_ui(ns, extra = tagList(
        h4("Seasonal factors"), tableOutput(ns("factors")),
        h4("Deseasonalised series and trend line"), plotOutput(ns("plot2"), height = "260px")))
    )
  )
}

fc_decomp_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    y <- fc_series_server(input, session, data, "seasonal")

    res <- reactive({
      y <- y(); n <- length(y)
      search_plot <- NULL
      if (input$mode == "search") {
        Ns <- 2:max(2, floor(n / 3))
        scores <- vapply(Ns, function(N) criterion_value(y, decomp_compute(y, N)$fitted,
                                                         input$criterion), numeric(1))
        N <- Ns[which.min(scores)]
        search_plot <- data.frame(value = Ns, score = scores, label = Ns,
                                  xlab = "Season length N", crit = input$criterion)
      } else {
        N <- as.integer(input$N)
        validate(need(!is.na(N) && N >= 2 && 2 * N <= n,
                      "N must be at least 2 and the data must cover two seasons."))
      }
      out <- decomp_compute(y, N)
      em <- error_measures(y, out$fitted)
      h <- max(1, input$h)
      tf <- n + seq_len(h)
      fs <- ((tf - 1) %% N) + 1
      fut <- data.frame(Period = tf, Season = fs, `Trend (a + b t)` = out$a + out$b * tf,
                        Factor = out$factors[fs],
                        Forecast = (out$a + out$b * tf) * out$factors[fs], check.names = FALSE)
      out$N <- N
      list(
        headline = paste0("Seasonal decomposition, N = ", N,
                          " — next-period forecast: ", fmt_num(fut$Forecast[1])),
        plot_title = paste0("Actual demand and decomposition forecast (N = ", N, ")"),
        actual = y, fitted = out$fitted, future = fut,
        errors = error_table(em),
        params = data.frame(Parameter = c("Season length N", "Trend intercept a",
                                          "Trend slope b"),
                            Value = c(N, out$a, out$b)),
        search_plot = search_plot, decomp = out,
        table = data.frame(Period = seq_len(n), Season = out$season, `Demand (D)` = y,
                           CMA = out$cma, `D / CMA` = out$ratio,
                           `Factor c` = out$factors[out$season],
                           Deseasonalised = out$deseason, Trend = out$trend,
                           `Forecast (F)` = out$fitted,
                           `Error (e = F - D)` = out$fitted - y, check.names = FALSE)
      )
    })
    fc_render_outputs(output, session, res)
    output$factors <- renderTable({
      d <- res()$decomp
      data.frame(Season = seq_len(d$N), `Average ratio` = d$raw,
                 `Normalised factor c` = d$factors, check.names = FALSE)
    }, digits = 4)
    output$plot2 <- renderPlot({
      d <- res()$decomp
      par(mar = c(4.5, 4.5, 1, 1))
      plot(d$deseason, type = "o", pch = 19, col = col_actual, xlab = "Period",
           ylab = "Deseasonalised demand")
      lines(d$trend, col = col_next, lwd = 2)
      legend("topleft", bty = "n", legend = c("Deseasonalised", "Regression trend"),
             col = c(col_actual, col_next), lty = 1, pch = c(19, NA))
    })
  })
}
