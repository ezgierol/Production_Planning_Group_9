# =============================================================================
# Forecasting > Stationary series > Simple Exponential Smoothing
#   S_t = alpha D_t + (1 - alpha) S_{t-1},   F_{t+1} = S_t
# Initial value S_0: from a regression D_t = a + b t (S_0 = a) or entered manually
# =============================================================================

# Initial value inputs shared by SES, Holt and Winters
fc_init_input <- function(ns, slope = TRUE, default_m = NA, m_help = NULL) {
  tagList(
    radioButtons(ns("init"), "Initial values",
                 choices = c("Fit a regression (D_t = a + b t)" = "regression",
                             "Enter manually" = "manual")),
    conditionalPanel("input.init == 'regression'", ns = ns,
      numericInput(ns("m"), "Periods used for the regression (empty = all)",
                   value = default_m, min = 2, step = 1),
      if (!is.null(m_help)) helpText(m_help)),
    conditionalPanel("input.init == 'manual'", ns = ns,
      numericInput(ns("S0"), "Initial level S0", value = NA),
      if (slope) numericInput(ns("G0"), "Initial slope G0", value = NA))
  )
}

# Returns list(S0, G0, text) from the initial value inputs
fc_get_init <- function(input, y, slope = TRUE, default_m = length(y)) {
  if (input$init == "manual") {
    validate(need(!is.na(input$S0), "Enter the initial level S0."))
    G0 <- if (slope) input$G0 else 0
    if (slope) validate(need(!is.na(G0), "Enter the initial slope G0."))
    list(S0 = input$S0, G0 = G0, how = "Manual")
  } else {
    m <- if (is.null(input$m) || is.na(input$m)) default_m else input$m
    r <- regression_init(y, m)
    list(S0 = r$a, G0 = r$b, m = r$m,
         how = paste0("Regression on periods 1–", r$m,
                      ": D = ", fmt_num(r$a, 3), " + ", fmt_num(r$b, 3), " t"))
  }
}

ses_compute <- function(y, alpha, S0) {
  n <- length(y)
  S <- numeric(n); f <- numeric(n)
  prev <- S0
  for (t in seq_len(n)) {
    f[t] <- prev
    S[t] <- alpha * y[t] + (1 - alpha) * prev
    prev <- S[t]
  }
  list(fitted = f, S = S, next_value = S[n])
}

fc_ses_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fc_series_input(ns),
      fc_mode_input(ns, "alpha"),
      conditionalPanel("input.mode == 'manual'", ns = ns,
        sliderInput(ns("alpha"), "Smoothing constant alpha", min = 0.01, max = 1,
                    value = 0.2, step = 0.01)),
      fc_init_input(ns, slope = FALSE,
                    m_help = "S0 = a (the regression line at t = 0)."),
      fc_horizon_input(ns)
    ),
    mainPanel(fc_output_ui(ns))
  )
}

fc_ses_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    y <- fc_series_server(input, session, data, "stationary")

    res <- reactive({
      y <- y(); n <- length(y)
      init <- fc_get_init(input, y, slope = FALSE)
      search_plot <- NULL
      if (input$mode == "search") {
        alphas <- seq(0.01, 1, by = 0.01)
        scores <- vapply(alphas, function(a) criterion_value(y, ses_compute(y, a, init$S0)$fitted,
                                                             input$criterion), numeric(1))
        alpha <- alphas[which.min(scores)]
        search_plot <- data.frame(value = alphas, score = scores, label = alphas,
                                  xlab = "alpha", crit = input$criterion)
      } else alpha <- input$alpha
      out <- ses_compute(y, alpha, init$S0)
      em <- error_measures(y, out$fitted)
      h <- max(1, input$h)
      list(
        headline = paste0("Simple exponential smoothing (alpha = ", alpha,
                          ") — next-period forecast: ", fmt_num(out$next_value)),
        plot_title = paste0("Actual demand and SES forecast (alpha = ", alpha, ")"),
        actual = y, fitted = out$fitted,
        future = data.frame(Period = n + seq_len(h), Forecast = rep(out$next_value, h)),
        errors = error_table(em),
        params = data.frame(Parameter = c("alpha", "S0", "Initial value from"),
                            Value = c(fmt_num(alpha, 2), fmt_num(init$S0, 3), init$how)),
        search_plot = search_plot,
        table = data.frame(Period = seq_len(n), `Demand (D)` = y,
                           `Forecast (F = S[t-1])` = out$fitted,
                           `Smoothed S[t]` = out$S,
                           `Error (e = F - D)` = out$fitted - y, check.names = FALSE)
      )
    })
    fc_render_outputs(output, session, res)
  })
}
