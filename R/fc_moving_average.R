# =============================================================================
# Forecasting > Stationary series > Moving Average  MA(N)
#   F_t = (D_{t-1} + ... + D_{t-N}) / N
# =============================================================================

ma_compute <- function(y, N) {
  n <- length(y)
  f <- rep(NA_real_, n)
  if (N < n) for (t in (N + 1):n) f[t] <- mean(y[(t - N):(t - 1)])
  list(fitted = f, next_value = mean(y[(n - N + 1):n]))
}

fc_ma_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fc_series_input(ns),
      fc_mode_input(ns, "N"),
      conditionalPanel("input.mode == 'manual'", ns = ns,
        numericInput(ns("N"), "N (number of past periods)", value = 3, min = 1, step = 1)),
      conditionalPanel("input.mode == 'search'", ns = ns,
        numericInput(ns("maxN"), "Largest N to try", value = 12, min = 2, step = 1)),
      fc_horizon_input(ns)
    ),
    mainPanel(fc_output_ui(ns))
  )
}

fc_ma_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    y <- fc_series_server(input, session, data, "stationary")

    res <- reactive({
      y <- y(); n <- length(y)
      search_plot <- NULL
      if (input$mode == "search") {
        maxN <- max(1, min(n - 1, input$maxN))
        Ns <- 1:maxN
        scores <- vapply(Ns, function(N) criterion_value(y, ma_compute(y, N)$fitted,
                                                         input$criterion), numeric(1))
        N <- Ns[which.min(scores)]
        search_plot <- data.frame(value = Ns, score = scores, label = Ns,
                                  xlab = "N", crit = input$criterion)
      } else {
        N <- as.integer(input$N)
        validate(need(!is.na(N) && N >= 1 && N < n, "N must be between 1 and n - 1."))
      }
      out <- ma_compute(y, N)
      em <- error_measures(y, out$fitted)
      h <- max(1, input$h)
      list(
        headline = paste0("MA(", N, ") — next-period forecast: ",
                          fmt_num(out$next_value)),
        plot_title = paste0("Actual demand and MA(", N, ") forecast"),
        actual = y, fitted = out$fitted,
        future = data.frame(Period = n + seq_len(h), Forecast = rep(out$next_value, h)),
        errors = error_table(em),
        params = data.frame(Parameter = "N", Value = N),
        search_plot = search_plot,
        table = data.frame(Period = seq_len(n), `Demand (D)` = y,
                           `Forecast (F)` = out$fitted,
                           `Error (e = F - D)` = out$fitted - y, check.names = FALSE)
      )
    })
    fc_render_outputs(output, session, res)
  })
}
