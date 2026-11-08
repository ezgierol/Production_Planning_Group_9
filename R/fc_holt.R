# =============================================================================
# Forecasting > Trend series > Holt's method (double exponential smoothing)
#   S_t = alpha D_t + (1 - alpha)(S_{t-1} + G_{t-1})
#   G_t = beta (S_t - S_{t-1}) + (1 - beta) G_{t-1}
#   F_{t, t+tau} = S_t + tau G_t
# =============================================================================

holt_compute <- function(y, alpha, beta, S0, G0) {
  n <- length(y)
  S <- numeric(n); G <- numeric(n); f <- numeric(n)
  s <- S0; g <- G0
  for (t in seq_len(n)) {
    f[t] <- s + g
    s_new <- alpha * y[t] + (1 - alpha) * (s + g)
    g <- beta * (s_new - s) + (1 - beta) * g
    s <- s_new
    S[t] <- s; G[t] <- g
  }
  list(fitted = f, S = S, G = G)
}

fc_holt_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fc_series_input(ns),
      fc_mode_input(ns, "alpha and beta"),
      conditionalPanel("input.mode == 'manual'", ns = ns,
        sliderInput(ns("alpha"), "alpha (level)", min = 0.01, max = 1, value = 0.1, step = 0.01),
        sliderInput(ns("beta"), "beta (trend)", min = 0.01, max = 1, value = 0.1, step = 0.01)),
      conditionalPanel("input.mode == 'search'", ns = ns,
        selectInput(ns("step"), "Grid step", choices = c("0.05" = 0.05, "0.02" = 0.02,
                                                         "0.01" = 0.01), selected = 0.05)),
      fc_init_input(ns, slope = TRUE,
                    m_help = "S0 = a (level at t = 0), G0 = b (slope)."),
      fc_horizon_input(ns, value = 4)
    ),
    mainPanel(fc_output_ui(ns))
  )
}

fc_holt_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    y <- fc_series_server(input, session, data, "trend")

    res <- reactive({
      y <- y(); n <- length(y)
      init <- fc_get_init(input, y, slope = TRUE)
      search_table <- NULL
      if (input$mode == "search") {
        step <- as.numeric(input$step)
        vals <- seq(step, 1, by = step)
        grid <- expand.grid(alpha = vals, beta = vals)
        scores <- mapply(function(a, b) criterion_value(
          y, holt_compute(y, a, b, init$S0, init$G0)$fitted, input$criterion),
          grid$alpha, grid$beta)
        ord <- order(scores)
        alpha <- grid$alpha[ord[1]]; beta <- grid$beta[ord[1]]
        top <- head(ord, 10)
        search_table <- data.frame(Rank = seq_along(top), alpha = grid$alpha[top],
                                   beta = grid$beta[top], Criterion = scores[top])
        names(search_table)[4] <- input$criterion
      } else {
        alpha <- input$alpha; beta <- input$beta
      }
      out <- holt_compute(y, alpha, beta, init$S0, init$G0)
      em <- error_measures(y, out$fitted)
      h <- max(1, input$h)
      fut <- data.frame(Period = n + seq_len(h), tau = seq_len(h),
                        Forecast = out$S[n] + seq_len(h) * out$G[n])
      list(
        headline = paste0("Holt's method (alpha = ", alpha, ", beta = ", beta,
                          ") — next-period forecast: ", fmt_num(fut$Forecast[1])),
        plot_title = "Actual demand and Holt's forecast",
        actual = y, fitted = out$fitted, future = fut,
        errors = error_table(em),
        params = data.frame(Parameter = c("alpha", "beta", "S0", "G0", "Initial values from"),
                            Value = c(alpha, beta, fmt_num(init$S0, 3), fmt_num(init$G0, 3),
                                      init$how)),
        search_table = search_table,
        table = data.frame(Period = seq_len(n), `Demand (D)` = y,
                           `Forecast (F = S[t-1] + G[t-1])` = out$fitted,
                           `Level S[t]` = out$S, `Trend G[t]` = out$G,
                           `Error (e = F - D)` = out$fitted - y, check.names = FALSE)
      )
    })
    fc_render_outputs(output, session, res)
  })
}
