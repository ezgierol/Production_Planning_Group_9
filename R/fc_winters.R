# =============================================================================
# Forecasting > Seasonal series > Winters' method (triple exponential smoothing)
#   S_t = alpha (D_t / c_{t-N}) + (1 - alpha)(S_{t-1} + G_{t-1})
#   G_t = beta (S_t - S_{t-1}) + (1 - beta) G_{t-1}
#   c_t = gamma (D_t / S_t) + (1 - gamma) c_{t-N}
#   F_{t, t+tau} = (S_t + tau G_t) c_{t+tau-N}
# Initial values: regression D_t = a + b t on the first m periods gives S0 = a,
# G0 = b; initial factors = average of D_t / (a + b t) for like seasons
# (normalised to sum N). Level/slope (and factors) can also be entered manually.
# =============================================================================

winters_initial_factors <- function(y, N, S0, G0, m) {
  m <- max(N, min(m, length(y)))
  t <- seq_len(m)
  ratio <- y[t] / (S0 + G0 * t)
  season <- ((t - 1) %% N) + 1
  c <- as.numeric(tapply(ratio, factor(season, levels = 1:N), mean))
  c[is.na(c)] <- 1
  c * N / sum(c)
}

winters_compute <- function(y, N, alpha, beta, gamma, S0, G0, c0) {
  n <- length(y)
  S <- numeric(n); G <- numeric(n); C <- numeric(n); f <- numeric(n)
  s <- S0; g <- G0; c <- c0
  for (t in seq_len(n)) {
    j <- ((t - 1) %% N) + 1
    f[t] <- (s + g) * c[j]
    s_new <- alpha * y[t] / c[j] + (1 - alpha) * (s + g)
    g <- beta * (s_new - s) + (1 - beta) * g
    s <- s_new
    c[j] <- gamma * y[t] / s + (1 - gamma) * c[j]
    S[t] <- s; G[t] <- g; C[t] <- c[j]
  }
  list(fitted = f, S = S, G = G, C = C, c_last = c)
}

fc_winters_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fc_series_input(ns),
      numericInput(ns("N"), "Season length N", value = 4, min = 2, step = 1),
      fc_mode_input(ns, "alpha, beta and gamma"),
      conditionalPanel("input.mode == 'manual'", ns = ns,
        sliderInput(ns("alpha"), "alpha (level)", min = 0.01, max = 1, value = 0.2, step = 0.01),
        sliderInput(ns("beta"), "beta (trend)", min = 0.01, max = 1, value = 0.1, step = 0.01),
        sliderInput(ns("gamma"), "gamma (seasonal)", min = 0.01, max = 1, value = 0.1, step = 0.01)),
      conditionalPanel("input.mode == 'search'", ns = ns,
        selectInput(ns("step"), "Grid step", choices = c("0.1" = 0.1, "0.05" = 0.05),
                    selected = 0.05)),
      fc_init_input(ns, slope = TRUE,
                    m_help = "Empty = first two seasons (2N periods)."),
      conditionalPanel("input.init == 'manual'", ns = ns,
        textInput(ns("c0"), "Initial seasonal factors (optional, comma separated)", ""),
        helpText("Leave empty to compute them as D_t / (S0 + G0 t) over the first 2N periods.")),
      fc_horizon_input(ns, value = 4)
    ),
    mainPanel(fc_output_ui(ns, extra = tagList(h4("Seasonal factors"),
                                               tableOutput(ns("factors")))))
  )
}

fc_winters_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    y <- fc_series_server(input, session, data, "seasonal")

    res <- reactive({
      y <- y(); n <- length(y)
      N <- as.integer(input$N)
      validate(need(!is.na(N) && N >= 2 && 2 * N <= n,
                    "N must be at least 2 and the data must cover two seasons."))
      init <- fc_get_init(input, y, slope = TRUE, default_m = 2 * N)
      m_used <- if (input$init == "regression") init$m else 2 * N
      c0 <- winters_initial_factors(y, N, init$S0, init$G0, m_used)
      c_how <- paste0("D / (S0 + G0 t), periods 1–", max(N, min(m_used, n)))
      if (input$init == "manual") {
        cm <- parse_numbers(input$c0)
        if (length(cm) > 0) {
          validate(need(length(cm) == N && all(!is.na(cm)) && all(cm > 0),
                        paste("Enter exactly", N, "positive seasonal factors.")))
          c0 <- cm; c_how <- "Manual"
        }
      }
      search_table <- NULL
      if (input$mode == "search") {
        step <- as.numeric(input$step)
        vals <- seq(step, 1, by = step)
        grid <- expand.grid(alpha = vals, beta = vals, gamma = vals)
        scores <- mapply(function(a, b, g) criterion_value(
          y, winters_compute(y, N, a, b, g, init$S0, init$G0, c0)$fitted, input$criterion),
          grid$alpha, grid$beta, grid$gamma)
        ord <- order(scores)
        alpha <- grid$alpha[ord[1]]; beta <- grid$beta[ord[1]]; gamma <- grid$gamma[ord[1]]
        top <- head(ord, 10)
        search_table <- data.frame(Rank = seq_along(top), alpha = grid$alpha[top],
                                   beta = grid$beta[top], gamma = grid$gamma[top],
                                   Criterion = scores[top])
        names(search_table)[5] <- input$criterion
      } else {
        alpha <- input$alpha; beta <- input$beta; gamma <- input$gamma
      }
      out <- winters_compute(y, N, alpha, beta, gamma, init$S0, init$G0, c0)
      em <- error_measures(y, out$fitted)
      h <- max(1, input$h)
      tau <- seq_len(h)
      fs <- ((n + tau - 1) %% N) + 1
      fut <- data.frame(Period = n + tau, tau = tau, Season = fs,
                        Forecast = (out$S[n] + tau * out$G[n]) * out$c_last[fs])
      list(
        headline = paste0("Winters' method (alpha = ", alpha, ", beta = ", beta,
                          ", gamma = ", gamma, ") — next-period forecast: ",
                          fmt_num(fut$Forecast[1])),
        plot_title = paste0("Actual demand and Winters' forecast (N = ", N, ")"),
        actual = y, fitted = out$fitted, future = fut,
        errors = error_table(em),
        params = data.frame(
          Parameter = c("N", "alpha", "beta", "gamma", "S0", "G0", "Level/slope from",
                        "Initial factors from"),
          Value = c(N, alpha, beta, gamma, fmt_num(init$S0, 3), fmt_num(init$G0, 3),
                    init$how, c_how)),
        search_table = search_table,
        factors = data.frame(Season = seq_len(N), `Initial c` = c0,
                             `Latest c` = out$c_last, check.names = FALSE),
        table = data.frame(Period = seq_len(n), Season = ((seq_len(n) - 1) %% N) + 1,
                           `Demand (D)` = y, `Forecast (F)` = out$fitted,
                           `Level S[t]` = out$S, `Trend G[t]` = out$G,
                           `Factor c[t]` = out$C,
                           `Error (e = F - D)` = out$fitted - y, check.names = FALSE)
      )
    })
    fc_render_outputs(output, session, res)
    output$factors <- renderTable(res()$factors, digits = 4)
  })
}
