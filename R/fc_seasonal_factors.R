# =============================================================================
# Forecasting > Stationary series > Seasonal factors ("quick and dirty" method)
#   1. Compute the overall sample mean of the data
#   2. Divide each observation by the mean
#   3. Average the ratios of like seasons -> seasonal factors c_1..c_N (sum = N)
#   Forecast for season j = mean x c_j
# =============================================================================

sf_compute <- function(y, N) {
  n <- length(y)
  avg <- mean(y)
  season <- ((seq_len(n) - 1) %% N) + 1
  ratio <- y / avg
  c <- tapply(ratio, factor(season, levels = 1:N), mean)
  c <- as.numeric(c)
  c[is.na(c)] <- 1
  fitted <- avg * c[season]
  list(mean = avg, factors = c, season = season, ratio = ratio, fitted = fitted)
}

fc_sf_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fc_series_input(ns),
      fc_mode_input(ns, "the season length N"),
      conditionalPanel("input.mode == 'manual'", ns = ns,
        numericInput(ns("N"), "Season length N (periods per cycle)", value = 4,
                     min = 2, step = 1)),
      fc_horizon_input(ns, value = 4),
      helpText("Errors are measured between the data and mean × seasonal factor.")
    ),
    mainPanel(fc_output_ui(ns, extra = tagList(h4("Seasonal factors"),
                                               tableOutput(ns("factors")))))
  )
}

fc_sf_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    y <- fc_series_server(input, session, data, "seasonal")

    res <- reactive({
      y <- y(); n <- length(y)
      search_plot <- NULL
      if (input$mode == "search") {
        Ns <- 2:max(2, floor(n / 2))
        scores <- vapply(Ns, function(N) criterion_value(y, sf_compute(y, N)$fitted,
                                                         input$criterion), numeric(1))
        N <- Ns[which.min(scores)]
        search_plot <- data.frame(value = Ns, score = scores, label = Ns,
                                  xlab = "Season length N", crit = input$criterion)
      } else {
        N <- as.integer(input$N)
        validate(need(!is.na(N) && N >= 2 && N <= n / 2,
                      "N must be at least 2 and the data must cover two seasons."))
      }
      out <- sf_compute(y, N)
      em <- error_measures(y, out$fitted)
      h <- max(1, input$h)
      fut_season <- ((n + seq_len(h) - 1) %% N) + 1
      fut <- data.frame(Period = n + seq_len(h), Season = fut_season,
                        Forecast = out$mean * out$factors[fut_season])
      list(
        headline = paste0("Seasonal factors, N = ", N, " — mean = ", fmt_num(out$mean),
                          ", next-period forecast: ", fmt_num(fut$Forecast[1])),
        plot_title = paste0("Actual demand and mean × seasonal factor (N = ", N, ")"),
        actual = y, fitted = out$fitted, future = fut,
        errors = error_table(em),
        params = data.frame(Parameter = c("Season length N", "Overall mean"),
                            Value = c(N, out$mean)),
        search_plot = search_plot,
        factors = data.frame(Season = 1:N, `Seasonal factor c` = out$factors,
                             check.names = FALSE),
        table = data.frame(Period = seq_len(n), Season = out$season, `Demand (D)` = y,
                           `D / mean` = out$ratio,
                           `Forecast (F = mean x c)` = out$fitted,
                           `Error (e = F - D)` = out$fitted - y, check.names = FALSE)
      )
    })
    fc_render_outputs(output, session, res)
    output$factors <- renderTable({
      f <- res()$factors
      rbind(f, data.frame(Season = "Sum", `Seasonal factor c` = sum(f[[2]]), check.names = FALSE))
    }, digits = 4)
  })
}
