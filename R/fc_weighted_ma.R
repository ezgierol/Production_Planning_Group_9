# =============================================================================
# Forecasting > Stationary series > Weighted Moving Average
#   F_t = w_1 D_{t-1} + w_2 D_{t-2} + ... + w_N D_{t-N},  sum(w) = 1
#   (w_1 is the weight of the most recent period)
# =============================================================================

wma_compute <- function(y, w) {
  n <- length(y); N <- length(w)
  f <- rep(NA_real_, n)
  if (N < n) for (t in (N + 1):n) f[t] <- sum(w * y[(t - 1):(t - N)])
  list(fitted = f, next_value = sum(w * y[n:(n - N + 1)]))
}

# All weight vectors of length N on a grid with the given step that sum to 1
wma_weight_grid <- function(N, step) {
  k <- round(1 / step)
  rec <- function(parts, remaining) {
    if (parts == 1) return(matrix(remaining, ncol = 1))
    do.call(rbind, lapply(remaining:0, function(v) cbind(v, rec(parts - 1, remaining - v))))
  }
  g <- rec(N, k) / k
  g[g[, 1] > 0, , drop = FALSE]          # most recent period must have a weight
}

fc_wma_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fc_series_input(ns),
      fc_mode_input(ns, "the weights"),
      conditionalPanel("input.mode == 'manual'", ns = ns,
        textInput(ns("weights"), "Weights, most recent period first",
                  value = "0.5, 0.3, 0.2"),
        helpText("N is the number of weights. Weights that do not sum to 1 are",
                 "normalised.")),
      conditionalPanel("input.mode == 'search'", ns = ns,
        numericInput(ns("N"), "N (number of weights)", value = 3, min = 2, max = 6),
        selectInput(ns("step"), "Grid step for the weights",
                    choices = c("0.1" = 0.1, "0.05" = 0.05), selected = 0.05)),
      fc_horizon_input(ns)
    ),
    mainPanel(fc_output_ui(ns))
  )
}

fc_wma_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    y <- fc_series_server(input, session, data, "stationary")

    res <- reactive({
      y <- y(); n <- length(y)
      search_table <- NULL
      note <- NULL
      if (input$mode == "search") {
        N <- as.integer(input$N)
        validate(need(!is.na(N) && N >= 2 && N <= 6 && N < n, "N must be between 2 and 6."))
        step <- as.numeric(input$step)
        if (N >= 5 && step < 0.1) { step <- 0.1; note <- "Step 0.1 used for N >= 5." }
        grid <- wma_weight_grid(N, step)
        scores <- apply(grid, 1, function(w) criterion_value(y, wma_compute(y, w)$fitted,
                                                             input$criterion))
        ord <- order(scores)
        w <- grid[ord[1], ]
        top <- head(ord, 10)
        search_table <- data.frame(
          Rank = seq_along(top),
          Weights = apply(grid[top, , drop = FALSE], 1, function(v) paste(v, collapse = ", ")),
          Criterion = scores[top])
        names(search_table)[3] <- input$criterion
      } else {
        w <- parse_numbers(input$weights)
        validate(need(length(w) >= 1 && all(!is.na(w)) && all(w >= 0) && sum(w) > 0,
                      "Enter non-negative weights separated by commas."),
                 need(length(w) < n, "Too many weights for this series."))
        if (abs(sum(w) - 1) > 1e-9) note <- "Weights were normalised to sum to 1."
        w <- w / sum(w)
      }
      N <- length(w)
      out <- wma_compute(y, w)
      em <- error_measures(y, out$fitted)
      h <- max(1, input$h)
      list(
        headline = paste0("WMA(", N, ") — next-period forecast: ", fmt_num(out$next_value),
                          if (!is.null(note)) paste0("  (", note, ")") else ""),
        plot_title = paste0("Actual demand and weighted moving average (N = ", N, ")"),
        actual = y, fitted = out$fitted,
        future = data.frame(Period = n + seq_len(h), Forecast = rep(out$next_value, h)),
        errors = error_table(em),
        params = data.frame(Parameter = c("N", paste0("w", seq_len(N), " (t-", seq_len(N), ")")),
                            Value = c(N, w)),
        search_table = search_table,
        table = data.frame(Period = seq_len(n), `Demand (D)` = y,
                           `Forecast (F)` = out$fitted,
                           `Error (e = F - D)` = out$fitted - y, check.names = FALSE)
      )
    })
    fc_render_outputs(output, session, res)
  })
}
