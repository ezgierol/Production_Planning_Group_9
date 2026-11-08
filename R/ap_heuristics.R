# =============================================================================
# Aggregate Planning > Heuristic plans
#   * Zero-inventory (chase) plan: production follows net demand every period
#   * Constant-workforce (level) plan: one workforce for the whole horizon,
#     the smallest one that never causes a shortage
#   * Mixed plan: workforce changes only in the periods the user selects
# The original plan may use fractional workers; the "whole workers" plan rounds
# every W_t up, produces K n_t W_t and carries the extra units as inventory.
# =============================================================================

# ---- Chase -------------------------------------------------------------------------
ap_chase_compute <- function(d) {
  T <- d$T
  W <- numeric(T)
  inv <- d$I0
  for (t in seq_len(T)) {
    need <- d$D[t] + (if (t == T) d$IT else 0) - inv
    W[t] <- max(0, need) / d$Kn[t]
    inv <- inv + d$Kn[t] * W[t] - d$D[t]
  }
  orig <- ap_evaluate(d, W, d$Kn * W, label = "Original")
  Wr <- ceiling(W - 1e-9)
  rnd <- if (all(abs(W - Wr) < 1e-9)) NULL else
    ap_evaluate(d, Wr, round(d$Kn * Wr), U = 0, label = "Whole workers")
  list(original = orig, rounded = rnd)
}

# ---- Level ------------------------------------------------------------------------
ap_level_compute <- function(d) {
  ratio <- cumsum(ap_net_demand(d)) / cumsum(d$Kn)
  W <- max(0, ratio)
  crit <- which.max(ratio)
  Wv <- rep(W, d$T)
  orig <- ap_evaluate(d, Wv, d$Kn * Wv, label = "Original")
  Wr <- rep(ceiling(W - 1e-9), d$T)
  rnd <- if (abs(W - Wr[1]) < 1e-9) NULL else
    ap_evaluate(d, Wr, round(d$Kn * Wr), U = 0, label = "Whole workers")
  list(original = orig, rounded = rnd, ratio = ratio, critical = crit, W = W)
}

# ---- Mixed ------------------------------------------------------------------------
ap_mixed_compute <- function(d, change) {
  T <- d$T
  change <- sort(unique(change[change >= 1 & change <= T]))
  starts <- sort(unique(c(1, change)))
  W <- numeric(T)
  inv <- d$I0
  seg_info <- list()
  for (k in seq_along(starts)) {
    s <- starts[k]
    e <- if (k < length(starts)) starts[k + 1] - 1 else T
    idx <- s:e
    if (s == 1 && !(1 %in% change)) {
      w <- d$W0                              # first segment keeps the current workforce
    } else {
      need <- d$D[idx] + ifelse(idx == T, d$IT, 0)
      w <- max(0, max((cumsum(need) - inv) / cumsum(d$Kn[idx])))
    }
    W[idx] <- w
    inv <- inv + sum(d$Kn[idx] * w - d$D[idx])
    seg_info[[k]] <- data.frame(Segment = k, Periods = paste0(d$period[s], "–", d$period[e]),
                                Workers = w)
  }
  orig <- ap_evaluate(d, W, d$Kn * W, label = "Original")
  Wr <- ceiling(W - 1e-9)
  rnd <- if (all(abs(W - Wr) < 1e-9)) NULL else
    ap_evaluate(d, Wr, round(d$Kn * Wr), U = 0, label = "Whole workers")
  list(original = orig, rounded = rnd, segments = do.call(rbind, seg_info))
}

# ---- UI / servers -------------------------------------------------------------------
ap_heuristic_note <- function() {
  helpText("Data, costs and productivity are entered on Aggregate Planning →",
           "Data, costs and productivity.")
}

ap_chase_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h3("Zero-inventory (chase) plan"),
    ap_heuristic_note(),
    ap_output_ui(ns)
  )
}

ap_chase_server <- function(id, d) {
  moduleServer(id, function(input, output, session) {
    res <- reactive({
      d <- d()
      r <- ap_chase_compute(d)
      r$d <- d
      r$name <- "Zero-inventory plan"
      r$notes <- c(
        "Workers each period: W_t = (net demand_t) / (K n_t); net demand uses I0 in the first period and adds the target I_T in the last period.",
        "Original plan: no inventory is carried (except the ending target).",
        "Whole-workers plan: W_t is rounded up, production = K n_t W_t (whole units), the extra units are carried as inventory.")
      r
    })
    ap_render_outputs(input, output, session, res)
    res
  })
}

ap_level_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h3("Constant-workforce (level) plan"),
    ap_heuristic_note(),
    ap_output_ui(ns, extra_top = tagList(
      h4("Minimum constant workforce"),
      tableOutput(ns("ratio"))))
  )
}

ap_level_server <- function(id, d) {
  moduleServer(id, function(input, output, session) {
    res <- reactive({
      d <- d()
      r <- ap_level_compute(d)
      r$d <- d
      r$name <- "Constant-workforce plan"
      r$notes <- c(
        "W = max over t of (cumulative net demand_t) / (cumulative K n_t), so that there is never a shortage.",
        paste0("The binding period is ", d$period[r$critical], " (W = ", fmt_num(r$W, 3), ")."),
        "Hiring or firing only happens in the first period (from W0 to W).")
      r
    })
    output$ratio <- renderTable({
      r <- res(); d <- r$d
      data.frame(Period = d$period, `Cumulative net demand` = cumsum(ap_net_demand(d)),
                 `Cumulative K n_t` = cumsum(d$Kn), Ratio = r$ratio,
                 Binding = ifelse(seq_len(d$T) == r$critical, "← max", ""),
                 check.names = FALSE)
    }, digits = 3)
    ap_render_outputs(input, output, session, res)
    res
  })
}

ap_mixed_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h3("Mixed plan"),
    ap_heuristic_note(),
    wellPanel(
      checkboxGroupInput(ns("change"), "Periods in which the workforce may change",
                         choices = NULL, inline = TRUE),
      helpText("Between two selected periods the workforce stays constant at the smallest",
               "level that avoids shortages. If the first period is not selected, the",
               "workforce stays at W0 until the first selected period.")
    ),
    ap_output_ui(ns, extra_top = tagList(h4("Workforce segments"), tableOutput(ns("segments"))))
  )
}

ap_mixed_server <- function(id, d) {
  moduleServer(id, function(input, output, session) {
    observe({
      dd <- d()
      cur <- isolate(input$change)
      sel <- if (length(cur)) intersect(cur, as.character(seq_len(dd$T))) else
        as.character(unique(c(1, ceiling(dd$T / 2) + 1)))
      sel <- sel[as.integer(sel) <= dd$T]
      updateCheckboxGroupInput(session, "change", choiceNames = dd$period,
                               choiceValues = as.character(seq_len(dd$T)), selected = sel,
                               inline = TRUE)
    })
    res <- reactive({
      d <- d()
      ch <- as.integer(input$change)
      r <- ap_mixed_compute(d, ch)
      r$d <- d
      r$name <- "Mixed plan"
      r$notes <- c(
        paste0("Workforce changes in: ", if (length(ch)) paste(d$period[sort(ch)], collapse = ", ") else "(none)", "."),
        "Within a segment: W = max over the segment of (cumulative net demand - inventory at the start of the segment) / (cumulative K n_t).",
        "Whole-workers plan: each segment's workforce is rounded up.")
      r
    })
    output$segments <- renderTable({
      r <- res()
      s <- r$segments
      if (!is.null(r$rounded)) s$`Whole workers` <- ceiling(s$Workers - 1e-9)
      s
    }, digits = 3)
    ap_render_outputs(input, output, session, res)
    res
  })
}
