# =============================================================================
# Aggregate Planning - shared pieces: data/cost tab, plan evaluation, outputs
# Notation (Nahmias): D_t demand, n_t working days, K units/worker/day,
# W_t workers, H_t hired, F_t fired, P_t production, I_t ending inventory,
# O_t overtime units, U_t idle (unused) units, S_t subcontracted units.
# =============================================================================

# ---- Example (Densepack, Nahmias) --------------------------------------------------
ap_example_periods <- function() {
  data.frame(Period = c("Jan", "Feb", "Mar", "Apr", "May", "Jun"),
             Demand = c(1280, 640, 900, 1200, 2000, 1400),
             Days = c(20, 24, 18, 26, 22, 15), stringsAsFactors = FALSE)
}

# ---- Data / cost tab ------------------------------------------------------------------
ap_data_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h3("Aggregate planning — data, costs and productivity"),
    p("These inputs are shared by every aggregate planning method."),
    fluidRow(
      column(5,
        wellPanel(
          h4("Data"),
          fileInput(ns("file"), "Upload Excel (.xlsx / .xls) or CSV: period names, demand forecasts, working days",
                    accept = c(".csv", ".txt", ".xlsx", ".xls")),
          uiOutput(ns("mapping_ui")),
          helpText("Double-click a cell to edit it (period names too)."),
          DT::DTOutput(ns("table")),
          br(),
          fluidRow(
            column(6, actionButton(ns("add"), "Add period", width = "100%")),
            column(6, actionButton(ns("remove"), "Remove last", width = "100%"))),
          br(),
          actionButton(ns("example"), "Load Densepack example", width = "100%"),
          br(), br(),
          downloadButton(ns("download"), "Download current data (CSV)")
        )
      ),
      column(7,
        wellPanel(
          h4("Productivity"),
          radioButtons(ns("k_mode"), NULL, inline = TRUE,
                       choices = c("Compute K from past data" = "past", "Enter K directly" = "direct")),
          conditionalPanel("input.k_mode == 'past'", ns = ns,
            fluidRow(
              column(4, numericInput(ns("past_units"), "Units produced", 245, min = 0)),
              column(4, numericInput(ns("past_workers"), "Workers", 76, min = 0)),
              column(4, numericInput(ns("past_days"), "Working days", 22, min = 0)))),
          conditionalPanel("input.k_mode == 'direct'", ns = ns,
            numericInput(ns("K"), "K (units per worker per day)", 0.14653, min = 0, step = 0.0001)),
          textOutput(ns("k_text")),
          checkboxInput(ns("round_kn"), "Round K n_t to 3 decimals (as in Nahmias)", TRUE),
          h4("Start and end"),
          fluidRow(
            column(4, numericInput(ns("W0"), "W0 (workers)", 300, min = 0)),
            column(4, numericInput(ns("I0"), "I0 (start inv.)", 500, min = 0)),
            column(4, numericInput(ns("IT"), "I_T (end inv.)", 600, min = 0))),
          h4("Costs ($)"),
          fluidRow(
            column(4, numericInput(ns("cH"), "Hire c_H (per worker)", 500, min = 0)),
            column(4, numericInput(ns("cF"), "Fire c_F (per worker)", 1000, min = 0)),
            column(4, numericInput(ns("cI"), "Hold c_I (per unit per period)", 80, min = 0))),
          fluidRow(
            column(4, numericInput(ns("cR"), "Regular c_R (per unit)", 0, min = 0)),
            column(4, numericInput(ns("cO"), "Overtime c_O (per unit)", 0, min = 0)),
            column(4, numericInput(ns("cU"), "Idle c_U (per unit)", 0, min = 0))),
          fluidRow(
            column(4, numericInput(ns("cS"), "Subcontract c_S (per unit)", 0, min = 0)))
        )
      )
    ),
    h4("Summary of the inputs"),
    tableOutput(ns("summary"))
  )
}

ap_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    tbl <- reactiveVal(ap_example_periods())
    raw <- reactiveVal(NULL)

    # ---- upload -------------------------------------------------------------------
    observeEvent(input$file, {
      df <- tryCatch(read_uploaded_table(input$file$datapath, input$file$name),
                     error = function(e) {
                       showNotification(paste("Could not read the file:", conditionMessage(e)),
                                        type = "error"); NULL })
      req(df)
      raw(df)
    })
    output$mapping_ui <- renderUI({
      df <- raw(); req(df)
      cols <- names(df); num <- numeric_columns(df)
      txt <- setdiff(cols, num)
      ns <- session$ns
      tagList(
        fluidRow(
          column(4, selectInput(ns("col_period"), "Period names",
                                choices = c("(numbered 1, 2, ...)" = "", cols),
                                selected = guess_column(c(txt, cols), c("period", "month", "name", "donem")))),
          column(4, selectInput(ns("col_demand"), "Demand",
                                choices = num, selected = guess_column(num, c("demand", "forecast", "talep")))),
          column(4, selectInput(ns("col_days"), "Working days",
                                choices = num, selected = guess_column(num, c("day", "gun"), fallback = 2)))
        ))
    })
    observeEvent(list(raw(), input$col_period, input$col_demand, input$col_days), {
      df <- raw(); req(df, input$col_demand, input$col_days)
      req(input$col_demand %in% names(df), input$col_days %in% names(df))
      dem <- to_number(df[[input$col_demand]])
      days <- to_number(df[[input$col_days]])
      keep <- !is.na(dem)
      per <- if (!is.null(input$col_period) && input$col_period %in% names(df))
        as.character(df[[input$col_period]]) else as.character(seq_along(dem))
      tbl(data.frame(Period = per[keep], Demand = dem[keep], Days = days[keep],
                     stringsAsFactors = FALSE))
    }, ignoreInit = TRUE)

    # ---- editing ------------------------------------------------------------------
    output$table <- DT::renderDT({
      DT::datatable(tbl(), rownames = FALSE, selection = "none",
                    editable = list(target = "cell"),
                    class = "compact",
                    options = list(dom = "t", paging = FALSE, ordering = FALSE,
                                   searching = FALSE, scrollX = TRUE))
    })
    observeEvent(input$table_cell_edit, {
      info <- input$table_cell_edit
      df <- tbl()
      r <- info$row; c <- info$col + 1          # DT gives 0-based columns without rownames
      if (c == 1) df[r, 1] <- as.character(info$value) else {
        v <- to_number(info$value)
        if (!is.na(v)) df[r, c] <- v
      }
      tbl(df)
    })
    observeEvent(input$add, {
      df <- tbl()
      df <- rbind(df, data.frame(Period = paste0("P", nrow(df) + 1),
                                 Demand = if (nrow(df)) df$Demand[nrow(df)] else 0,
                                 Days = if (nrow(df)) df$Days[nrow(df)] else 20))
      tbl(df)
    })
    observeEvent(input$remove, { df <- tbl(); if (nrow(df) > 1) tbl(df[-nrow(df), ]) })
    observeEvent(input$example, {
      tbl(ap_example_periods()); raw(NULL)
      updateRadioButtons(session, "k_mode", selected = "past")
      vals <- c(past_units = 245, past_workers = 76, past_days = 22, W0 = 300, I0 = 500,
                IT = 600, cH = 500, cF = 1000, cI = 80, cR = 0, cO = 0, cU = 0, cS = 0)
      for (nm in names(vals)) updateNumericInput(session, nm, value = unname(vals[nm]))
      updateCheckboxInput(session, "round_kn", value = TRUE)
    })
    output$download <- downloadHandler(
      filename = function() "aggregate_planning_data.csv",
      content = function(file) utils::write.csv(tbl(), file, row.names = FALSE))

    # ---- K --------------------------------------------------------------------------
    K <- reactive({
      if (input$k_mode == "past") {
        validate(need(isTRUE(input$past_workers > 0) && isTRUE(input$past_days > 0) &&
                        !is.na(input$past_units), "Enter the past production data."))
        input$past_units / (input$past_workers * input$past_days)
      } else {
        validate(need(isTRUE(input$K > 0), "K must be positive."))
        input$K
      }
    })
    output$k_text <- renderText({
      if (input$k_mode == "past")
        paste0("K = ", input$past_units, " / (", input$past_workers, " × ",
               input$past_days, ") = ", formatC(K(), format = "f", digits = 5),
               " units per worker per day")
      else paste0("K = ", K(), " units per worker per day")
    })

    dat <- reactive({
      df <- tbl()
      validate(need(nrow(df) >= 1, "Enter at least one period."),
               need(all(!is.na(df$Demand)) && all(df$Demand >= 0), "Demand must be non-negative."),
               need(all(!is.na(df$Days)) && all(df$Days > 0), "Working days must be positive."))
      num <- function(x, nm) {
        validate(need(!is.null(x) && !is.na(x) && x >= 0, paste(nm, "must be a non-negative number.")))
        x
      }
      k <- K()
      Kn <- k * df$Days
      if (isTRUE(input$round_kn)) Kn <- round(Kn, 3)
      list(period = as.character(df$Period), D = df$Demand, days = df$Days, T = nrow(df),
           K = k, Kn = Kn,
           W0 = num(input$W0, "W0"), I0 = num(input$I0, "I0"), IT = num(input$IT, "I_T"),
           cost = list(H = num(input$cH, "c_H"), F = num(input$cF, "c_F"),
                       I = num(input$cI, "c_I"), R = num(input$cR, "c_R"),
                       O = num(input$cO, "c_O"), U = num(input$cU, "c_U"),
                       S = num(input$cS, "c_S")))
    })

    output$summary <- renderTable({
      d <- dat()
      nd <- ap_net_demand(d)
      int_cols(data.frame(Period = d$period, Demand = d$D, Days = d$days, `K n_t` = d$Kn,
                 `Net demand` = nd, `Cumulative net demand` = cumsum(nd),
                 `Cumulative K n_t` = cumsum(d$Kn), check.names = FALSE))
    }, digits = 3)

    dat
  })
}

# Net demand: first period minus starting inventory, last period plus ending target
ap_net_demand <- function(d) {
  nd <- d$D
  nd[1] <- nd[1] - d$I0
  nd[d$T] <- nd[d$T] + d$IT
  nd
}

# ---- Plan evaluation ------------------------------------------------------------------
# W: workers, P: in-house production (regular + overtime), O: overtime units,
# S: subcontracted units, U: idle units (computed if NULL).
ap_evaluate <- function(d, W, P, O = 0, S = 0, U = NULL, label = "Plan") {
  T <- d$T
  O <- rep_len(O, T); S <- rep_len(S, T)
  Wprev <- c(d$W0, W[-T])
  H <- pmax(W - Wprev, 0)
  F <- pmax(Wprev - W, 0)
  I <- d$I0 + cumsum(P + S - d$D)
  if (is.null(U)) U <- pmax(d$Kn * W + O - P, 0)
  U[abs(U) < 1e-7] <- 0
  cst <- d$cost
  items <- c(Hiring = cst$H * sum(H), Firing = cst$F * sum(F),
             Holding = cst$I * sum(pmax(I, 0)), `Regular time` = cst$R * sum(P),
             Overtime = cst$O * sum(O), `Idle time` = cst$U * sum(U),
             Subcontracting = cst$S * sum(S))
  plan <- data.frame(Period = d$period, Demand = d$D, Days = d$days, `K n_t` = d$Kn,
                     Workers = W, Hired = H, Fired = F, Production = P,
                     Overtime = O, Idle = U, Subcontract = S, Inventory = I,
                     check.names = FALSE)
  list(label = label, plan = plan, W = W, H = H, F = F, P = P, I = I, O = O, U = U, S = S,
       items = items, total = sum(items),
       shortage = any(I < -1e-6),
       end_ok = I[T] >= d$IT - 1e-6)
}

ap_plan_table <- function(ev) {
  p <- ev$plan
  for (col in c("Overtime", "Idle", "Subcontract"))
    if (all(abs(p[[col]]) < 1e-9)) p[[col]] <- NULL
  tot <- p[1, , drop = FALSE]
  tot[1, ] <- NA
  tot$Period <- "Total"
  for (col in intersect(c("Demand", "Days", "Hired", "Fired", "Production", "Overtime",
                          "Idle", "Subcontract"), names(p))) tot[[col]] <- sum(p[[col]])
  out <- rbind(p, tot)
  for (col in names(out)[-1]) {
    v <- out[[col]]
    out[[col]] <- ifelse(is.na(v), "", fmt_num(v, if (col == "K n_t") 3 else 2))
  }
  out
}

# Right-aligned numeric tables (first column left) and horizontal scrolling
ap_table_css <- function() {
  tags$style(HTML("
    .ap-num table td, .ap-num table th { text-align: right; white-space: nowrap; }
    .ap-num table td:first-child, .ap-num table th:first-child { text-align: left; }
    .ap-scroll { overflow-x: auto; }"))
}

ap_cost_table <- function(a, b = NULL, labels = c("Original", "Whole workers")) {
  keep <- function(ev) names(ev$items)[ev$items != 0]
  show <- union(c("Hiring", "Firing", "Holding"), c(keep(a), if (!is.null(b)) keep(b)))
  show <- names(a$items)[names(a$items) %in% show]
  out <- data.frame(`Cost item` = c(show, "TOTAL"),
                    x = c(a$items[show], a$total), check.names = FALSE)
  names(out)[2] <- if (is.null(b)) "Cost ($)" else paste(labels[1], "($)")
  if (!is.null(b)) {
    out[[paste(labels[2], "($)")]] <- c(b$items[show], b$total)
    out[["Difference ($)"]] <- out[[3]] - out[[2]]
  }
  out
}

# ---- Common output UI / rendering --------------------------------------------------------
ap_output_ui <- function(ns, extra_top = NULL) {
  tagList(
    ap_table_css(),
    div(style = "background:#E8F3E8;border-left:4px solid #2E8B57;padding:10px 14px;margin:6px 0 14px;",
        strong(textOutput(ns("headline"), inline = TRUE))),
    uiOutput(ns("warnings")),
    extra_top,
    uiOutput(ns("which_ui")),
    fluidRow(
      column(6, h4("Total cost"), tableOutput(ns("costs"))),
      column(6, h4("Notes"), uiOutput(ns("notes")))),
    h4("Plan"),
    div(class = "ap-num ap-scroll", tableOutput(ns("plan"))),
    fluidRow(
      column(6, plotOutput(ns("plot_w"), height = "300px")),
      column(6, plotOutput(ns("plot_cum"), height = "300px")))
  )
}

# res() returns list(original = ap_evaluate(...), rounded = ap_evaluate(...) or NULL,
#                    name, notes (character), d (data))
ap_render_outputs <- function(input, output, session, res) {
  ns <- session$ns
  output$headline <- renderText({
    r <- res()
    if (is.null(r$rounded))
      paste0(r$name, " — total cost ", fmt_money(r$original$total))
    else paste0(r$name, " ", fmt_money(r$original$total), " — workers rounded up ",
                fmt_money(r$rounded$total), " (rounding adds ",
                fmt_money(r$rounded$total - r$original$total), ")")
  })
  output$which_ui <- renderUI({
    r <- res()
    if (is.null(r$rounded)) return(NULL)
    radioButtons(ns("which"), "Plan to show", inline = TRUE,
                 choices = c("Original (workers may be fractional)" = "original",
                             "Workers rounded up (whole workers)" = "rounded"),
                 selected = isolate(if (is.null(input$which)) "rounded" else input$which))
  })
  shown <- reactive({
    r <- res()
    if (!is.null(r$rounded) && identical(input$which, "rounded")) r$rounded else
      if (!is.null(r$rounded) && is.null(input$which)) r$rounded else r$original
  })
  output$warnings <- renderUI({
    ev <- shown(); d <- res()$d
    msgs <- character(0)
    if (ev$shortage) msgs <- c(msgs, "Inventory becomes negative in at least one period (shortage).")
    if (!ev$end_ok) msgs <- c(msgs, paste0("Ending inventory is below the target I_T = ", d$IT, "."))
    if (!length(msgs)) return(NULL)
    div(style = "color:#A94442;background:#F2DEDE;padding:8px 12px;margin-bottom:10px;",
        HTML(paste(msgs, collapse = "<br>")))
  })
  output$costs <- renderTable({
    r <- res()
    if (is.null(r$rounded)) ap_cost_table(r$original) else ap_cost_table(r$original, r$rounded)
  }, digits = 2, format.args = list(big.mark = ","))
  output$notes <- renderUI(HTML(paste0("<ul>", paste0("<li>", res()$notes, "</li>", collapse = ""),
                                       "</ul>")))
  output$plan <- renderTable(ap_plan_table(shown()), na = "")
  output$plot_w <- renderPlot({
    r <- res(); d <- r$d
    m <- rbind(r$original$W, if (!is.null(r$rounded)) r$rounded$W)
    par(mar = c(4.5, 4.5, 3, 1))
    bp <- barplot(m, beside = TRUE, names.arg = d$period, border = NA,
                  col = c("#9DB4CF", "#5B7DB1")[seq_len(nrow(m))],
                  ylim = c(0, max(m, d$W0, 1) * 1.35), ylab = "Workers",
                  main = expression("Workforce " * W[t]))
    text(bp, m, labels = round(m, 1), pos = 3, cex = 0.75)
    abline(h = d$W0, lty = 2, col = "grey40")
    legend("top", bty = "n", cex = 0.85, horiz = TRUE,
           legend = c(if (nrow(m) == 2) c("Original", "Whole workers") else "Workers",
                      paste0("W0 = ", d$W0)),
           fill = c(c("#9DB4CF", "#5B7DB1")[seq_len(nrow(m))], NA),
           border = NA, lty = c(rep(NA, nrow(m)), 2), col = c(rep(NA, nrow(m)), "grey40"))
  })
  output$plot_cum <- renderPlot({
    r <- res(); d <- r$d
    cnd <- cumsum(ap_net_demand(d))
    c1 <- cumsum(r$original$P + r$original$S)
    c2 <- if (!is.null(r$rounded)) cumsum(r$rounded$P + r$rounded$S) else NULL
    T <- d$T
    par(mar = c(4.5, 4.5, 3, 1))
    plot(seq_len(T), cnd, type = "o", pch = 19, col = col_actual, lwd = 2, xaxt = "n",
         ylim = range(0, cnd, c1, c2), xlab = "Period", ylab = "Units",
         main = "Cumulative supply vs. cumulative net demand")
    axis(1, at = seq_len(T), labels = d$period)
    grid(col = "#E5E5E5")
    lines(seq_len(T), c1, type = "o", pch = 17, lty = 2, col = col_forecast, lwd = 2)
    if (!is.null(c2)) lines(seq_len(T), c2, type = "o", pch = 15, lty = 3, col = col_next, lwd = 2)
    legend("topleft", bty = "n", cex = 0.85,
           legend = c("Cumulative net demand", "Cumulative production (original)",
                      if (!is.null(c2)) "Cumulative production (whole workers)"),
           col = c(col_actual, col_forecast, if (!is.null(c2)) col_next),
           lty = c(1, 2, if (!is.null(c2)) 3), pch = c(19, 17, if (!is.null(c2)) 15))
  })
  shown
}

# ---- Menu and server wiring (called from app.R) ----------------------------------------
aggregate_planning_menu <- function() {
  navbarMenu(
    "Aggregate Planning",
    tabPanel("Data, costs and productivity", value = "ap_data_tab", ap_data_ui("ap_data")),
    "----",
    "Heuristic plans",
    tabPanel("Zero-inventory (chase) plan", value = "ap_chase_tab", ap_chase_ui("ap_chase")),
    tabPanel("Constant-workforce (level) plan", value = "ap_level_tab", ap_level_ui("ap_level")),
    tabPanel("Mixed plan", value = "ap_mixed_tab", ap_mixed_ui("ap_mixed")),
    "----",
    "Optimisation",
    tabPanel("Linear programming", value = "ap_lp_tab", ap_lp_ui("ap_lp")),
    tabPanel("Transportation method", value = "ap_trans_tab", ap_trans_ui("ap_trans")),
    "----",
    "Disaggregation",
    tabPanel("Square-root rule (product families)", value = "ap_disagg_tab",
             ap_disagg_ui("ap_disagg"))
  )
}

aggregate_planning_server <- function() {
  d <- ap_data_server("ap_data")
  chase <- ap_chase_server("ap_chase", d)
  level <- ap_level_server("ap_level", d)
  mixed <- ap_mixed_server("ap_mixed", d)
  lp    <- ap_lp_server("ap_lp", d)
  trans <- ap_trans_server("ap_trans", d)
  ap_disagg_server("ap_disagg", d,
                   plans = list(`Chase plan` = chase, `Level plan` = level,
                                `Mixed plan` = mixed, `Linear programming` = lp,
                                `Transportation (optimal)` = trans))
}
