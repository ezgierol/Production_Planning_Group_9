# =============================================================================
# Aggregate Planning > Transportation method
# Sources : initial inventory, and regular time (RT), overtime (OT) and
#           subcontracting (SC) of every period
# Columns : demand of every period, the ending-inventory target, excess capacity
# Unit cost of a unit made in period i for period j (j >= i):
#     RT: c_R + c_I (j - i)    OT: c_R + c_O + c_I (j - i)    SC: c_S + c_I (j - i)
# Production after the period of use (backorders) is not allowed.
# Starting solution: north-west corner. Optimal: cheapest cell first
# (checked against lpSolve::lp.transport).
# =============================================================================

ap_trans_problem <- function(d, cap) {
  T <- d$T; cst <- d$cost
  src <- data.frame(label = character(0), period = integer(0), type = character(0),
                    supply = numeric(0), base = numeric(0), stringsAsFactors = FALSE)
  if (d$I0 > 0) src <- rbind(src, data.frame(label = "Initial inventory", period = 1L,
                                             type = "I0", supply = d$I0, base = 0))
  for (t in seq_len(T)) {
    rows <- data.frame(label = paste(d$period[t], c("RT", "OT", "SC")), period = t,
                       type = c("RT", "OT", "SC"),
                       supply = c(d$Kn[t] * cap$Workers[t], cap$Overtime[t], cap$Subcontract[t]),
                       base = c(cst$R, cst$R + cst$O, cst$S))
    src <- rbind(src, rows[rows$supply > 1e-9, ])
  }
  dem_lab <- c(d$period, if (d$IT > 0) "End inv.")
  dem <- c(d$D, if (d$IT > 0) d$IT)
  dem_per <- c(seq_len(T), if (d$IT > 0) T + 1)       # T + 1 = held to the end
  C <- outer(seq_len(nrow(src)), seq_along(dem), function(i, j) {
    p <- src$period[i]; q <- dem_per[j]
    ifelse(p > pmin(q, T), Inf, src$base[i] + cst$I * (q - p))
  })
  list(src = src, dem = dem, dem_lab = dem_lab, C = C,
       excess = sum(src$supply) - sum(dem))
}

ap_trans_feasible <- function(pr, d) {
  if (pr$excess < -1e-6) return("Total capacity is smaller than total demand plus the ending inventory target.")
  for (t in seq_len(d$T)) {
    s <- sum(pr$src$supply[pr$src$period <= t])
    need <- sum(pr$dem[seq_len(t)])
    if (s < need - 1e-6) return(paste0("Not enough capacity up to ", d$period[t],
                                       " (backorders are not allowed)."))
  }
  NULL
}

ap_trans_nw <- function(pr) {
  s <- pr$src$supply; dm <- c(pr$dem, max(0, pr$excess))
  X <- matrix(0, length(s), length(dm))
  i <- 1; j <- 1
  while (i <= length(s) && j <= length(dm)) {
    q <- min(s[i], dm[j])
    X[i, j] <- X[i, j] + q
    s[i] <- s[i] - q; dm[j] <- dm[j] - q
    if (s[i] <= 1e-9) i <- i + 1 else j <- j + 1
  }
  X
}

# Cheapest cell first, period by period: every column (in time order) is filled
# from the cheapest source that still has capacity. Because a unit made in
# period i costs base_i + c_I (j - i) in column j, the ranking of the sources is
# the same in every column, so this greedy rule gives the optimal solution.
ap_trans_cheapest <- function(pr) {
  s <- pr$src$supply; dm <- pr$dem
  nr <- length(s); nc <- length(dm)
  X <- matrix(0, nr, nc + 1)
  C <- pr$C
  for (j in seq_len(nc)) {
    while (dm[j] > 1e-9) {
      cand <- which(s > 1e-9 & is.finite(C[, j]))
      if (!length(cand)) break
      i <- cand[order(C[cand, j], -pr$src$period[cand], cand)][1]
      q <- min(s[i], dm[j])
      X[i, j] <- X[i, j] + q
      s[i] <- s[i] - q; dm[j] <- dm[j] - q
    }
  }
  X[, nc + 1] <- pmax(s, 0)          # what is left is excess (unused) capacity
  X
}

# Exact optimum with lpSolve (used only as a check)
ap_trans_lp_check <- function(pr) {
  cells <- which(is.finite(pr$C), arr.ind = TRUE)
  nr <- nrow(pr$C); nc <- ncol(pr$C)
  A <- matrix(0, nr + nc, nrow(cells))
  for (k in seq_len(nrow(cells))) {
    A[cells[k, 1], k] <- 1
    A[nr + cells[k, 2], k] <- 1
  }
  sol <- lpSolve::lp("min", pr$C[cells], A, c(rep("<=", nr), rep(">=", nc)),
                     c(pr$src$supply, pr$dem))
  if (sol$status != 0) NULL else sol$objval
}

ap_trans_evaluate <- function(d, pr, X, cap, label) {
  T <- d$T
  nc <- length(pr$dem)
  used <- rowSums(X[, seq_len(nc), drop = FALSE])
  by_type <- function(tp) vapply(seq_len(T), function(t)
    sum(used[pr$src$type == tp & pr$src$period == t]), numeric(1))
  RT <- by_type("RT"); OT <- by_type("OT"); SC <- by_type("SC")
  U <- pmax(d$Kn * cap$Workers - RT, 0)
  ev <- ap_evaluate(d, cap$Workers, RT + OT, OT, SC, U, label = label)
  ev$plan$`Regular time` <- RT
  ev$plan <- ev$plan[, c("Period", "Demand", "Days", "K n_t", "Workers", "Hired", "Fired",
                         "Regular time", "Overtime", "Production", "Idle", "Subcontract",
                         "Inventory")]
  ev$transport_cost <- sum(ifelse(is.finite(pr$C), pr$C, 0) * X[, seq_len(nc), drop = FALSE])
  ev$X <- X
  ev
}

ap_trans_alloc_table <- function(pr, X) {
  nc <- length(pr$dem)
  M <- matrix("", nrow(X), nc + 1)
  for (i in seq_len(nrow(X))) for (j in seq_len(nc + 1)) {
    if (j <= nc && !is.finite(pr$C[i, j])) M[i, j] <- "×"
    else if (X[i, j] > 1e-9) M[i, j] <- fmt_num(X[i, j], 2)
  }
  out <- data.frame(Source = pr$src$label, M, Capacity = fmt_num(pr$src$supply, 2),
                    check.names = FALSE, stringsAsFactors = FALSE)
  names(out)[2:(nc + 2)] <- c(pr$dem_lab, "Excess")
  rbind(out, c("Demand", fmt_num(pr$dem, 2), fmt_num(max(0, pr$excess), 2), ""))
}

ap_trans_cost_table <- function(pr) {
  M <- ifelse(is.finite(pr$C), formatC(pr$C, format = "fg", digits = 8), "×")
  out <- data.frame(Source = pr$src$label, M, Excess = "0", check.names = FALSE,
                    stringsAsFactors = FALSE)
  names(out)[2:(ncol(M) + 1)] <- pr$dem_lab
  out
}

ap_trans_run <- function(d, cap, method) {
  pr <- ap_trans_problem(d, cap)
  msg <- ap_trans_feasible(pr, d)
  if (!is.null(msg)) return(list(ok = FALSE, msg = msg))
  X <- if (method == "nw") ap_trans_nw(pr) else ap_trans_cheapest(pr)
  ev <- ap_trans_evaluate(d, pr, X, cap,
                          if (method == "nw") "North-west corner" else "Cheapest cell first")
  list(ok = TRUE, pr = pr, ev = ev)
}

# ---- Sub-module showing one solution (NW corner or optimal) ---------------------------
ap_trans_view_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("status")),
    ap_output_ui(ns),
    h4("Transportation tableau: allocation (× = not allowed)"),
    div(class = "ap-num ap-scroll", tableOutput(ns("alloc"))),
    uiOutput(ns("check"))
  )
}

ap_trans_view_server <- function(id, res) {
  moduleServer(id, function(input, output, session) {
    output$status <- renderUI({
      r <- res()
      if (isTRUE(r$ok)) NULL else
        div(style = "color:#A94442;background:#F2DEDE;padding:10px;", r$msg)
    })
    ok_res <- reactive({ r <- res(); req(isTRUE(r$ok)); r })
    ap_render_outputs(input, output, session, ok_res)
    output$alloc <- renderTable({
      r <- ok_res()
      ev <- if (!is.null(r$rounded) && identical(input$which, "rounded")) r$rounded else r$original
      ap_trans_alloc_table(ev$pr, ev$X)
    })
    output$check <- renderUI({
      r <- ok_res()
      if (is.null(r$lp_check)) return(NULL)
      p(em(r$lp_check))
    })
  })
}

ap_trans_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h3("Transportation method"),
    helpText("Data, costs and productivity are entered on Aggregate Planning →",
             "Data, costs and productivity."),
    sidebarLayout(
      sidebarPanel(width = 4,
        h4("Capacities"),
        fluidRow(
          column(7, numericInput(ns("w_all"), "Regular-time workers", value = 300, min = 0)),
          column(5, br(), actionButton(ns("w_apply"), "Apply to all"))),
        fluidRow(
          column(7, numericInput(ns("ot_pct"), "Overtime capacity (% of regular)", 20, min = 0)),
          column(5, br(), actionButton(ns("ot_apply"), "Apply to all"))),
        fluidRow(
          column(7, numericInput(ns("sc_all"), "Subcontracting capacity (units)", 500, min = 0)),
          column(5, br(), actionButton(ns("sc_apply"), "Apply to all"))),
        helpText("Double-click a cell to change a single period. Regular capacity = K n_t × workers."),
        div(style = "font-size: 90%;", DT::DTOutput(ns("caps"))),
        br(),
        helpText("Unit costs: RT = c_R, OT = c_R + c_O, SC = c_S, plus c_I for every period",
                 "a unit is held. Hiring/firing and idle costs follow from the workforce.")
      ),
      mainPanel(width = 8,
        tabsetPanel(
          tabPanel("Optimal (cheapest cell first)", br(), ap_trans_view_ui(ns("opt"))),
          tabPanel("North-west corner (starting solution)", br(), ap_trans_view_ui(ns("nw"))),
          tabPanel("Unit costs", br(), div(class = "ap-num ap-scroll", tableOutput(ns("unit_costs"))))
        )
      )
    )
  )
}

ap_trans_server <- function(id, d) {
  moduleServer(id, function(input, output, session) {
    caps <- reactiveVal(NULL)

    # (Re)build the capacity table when the periods change
    observe({
      dd <- d()
      cur <- isolate(caps())
      if (is.null(cur) || nrow(cur) != dd$T) {
        reg <- dd$Kn * dd$W0
        cur <- data.frame(Period = dd$period, Workers = dd$W0,
                          Overtime = round(reg * 0.20, 2), Subcontract = 500,
                          stringsAsFactors = FALSE)
        updateNumericInput(session, "w_all", value = dd$W0)
      } else cur$Period <- dd$period
      caps(cur)
    })
    observeEvent(input$w_apply, { c <- caps(); c$Workers <- input$w_all; caps(c) })
    observeEvent(input$ot_apply, {
      c <- caps(); c$Overtime <- round(d()$Kn * c$Workers * input$ot_pct / 100, 2); caps(c)
    })
    observeEvent(input$sc_apply, { c <- caps(); c$Subcontract <- input$sc_all; caps(c) })

    output$caps <- DT::renderDT({
      c <- caps(); req(c)
      show <- data.frame(Period = c$Period, Workers = c$Workers,
                         `Regular cap.` = round(d()$Kn * c$Workers, 2),
                         `Overtime cap.` = c$Overtime, `Subcontract cap.` = c$Subcontract,
                         check.names = FALSE)
      DT::datatable(show, rownames = FALSE, selection = "none",
                    editable = list(target = "cell", disable = list(columns = c(0, 2))),
                    class = "compact", options = list(dom = "t", paging = FALSE,
                                                      ordering = FALSE, scrollX = TRUE))
    })
    observeEvent(input$caps_cell_edit, {
      info <- input$caps_cell_edit
      c <- caps()
      v <- to_number(info$value)
      if (is.na(v) || v < 0) return()
      col <- c(NA, "Workers", NA, "Overtime", "Subcontract")[info$col + 1]
      if (!is.na(col)) { c[[col]][info$row] <- v; caps(c) }
    })

    make <- function(method) reactive({
      dd <- d(); c <- caps(); req(c, nrow(c) == dd$T)
      r <- ap_trans_run(dd, c, method)
      if (!r$ok) return(r)
      r$ev$pr <- r$pr
      out <- list(ok = TRUE, d = dd, original = r$ev, rounded = NULL,
                  name = if (method == "nw") "North-west corner solution" else "Optimal solution (cheapest cell first)")
      if (any(abs(c$Workers - ceiling(c$Workers - 1e-9)) > 1e-9)) {
        c2 <- c; c2$Workers <- ceiling(c$Workers - 1e-9)
        r2 <- ap_trans_run(dd, c2, method)
        if (r2$ok) { r2$ev$pr <- r2$pr; out$rounded <- r2$ev }
      }
      out$notes <- c(
        paste0("Transportation cost (production + holding) = ", fmt_money(r$ev$transport_cost), "."),
        "Workers are the regular-time workforce entered on the left; unused regular capacity is shown as idle.",
        if (method == "nw") "The north-west corner rule ignores costs: it fills the periods in order with the earliest sources." else
          "Cheapest cell first: each period (column) is filled from the cheapest source that still has capacity; what is left over is excess capacity.")
      if (method == "opt") {
        lp_obj <- ap_trans_lp_check(r$pr)
        if (!is.null(lp_obj))
          out$lp_check <- paste0("Check with lpSolve: optimal transportation cost = ", fmt_money(lp_obj),
                                 if (abs(lp_obj - r$ev$transport_cost) < 1e-4) " (same as cheapest cell first)."
                                 else " (differs from cheapest cell first).")
      }
      out
    })
    opt <- make("opt")
    nw  <- make("nw")
    ap_trans_view_server("opt", opt)
    ap_trans_view_server("nw", nw)

    output$unit_costs <- renderTable({
      dd <- d(); c <- caps(); req(c, nrow(c) == dd$T)
      ap_trans_cost_table(ap_trans_problem(dd, c))
    })

    reactive({ r <- opt(); req(isTRUE(r$ok)); r })
  })
}
