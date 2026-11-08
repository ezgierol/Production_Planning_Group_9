# =============================================================================
# Aggregate Planning > Linear programming (Nahmias formulation)
#   min  sum_t ( c_H H_t + c_F F_t + c_I I_t + c_R P_t + c_O O_t + c_U U_t + c_S S_t )
#   s.t. (A) W_t = W_{t-1} + H_t - F_t
#        (B) P_t = K n_t W_t + O_t - U_t
#        (C) I_t = I_{t-1} + P_t + S_t - D_t
#        (D) all variables >= 0
#        (E) W_0, I_0 given, I_T >= target
#   Options: overtime / idle time / subcontracting allowed or not (O_t = 0 ...),
#   no firing, limits on overtime and subcontracting, integer workers.
# Solved with lpSolve.
# =============================================================================

ap_lp_vars <- c("W", "H", "F", "P", "I", "O", "U", "S")

ap_lp_build <- function(d, opt) {
  T <- d$T
  nv <- 8 * T
  ix <- function(v, t) (t - 1) * 8 + match(v, ap_lp_vars)
  rows <- list(); dirs <- character(0); rhs <- numeric(0); names_ <- character(0)
  add <- function(coefs, dir, b, name) {
    r <- numeric(nv)
    for (k in names(coefs)) r[as.integer(k)] <- r[as.integer(k)] + coefs[[k]]
    rows[[length(rows) + 1]] <<- r
    dirs <<- c(dirs, dir); rhs <<- c(rhs, b); names_ <<- c(names_, name)
  }
  cf <- function(...) { a <- list(...); stats::setNames(a[c(FALSE, TRUE)], unlist(a[c(TRUE, FALSE)])) }
  for (t in seq_len(T)) {
    # (A) W_t - W_{t-1} - H_t + F_t = 0     (t = 1: W_1 - H_1 + F_1 = W0)
    co <- cf(ix("W", t), 1, ix("H", t), -1, ix("F", t), 1)
    if (t > 1) co[[as.character(ix("W", t - 1))]] <- -1
    add(co, "=", if (t == 1) d$W0 else 0, paste0("(A) workforce, t=", t))
    # (B) -K n_t W_t + P_t - O_t + U_t = 0
    add(cf(ix("W", t), -d$Kn[t], ix("P", t), 1, ix("O", t), -1, ix("U", t), 1), "=", 0,
        paste0("(B) production, t=", t))
    # (C) P_t - I_t + I_{t-1} + S_t = D_t   (t = 1: P_1 - I_1 + S_1 = D_1 - I0)
    co <- cf(ix("P", t), 1, ix("I", t), -1, ix("S", t), 1)
    if (t > 1) co[[as.character(ix("I", t - 1))]] <- 1
    add(co, "=", d$D[t] - (if (t == 1) d$I0 else 0), paste0("(C) inventory, t=", t))
    # Option constraints
    if (!opt$overtime) add(cf(ix("O", t), 1), "=", 0, paste0("no overtime, t=", t))
    else if (!is.na(opt$ot_pct))
      add(cf(ix("O", t), 1, ix("W", t), -opt$ot_pct / 100 * d$Kn[t]), "<=", 0,
          paste0("overtime limit, t=", t))
    if (!opt$idle) add(cf(ix("U", t), 1), "=", 0, paste0("no idle time, t=", t))
    if (!opt$sub) add(cf(ix("S", t), 1), "=", 0, paste0("no subcontracting, t=", t))
    else if (!is.na(opt$sub_cap))
      add(cf(ix("S", t), 1), "<=", opt$sub_cap, paste0("subcontracting limit, t=", t))
    if (opt$no_fire) add(cf(ix("F", t), 1), "=", 0, paste0("no firing, t=", t))
  }
  # (E) I_T >= target
  add(cf(ix("I", T), 1), ">=", d$IT, "(E) ending inventory")

  cst <- d$cost
  obj <- rep(c(0, cst$H, cst$F, cst$R, cst$I, cst$O, cst$U, cst$S), T)
  list(obj = obj, A = do.call(rbind, rows), dir = dirs, rhs = rhs, names = names_,
       nv = nv, ix = ix,
       int = if (opt$integer) unlist(lapply(seq_len(T), function(t) c(ix("W", t), ix("H", t), ix("F", t)))) else NULL)
}

ap_lp_solve <- function(d, opt) {
  m <- ap_lp_build(d, opt)
  sol <- lpSolve::lp("min", m$obj, m$A, m$dir, m$rhs, int.vec = m$int)
  if (sol$status != 0) {
    return(list(ok = FALSE, model = m,
                msg = if (sol$status == 2) "The LP is infeasible with these options (try allowing overtime / subcontracting or relaxing the limits)." else
                  if (sol$status == 3) "The LP is unbounded." else paste("lpSolve status", sol$status)))
  }
  x <- sol$solution
  get <- function(v) x[m$ix(v, seq_len(d$T))]
  W <- get("W"); P <- get("P"); O <- get("O"); U <- get("U"); S <- get("S")
  orig <- ap_evaluate(d, W, P, O, S, U, label = "LP solution")
  Wr <- ceiling(W - 1e-6)
  rnd <- NULL
  if (any(abs(W - Wr) >= 1e-6)) {
    # Whole workers: if idle time is allowed the LP production is kept (extra capacity
    # is idle); otherwise all regular capacity K n_t W_t is used. Production is
    # rounded to whole units and inventory is recomputed.
    Pr <- if (opt$idle) P else d$Kn * Wr + O
    rnd <- ap_evaluate(d, Wr, round(Pr), O, S, U = if (opt$idle) NULL else 0, label = "Whole workers")
  }
  list(ok = TRUE, model = m, original = orig, rounded = rnd, objective = sol$objval)
}

# Pretty print of the constraints of one period, e.g. "W1 - H1 + F1 = 300"
ap_lp_constraint_text <- function(m, d, t) {
  T <- d$T
  rows <- grep(paste0("t=", t, "$"), m$names)
  if (t == T) rows <- c(rows, length(m$names))
  lab <- function(j) { tt <- (j - 1) %/% 8 + 1; paste0(ap_lp_vars[(j - 1) %% 8 + 1], tt) }
  fmtc <- function(v) as.character(signif(abs(v), 7))
  out <- vapply(rows, function(r) {
    a <- m$A[r, ]
    nz <- which(abs(a) > 1e-12)
    terms <- vapply(seq_along(nz), function(k) {
      j <- nz[k]; v <- a[j]
      coef <- if (abs(abs(v) - 1) < 1e-12) "" else paste0(fmtc(v), " ")
      sgn <- if (v < 0) { if (k == 1) "-" else "- " } else if (k == 1) "" else "+ "
      paste0(sgn, coef, lab(j))
    }, character(1))
    dir <- c("=" = "=", "<=" = "≤", ">=" = "≥")[[m$dir[r]]]
    rhs_txt <- if (m$rhs[r] < 0) paste0("-", fmtc(m$rhs[r])) else fmtc(m$rhs[r])
    sprintf("%-28s %s %s %s", m$names[r], paste(terms, collapse = " "), dir, rhs_txt)
  }, character(1))
  paste(out, collapse = "\n")
}

ap_lp_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h3("Aggregate planning by linear programming"),
    helpText("Data, costs and productivity are entered on Aggregate Planning →",
             "Data, costs and productivity."),
    sidebarLayout(
      sidebarPanel(width = 3,
        h4("Options"),
        checkboxInput(ns("overtime"), "Allow overtime", FALSE),
        conditionalPanel("input.overtime", ns = ns,
          numericInput(ns("ot_pct"), "Max overtime (% of regular capacity, empty = no limit)",
                       value = NA, min = 0)),
        checkboxInput(ns("idle"), "Allow idle time", FALSE),
        checkboxInput(ns("sub"), "Allow subcontracting", FALSE),
        conditionalPanel("input.sub", ns = ns,
          numericInput(ns("sub_cap"), "Max subcontracting per period (empty = no limit)",
                       value = NA, min = 0)),
        checkboxInput(ns("no_fire"), "No firing allowed", FALSE),
        checkboxInput(ns("integer"), "Whole number of workers (integer LP)", FALSE),
        helpText("Overtime units cost c_R + c_O (c_R applies to all in-house production,",
                 "as in the objective function).")
      ),
      mainPanel(width = 9,
        tabsetPanel(
          tabPanel("Optimal plan", br(), uiOutput(ns("status")),
                   conditionalPanel("output.ok", ns = ns, ap_output_ui(ns))),
          tabPanel("The LP model", br(),
            withMathJax(),
            p(strong("Minimise")),
            p("$$\\sum_{t=1}^{T}\\left(c_H H_t + c_F F_t + c_I I_t + c_R P_t + c_O O_t + c_U U_t + c_S S_t\\right)$$"),
            p(strong("subject to, for every period t:")),
            p("$$\\text{(A)}\\; W_t = W_{t-1} + H_t - F_t \\qquad \\text{(B)}\\; P_t = K n_t W_t + O_t - U_t \\qquad \\text{(C)}\\; I_t = I_{t-1} + P_t + S_t - D_t$$"),
            p("$$\\text{(D)}\\; \\text{all variables} \\ge 0 \\qquad \\text{(E)}\\; W_0, I_0 \\text{ given}, \\; I_T \\ge \\text{target}$$"),
            textOutput(ns("model_size")),
            br(),
            selectInput(ns("period"), "Show the constraints with numbers for period:", choices = NULL),
            verbatimTextOutput(ns("constraints"))
          )
        )
      )
    )
  )
}

ap_lp_server <- function(id, d) {
  moduleServer(id, function(input, output, session) {
    opt <- reactive(list(
      overtime = isTRUE(input$overtime), idle = isTRUE(input$idle), sub = isTRUE(input$sub),
      no_fire = isTRUE(input$no_fire), integer = isTRUE(input$integer),
      ot_pct = if (is.null(input$ot_pct)) NA else input$ot_pct,
      sub_cap = if (is.null(input$sub_cap)) NA else input$sub_cap))

    sol <- reactive(ap_lp_solve(d(), opt()))

    output$ok <- reactive(isTRUE(sol()$ok))
    outputOptions(output, "ok", suspendWhenHidden = FALSE)
    output$status <- renderUI({
      s <- sol()
      if (s$ok) NULL else div(style = "color:#A94442;background:#F2DEDE;padding:10px;", s$msg)
    })

    res <- reactive({
      s <- sol(); req(s$ok)
      dd <- d()
      o <- opt()
      allowed <- c(if (o$overtime) "overtime", if (o$idle) "idle time", if (o$sub) "subcontracting")
      list(original = s$original, rounded = s$rounded, d = dd,
           name = if (o$integer) "Integer LP optimum" else "LP optimum",
           notes = c(
             paste0("Allowed: ", if (length(allowed)) paste(allowed, collapse = ", ") else "no overtime, no idle time, no subcontracting", ".",
                    if (o$no_fire) " No firing." else ""),
             if (!is.null(s$rounded)) paste0("Whole-workers plan: W_t of the LP is rounded up, production is ",
                    if (o$idle) "kept from the LP (extra capacity is idle)" else "K n_t W_t (+ overtime)",
                    " rounded to whole units, and inventory is recomputed.") else
               "The LP solution already has whole numbers of workers.",
             paste0("lpSolve objective value: ", fmt_money(s$objective))))
    })
    ap_render_outputs(input, output, session, res)

    observe({
      dd <- d()
      updateSelectInput(session, "period", choices = stats::setNames(seq_len(dd$T), dd$period),
                        selected = isolate({
                          cur <- suppressWarnings(as.integer(input$period))
                          if (length(cur) == 1 && !is.na(cur) && cur <= dd$T) cur else 1
                        }))
    })
    output$model_size <- renderText({
      m <- sol()$model
      paste0("This model has ", m$nv, " variables (8 per period) and ", length(m$rhs),
             " constraints, including the option constraints.")
    })
    output$constraints <- renderText({
      req(input$period)
      ap_lp_constraint_text(sol()$model, d(), as.integer(input$period))
    })
    res
  })
}
