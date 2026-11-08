# =============================================================================
# Aggregate Planning > Disaggregation with the square-root rule
# The aggregate production X_t of a chosen plan is split among product families.
# Bitran-Hax family problem: minimise total setup cost  sum_j K_j lambda_j / y_j
# subject to sum_j y_j = X_t and a_j <= y_j <= b_j.  Without bounds the solution
# is the square-root rule:  y_j = X_t sqrt(K_j lambda_j) / sum_i sqrt(K_i lambda_i).
# With bounds:  y_j = min(b_j, max(a_j, s sqrt(K_j lambda_j))), s chosen so that
# the y_j add up to X_t (found by bisection).
# =============================================================================

ap_example_families <- function() {
  data.frame(Family = c("Family A", "Family B", "Family C"),
             `Annual demand` = c(12000, 6000, 3000),
             `Setup cost` = c(400, 900, 1500),
             `Lower bound` = c(NA, NA, NA), `Upper bound` = c(NA, NA, NA),
             check.names = FALSE, stringsAsFactors = FALSE)
}

sqrt_rule_allocate <- function(X, w, lb, ub) {
  if (X <= 0) return(list(y = rep(0, length(w)), status = "OK"))
  lb[is.na(lb)] <- 0
  ub[is.na(ub)] <- Inf
  if (sum(lb) > X + 1e-9) return(list(y = lb, status = "Lower bounds add up to more than the aggregate production."))
  if (sum(ub) < X - 1e-9) return(list(y = pmin(ub, 1e15), status = "Upper bounds add up to less than the aggregate production."))
  f <- function(s) sum(pmin(ub, pmax(lb, s * w)))
  lo <- 0; hi <- 1
  while (f(hi) < X && hi < 1e15) hi <- hi * 2
  for (k in 1:200) { mid <- (lo + hi) / 2; if (f(mid) < X) lo <- mid else hi <- mid }
  y <- pmin(ub, pmax(lb, hi * w))
  list(y = y * X / sum(y), status = "OK")
}

ap_disagg_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h3("Disaggregation of the aggregate plan (square-root rule)"),
    sidebarLayout(
      sidebarPanel(width = 4,
        selectInput(ns("plan"), "Aggregate plan to disaggregate", choices = NULL),
        radioButtons(ns("version"), "Version", inline = TRUE,
                     choices = c("Whole workers" = "rounded", "Original" = "original")),
        helpText("Aggregate production X_t = in-house production + subcontracting of the plan."),
        hr(),
        h4("Product families"),
        fileInput(ns("file"), "Upload families (Excel or CSV)", accept = c(".csv", ".txt", ".xlsx", ".xls")),
        helpText("Columns: Family, Annual demand, Setup cost and optionally Lower bound,",
                 "Upper bound (units per period). Double-click a cell to edit."),
        DT::DTOutput(ns("families")),
        br(),
        fluidRow(
          column(4, actionButton(ns("add"), "Add", width = "100%")),
          column(4, actionButton(ns("remove"), "Remove last", width = "100%")),
          column(4, actionButton(ns("example"), "Example", width = "100%")))
      ),
      mainPanel(width = 8,
        div(style = "background:#E8F3E8;border-left:4px solid #2E8B57;padding:10px 14px;margin:6px 0 14px;",
            strong(textOutput(ns("headline"), inline = TRUE))),
        uiOutput(ns("warn")),
        h4("Square-root weights"),
        tableOutput(ns("weights")),
        h4("Production of each family by period"),
        tableOutput(ns("alloc")),
        plotOutput(ns("plot"), height = "330px")
      )
    )
  )
}

ap_disagg_server <- function(id, d, plans) {
  moduleServer(id, function(input, output, session) {
    updateSelectInput(session, "plan", choices = names(plans), selected = "Linear programming")
    fam <- reactiveVal(ap_example_families())

    observeEvent(input$file, {
      df <- tryCatch(read_uploaded_table(input$file$datapath, input$file$name),
                     error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
      req(df)
      num <- numeric_columns(df)
      txt <- setdiff(names(df), num)
      get <- function(p) {
        col <- grep(paste(p, collapse = "|"), num, ignore.case = TRUE, value = TRUE)
        if (!length(col)) rep(NA_real_, nrow(df)) else to_number(df[[col[1]]])
      }
      dem_col <- guess_column(num, c("demand", "lambda", "talep"), 1)
      set_col <- guess_column(num, c("setup", "kurulum", "^k$"), 2)
      out <- data.frame(
        Family = if (length(txt)) as.character(df[[txt[1]]]) else paste("Family", seq_len(nrow(df))),
        `Annual demand` = to_number(df[[dem_col]]), `Setup cost` = to_number(df[[set_col]]),
        `Lower bound` = get(c("lower", "min", "alt")), `Upper bound` = get(c("upper", "max", "ust")),
        check.names = FALSE, stringsAsFactors = FALSE)
      fam(out[!is.na(out$`Annual demand`), ])
    })
    observeEvent(input$example, fam(ap_example_families()))
    observeEvent(input$add, {
      f <- fam()
      fam(rbind(f, data.frame(Family = paste("Family", nrow(f) + 1), `Annual demand` = 1000,
                              `Setup cost` = 500, `Lower bound` = NA, `Upper bound` = NA,
                              check.names = FALSE)))
    })
    observeEvent(input$remove, { f <- fam(); if (nrow(f) > 1) fam(f[-nrow(f), ]) })
    output$families <- DT::renderDT({
      DT::datatable(fam(), rownames = FALSE, selection = "none",
                    editable = list(target = "cell"), class = "compact",
                    options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE))
    })
    observeEvent(input$families_cell_edit, {
      info <- input$families_cell_edit
      f <- fam(); c <- info$col + 1
      if (c == 1) f[info$row, 1] <- as.character(info$value) else
        f[info$row, c] <- to_number(info$value)
      fam(f)
    })

    plan <- reactive({
      req(input$plan, input$plan %in% names(plans))
      r <- plans[[input$plan]]()
      ev <- if (input$version == "rounded" && !is.null(r$rounded)) r$rounded else r$original
      list(ev = ev, d = r$d,
           version = if (input$version == "rounded" && !is.null(r$rounded)) "whole workers" else "original")
    })

    res <- reactive({
      f <- fam(); p <- plan(); dd <- p$d
      validate(need(nrow(f) >= 1, "Enter at least one family."),
               need(all(!is.na(f$`Annual demand`)) && all(f$`Annual demand` >= 0) &&
                      all(!is.na(f$`Setup cost`)) && all(f$`Setup cost` >= 0),
                    "Annual demand and setup cost must be non-negative numbers."))
      w <- sqrt(f$`Setup cost` * f$`Annual demand`)
      validate(need(sum(w) > 0, "At least one family needs a positive K x lambda."))
      X <- p$ev$P + p$ev$S
      status <- character(0)
      Y <- sapply(seq_along(X), function(t) {
        a <- sqrt_rule_allocate(X[t], w, f$`Lower bound`, f$`Upper bound`)
        if (a$status != "OK") status <<- c(status, paste0(dd$period[t], ": ", a$status))
        a$y
      })
      Y <- matrix(Y, nrow = nrow(f))
      list(f = f, w = w, X = X, Y = Y, d = dd, status = status, p = p)
    })

    output$headline <- renderText({
      r <- res()
      paste0("Disaggregating the ", input$plan, " (", r$p$version, "): total production ",
             fmt_num(sum(r$X)), " units over ", length(r$X), " periods")
    })
    output$warn <- renderUI({
      s <- res()$status
      if (!length(s)) return(NULL)
      div(style = "color:#A94442;background:#F2DEDE;padding:8px 12px;margin-bottom:10px;",
          HTML(paste(s, collapse = "<br>")))
    })
    output$weights <- renderTable({
      r <- res()
      int_cols(data.frame(Family = r$f$Family, `Annual demand (lambda)` = r$f$`Annual demand`,
                 `Setup cost (K)` = r$f$`Setup cost`, `sqrt(K lambda)` = r$w,
                 `Share (no bounds)` = r$w / sum(r$w), check.names = FALSE))
    }, digits = 4)
    output$alloc <- renderTable({
      r <- res()
      m <- r$Y
      out <- data.frame(Family = r$f$Family, m, Total = rowSums(m), check.names = FALSE)
      names(out)[2:(ncol(m) + 1)] <- r$d$period
      tot <- data.frame(Family = "Aggregate X_t", t(r$X), Total = sum(r$X), check.names = FALSE)
      names(tot) <- names(out)
      rbind(out, tot)
    }, digits = 2)
    output$plot <- renderPlot({
      r <- res()
      cols <- grDevices::hcl.colors(nrow(r$Y), "Set 2")
      par(mar = c(4.5, 4.5, 3, 1))
      barplot(r$Y, names.arg = r$d$period, col = cols, border = NA, ylab = "Units",
              main = "Family production by period")
      legend("topleft", bty = "n", fill = cols, border = NA, legend = r$f$Family, cex = 0.85)
    })
  })
}
