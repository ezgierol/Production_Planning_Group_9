# =============================================================================
# Forecasting - shared pieces: data upload tab, common inputs/outputs, plots
# =============================================================================

# ---- Example data (used until the user uploads a file) ------------------------------
fc_example_data <- function() {
  t <- 1:36
  stationary <- round(240 + 18 * sin(1.7 * t) + 12 * cos(0.9 * t + 1) + 6 * sin(5.3 * t))
  trend      <- round(150 + 6.5 * t + 14 * sin(1.3 * t) + 8 * cos(3.1 * t))
  season     <- rep(c(0.80, 1.15, 1.35, 0.70), length.out = 36)
  seasonal   <- round((200 + 5 * t) * season + 10 * sin(2.3 * t))
  data.frame(Period = t, Stationary = stationary, Trend = trend,
             Seasonal = seasonal)
}

# ---- Data tab -----------------------------------------------------------------------
fc_data_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Forecasting data"),
      fileInput(ns("file"), "Upload an Excel (.xlsx / .xls) or CSV file",
                accept = c(".csv", ".txt", ".xlsx", ".xls")),
      uiOutput(ns("sheet_ui")),
      actionButton(ns("example"), "Load example data", class = "btn-default"),
      br(), br(),
      downloadButton(ns("download_example"), "Download example CSV"),
      hr(),
      helpText("One row per period. Every numeric column is a separate time",
               "series; you select the series on each method's page.",
               "CSV files may be comma or semicolon separated.")
    ),
    mainPanel(
      h4(textOutput(ns("info"))),
      tableOutput(ns("preview"))
    )
  )
}

fc_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data   <- reactiveVal(fc_example_data())
    source <- reactiveVal("Example data loaded (36 periods). Upload your own file to replace it.")

    output$sheet_ui <- renderUI({
      req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      if (!ext %in% c("xlsx", "xls")) return(NULL)
      sheets <- readxl::excel_sheets(input$file$datapath)
      selectInput(session$ns("sheet"), "Sheet", choices = sheets, selected = sheets[1])
    })

    observeEvent(list(input$file, input$sheet), {
      req(input$file)
      df <- tryCatch(
        read_uploaded_table(input$file$datapath, input$file$name, input$sheet),
        error = function(e) {
          showNotification(paste("Could not read the file:", conditionMessage(e)),
                           type = "error")
          NULL
        })
      req(df)
      if (length(numeric_columns(df)) == 0) {
        showNotification("No numeric column was found in the file.", type = "error")
        return()
      }
      data(df)
      source(paste0("Uploaded: ", input$file$name, " (", nrow(df), " rows)"))
    }, ignoreInit = TRUE)

    observeEvent(input$example, {
      data(fc_example_data())
      source("Example data loaded (36 periods).")
    })

    output$download_example <- downloadHandler(
      filename = function() "forecasting_example.csv",
      content = function(file) utils::write.csv(fc_example_data(), file, row.names = FALSE)
    )

    output$info <- renderText({
      paste0(source(), " — numeric series: ",
             paste(numeric_columns(data()), collapse = ", "))
    })
    output$preview <- renderTable(utils::head(data(), 60))

    data
  })
}

# ---- Common inputs ------------------------------------------------------------------
fc_series_input <- function(ns) {
  tagList(
    selectInput(ns("series"), "Series", choices = NULL),
    helpText("Upload data on the Forecasting → Data page.")
  )
}

fc_mode_input <- function(ns, what = "the parameters") {
  tagList(
    radioButtons(ns("mode"), paste("How should", what, "be chosen?"),
                 choices = c("I will enter them" = "manual",
                             "Find the best values" = "search")),
    conditionalPanel(
      condition = "input.mode == 'search'", ns = ns,
      selectInput(ns("criterion"), "Criterion for the search",
                  choices = c("MSE", "MAD", "MAPE"))
    )
  )
}

fc_horizon_input <- function(ns, value = 1) {
  numericInput(ns("h"), "Forecast horizon (periods ahead)", value = value,
               min = 1, max = 60, step = 1)
}

# Keeps the series list in sync with the uploaded data and returns the series
fc_series_server <- function(input, session, data, preferred = NULL) {
  observe({
    cols <- numeric_columns(data())
    current <- isolate(input$series)
    sel <- if (!is.null(current) && current %in% cols) current else
      guess_column(cols, c(preferred, "demand", "sales"), fallback = 1)
    # Do not use a 'Period' / 't' column as a series when another one exists
    if (!is.null(sel) && grepl("^(period|t|month|time)$", sel, ignore.case = TRUE) &&
        length(cols) > 1) sel <- setdiff(cols, sel)[1]
    updateSelectInput(session, "series", choices = cols, selected = sel)
  })
  reactive({
    df <- data()
    req(input$series, input$series %in% names(df))
    y <- to_number(df[[input$series]])
    y <- y[!is.na(y)]
    validate(need(length(y) >= 4, "The series needs at least 4 numeric values."))
    y
  })
}

# ---- Common outputs -----------------------------------------------------------------
fc_output_ui <- function(ns, extra = NULL) {
  tagList(
    h4(textOutput(ns("headline"))),
    plotOutput(ns("plot"), height = "360px"),
    fluidRow(
      column(5, h4("Error measures"), tableOutput(ns("errors"))),
      column(7, h4("Parameters"), tableOutput(ns("params")))
    ),
    uiOutput(ns("search_ui")),
    extra,
    h4("Forecasts"),
    tableOutput(ns("future")),
    h4("Forecasts by period"),
    tableOutput(ns("table"))
  )
}

# res() must return a list with: headline, actual, fitted, future (data.frame
# with Period & Forecast [& Lower, Upper]), errors, params, table and optionally
# search_plot (data.frame: value, score, label) or search_table.
fc_render_outputs <- function(output, session, res, ylab = "Demand") {
  output$headline <- renderText(res()$headline)
  output$plot     <- renderPlot(fc_plot(res(), ylab = ylab))
  output$errors   <- renderTable(res()$errors, digits = 3)
  output$params   <- renderTable(res()$params, digits = 4)
  output$future   <- renderTable(int_cols(res()$future), digits = 2)
  output$table    <- renderTable(int_cols(res()$table), digits = 3, na = "")

  output$search_ui <- renderUI({
    r <- res()
    if (is.null(r$search_plot) && is.null(r$search_table)) return(NULL)
    tagList(
      h4("Parameter search"),
      if (!is.null(r$search_plot)) plotOutput(session$ns("search_plot"), height = "240px"),
      if (!is.null(r$search_table)) tagList(
        p("Best parameter combinations (lowest criterion first):"),
        tableOutput(session$ns("search_table")))
    )
  })
  output$search_plot <- renderPlot({
    sp <- res()$search_plot
    req(sp)
    best <- which.min(sp$score)
    cols <- ifelse(seq_len(nrow(sp)) == best, col_next, "#9DB4CF")
    if (nrow(sp) <= 30) {
      barplot(sp$score, names.arg = sp$label, col = cols, border = NA,
              xlab = sp$xlab[1], ylab = sp$crit[1],
              main = paste0("Best ", sp$xlab[1], " by ", sp$crit[1], " = ", sp$label[best]))
    } else {
      plot(sp$value, sp$score, type = "l", col = col_forecast, lwd = 2,
           xlab = sp$xlab[1], ylab = sp$crit[1],
           main = paste0("Best ", sp$xlab[1], " by ", sp$crit[1], " = ", sp$label[best]))
      points(sp$value[best], sp$score[best], pch = 19, col = col_next, cex = 1.4)
    }
  })
  output$search_table <- renderTable(res()$search_table, digits = 4)
}

# Shows whole-number columns (period, season, demand ...) without decimals
int_cols <- function(df) {
  if (is.null(df)) return(df)
  for (j in seq_along(df)) {
    v <- df[[j]]
    if (is.numeric(v) && all(is.na(v) | (abs(v - round(v)) < 1e-9 & abs(v) < 2e9)))
      df[[j]] <- as.integer(round(v))
  }
  df
}

fc_plot <- function(r, ylab = "Demand") {
  y <- r$actual
  n <- length(y)
  fut <- r$future
  h <- if (is.null(fut)) 0 else nrow(fut)
  x_all <- seq_len(n + h)
  vals <- c(y, r$fitted, fut$Forecast, fut$Lower, fut$Upper)
  ylim <- range(vals[is.finite(vals)])
  par(mar = c(4.5, 4.5, 3, 1))
  plot(seq_len(n), y, type = "o", pch = 19, col = col_actual, xlim = c(1, n + h),
       ylim = ylim, xlab = "Period", ylab = ylab,
       main = if (is.null(r$plot_title)) "Actual demand and forecast" else r$plot_title)
  grid(col = "#E5E5E5")
  if (!is.null(fut$Lower)) {
    polygon(c(n + seq_len(h), rev(n + seq_len(h))), c(fut$Lower, rev(fut$Upper)),
            col = adjustcolor(col_next, 0.15), border = NA)
  }
  lines(seq_len(n), r$fitted, type = "o", lty = 2, pch = 17, col = col_forecast)
  if (h > 0) {
    lines(c(n, n + seq_len(h)), c(r$fitted[n], fut$Forecast), lty = 3, col = col_next)
    points(n + seq_len(h), fut$Forecast, pch = 17, col = col_next, cex = 1.6)
  }
  legend("topleft", bty = "n", cex = 0.9,
         legend = c("Actual demand", "Forecast (one step ahead)", "Future forecast"),
         col = c(col_actual, col_forecast, col_next), lty = c(1, 2, NA),
         pch = c(19, 17, 17))
}

# ---- Menu and server wiring (called from app.R) ---------------------------------------
forecasting_menu <- function() {
  navbarMenu(
    "Forecasting",
    tabPanel("Data", value = "fc_data_tab", fc_data_ui("fc_data")),
    "----",
    "Stationary series",
    tabPanel("Moving Average", value = "fc_ma_tab", fc_ma_ui("fc_ma")),
    tabPanel("Weighted Moving Average", value = "fc_wma_tab", fc_wma_ui("fc_wma")),
    tabPanel("Simple Exponential Smoothing", value = "fc_ses_tab", fc_ses_ui("fc_ses")),
    tabPanel("Seasonal Factors (quick and dirty)", value = "fc_sf_tab", fc_sf_ui("fc_sf")),
    "----",
    "Trend series",
    tabPanel("Holt's Method", value = "fc_holt_tab", fc_holt_ui("fc_holt")),
    "----",
    "Seasonal series",
    tabPanel("Seasonal Decomposition (moving averages)", value = "fc_decomp_tab",
             fc_decomp_ui("fc_decomp")),
    tabPanel("Winters' Method", value = "fc_winters_tab", fc_winters_ui("fc_winters")),
    "----",
    "Box–Jenkins",
    tabPanel("AR(p)", value = "fc_ar_tab", fc_bj_ui("fc_ar", "AR")),
    tabPanel("MA(q)", value = "fc_mabj_tab", fc_bj_ui("fc_mabj", "MA")),
    tabPanel("ARMA(p, q)", value = "fc_arma_tab", fc_bj_ui("fc_arma", "ARMA")),
    tabPanel("ARIMA(p, d, q)", value = "fc_arima_tab", fc_bj_ui("fc_arima", "ARIMA"))
  )
}

forecasting_server <- function() {
  data <- fc_data_server("fc_data")
  fc_ma_server("fc_ma", data)
  fc_wma_server("fc_wma", data)
  fc_ses_server("fc_ses", data)
  fc_sf_server("fc_sf", data)
  fc_holt_server("fc_holt", data)
  fc_decomp_server("fc_decomp", data)
  fc_winters_server("fc_winters", data)
  fc_bj_server("fc_ar", data, "AR")
  fc_bj_server("fc_mabj", data, "MA")
  fc_bj_server("fc_arma", data, "ARMA")
  fc_bj_server("fc_arima", data, "ARIMA")
  invisible(data)
}
