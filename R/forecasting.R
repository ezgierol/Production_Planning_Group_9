# R/forecasting.R

library(shiny)
library(readxl)
library(forecast)

# ============================================================
# HELPER FUNCTIONS
# ============================================================

calculate_metrics <- function(actual, fitted) {
  idx <- which(
    !is.na(actual) &
      !is.na(fitted) &
      is.finite(actual) &
      is.finite(fitted) &
      actual != 0
  )

  if (length(idx) == 0) {
    return(list(MAD = NA, MSE = NA, MAPE = NA))
  }

  error <- actual[idx] - fitted[idx]

  list(
    MAD = mean(abs(error)),
    MSE = mean(error^2),
    MAPE = mean(abs(error / actual[idx])) * 100
  )
}


# Moving Average
moving_average <- function(y, n, h) {

  len <- length(y)

  fitted <- rep(NA, len)

  if (n >= len) {
    return(NULL)
  }

  for (t in (n + 1):len) {
    fitted[t] <- mean(y[(t - n):(t - 1)])
  }

  future <- numeric(h)
  temp <- as.numeric(y)

  for (i in 1:h) {
    future[i] <- mean(tail(temp, n))
    temp <- c(temp, future[i])
  }

  list(
    fitted = fitted,
    future = future
  )
}


# Weighted Moving Average
weighted_moving_average <- function(y, weights, h) {

  weights <- as.numeric(weights)
  weights <- weights[is.finite(weights)]

  if (length(weights) == 0 || sum(weights) == 0) {
    return(NULL)
  }

  weights <- weights / sum(weights)

  n <- length(weights)
  len <- length(y)

  if (n >= len) {
    return(NULL)
  }

  fitted <- rep(NA, len)

  for (t in (n + 1):len) {
    fitted[t] <- sum(y[(t - n):(t - 1)] * weights)
  }

  future <- numeric(h)
  temp <- as.numeric(y)

  for (i in 1:h) {
    future[i] <- sum(tail(temp, n) * weights)
    temp <- c(temp, future[i])
  }

  list(
    fitted = fitted,
    future = future
  )
}


# ============================================================
# USER INTERFACE
# ============================================================

forecasting_ui <- function(id) {

  ns <- NS(id)

  sidebarLayout(

    sidebarPanel(

      h4("1. Data Input"),

      fileInput(
        ns("file"),
        "Upload Data (.csv, .xlsx, .xls)",
        accept = c(".csv", ".xlsx", ".xls")
      ),

      helpText(
        "Upload a data file and select the time series column."
      ),

      uiOutput(ns("column_selector")),

      numericInput(
        ns("frequency"),
        "Seasonal Frequency:",
        value = 4,
        min = 1,
        step = 1
      ),

      numericInput(
        ns("horizon"),
        "Forecast Horizon:",
        value = 4,
        min = 1,
        step = 1
      ),

      hr(),

      h4("2. Forecasting Method"),

      selectInput(
        ns("category"),
        "Forecasting Category:",
        choices = c(
          "Stationary Series",
          "Trend Series",
          "Seasonal Series",
          "Box-Jenkins"
        )
      ),

      uiOutput(ns("method_controls"))

    ),

    mainPanel(

      tabsetPanel(

        tabPanel(
          "Forecast & Evaluation",

          h4("Forecast Plot"),

          plotOutput(
            ns("forecast_plot"),
            height = "400px"
          ),

          hr(),

          h4("Performance Metrics"),

          tableOutput(
            ns("metrics_table")
          ),

          hr(),

          h4("Forecast Values"),

          tableOutput(
            ns("forecast_table")
          )
        ),

        tabPanel(
          "Box-Jenkins Diagnostics",

          h4("ACF and PACF"),

          plotOutput(
            ns("acf_pacf_plot"),
            height = "400px"
          ),

          hr(),

          h4("Parameter Estimates"),

          tableOutput(
            ns("parameter_table")
          ),

          hr(),

          h4("Model Summary"),

          verbatimTextOutput(
            ns("model_summary")
          )
        )
      )
    )
  )
}


# ============================================================
# SERVER
# ============================================================

forecasting_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    # --------------------------------------------------------
    # 1. DATA INPUT
    # --------------------------------------------------------

    raw_data <- reactive({

      if (is.null(input$file)) {

        return(
          data.frame(
            Period = 1:24,
            Demand = c(
              120, 130, 110, 150,
              170, 160, 180, 140,
              200, 210, 190, 220,
              230, 210, 240, 250,
              260, 245, 275, 290,
              300, 280, 310, 325
            )
          )
        )

      }

      extension <- tolower(tools::file_ext(input$file$name))

      if (extension == "csv") {

        read.csv(
          input$file$datapath,
          stringsAsFactors = FALSE
        )

      } else if (extension %in% c("xlsx", "xls")) {

        as.data.frame(
          readxl::read_excel(input$file$datapath)
        )

      } else {

        validate(
          need(
            FALSE,
            "Unsupported file format."
          )
        )
      }
    })


    # --------------------------------------------------------
    # 2. COLUMN SELECTION
    # --------------------------------------------------------

    output$column_selector <- renderUI({

      df <- raw_data()

      numeric_columns <- names(
        df
      )[
        sapply(df, is.numeric)
      ]

      validate(
        need(
          length(numeric_columns) > 0,
          "The uploaded file must contain at least one numeric column."
        )
      )

      selectInput(
        session$ns("selected_column"),
        "Select Series:",
        choices = numeric_columns,
        selected = numeric_columns[1]
      )
    })


    # --------------------------------------------------------
    # 3. TIME SERIES
    # --------------------------------------------------------

    time_series <- reactive({

      df <- raw_data()

      req(input$selected_column)

      y <- as.numeric(
        na.omit(
          df[[input$selected_column]]
        )
      )

      validate(
        need(
          length(y) >= 5,
          "The selected series must contain at least 5 observations."
        )
      )

      freq <- input$frequency

      if (is.null(freq) || is.na(freq) || freq < 1) {
        freq <- 1
      }

      ts(
        y,
        frequency = freq
      )
    })


    # --------------------------------------------------------
    # 4. METHOD CONTROLS
    # --------------------------------------------------------

    output$method_controls <- renderUI({

      req(input$category)

      ns <- session$ns


      # ======================================================
      # STATIONARY SERIES
      # ======================================================

      if (input$category == "Stationary Series") {

        tagList(

          selectInput(
            ns("stationary_method"),
            "Method:",
            choices = c(
              "Moving Average",
              "Weighted Moving Average",
              "Simple Exponential Smoothing",
              "Seasonal Factors (Quick and Dirty)"
            )
          ),

          conditionalPanel(

            condition = sprintf(
              "input['%s'] == 'Moving Average'",
              ns("stationary_method")
            ),

            radioButtons(
              ns("ma_mode"),
              "Parameter Selection:",
              choices = c(
                "Manual" = "manual",
                "Automatic (Best N)" = "auto"
              ),
              selected = "manual"
            ),

            conditionalPanel(

              condition = sprintf(
                "input['%s'] == 'manual'",
                ns("ma_mode")
              ),

              sliderInput(
                ns("ma_n"),
                "Window Size (N):",
                min = 1,
                max = 15,
                value = 3,
                step = 1
              )
            ),

            conditionalPanel(

              condition = sprintf(
                "input['%s'] == 'auto'",
                ns("ma_mode")
              ),

              sliderInput(
                ns("ma_max_n"),
                "Maximum N:",
                min = 2,
                max = 15,
                value = 10,
                step = 1
              ),

              selectInput(
                ns("ma_metric"),
                "Optimization Criterion:",
                choices = c(
                  "MAD",
                  "MSE",
                  "MAPE"
                )
              )
            )
          ),


          conditionalPanel(

            condition = sprintf(
              "input['%s'] == 'Weighted Moving Average'",
              ns("stationary_method")
            ),

            textInput(
              ns("wma_weights"),
              "Weights (oldest to newest):",
              value = "0.2, 0.3, 0.5"
            ),

            helpText(
              "Weights are automatically normalized."
            )
          ),


          conditionalPanel(

            condition = sprintf(
              "input['%s'] == 'Simple Exponential Smoothing'",
              ns("stationary_method")
            ),

            radioButtons(
              ns("ses_mode"),
              "Alpha Selection:",
              choices = c(
                "Manual" = "manual",
                "Automatic (Best Alpha)" = "auto"
              ),
              selected = "manual"
            ),

            conditionalPanel(

              condition = sprintf(
                "input['%s'] == 'manual'",
                ns("ses_mode")
              ),

              sliderInput(
                ns("ses_alpha"),
                "Alpha:",
                min = 0.01,
                max = 0.99,
                value = 0.20,
                step = 0.01
              )
            ),

            radioButtons(
              ns("ses_initial"),
              "Initial Level:",
              choices = c(
                "Regression Fit" = "regression",
                "Manual Entry" = "manual"
              ),
              selected = "regression"
            ),

            conditionalPanel(

              condition = sprintf(
                "input['%s'] == 'manual'",
                ns("ses_initial")
              ),

              numericInput(
                ns("ses_l0"),
                "Initial Level (L0):",
                value = 100
              )
            )
          )
        )


      # ======================================================
      # TREND SERIES
      # ======================================================

      } else if (input$category == "Trend Series") {

        tagList(

          h5("Holt's Double Exponential Smoothing"),

          radioButtons(
            ns("holt_mode"),
            "Parameter Selection:",
            choices = c(
              "Manual" = "manual",
              "Automatic (Best Parameters)" = "auto"
            ),
            selected = "manual"
          ),

          conditionalPanel(

            condition = sprintf(
              "input['%s'] == 'manual'",
              ns("holt_mode")
            ),

            sliderInput(
              ns("holt_alpha"),
              "Alpha (Level):",
              min = 0.01,
              max = 0.99,
              value = 0.20,
              step = 0.01
            ),

            sliderInput(
              ns("holt_beta"),
              "Beta (Trend):",
              min = 0.01,
              max = 0.99,
              value = 0.10,
              step = 0.01
            )
          ),

          radioButtons(
            ns("holt_initial"),
            "Initial Values:",
            choices = c(
              "Regression Fit" = "regression",
              "Manual Entry" = "manual"
            ),
            selected = "regression"
          ),

          conditionalPanel(

            condition = sprintf(
              "input['%s'] == 'manual'",
              ns("holt_initial")
            ),

            numericInput(
              ns("holt_l0"),
              "Initial Level (L0):",
              value = 100
            ),

            numericInput(
              ns("holt_b0"),
              "Initial Slope (T0):",
              value = 2
            )
          )
        )


      # ======================================================
      # SEASONAL SERIES
      # ======================================================

      } else if (input$category == "Seasonal Series") {

        tagList(

          selectInput(
            ns("seasonal_method"),
            "Method:",
            choices = c(
              "Seasonal Decomposition",
              "Winters' Method"
            )
          ),

          conditionalPanel(

            condition = sprintf(
              "input['%s'] == 'Seasonal Decomposition'",
              ns("seasonal_method")
            ),

            selectInput(
              ns("decomp_type"),
              "Decomposition Type:",
              choices = c(
                "Additive" = "additive",
                "Multiplicative" = "multiplicative"
              )
            )
          ),

          conditionalPanel(

            condition = sprintf(
              "input['%s'] == 'Winters\\' Method'",
              ns("seasonal_method")
            ),

            selectInput(
              ns("winters_type"),
              "Seasonal Type:",
              choices = c(
                "Additive" = "additive",
                "Multiplicative" = "multiplicative"
              )
            ),

            radioButtons(
              ns("winters_mode"),
              "Parameter Selection:",
              choices = c(
                "Manual" = "manual",
                "Automatic" = "auto"
              ),
              selected = "manual"
            ),

            conditionalPanel(

              condition = sprintf(
                "input['%s'] == 'manual'",
                ns("winters_mode")
              ),

              sliderInput(
                ns("winters_alpha"),
                "Alpha (Level):",
                min = 0.01,
                max = 0.99,
                value = 0.20,
                step = 0.01
              ),

              sliderInput(
                ns("winters_beta"),
                "Beta (Trend):",
                min = 0.01,
                max = 0.99,
                value = 0.10,
                step = 0.01
              ),

              sliderInput(
                ns("winters_gamma"),
                "Gamma (Seasonality):",
                min = 0.01,
                max = 0.99,
                value = 0.10,
                step = 0.01
              )
            ),

            radioButtons(
              ns("winters_initial"),
              "Initial Values:",
              choices = c(
                "Regression Fit" = "regression",
                "Manual Entry" = "manual"
              ),
              selected = "regression"
            ),

            conditionalPanel(

              condition = sprintf(
                "input['%s'] == 'manual'",
                ns("winters_initial")
              ),

              numericInput(
                ns("winters_l0"),
                "Initial Level (L0):",
                value = 100
              ),

              numericInput(
                ns("winters_b0"),
                "Initial Slope (T0):",
                value = 2
              )
            )
          )
        )


      # ======================================================
      # BOX-JENKINS
      # ======================================================

      } else {

        tagList(

          selectInput(
            ns("bj_method"),
            "Box-Jenkins Model:",
            choices = c(
              "AR",
              "MA",
              "ARMA",
              "ARIMA"
            )
          ),

          radioButtons(
            ns("bj_mode"),
            "Parameter Selection:",
            choices = c(
              "Manual",
              "Automatic (Best Model)"
            ),
            selected = "Manual"
          ),

          conditionalPanel(

            condition = sprintf(
              "input['%s'] == 'Manual'",
              ns("bj_mode")
            ),

            conditionalPanel(

              condition = sprintf(
                "input['%s'] == 'AR' || input['%s'] == 'ARMA' || input['%s'] == 'ARIMA'",
                ns("bj_method"),
                ns("bj_method"),
                ns("bj_method")
              ),

              numericInput(
                ns("bj_p"),
                "AR Order (p):",
                value = 1,
                min = 0,
                max = 5
              )
            ),

            conditionalPanel(

              condition = sprintf(
                "input['%s'] == 'MA' || input['%s'] == 'ARMA' || input['%s'] == 'ARIMA'",
                ns("bj_method"),
                ns("bj_method"),
                ns("bj_method")
              ),

              numericInput(
                ns("bj_q"),
                "MA Order (q):",
                value = 1,
                min = 0,
                max = 5
              )
            ),

            conditionalPanel(

              condition = sprintf(
                "input['%s'] == 'ARIMA'",
                ns("bj_method")
              ),

              numericInput(
                ns("bj_d"),
                "Differencing Order (d):",
                value = 1,
                min = 0,
                max = 2
              )
            )
          )
        )
      }
    })


    # --------------------------------------------------------
    # 5. FORECASTING ENGINE
    # --------------------------------------------------------

    model_result <- reactive({

      y_ts <- time_series()

      y <- as.numeric(y_ts)

      n <- length(y)

      h <- input$horizon

      req(input$category)


      # ======================================================
      # STATIONARY SERIES
      # ======================================================

      if (input$category == "Stationary Series") {

        method <- input$stationary_method

        # ----------------------------------------------------
        # Moving Average
        # ----------------------------------------------------

        if (method == "Moving Average") {

          if (input$ma_mode == "manual") {

            best_n <- min(
              input$ma_n,
              n - 1
            )

          } else {

            max_n <- min(
              input$ma_max_n,
              n - 1
            )

            best_n <- 1
            best_score <- Inf

            for (candidate in 1:max_n) {

              temp <- moving_average(
                y,
                candidate,
                1
              )

              if (!is.null(temp)) {

                metrics <- calculate_metrics(
                  y,
                  temp$fitted
                )

                score <- switch(
                  input$ma_metric,
                  "MAD" = metrics$MAD,
                  "MSE" = metrics$MSE,
                  "MAPE" = metrics$MAPE
                )

                if (!is.na(score) &&
                    score < best_score) {

                  best_score <- score
                  best_n <- candidate
                }
              }
            }
          }

          result <- moving_average(
            y,
            best_n,
            h
          )

          return(
            list(
              fitted = result$fitted,
              future = result$future,
              model_name = paste(
                "Moving Average (N =", best_n, ")"
              ),
              model = NULL
            )
          )
        }


        # ----------------------------------------------------
        # Weighted Moving Average
        # ----------------------------------------------------

        if (method == "Weighted Moving Average") {

          weights <- as.numeric(
            trimws(
              unlist(
                strsplit(
                  input$wma_weights,
                  ","
                )
              )
            )
          )

          weights <- weights[
            is.finite(weights)
          ]

          validate(
            need(
              length(weights) >= 1 &&
                sum(weights) > 0,
              "Please enter valid positive weights."
            )
          )

          result <- weighted_moving_average(
            y,
            weights,
            h
          )

          validate(
            need(
              !is.null(result),
              "The number of weights must be smaller than the number of observations."
            )
          )

          return(
            list(
              fitted = result$fitted,
              future = result$future,
              model_name = "Weighted Moving Average",
              model = NULL
            )
          )
        }


        # ----------------------------------------------------
        # Simple Exponential Smoothing
        # ----------------------------------------------------

        if (method == "Simple Exponential Smoothing") {

          if (input$ses_initial == "regression") {

            reg <- lm(
              y ~ seq_along(y)
            )

            initial_level <- as.numeric(
              coef(reg)[1] +
                coef(reg)[2]
            )

          } else {

            initial_level <- input$ses_l0
          }


          if (input$ses_mode == "manual") {

            alpha <- input$ses_alpha

            fit <- HoltWinters(
              y_ts,
              alpha = alpha,
              beta = FALSE,
              gamma = FALSE,
              l.start = initial_level
            )

          } else {

            fit <- HoltWinters(
              y_ts,
              beta = FALSE,
              gamma = FALSE
            )
          }


          fitted <- rep(
            NA,
            n
          )

          fitted[
            2:n
          ] <- fit$fitted[, "xhat"]


          future <- as.numeric(
            predict(
              fit,
              n.ahead = h
            )
          )


          return(
            list(
              fitted = fitted,
              future = future,
              model_name = "Simple Exponential Smoothing",
              model = fit
            )
          )
        }


        # ----------------------------------------------------
        # Seasonal Factors
        # ----------------------------------------------------

        if (method == "Seasonal Factors (Quick and Dirty)") {

          freq <- frequency(y_ts)

          validate(
            need(
              freq > 1,
              "Seasonal frequency must be greater than 1 for seasonal factors."
            )
          )

          validate(
            need(
              n >= 2 * freq,
              "At least two complete seasonal cycles are required."
            )
          )


          seasonal_mean <- tapply(
            y,
            rep(
              1:freq,
              length.out = n
            ),
            mean
          )

          seasonal_index <- seasonal_mean /
            mean(seasonal_mean)

          fitted <- rep(
            seasonal_index,
            length.out = n
          ) * mean(y)


          future <- rep(
            seasonal_index,
            length.out = n + h
          )[
            (n + 1):(n + h)
          ] * mean(y)


          return(
            list(
              fitted = fitted,
              future = future,
              model_name = "Seasonal Factors",
              model = NULL
            )
          )
        }
      }


      # ======================================================
      # TREND SERIES - HOLT
      # ======================================================

      if (input$category == "Trend Series") {

        if (input$holt_initial == "regression") {

          t <- seq_along(y)

          regression <- lm(
            y ~ t
          )

          initial_level <- coef(regression)[1] +
            coef(regression)[2]

          initial_slope <- coef(regression)[2]

        } else {

          initial_level <- input$holt_l0
          initial_slope <- input$holt_b0
        }


        if (input$holt_mode == "manual") {

          fit <- HoltWinters(
            y_ts,
            alpha = input$holt_alpha,
            beta = input$holt_beta,
            gamma = FALSE,
            l.start = initial_level,
            b.start = initial_slope
          )

        } else {

          fit <- HoltWinters(
            y_ts,
            gamma = FALSE
          )
        }


        fitted <- rep(
          NA,
          n
        )

        start <- n - nrow(fit$fitted) + 1

        fitted[start:n] <-
          fit$fitted[, "xhat"]


        future <- as.numeric(
          predict(
            fit,
            n.ahead = h
          )
        )


        return(
          list(
            fitted = fitted,
            future = future,
            model_name = "Holt's Method",
            model = fit
          )
        )
      }


      # ======================================================
      # SEASONAL SERIES
      # ======================================================

      if (input$category == "Seasonal Series") {

        freq <- frequency(y_ts)

        validate(
          need(
            freq > 1,
            "Seasonal frequency must be greater than 1."
          )
        )

        validate(
          need(
            n >= 2 * freq,
            "At least two complete seasonal cycles are required."
          )
        )


        # ----------------------------------------------------
        # Seasonal Decomposition
        # ----------------------------------------------------

        if (
          input$seasonal_method ==
          "Seasonal Decomposition"
        ) {

          decomposition <- decompose(
            y_ts,
            type = input$decomp_type
          )


          if (
            input$decomp_type ==
            "additive"
          ) {

            fitted <- decomposition$trend +
              decomposition$seasonal

            trend <- na.omit(
              decomposition$trend
            )

            trend_time <- seq_along(trend)

            regression <- lm(
              trend ~ trend_time
            )

            future_trend <- predict(
              regression,
              newdata = data.frame(
                trend_time =
                  (length(trend) + 1):
                    (length(trend) + h)
              )
            )

            seasonal_values <-
              rep(
                decomposition$seasonal,
                length.out = n + h
              )[
                (n + 1):(n + h)
              ]

            future <-
              as.numeric(future_trend) +
              seasonal_values

          } else {

            fitted <- decomposition$trend *
              decomposition$seasonal

            trend <- na.omit(
              decomposition$trend
            )

            trend_time <- seq_along(trend)

            regression <- lm(
              trend ~ trend_time
            )

            future_trend <- predict(
              regression,
              newdata = data.frame(
                trend_time =
                  (length(trend) + 1):
                    (length(trend) + h)
              )
            )

            seasonal_values <-
              rep(
                decomposition$seasonal,
                length.out = n + h
              )[
                (n + 1):(n + h)
              ]

            future <-
              as.numeric(future_trend) *
              seasonal_values
          }


          return(
            list(
              fitted = fitted,
              future = future,
              model_name =
                paste(
                  "Seasonal Decomposition -",
                  input$decomp_type
                ),
              model = decomposition
            )
          )
        }


        # ----------------------------------------------------
        # Winters' Method
        # ----------------------------------------------------

        if (
          input$seasonal_method ==
          "Winters' Method"
        ) {

          if (
            input$winters_initial ==
            "regression"
          ) {

            t <- seq_along(y)

            regression <- lm(
              y ~ t
            )

            initial_level <-
              coef(regression)[1] +
              coef(regression)[2]

            initial_slope <-
              coef(regression)[2]

          } else {

            initial_level <-
              input$winters_l0

            initial_slope <-
              input$winters_b0
          }


          if (
            input$winters_mode ==
            "manual"
          ) {

            fit <- HoltWinters(
              y_ts,
              alpha = input$winters_alpha,
              beta = input$winters_beta,
              gamma = input$winters_gamma,
              seasonal = input$winters_type,
              l.start = initial_level,
              b.start = initial_slope
            )

          } else {

            fit <- HoltWinters(
              y_ts,
              seasonal = input$winters_type
            )
          }


          fitted <- rep(
            NA,
            n
          )

          start <-
            n - nrow(fit$fitted) + 1

          fitted[start:n] <-
            fit$fitted[, "xhat"]


          future <- as.numeric(
            predict(
              fit,
              n.ahead = h
            )
          )


          return(
            list(
              fitted = fitted,
              future = future,
              model_name = "Winters' Method",
              model = fit
            )
          )
        }
      }


      # ======================================================
      # BOX-JENKINS
      # ======================================================

      if (input$category == "Box-Jenkins") {

        method <- input$bj_method


        # ----------------------------------------------------
        # Automatic Model
        # ----------------------------------------------------

        if (
          input$bj_mode ==
          "Automatic (Best Model)"
        ) {

          if (method == "AR") {

            fit <- auto.arima(
              y_ts,
              max.p = 5,
              max.q = 0,
              max.d = 0,
              seasonal = FALSE
            )

          } else if (method == "MA") {

            fit <- auto.arima(
              y_ts,
              max.p = 0,
              max.q = 5,
              max.d = 0,
              seasonal = FALSE
            )

          } else if (method == "ARMA") {

            fit <- auto.arima(
              y_ts,
              max.p = 5,
              max.q = 5,
              max.d = 0,
              seasonal = FALSE
            )

          } else {

            fit <- auto.arima(
              y_ts,
              max.p = 5,
              max.q = 5,
              max.d = 2,
              seasonal = FALSE
            )
          }

        } else {

          # --------------------------------------------------
          # Manual Model
          # --------------------------------------------------

          p <- 0
          q <- 0
          d <- 0

          if (
            method %in%
            c("AR", "ARMA", "ARIMA")
          ) {
            p <- input$bj_p
          }

          if (
            method %in%
            c("MA", "ARMA", "ARIMA")
          ) {
            q <- input$bj_q
          }

          if (
            method == "ARIMA"
          ) {
            d <- input$bj_d
          }

          fit <- forecast::Arima(
            y_ts,
            order = c(p, d, q)
          )
        }


        fitted <- as.numeric(
          fitted(fit)
        )

        future <- as.numeric(
          forecast(
            fit,
            h = h
          )$mean
        )


        return(
          list(
            fitted = fitted,
            future = future,
            model_name =
              paste(
                method,
                "Box-Jenkins"
              ),
            model = fit
          )
        )
      }
    })


    # --------------------------------------------------------
    # 6. FORECAST PLOT
    # --------------------------------------------------------

    output$forecast_plot <- renderPlot({

      y <- as.numeric(
        time_series()
      )

      result <- model_result()

      fitted <- result$fitted
      future <- result$future

      n <- length(y)
      h <- length(future)

      all_values <- c(
        y,
        fitted,
        future
      )

      plot(
        1:n,
        y,
        type = "o",
        pch = 16,
        col = "black",
        lwd = 2,
        xlim = c(1, n + h),
        ylim = range(
          all_values,
          na.rm = TRUE
        ),
        xlab = "Period",
        ylab = "Value",
        main = result$model_name
      )

      lines(
        1:n,
        fitted,
        col = "blue",
        lty = 2,
        lwd = 2
      )

      lines(
        (n + 1):(n + h),
        future,
        type = "o",
        pch = 17,
        col = "red",
        lwd = 2
      )

      abline(
        v = n,
        lty = 3
      )

      legend(
        "topleft",
        legend = c(
          "Actual",
          "Fitted",
          "Forecast"
        ),
        col = c(
          "black",
          "blue",
          "red"
        ),
        pch = c(
          16,
          NA,
          17
        ),
        lty = c(
          1,
          2,
          1
        ),
        lwd = 2
      )

      grid()
    })


    # --------------------------------------------------------
    # 7. PERFORMANCE METRICS
    # --------------------------------------------------------

    output$metrics_table <- renderTable({

      y <- as.numeric(
        time_series()
      )

      result <- model_result()

      metrics <- calculate_metrics(
        y,
        result$fitted
      )

      data.frame(
        Metric = c(
          "MAD",
          "MSE",
          "MAPE (%)"
        ),
        Value = c(
          round(metrics$MAD, 3),
          round(metrics$MSE, 3),
          round(metrics$MAPE, 2)
        )
      )
    })


    # --------------------------------------------------------
    # 8. FUTURE FORECAST TABLE
    # --------------------------------------------------------

    output$forecast_table <- renderTable({

      result <- model_result()

      n <- length(
        time_series()
      )

      data.frame(
        Period = (
          n + 1
        ):(
          n + length(result$future)
        ),

        Forecast = round(
          result$future,
          3
        )
      )
    })


    # --------------------------------------------------------
    # 9. ACF / PACF
    # --------------------------------------------------------

    output$acf_pacf_plot <- renderPlot({

      y <- time_series()

      par(
        mfrow = c(1, 2)
      )

      forecast::Acf(
        y,
        main = "Autocorrelation (ACF)"
      )

      forecast::Pacf(
        y,
        main = "Partial Autocorrelation (PACF)"
      )

      par(
        mfrow = c(1, 1)
      )
    })


    # --------------------------------------------------------
    # 10. BOX-JENKINS PARAMETERS
    # --------------------------------------------------------

    output$parameter_table <- renderTable({

      result <- model_result()

      fit <- result$model

      validate(
        need(
          inherits(fit, "Arima"),
          "Select Box-Jenkins to view parameter estimates."
        )
      )

      coefficients <- coef(fit)

      data.frame(
        Parameter = names(
          coefficients
        ),
        Estimate = round(
          as.numeric(
            coefficients
          ),
          4
        )
      )
    })


    # --------------------------------------------------------
    # 11. BOX-JENKINS SUMMARY
    # --------------------------------------------------------

    output$model_summary <- renderPrint({

      result <- model_result()

      fit <- result$model

      if (
        inherits(
          fit,
          "Arima"
        )
      ) {

        print(
          summary(fit)
        )

      } else {

        cat(
          "Select a Box-Jenkins model to view the model summary."
        )
      }
    })

  })
}
