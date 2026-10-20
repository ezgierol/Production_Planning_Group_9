library(shiny)
library(forecast)

# ============================================================
# FORECASTING MODULE
# ============================================================

forecastingUI <- function(id) {
  ns <- NS(id)

  tagList(
    h2("Forecasting"),

    sidebarLayout(

      sidebarPanel(

        fileInput(
          ns("file"),
          "Upload CSV File",
          accept = ".csv"
        ),

        selectInput(
          ns("series"),
          "Select Series",
          choices = NULL
        ),

        numericInput(
          ns("frequency"),
          "Seasonal Frequency",
          value = 12,
          min = 2,
          step = 1
        ),

        numericInput(
          ns("horizon"),
          "Forecast Horizon",
          value = 5,
          min = 1,
          step = 1
        ),

        hr(),

        selectInput(
          ns("category"),
          "Series Type",
          choices = c(
            "Stationary Series",
            "Trend Series",
            "Seasonal Series",
            "Box-Jenkins"
          )
        ),

        uiOutput(ns("method_ui")),

        hr(),

        actionButton(
          ns("run"),
          "Run Forecast",
          class = "btn-primary"
        )
      ),

      mainPanel(

        h3("Forecast Plot"),
        plotOutput(ns("forecast_plot"), height = "450px"),

        h3("Forecast Values"),
        tableOutput(ns("forecast_table")),

        h3("Accuracy Measures"),
        tableOutput(ns("metrics")),

        uiOutput(ns("additional_output"))
      )
    )
  )
}


forecastingServer <- function(id) {

  moduleServer(id, function(input, output, session) {

    # --------------------------------------------------------
    # DATA
    # --------------------------------------------------------

    data_reactive <- reactive({

      if (is.null(input$file)) {

        data.frame(
          Demand = c(
            120, 125, 130, 128, 135,
            140, 138, 145, 150, 148,
            155, 160, 158, 165, 170
          )
        )

      } else {

        data <- read.csv(
          input$file$datapath,
          header = TRUE,
          stringsAsFactors = FALSE
        )

        numeric_columns <- names(
          data[sapply(data, is.numeric)]
        )

        if (length(numeric_columns) == 0) {
          stop("No numeric column was found in the uploaded file.")
        }

        data
      }
    })


    observe({

      data <- data_reactive()

      numeric_columns <- names(
        data[sapply(data, is.numeric)]
      )

      updateSelectInput(
        session,
        "series",
        choices = numeric_columns,
        selected = numeric_columns[1]
      )
    })


    selected_series <- reactive({

      data <- data_reactive()

      req(input$series)

      y <- data[[input$series]]

      y <- as.numeric(y)

      y <- y[!is.na(y)]

      return(y)
    })


    # --------------------------------------------------------
    # METHOD UI
    # --------------------------------------------------------

    output$method_ui <- renderUI({

      if (input$category == "Stationary Series") {

        selectInput(
          session$ns("method"),
          "Method",
          choices = c(
            "Moving Average",
            "Weighted Moving Average",
            "Simple Exponential Smoothing",
            "Seasonal Factors"
          )
        )

      } else if (input$category == "Trend Series") {

        selectInput(
          session$ns("method"),
          "Method",
          choices = c(
            "Holt's Method"
          )
        )

      } else if (input$category == "Seasonal Series") {

        selectInput(
          session$ns("method"),
          "Method",
          choices = c(
            "Seasonal Decomposition",
            "Winters' Method"
          )
        )

      } else {

        selectInput(
          session$ns("method"),
          "Method",
          choices = c(
            "AR",
            "MA",
            "ARMA",
            "ARIMA"
          )
        )
      }
    })


    # --------------------------------------------------------
    # FORECAST CALCULATION
    # --------------------------------------------------------

    result <- eventReactive(input$run, {

      y <- selected_series()

      h <- input$horizon

      freq <- input$frequency

      method <- input$method

      req(method)

      # ------------------------------------------------------
      # STATIONARY SERIES
      # ------------------------------------------------------

      if (input$category == "Stationary Series") {

        # ----------------------------------------------------
        # MOVING AVERAGE
        # ----------------------------------------------------

        if (method == "Moving Average") {

          n <- 3

          if (length(y) < n) {
            stop("Not enough observations for Moving Average.")
          }

          forecasts <- rep(
            mean(tail(y, n)),
            h
          )

          fitted <- rep(
            NA,
            length(y)
          )

          for (i in (n + 1):length(y)) {

            fitted[i] <- mean(
              y[(i - n):(i - 1)]
            )
          }

          list(
            forecasts = forecasts,
            fitted = fitted,
            type = "Moving Average"
          )
        }


        # ----------------------------------------------------
        # WEIGHTED MOVING AVERAGE
        # ----------------------------------------------------

        else if (method == "Weighted Moving Average") {

          n <- 3

          weights <- c(
            0.2,
            0.3,
            0.5
          )

          if (length(y) < n) {
            stop("Not enough observations for Weighted Moving Average.")
          }

          fitted <- rep(
            NA,
            length(y)
          )

          for (i in (n + 1):length(y)) {

            fitted[i] <- sum(
              y[(i - n):(i - 1)] * weights
            )
          }

          forecasts <- rep(
            sum(
              tail(y, n) * weights
            ),
            h
          )

          list(
            forecasts = forecasts,
            fitted = fitted,
            type = "Weighted Moving Average"
          )
        }


        # ----------------------------------------------------
        # SIMPLE EXPONENTIAL SMOOTHING
        # ----------------------------------------------------

        else if (method == "Simple Exponential Smoothing") {

          model <- ses(
            y,
            h = h
          )

          list(
            forecasts = as.numeric(model$mean),
            fitted = as.numeric(model$fitted),
            type = "Simple Exponential Smoothing",
            model = model
          )
        }


        # ----------------------------------------------------
        # SEASONAL FACTORS
        # ----------------------------------------------------

        else {

          if (length(y) < 2 * freq) {
            stop(
              "At least two complete seasons are required."
            )
          }

          ts_data <- ts(
            y,
            frequency = freq
          )

          decomposition <- decompose(
            ts_data,
            type = "multiplicative"
          )

          seasonal_indices <-
            decomposition$figure

          last_season <- tail(
            seasonal_indices,
            freq
          )

          base_forecast <- mean(
            tail(y, freq)
          )

          forecasts <- rep(
            base_forecast,
            h
          )

          for (i in seq_len(h)) {

            season_index <-
              ((length(y) + i - 1) %% freq) + 1

            forecasts[i] <-
              base_forecast *
              seasonal_indices[season_index]
          }

          list(
            forecasts = forecasts,
            fitted = rep(NA, length(y)),
            type = "Seasonal Factors"
          )
        }
      }


      # ------------------------------------------------------
      # TREND SERIES
      # ------------------------------------------------------

      else if (input$category == "Trend Series") {

        model <- holt(
          y,
          h = h
        )

        list(
          forecasts = as.numeric(model$mean),
          fitted = as.numeric(model$fitted),
          type = "Holt's Method",
          model = model
        )
      }


      # ------------------------------------------------------
      # SEASONAL SERIES
      # ------------------------------------------------------

      else if (input$category == "Seasonal Series") {

        ts_data <- ts(
          y,
          frequency = freq
        )


        # ----------------------------------------------------
        # SEASONAL DECOMPOSITION
        # ----------------------------------------------------

        if (method == "Seasonal Decomposition") {

          if (length(y) < 2 * freq) {
            stop(
              "At least two complete seasons are required."
            )
          }

          decomposition <- decompose(
            ts_data,
            type = "additive"
          )

          seasonal_component <-
            decomposition$seasonal

          trend_component <-
            decomposition$trend

          last_trend <- mean(
            tail(
              trend_component[
                !is.na(trend_component)
              ],
              freq
            )
          )

          seasonal_values <-
            tail(
              seasonal_component[
                !is.na(seasonal_component)
              ],
              freq
            )

          forecasts <- numeric(h)

          for (i in seq_len(h)) {

            season_index <-
              ((length(y) + i - 1) %% freq) + 1

            forecasts[i] <-
              last_trend +
              seasonal_values[season_index]
          }

          list(
            forecasts = forecasts,
            fitted = rep(NA, length(y)),
            type = "Seasonal Decomposition",
            decomposition = decomposition
          )
        }


        # ----------------------------------------------------
        # WINTERS
        # ----------------------------------------------------

        else {

          model <- hw(
            ts_data,
            h = h,
            seasonal = "additive"
          )

          list(
            forecasts = as.numeric(model$mean),
            fitted = as.numeric(model$fitted),
            type = "Winters' Method",
            model = model
          )
        }
      }


      # ------------------------------------------------------
      # BOX-JENKINS
      # ------------------------------------------------------

      else {

        # AR
        if (method == "AR") {

          model <- Arima(
            y,
            order = c(1, 0, 0)
          )
        }


        # MA
        else if (method == "MA") {

          model <- Arima(
            y,
            order = c(0, 0, 1)
          )
        }


        # ARMA
        else if (method == "ARMA") {

          model <- Arima(
            y,
            order = c(1, 0, 1)
          )
        }


        # ARIMA
        else {

          model <- auto.arima(
            y,
            seasonal = FALSE
          )
        }

        forecast_model <- forecast(
          model,
          h = h
        )

        list(
          forecasts = as.numeric(
            forecast_model$mean
          ),
          fitted = as.numeric(
            fitted(model)
          ),
          type = method,
          model = model,
          forecast_model = forecast_model
        )
      }

    })


    # --------------------------------------------------------
    # FORECAST PLOT
    # --------------------------------------------------------

    output$forecast_plot <- renderPlot({

      res <- result()

      y <- selected_series()

      h <- length(res$forecasts)

      total_length <-
        length(y) + h

      plot(
        1:length(y),
        y,
        type = "l",
        lwd = 2,
        xlab = "Time",
        ylab = "Value",
        main = paste(
          res$type,
          "Forecast"
        ),
        xlim = c(
          1,
          total_length
        )
      )

      lines(
        (length(y) + 1):total_length,
        res$forecasts,
        lwd = 2,
        lty = 2
      )

      abline(
        v = length(y),
        lty = 3
      )

      legend(
        "topleft",
        legend = c(
          "Actual",
          "Forecast"
        ),
        lty = c(
          1,
          2
        ),
        lwd = 2
      )
    })


    # --------------------------------------------------------
    # FORECAST TABLE
    # --------------------------------------------------------

    output$forecast_table <- renderTable({

      res <- result()

      data.frame(
        Period = seq_along(
          res$forecasts
        ),
        Forecast = round(
          res$forecasts,
          3
        )
      )
    })


    # --------------------------------------------------------
    # ACCURACY MEASURES
    # --------------------------------------------------------

    output$metrics <- renderTable({

      res <- result()

      y <- selected_series()

      fitted <- res$fitted

      valid <- !is.na(fitted)

      actual <- y[valid]
      predicted <- fitted[valid]

      if (length(actual) == 0) {

        return(
          data.frame(
            MAD = NA,
            MSE = NA,
            MAPE = NA
          )
        )
      }

      errors <- actual - predicted

      MAD <- mean(
        abs(errors)
      )

      MSE <- mean(
        errors^2
      )

      non_zero <- actual != 0

      if (sum(non_zero) == 0) {

        MAPE <- NA

      } else {

        MAPE <- mean(
          abs(
            errors[non_zero] /
              actual[non_zero]
          )
        ) * 100
      }

      data.frame(
        MAD = round(MAD, 3),
        MSE = round(MSE, 3),
        MAPE = round(MAPE, 3)
      )
    })


    # --------------------------------------------------------
    # ADDITIONAL OUTPUT
    # --------------------------------------------------------

    output$additional_output <- renderUI({

      res <- result()

      if (
        input$category == "Box-Jenkins"
      ) {

        tagList(

          h3("ACF Plot"),

          plotOutput(
            session$ns("acf_plot"),
            height = "350px"
          ),

          h3("PACF Plot"),

          plotOutput(
            session$ns("pacf_plot"),
            height = "350px"
          ),

          h3("Parameter Estimates"),

          tableOutput(
            session$ns("parameters")
          ),

          h3("Model Summary"),

          verbatimTextOutput(
            session$ns("model_summary")
          )
        )

      } else {

        NULL
      }
    })


    # --------------------------------------------------------
    # ACF
    # --------------------------------------------------------

    output$acf_plot <- renderPlot({

      y <- selected_series()

      Acf(
        y,
        main = "ACF"
      )
    })


    # --------------------------------------------------------
    # PACF
    # --------------------------------------------------------

    output$pacf_plot <- renderPlot({

      y <- selected_series()

      Pacf(
        y,
        main = "PACF"
      )
    })


    # --------------------------------------------------------
    # PARAMETER ESTIMATES
    # --------------------------------------------------------

    output$parameters <- renderTable({

      res <- result()

      model <- res$model

      coefficients <- coef(model)

      if (length(coefficients) == 0) {

        return(
          data.frame(
            Parameter = character(0),
            Estimate = numeric(0)
          )
        )
      }

      data.frame(
        Parameter = names(coefficients),
        Estimate = round(
          as.numeric(coefficients),
          4
        )
      )
    })


    # --------------------------------------------------------
    # MODEL SUMMARY
    # --------------------------------------------------------

    output$model_summary <- renderPrint({

      res <- result()

      if (!is.null(res$model)) {

        print(
          summary(res$model)
        )
      }
    })

  })
}
