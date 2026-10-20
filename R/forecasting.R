# R/forecasting.R
library(shiny)
library(readxl)
library(forecast)

# --- Yardimci Hata Fonksiyonu ---
calc_metrics <- function(actual, fitted) {
  valid_idx <- which(!is.na(actual) & !is.na(fitted) & actual != 0)
  if (length(valid_idx) == 0) return(list(mad = NA, mse = NA, mape = NA))
  
  e <- actual[valid_idx] - fitted[valid_idx]
  mad <- mean(abs(e))
  mse <- mean(e^2)
  mape <- mean(abs(e / actual[valid_idx])) * 100
  list(mad = round(mad, 3), mse = round(mse, 3), mape = paste0(round(mape, 2), " %"))
}

forecasting_ui <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Data Input"),
      fileInput(ns("file"), "Upload Data (.csv, .xlsx, .xls)", accept = c(".csv", ".xlsx", ".xls")),
      uiOutput(ns("col_selector_ui")),
      numericInput(ns("freq"), "Seasonal Frequency (e.g., 4 for quarterly, 12 for monthly):", value = 4, min = 1),
      numericInput(ns("h_ahead"), "Forecast Horizon (Periods ahead):", value = 4, min = 1),
      hr(),
      
      h4("2. Method & Settings"),
      selectInput(ns("category"), "Forecasting Category:",
                  choices = c("Stationary Series", "Trend Series", "Seasonal Series", "Box-Jenkins")),
      
      uiOutput(ns("method_controls_ui"))
    ),
    
    mainPanel(
      tabsetPanel(
        id = ns("main_tabs"),
        tabPanel("Forecast & Evaluation",
                 h4("Forecast Plot"),
                 plotOutput(ns("main_plot"), height = "380px"),
                 hr(),
                 h4("Error Metrics (MAD, MSE, MAPE)"),
                 tableOutput(ns("metrics_table")),
                 hr(),
                 h4("Forecast Values (Future Periods)"),
                 tableOutput(ns("future_table"))
        ),
        tabPanel("Box-Jenkins Diagnostics",
                 h4("ACF and PACF Diagnostics"),
                 plotOutput(ns("acf_pacf_plot"), height = "400px"),
                 hr(),
                 h4("Model Coefficients & Summary"),
                 verbatimTextOutput(ns("arima_summary"))
        )
      )
    )
  )
}

forecasting_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Ham Veriyi Yukleme
    raw_df <- reactive({
      req_file <- input$file
      if (is.null(req_file)) {
        # Varsayilan ornek veri (Quarterly trend + mevsimsellik)
        t_seq <- 1:24
        sample_y <- 100 + 3.5 * t_seq + c(15, -10, 25, -30) + rnorm(24, 0, 4)
        return(data.frame(Period = t_seq, Demand = round(sample_y, 1)))
      }
      
      ext <- tools::file_ext(req_file$name)
      if (ext == "csv") {
        read.csv(req_file$datapath)
      } else if (ext %in% c("xlsx", "xls")) {
        as.data.frame(readxl::read_excel(req_file$datapath))
      } else {
        showNotification("Unsupported file format", type = "error")
        return(NULL)
      }
    })
    
    # Sutun Secim Arayuzu
    output$col_selector_ui <- renderUI({
      df <- raw_df()
      if (is.null(df)) return(NULL)
      num_cols <- names(df)[sapply(df, is.numeric)]
      if (length(num_cols) == 0) num_cols <- names(df)
      selectInput(ns("selected_col"), "Select Series / Column:", choices = num_cols, selected = num_cols[length(num_cols)])
    })
    
    # Seriyi ts objesi olarak alma
    target_ts <- reactive({
      df <- raw_df()
      req(df, input$selected_col)
      vec <- as.numeric(na.omit(df[[input$selected_col]]))
      freq_val <- if (is.null(input$freq) \vert{}\vert{} is.na(input$freq) || input$freq < 1) 1 else input$freq
      ts(vec, frequency = freq_val)
    })
    
    # 2. Dinamik Kontrol Menuleri
    output$method_controls_ui <- renderUI({
      req(input$category)
      
      if (input$category == "Stationary Series") {
        tagList(
          selectInput(ns("stat_method"), "Method:", 
                      choices = c("Moving Average", "Weighted Moving Average", 
                                  "Simple Exponential Smoothing (SES)", "Seasonal Factors (Quick & Dirty)")),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Moving Average'", ns("stat_method")),
            radioButtons(ns("ma_mode"), "Selection Mode:", choices = c("Manual", "Optimize Best N")),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Manual'", ns("ma_mode")),
              sliderInput(ns("ma_n"), "Window Size (N):", min = 2, max = 12, value = 3)
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Optimize Best N'", ns("ma_mode")),
              selectInput(ns("ma_criterion"), "Criterion to Minimize:", choices = c("MAD", "MSE", "MAPE"))
            )
          ),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Weighted Moving Average'", ns("stat_method")),
            textInput(ns("wma_weights"), "Weights (comma-separated, newest last, e.g. 0.2, 0.3, 0.5):", value = "0.2, 0.3, 0.5")
          ),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Simple Exponential Smoothing (SES)'", ns("stat_method")),
            radioButtons(ns("ses_mode"), "Parameter Mode:", choices = c("Manual Alpha", "Auto-Optimize Alpha")),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Manual Alpha'", ns("ses_mode")),
              sliderInput(ns("ses_alpha"), "Alpha (Smoothing Parameter):", min = 0.01, max = 0.99, value = 0.2, step = 0.01)
            ),
            radioButtons(ns("ses_init_mode"), "Initial Level (L0):", choices = c("Regression Fit / First Value", "Manual Entry")),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Manual Entry'", ns("ses_init_mode")),
              numericInput(ns("ses_l0"), "Initial Level (L0):", value = 100)
            )
          )
        )
        
      } else if (input$category == "Trend Series") {
        tagList(
          h5("Holt's Linear Method"),
          radioButtons(ns("holt_mode"), "Parameter Mode:", choices = c("Manual", "Auto-Optimize")),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Manual'", ns("holt_mode")),
            sliderInput(ns("holt_alpha"), "Alpha (Level):", min = 0.01, max = 0.99, value = 0.2, step = 0.01),
            sliderInput(ns("holt_beta"), "Beta (Trend):", min = 0.01, max = 0.99, value = 0.1, step = 0.01)
          ),
          radioButtons(ns("holt_init_mode"), "Initial Values (L0 & T0):", choices = c("Linear Regression Fit", "Manual Entry")),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Manual Entry'", ns("holt_init_mode")),
            numericInput(ns("holt_l0"), "Initial Level (L0):", value = 100),
            numericInput(ns("holt_b0"), "Initial Slope (T0):", value = 2)
          )
        )
        
      } else if (input$category == "Seasonal Series") {
        tagList(
          selectInput(ns("season_method"), "Method:", choices = c("Seasonal Decomposition", "Winters' Method")),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Seasonal Decomposition'", ns("season_method")),
            selectInput(ns("decomp_type"), "Type:", choices = c("additive", "multiplicative"))
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Winters\\' Method'", ns("season_method")),
            selectInput(ns("winters_type"), "Seasonal Type:", choices = c("additive", "multiplicative")),
            radioButtons(ns("winters_mode"), "Parameter Mode:", choices = c("Manual", "Auto-Optimize")),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Manual'", ns("winters_mode")),
              sliderInput(ns("winters_alpha"), "Alpha (Level):", min = 0.01, max = 0.99, value = 0.2, step = 0.01),
              sliderInput(ns("winters_beta"), "Beta (Trend):", min = 0.01, max = 0.99, value = 0.1, step = 0.01),
              sliderInput(ns("winters_gamma"), "Gamma (Season):", min = 0.01, max = 0.99, value = 0.1, step = 0.01)
            ),
            radioButtons(ns("winters_init_mode"), "Initial Values:", choices = c("Regression Fit", "Manual Entry")),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Manual Entry'", ns("winters_init_mode")),
              numericInput(ns("winters_l0"), "Initial Level (L0):", value = 100),
              numericInput(ns("winters_b0"), "Initial Slope (T0):", value = 2)
            )
          )
        )
        
      } else if (input$category == "Box-Jenkins") {
        tagList(
          radioButtons(ns("arima_mode"), "Model Selection:", choices = c("Manual (Specify p,d,q)", "Auto-ARIMA (Automatic)")),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Manual (Specify p,d,q)'", ns("arima_mode")),
            numericInput(ns("arima_p"), "AR Order (p):", value = 1, min = 0, max = 5),
            numericInput(ns("arima_d"), "Differencing (d):", value = 0, min = 0, max = 2),
            numericInput(ns("arima_q"), "MA Order (q):", value = 1, min = 0, max = 5)
          )
        )
      }
    })
    
    # 3. Model Hesaplamalari (Forecasting Engine)
    model_output <- reactive({
      y <- target_ts()
      req(length(y) > 3, input$category)
      h <- input$h_ahead
      n_len <- length(y)
      
      fitted_vals <- rep(NA, n_len)
      future_vals <- rep(NA, h)
      fit_obj <- NULL
      
      # A) STATIONARY SERIES
      if (input$category == "Stationary Series") {
        req(input$stat_method)
        
        if (input$stat_method == "Moving Average") {
          n_best <- input$ma_n
          if (input$ma_mode == "Optimize Best N") {
            best_val <- Inf
            crit <- input$ma_criterion
            for (cand in 2:min(10, n_len - 1)) {
              c_fit <- rep(NA, n_len)
              for (i in (cand + 1):n_len) c_fit[i] <- mean(y[(i - cand):(i - 1)])
              m <- calc_metrics(y, c_fit)
              score <- if (crit == "MAD") m$mad else if (crit == "MSE") m$mse else as.numeric(gsub(" %", "", m$mape))
              if (!is.na(score) && score < best_val) {
                best_val <- score
                n_best <- cand
              }
            }
          }
          for (i in (n_best + 1):n_len) fitted_vals[i] <- mean(y[(i - n_best):(i - 1)])
          future_vals <- rep(mean(tail(y, n_best)), h)
          
        } else if (input$stat_method == "Weighted Moving Average") {
          w <- as.numeric(trimws(unlist(strsplit(input$wma_weights, ","))))
          w <- w[!is.na(w)]
          if (length(w) == 0) w <- c(0.5, 0.5)
          w <- w / sum(w)
          k <- length(w)
          if (n_len > k) {
            for (i in (k + 1):n_len) fitted_vals[i] <- sum(y[(i - k):(i - 1)] * w)
            future_vals <- rep(sum(tail(y, k) * w), h)
          }
          
        } else if (input$stat_method == "Simple Exponential Smoothing (SES)") {
          alpha_val <- if (input$ses_mode == "Manual Alpha") input$ses_alpha else NULL
          init_val <- if (input$ses_init_mode == "Manual Entry") input$ses_l0 else y[1]
          fit_obj <- HoltWinters(y, alpha = alpha_val, beta = FALSE, gamma = FALSE, l.start = init_val)
          fitted_vals[2:n_len] <- fit_obj$fitted[, "xhat"]
          future_vals <- as.numeric(predict(fit_obj, n.ahead = h))
          
        } else if (input$stat_method == "Seasonal Factors (Quick & Dirty)") {
          freq_val <- frequency(y)
          if (freq_val > 1 && n_len >= 2 * freq_val) {
            n_cycles <- floor(n_len / freq_val)
            mat <- matrix(y[1:(n_cycles * freq_val)], ncol = freq_val, byrow = TRUE)
            cycle_means <- rowMeans(mat)
            ratios <- mat / cycle_means
            season_indices <- colMeans(ratios)
            season_indices <- season_indices / mean(season_indices)
            base_deseason <- mean(y)
            fitted_vals <- rep(season_indices, length.out = n_len) * base_deseason
            future_vals <- rep(season_indices, length.out = (n_len + h))[(n_len + 1):(n_len + h)] * base_deseason
          } else {
            fitted_vals <- rep(mean(y), n_len)
            future_vals <- rep(mean(y), h)
          }
        }
        
        # B) TREND SERIES (Holt's Linear)
      } else if (input$category == "Trend Series") {
        a_val <- if (input$holt_mode == "Manual") input$holt_alpha else NULL
        b_val <- if (input$holt_mode == "Manual") input$holt_beta else NULL
        
        l_init <- NULL
        b_init <- NULL
        if (input$holt_init_mode == "Manual Entry") {
          l_init <- input$holt_l0
          b_init <- input$holt_b0
        } else {
          t_reg <- 1:min(8, n_len)
          reg <- lm(y[t_reg] ~ t_reg)
          l_init <- as.numeric(coef(reg)[1])
          b_init <- as.numeric(coef(reg)[2])
        }
        
        fit_obj <- HoltWinters(y, alpha = a_val, beta = b_val, gamma = FALSE, l.start = l_init, b.start = b_init)
        fitted_vals[3:n_len] <- fit_obj$fitted[, "xhat"]
        future_vals <- as.numeric(predict(fit_obj, n.ahead = h))
        
        # C) SEASONAL SERIES
      } else if (input$category == "Seasonal Series") {
        req(input$season_method)
        freq_val <- frequency(y)
        
        if (input$season_method == "Seasonal Decomposition") {
          if (freq_val > 1 && n_len >= 2 * freq_val) {
            fit_decomp <- decompose(y, type = input$decomp_type)
            fit_obj <- fit_decomp
            trend_part <- na.omit(fit_decomp$trend)
            last_trend <- tail(trend_part, 1)
            slope <- mean(diff(trend_part), na.rm = TRUE)
            
            if (input$decomp_type == "additive") {
              fitted_vals <- fit_decomp$trend + fit_decomp$seasonal
              future_trend <- last_trend + (1:h) * slope
              future_season <- rep(fit_decomp$seasonal, length.out = (n_len + h))[(n_len + 1):(n_len + h)]
              future_vals <- future_trend + future_season
            } else {
              fitted_vals <- fit_decomp$trend * fit_decomp$seasonal
              future_trend <- last_trend + (1:h) * slope
              future_season <- rep(fit_decomp$seasonal, length.out = (n_len + h))[(n_len + 1):(n_len + h)]
              future_vals <- future_trend * future_season
            }
          }
          
        } else if (input$season_method == "Winters' Method") {
          req(freq_val > 1, n_len >= 2 * freq_val)
          is_mult <- (input$winters_type == "multiplicative")
          a_val <- if (input$winters_mode == "Manual") input$winters_alpha else NULL
          b_val <- if (input$winters_mode == "Manual") input$winters_beta else NULL
          g_val <- if (input$winters_mode == "Manual") input$winters_gamma else NULL
          
          l_init <- if (input$winters_init_mode == "Manual Entry") input$winters_l0 else NULL
          b_init <- if (input$winters_init_mode == "Manual Entry") input$winters_b0 else NULL
          
          fit_obj <- HoltWinters(y, alpha = a_val, beta = b_val, gamma = g_val, 
                                 seasonal = input$winters_type, l.start = l_init, b.start = b_init)
          start_idx <- n_len - nrow(fit_obj$fitted) + 1
          fitted_vals[start_idx:n_len] <- fit_obj$fitted[, "xhat"]
          future_vals <- as.numeric(predict(fit_obj, n.ahead = h))
        }
        
        # D) BOX-JENKINS (AR, MA, ARMA, ARIMA)
      } else if (input$category == "Box-Jenkins") {
        if (input$arima_mode == "Auto-ARIMA (Automatic)") {
          fit_obj <- forecast::auto.arima(y)
        } else {
          p <- input$arima_p
          d <- input$arima_d
          q <- input$arima_q
          fit_obj <- forecast::Arima(y, order = c(p, d, q))
        }
        fitted_vals <- as.numeric(fitted(fit_obj))
        fc <- forecast::forecast(fit_obj, h = h)
        future_vals <- as.numeric(fc$mean)
      }
      
      list(fitted = fitted_vals, future = future_vals, model = fit_obj)
    })
    
    # 4. Ana Grafik Ciktisi
    output$main_plot <- renderPlot({
      y <- target_ts()
      res <- model_output()
      req(res)
      
      n_len <- length(y)
      h <- length(res$future)
      total_len <- n_len + h
      
      full_forecast <- c(res$fitted, res$future)
      all_vals <- c(as.numeric(y), full_forecast)
      ylim_range <- range(all_vals, na.rm = TRUE)
      
      plot(1:n_len, y, type = "b", pch = 16, col = "black", lwd = 2,
           xlim = c(1, total_len), ylim = ylim_range,
           xlab = "Period", ylab = "Value", main = paste("Forecasting -", input$category))
      
      # Gecmis Uyum (Fitted)
      lines(1:n_len, res$fitted, col = "blue", lty = 2, lwd = 1.5)
      
      # Gelecek Tahmin (Future)
      lines((n_len + 1):total_len, res$future, type = "b", col = "red", pch = 17, lwd = 2, lty = 1)
      
      abline(v = n_len, col = "gray", lty = 3)
      legend("topleft", legend = c("Actual Data", "Fitted (In-Sample)", "Forecast (Out-of-Sample)"),
             col = c("black", "blue", "red"), pch = c(16, NA, 17), lty = c(1, 2, 1), lwd = 2)
      grid()
    })
    
    # 5. Hata Metrikleri Tablosu (MAD, MSE, MAPE)
    output$metrics_table <- renderTable({
      y <- target_ts()
      res <- model_output()
      req(res)
      
      metrics <- calc_metrics(as.numeric(y), res$fitted)
      data.frame(
        Metric = c("Mean Absolute Deviation (MAD)", "Mean Squared Error (MSE)", "Mean Absolute Percentage Error (MAPE)"),
        Value = c(as.character(metrics$mad), as.character(metrics$mse), metrics$mape)
      )
    })
    
    # 6. Gelecek Tahmin Degerleri Tablosu
    output$future_table <- renderTable({
      y <- target_ts()
      res <- model_output()
      req(res)
      
      n_len <- length(y)
      h <- length(res$future)
      data.frame(
        Period = (n_len + 1):(n_len + h),
        `Forecast Value` = round(res$future, 3)
      )
    })
    
    # 7. Box-Jenkins ACF / PACF Grafigi
    output$acf_pacf_plot <- renderPlot({
      y <- target_ts()
      req(y)
      par(mfrow = c(1, 2))
      forecast::Acf(y, main = "Autocorrelation (ACF)")
      forecast::Pacf(y, main = "Partial Autocorrelation (PACF)")
    })
    
    # 8. Box-Jenkins Model Ozeti
    output$arima_summary <- renderPrint({
      res <- model_output()
      req(res$model)
      if (inherits(res$model, "Arima")) {
        summary(res$model)
      } else {
        cat("Diagnostic summary is available when Box-Jenkins model is selected.")
      }
    })
    
  })
}
