forecasting_ui <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      fileInput(ns("file"), "Upload Data (.csv)", accept = c(".csv")),
      helpText("If no file is uploaded, a default sample series will be used."),
      hr(),
      radioButtons(
        ns("mode"), "Method for Choosing N:",
        choices = list("Manual" = "manual", "Automatic (Best N)" = "auto"),
        selected = "manual"
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'manual'", ns("mode")),
        sliderInput(ns("n_manual"), "Select N (Window Size):", min = 1, max = 15, value = 3, step = 1)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'auto'", ns("mode")),
        sliderInput(ns("n_max"), "Max N to Search:", min = 2, max = 20, value = 10, step = 1),
        selectInput(
          ns("metric"), "Optimization Criterion (Minimize):",
          choices = list("MAD" = "MAD", "MSE" = "MSE", "MAPE (%)" = "MAPE"),
          selected = "MAD"
        )
      )
    ),
    
    mainPanel(
      h4("Moving Average Forecast Plot"),
      plotOutput(ns("forecast_plot")),
      hr(),
      h4("Performance Metrics"),
      tableOutput(ns("metrics_table"))
    )
  )
}

forecasting_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Veri Okuma / Hazırlama
    data_series <- reactive({
      req_file <- input$file
      if (is.null(req_file)) {
        # Varsayılan örnek talep serisi
        return(c(120, 130, 110, 150, 170, 160, 180, 140, 200, 210, 190, 220, 230, 210, 240))
      } else {
        df <- read.csv(req_file$datapath)
        # Sayısal olan ilk sütunu seri olarak al
        num_col <- df[[which(sapply(df, is.numeric))[1]]]
        return(as.numeric(na.omit(num_col)))
      }
    })
    
    # 2. Moving Average ve Hata Hesaplama Fonksiyonu
    calc_ma <- function(y, n) {
      len <- length(y)
      if (n >= len) return(NULL)
      
      forecasts <- rep(NA, len)
      for (t in (n + 1):len) {
        forecasts[t] <- mean(y[(t - n):(t - 1)])
      }
      
      errors <- y[(n + 1):len] - forecasts[(n + 1):len]
      mad <- mean(abs(errors))
      mse <- mean(errors^2)
      mape <- mean(abs(errors / y[(n + 1):len])) * 100
      
      list(forecasts = forecasts, mad = mad, mse = mse, mape = mape)
    }
    
    # 3. Modele göre En İyi N veya Seçilen N Belirleme
    results <- reactive({
      y <- data_series()
      len <- length(y)
      
      if (input$mode == "manual") {
        n_val <- min(input$n_manual, len - 1)
        res <- calc_ma(y, n_val)
        return(list(n = n_val, res = res))
      } else {
        max_n <- min(input$n_max, len - 1)
        best_n <- 1
        best_score <- Inf
        best_res <- NULL
        
        for (candidate_n in 1:max_n) {
          curr_res <- calc_ma(y, candidate_n)
          score <- switch(input$metric,
                          "MAD" = curr_res$mad,
                          "MSE" = curr_res$mse,
                          "MAPE" = curr_res$mape)
          if (!is.null(score) && score < best_score) {
            best_score <- score
            best_n <- candidate_n
            best_res <- curr_res
          }
        }
        return(list(n = best_n, res = best_res))
      }
    })
    
    # 4. Grafik Çıktısı
    output$forecast_plot <- renderPlot({
      y <- data_series()
      res_data <- results()
      forecasts <- res_data$res$forecasts
      n_used <- res_data$n
      
      plot(y, type = "o", col = "blue", pch = 16, lwd = 2,
           ylab = "Demand", xlab = "Period",
           main = paste0("Moving Average Forecast (N = ", n_used, ")"),
           ylim = range(c(y, forecasts), na.rm = TRUE))
      lines(forecasts, type = "o", col = "red", pch = 17, lwd = 2, lty = 2)
      legend("topleft", legend = c("Actual Demand", "Forecast"),
             col = c("blue", "red"), pch = c(16, 17), lty = c(1, 2), lwd = 2)
      grid()
    })
    
    # 5. Metrik Tablosu
    output$metrics_table <- renderTable({
      res_data <- results()
      res <- res_data$res
      
      data.frame(
        Metric = c("Selected / Best N", "MAD", "MSE", "MAPE (%)"),
        Value = c(
          res_data$n,
          round(res$mad, 3),
          round(res$mse, 3),
          paste0(round(res$mape, 2), " %")
        )
      )
    })
    
  })
}
