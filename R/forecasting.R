forecasting_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(ns("x"), "Girdi", 0, 10, 5),
    plotOutput(ns("grafik"))
  )
}

forecasting_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$grafik <- renderPlot({
      plot(1:10, input$x * (1:10))
    })
  })
}