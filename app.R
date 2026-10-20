library(shiny)

ui <- navbarPage("Group 9 - Production Planning",
  tabPanel("Learning Curve", learning_ui("learning")),
  tabPanel("Forecasting", forecasting_ui("forecasting"))
)

server <- function(input, output, session) {
  learning_server("learning")
  forecasting_server("forecasting")
}

shinyApp(ui, server)
