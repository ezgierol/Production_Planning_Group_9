library(shiny)
library(readxl)
library(DT)
library(lpSolve)
library(forecast)

# All module files in the R/ folder are loaded automatically by Shiny.
# Modules that already existed in the group project (e.g. learning_curve.R)
# are added to the menu only if their files are present in R/.

existing_tabs <- list()
if (exists("learning_curve_ui")) {
  existing_tabs[[length(existing_tabs) + 1]] <-
    tabPanel("Learning Curve", learning_curve_ui("learning_curve"))
}

ui <- do.call(navbarPage, c(
  list(title = "Production Planning", id = "main_nav"),
  existing_tabs,
  list(
    forecasting_menu(),          # Assignment 2 - R/fc_*.R
    aggregate_planning_menu()    # Assignment 3 - R/ap_*.R
  )
))

server <- function(input, output, session) {
  if (exists("learning_curve_server")) learning_curve_server("learning_curve")
  forecasting_server()
  aggregate_planning_server()
}

shinyApp(ui, server)
