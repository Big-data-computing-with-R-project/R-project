library(shiny)
library(shinydashboard)
library(DT)
library(fs)
library(wbstats)
library(leaflet)
library(plotly)
library(tidyverse)

# Define UI ----
ui <- fluidPage(
  dashboardHeader(title = "Covid-19"),
  dashboardSidebar(),
  dashboardBody(
  )
)

# Define server logic ----
server <- function(input, output) {
 
}

# Run the app ----
shinyApp(ui = ui, server = server)
