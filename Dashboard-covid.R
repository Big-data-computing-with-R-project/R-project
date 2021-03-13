library(shiny)
library(DT)
library(fs)
library(wbstats)
library(leaflet)
library(plotly)
library(tidyverse)
library(markdown)
library(gridExtra)
library(dplyr)
library(lubridate)
library(magrittr)
library(ggplot2)
library(tidyr)
library(normalr)
library(ggcorrplot)
library(flexdashboard)
library(shinydashboard)

# ------------------------ Define UI -------------------
ui <- fluidPage(
  title = "COVID-19 Global Cases",
  #tags$head(
  #  tags$link(rel = "shortcut icon", type = "image/png", href = "logo.png")
  #),
  tags$style(type = "text/css", ".container-fluid {padding-left: 0px; padding-right: 0px !important;}"),
  tags$style(type = "text/css", ".navbar {margin-bottom: 0px;}"),
  tags$style(type = "text/css", ".content {padding: 0px;}"),
  tags$style(type = "text/css", ".row {margin-left: 0px; margin-right: 0px;}"),
  tags$style(HTML(".col-sm-12 { padding: 5px; margin-bottom: -15px; }")),
  tags$style(HTML(".col-sm-6 { padding: 5px; margin-bottom: -15px; }")),
  navbarPage(title = div("COVID-19", style = "padding-left: 10px"),
             inverse = TRUE,
             collapsible = TRUE,
             fluid       = TRUE,
             tabPanel("Over all",
                      fluidRow( 
                        tags$head(
                        tags$style(type = "text/css", "#overview_map {height: 48vh !important;}"),
                        tags$style(type = 'text/css', ".slider-animate-button { font-size: 20pt !important; }"),
                        tags$style(type = 'text/css', ".slider-animate-container { text-align: left !important; }"),
                        tags$style(type = "text/css", "@media (max-width: 991px) { .details { display: flex; flex-direction: column; } }"),
                        tags$style(type = "text/css", "@media (max-width: 991px) { .details .map { order: 1; width: 100%; } }"),
                        tags$style(type = "text/css", "@media (max-width: 991px) { .details .summary { order: 3; width: 100%; } }"),
                        tags$style(type = "text/css", "@media (max-width: 991px) { .details .slider { order: 2; width: 100%; } }")
                      ),
                        column(12,style='padding:10px;',
                               fluidRow(
                                 column(4,
                                        valueBoxOutput("confirmBox")),
                                 column(4,
                                        valueBoxOutput("recoverBox")),
                                 column(4,
                                        valueBoxOutput("deathBox")),
                               ),
                               fluidRow(
                                 column(8,
                                        leafletOutput("plot1")
                                 ),
                                 column(4,"Fluid 6"
                                 )
                               )
                        )
                      )
             ),
             tabPanel("US",
                      
             ),
             tabPanel("Thailand",
                      
             ),
             tabPanel("DATA summary",
                      column(12,style='padding:20px;',
                             DT::dataTableOutput("dataTable")
                      )
                      
             )
  )
)

# Define server logic ----
server <- function(input, output) {
  output$dataTable = DT::renderDataTable({
    data.latest.all
  })
  output$plot1 <- renderLeaflet({
    map.confirmed 
  })
  output$confirmBox <- renderValueBox({
    valueBox(
      mostconfirm$confirmed,
      subtitle = "Confirmed",
      icon     = icon("file-medical"),
      color    = "blue",
    )
  })
  output$recoverBox <- renderValueBox({
    valueBox(
      mostrecover$recovered,
      subtitle = "Recover",
      icon     = icon("file-medical"),
      color    = "yellow",
      width    = NULL
    )
  })
  output$deathBox <- renderValueBox({
    valueBox(
      mostdeath$deaths,
      subtitle = "Death",
      icon     = icon("file-medical"),
      color    = "yellow",
      width    = NULL
    )
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
