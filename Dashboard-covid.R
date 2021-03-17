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
                                 column(
                                   valueBoxOutput("confirmBox"),
                                   valueBoxOutput("recoverBox"),
                                   valueBoxOutput("deathBox"),
                                   width = 12,
                                   style = "margin-left: -20px"
                               )
                               ),
                               fluidRow(
                                 column(8,
                                        leafletOutput("plotmap")
                                 ),
                                 column(
                                   uiOutput("summaryTables"),
                                   class = "summary",
                                   width = 4,
                                   style = 'padding:0px;'
                                 ),
                               )
                        )
                      )
             ),
             tabPanel("US",(fluidRow
                            (fluidRow(
                              column(6,style='padding:15px;',
                                     plotlyOutput("plotuscaes")
                                     ),
                              column(6,style='padding:15px',
                                     plotlyOutput("plotdailycaseus")
                                    )
                            ),
                            fluidRow(
                              column(6,
                                     plotlyOutput("plotrateus")),
                              column(6,style = "height:200px;background-color: white;",
                                     DT::dataTableOutput("result"))
                            ),
                            fluidRow(
                              column(6,
                                     plotlyOutput("plotcorrus")
                                     ),
                              column(6,
                                     )
                            )
                      )
                    )    
             ),
             tabPanel("Thailand",(fluidRow
                                  (fluidRow(
                                    column(6,style='padding:15px;',
                                           plotlyOutput("plotthaicaes")
                                    ),
                                    column(6,style='padding:15px',
                                           plotlyOutput("plotdailycasethai")
                                    )
                                  ),
                                    fluidRow(
                                      column(6,
                                             plotlyOutput("plotthai")
                                             ),
                                      column(6,style = "height:200px;background-color: white;",
                                             DT::dataTableOutput("resultth")
                                             )
                                    ),
                                    fluidRow(
                                      column(6,
                                             #plotlyOutput("plotcorrus")
                                      ),
                                      column(6,
                                      )
                                    )
                                  )
                                )    
                      
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
  #--------------- over all ------
  output$dataTable = DT::renderDataTable({
    data.latest.all
  })
  output$plotmap <- renderLeaflet({
    map.confirmed 
  })
  output$confirmBox <- renderValueBox({
    valueBox(
      mostconfirm$confirmed,
      subtitle = "Confirmed",
      icon     = icon("file-medical"),
      color    = "red",
    )
  })
  output$recoverBox <- renderValueBox({
    valueBox(
      mostrecover$recovered,
      subtitle = "Recover",
      icon     = icon("heart"),
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
  #--------- US -------------
  output$plotuscaes <- renderPlotly({
    gly.plot_us.cases
  })
  output$plotdailycaseus <- renderPlotly({
    gly.plot_us.newconf
  })
  output$plotrateus <- renderPlotly({
    gly.us.top1
  })
  output$plotcorrus <- renderPlotly({
    heatmapus
  })
  output$result <- DT::renderDataTable(
    DT::datatable(
      data.us.latest.show, options = list(
        lengthMenu = FALSE,
        lengthChange = FALSE,
        pageLength = 6
      )
    )
  )
  #-------- Thailand ----------
  output$plotthaicaes <- renderPlotly({
    gly.plot_thai.cumulative
  })
  output$plotdailycasethai <- renderPlotly({
    gly.plot_thai.cases
  })
  output$plotthai <- renderPlotly({
    gly.province.thai
  })
  output$resultth <- DT::renderDataTable(
    DT::datatable(
      data.province, options = list(
        lengthMenu = FALSE,
        lengthChange = FALSE,
        pageLength = 6
      )
    )
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)
