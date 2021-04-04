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
library(Metrics)
library(gplots)
library(shinyWidgets)
library(deSolve) 
#install.packages("shiny")
#install.packages("shinydashboard")
#system("defaults write org.R-project.R force.LANG en_US.UTF-8")
Sys.setlocale("LC_ALL","thai")
source("ui_overview.R", local = TRUE)
source("Thai.R", encoding = "UTF-8", local = TRUE)
source("us.R", encoding = "UTF-8", local = TRUE)
source("model.R", encoding = "UTF-8", local = TRUE)
source("ui_fulltable.R", encoding = "UTF-8", local = TRUE)
source("map.R",local = TRUE)
source("fullTable.R", encoding = "UTF-8",local = TRUE)
# ------------------------ Define UI -------------------
ui <- fluidPage(
  title = "COVID-19 Global Cases",
  #tags$head(
  #  tags$link(rel = "shortcut icon", type = "image/png", href = "logo.png")
  #),
  tags$style(HTML("@import url('https://fonts.googleapis.com/css2?family=Kanit&display=swap');
                  body{
                    font-family: 'Kanit', sans-serif;
                    /*overflow-y: hidden; 
                    width: 100%;*/
                  }
                 ")),
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
             tabPanel("Overview", page_overview, value = "page-overview"),
             tabPanel("US", page_us, value = "page-us"),
             tabPanel("Thai", page_thai, value = "page-thai"),
             tabPanel("Table", page_fullTable, value = "page-fullTable" ),
             tabPanel("Model", page_model, value = "page-model")
  )
)
# Define server logic ----
server <- function(input, output) {
  # Trigger once an hour
  dataLoadingTrigger <- reactiveTimer(3600000)
  
  observeEvent(dataLoadingTrigger, {
    updateData()
  })
  observe({
    data <- data_atDate(input$timeSlider)
  })
  #--------------- over all ------
  output$dataTable = DT::renderDataTable({
    data.latest.all
  })
  output$plotmap <- renderLeaflet({
    map.confirmed 
  })
  
  output$overview_map <- renderLeaflet(map)
  
  #---------------- summary table -----------------------
  
  output$summaryTables <- renderUI({
    tabBox(
      tabPanel("Country.Region",
               div(
                 dataTableOutput("summaryDT_country"),
                 style = "margin-top: -10px")
      ),
      tabPanel("Province.State",
               div(
                 dataTableOutput("summaryDT_state"),
                 style = "margin-top: -10px"
               )
      ),
      width = 12
    )
  })
  output$summaryDT_country <- renderDataTable(getSummaryDT(data_atDate(current_date), "Country.Region", selectable = TRUE))
  proxy_summaryDT_country  <- dataTableProxy("summaryDT_country")
  output$summaryDT_state   <- renderDataTable(getSummaryDT(data_atDate(current_date), "Province.State", selectable = TRUE))
  proxy_summaryDT_state    <- dataTableProxy("summaryDT_state")
  
  observeEvent(input$timeSlider, {
    data <- data_atDate(input$timeSlider)
    replaceData(proxy_summaryDT_country, summariseData(data, "Country.Region"), rownames = FALSE)
    replaceData(proxy_summaryDT_state, summariseData(data, "Province.State"), rownames = FALSE)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$summaryDT_country_row_last_clicked, {
    selectedRow     <- input$summaryDT_country_row_last_clicked
    selectedCountry <- summariseData(data_atDate(input$timeSlider), "Country.Region")[selectedRow, "Country.Region"]
    location        <- data_evolution %>%
      distinct(`Country.Region`, Lat, Long) %>%
      dplyr::filter(`Country.Region` == selectedCountry) %>%
      summarise(
        Lat  = mean(Lat),
        Long = mean(Long)
      )
    leafletProxy("overview_map") %>%
      setView(lng = location$Long, lat = location$Lat, zoom = 4)
  })
  
  observeEvent(input$summaryDT_state_row_last_clicked, {
    selectedRow     <- input$summaryDT_state_row_last_clicked
    selectedCountry <- summariseData(data_atDate(input$timeSlider), "Province.State")[selectedRow, "Province.State"]
    location <- data_evolution %>%
      distinct(`Province.State`, Lat, Long) %>%
      dplyr::filter(`Province.State` == selectedCountry) %>%
      summarise(
        Lat  = mean(Lat),
        Long = mean(Long)
      )
    leafletProxy("overview_map") %>%
      setView(lng = location$Long, lat = location$Lat, zoom = 4)
  })
  observe({
    req(input$timeSlider, input$overview_map_zoom)
    zoomLevel               <- input$overview_map_zoom
    data                    <- data_atDate(input$timeSlider) %>% addLabel()
    #data$confirmedPerCapita <- data$confirmed / data$population * 100000
    #data$activePerCapita    <- data$active / data$population * 100000
    
    leafletProxy("overview_map", data = (data %>% select(-(Province.State)))) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng          = ~Long,
        lat          = ~Lat,
        radius       = ~log(confirmed^(zoomLevel/2)),
        stroke       = FALSE,
        color        = "red",
        fillOpacity  = 0.5,
        label        = ~label,
        labelOptions = labelOptions(textsize = 15),
        group        = "Confirmed"
      ) %>%
      # addCircleMarkers(
      #   lng          = ~Long,
      #   lat          = ~Lat,
      #   radius       = ~log(confirmedPerCapita^(zoomLevel)),
      #   stroke       = FALSE,
      #   color        = "maroon",
      #   fillOpacity  = 0.5,
      #   label        = ~label,
      #   labelOptions = labelOptions(textsize = 15),
      #   group        = "Confirmed (per capita)"
      # ) %>%
    addCircleMarkers(
      lng          = ~Long,
      lat          = ~Lat,
      radius       = ~log(recovered^(zoomLevel)),
      stroke       = FALSE,
      color        = "#74d47b",
      fillOpacity  = 0.5,
      label        = ~label,
      labelOptions = labelOptions(textsize = 15),
      group = "Recovered"
    ) %>%
      addCircleMarkers(
        lng          = ~Long,
        lat          = ~Lat,
        radius       = ~log(deceased^(zoomLevel)),
        stroke       = FALSE,
        color        = "	#8ec3f4", #light-blue
        fillOpacity  = 0.5,
        label        = ~label,
        labelOptions = labelOptions(textsize = 15),
        group        = "Deaths"
      ) %>%
      addCircleMarkers(
        lng          = ~Long,
        lat          = ~Lat,
        radius       = ~log(active^(zoomLevel / 2)),
        stroke       = FALSE,
        color        = "#ffc022", #yellow
        fillOpacity  = 0.5,
        label        = ~label,
        labelOptions = labelOptions(textsize = 15),
        group        = "Current Confirmed"
      ) #%>%
    # addCircleMarkers(
    #   lng          = ~Long,
    #   lat          = ~Lat,
    #   radius       = ~log(activePerCapita^(zoomLevel)),
    #   stroke       = FALSE,
    #   color        = "#EEEEEE",
    #   fillOpacity  = 0.5,
    #   label        = ~label,
    #   labelOptions = labelOptions(textsize = 15),
    #   group        = "Active (per capita)"
    # )
  })
  #--------- keyfigures ----------------
  observe({
    data <- data_atDate(input$timeSlider)
  })
  key_figures <- reactive({
    data           <- sumData(input$timeSlider)
    data_yesterday <- sumData(input$timeSlider - 1)
    
    data_new <- list(
      new_confirmed = (data$confirmed - data_yesterday$confirmed) / data_yesterday$confirmed * 100,
      new_recovered = (data$recovered - data_yesterday$recovered) / data_yesterday$recovered * 100,
      new_deceased  = (data$deceased - data_yesterday$deceased) / data_yesterday$deceased * 100,
      new_countries = data$countries - data_yesterday$countries
    )
    
    keyFigures <- list(
      "confirmed" = HTML(paste(format(data$confirmed, big.mark = ","), sprintf("<h4>(%+.1f %%)</h4>", data_new$new_confirmed))),
      "recovered" = HTML(paste(format(data$recovered, big.mark = ","), sprintf("<h4>(%+.1f %%)</h4>", data_new$new_recovered))),
      "deceased"  = HTML(paste(format(data$deceased, big.mark = ","), sprintf("<h4>(%+.1f %%)</h4>", data_new$new_deceased))),
      "countries" = HTML(paste(format(data$countries, big.mark = ","), "/ 195", sprintf("<h4>(%+d)</h4>", data_new$new_countries)))
    )
    return(keyFigures)
  })
  
  output$valueBox_confirmed <- renderValueBox({
    valueBox(
      key_figures()$confirmed,
      subtitle = "Confirmed",
      icon     = icon("file-medical"),
      color    = "red"
    )
  })
  
  output$valueBox_recovered <- renderValueBox({
    valueBox(
      key_figures()$recovered,
      subtitle = "Recovered",
      icon     = icon("heart"),
      color    = "green"
    )
  })
  
  output$valueBox_deceased <- renderValueBox({
    valueBox(
      key_figures()$deceased,
      subtitle = "Deaths",
      icon     = icon("heartbeat"),
      color    = "light-blue"
    )
  })
  
  output$valueBox_countries <- renderValueBox({
    valueBox(
      key_figures()$countries,
      subtitle = "Affected Countries",
      icon     = icon("flag"),
      color    = "yellow"
    )
  })
  
  output$box_keyFigures <- renderUI(box(
    title = paste0("Key Figures (", strftime(input$timeSlider, format = "%d.%m.%Y"), ")"),
    fluidRow(
      column(
        valueBoxOutput("valueBox_confirmed", width = 3),
        valueBoxOutput("valueBox_recovered", width = 3),
        valueBoxOutput("valueBox_deceased", width = 3),
        valueBoxOutput("valueBox_countries", width = 3),
        width = 12
        #style = "margin-left: 5px"
      )
    ),
    #div("Last updated: ", strftime(changed_date, format = "%d.%m.%Y - %R %Z")),
    width = 12
    )
  )
  #--------- US -------------
  key_figuresus <- reactive({
    keyFiguresus <- list(
      "confirmed" = HTML(paste(format(us_countriesConfirmed$value, big.mark = ","))),
      "recovered" = HTML(paste(format(us_countriesRecovered$value, big.mark = ","))),
      "deceased"  = HTML(paste(format(us_countriesDeceased$value, big.mark = ","))),
      "active" = HTML(paste(format(us_countriesActive$value, big.mark = ",")))
    )
    return(keyFiguresus)
  })
  
  output$valueBox_confirmedUS <- renderValueBox({
    valueBox(
      key_figuresus()$confirmed,
      subtitle = "Confirmed",
      icon     = icon("file-medical"),
      color    = "red"
    )
  })
  
  output$valueBox_recoveredUS <- renderValueBox({
    valueBox(
      key_figuresus()$recovered,
      subtitle = "Recovered",
      icon     = icon("heart"),
      color    = "green"
    )
  })
  
  output$valueBox_deceasedUS <- renderValueBox({
    valueBox(
      key_figuresus()$deceased,
      subtitle = "Deaths",
      icon     = icon("heartbeat"),
      color    = "light-blue"
    )
  })
  
  output$valueBox_activeUS <- renderValueBox({
    valueBox(
      key_figuresus()$active,
      subtitle = "Current Confirmed",
      icon     = icon("flag"),
      color    = "yellow"
    )
  })
  
  output$plotuscase <- renderPlotly({
    gly.plot_us.cases
  })
  
#   output$plotuscase <- renderPlotly({
#   plotus <- data_evolution %>% dplyr::filter(Country.Region == "US") %>%
#     select(Country.Region, date, value) 
#   
# })
  
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
        pageLength = 8
      )
    )
  )
  output$clustering <- renderPlot({
    heatmap.2(as.matrix(norm_data_plot),
              scale="none", 
              col = colorRampPalette(c("#6D9EC1","white","#E46726"))(n = 200),
              margins=c(10,6),trace="column")
  })
  #-------- Thailand ----------
  key_figuresth <- reactive({
    keyFiguresth <- list(
      "confirmed" = HTML(paste(format(th_countriesConfirmed$value, big.mark = ","))),
      "recovered" = HTML(paste(format(th_countriesRecovered$value, big.mark = ","))),
      "deceased"  = HTML(paste(format(th_countriesDeceased$value, big.mark = ","))),
      "active" = HTML(paste(format(th_countriesActive$value, big.mark = ",")))
    )
    return(keyFiguresth)
  })
  output$valueBox_confirmedTH <- renderValueBox({
    valueBox(
      key_figuresth()$confirmed,
      subtitle = "Confirmed",
      icon     = icon("file-medical"),
      color    = "red"
    )
  })
  
  output$valueBox_recoveredTH <- renderValueBox({
    valueBox(
      key_figuresth()$recovered,
      subtitle = "Recovered",
      icon     = icon("heart"),
      color    = "green"
    )
  })
  
  output$valueBox_deceasedTH <- renderValueBox({
    valueBox(
      key_figuresth()$deceased,
      subtitle = "Deaths",
      icon     = icon("heartbeat"),
      color    = "light-blue"
    )
  })
  
  output$valueBox_activeTH <- renderValueBox({
    valueBox(
      key_figuresth()$active,
      subtitle = "Current Confirmed",
      icon     = icon("flag"),
      color    = "yellow"
    )
  })
  output$plotthaicase <- renderPlotly({
    gly.plot_thai.cumulative
  })
  output$plotdailycasethai <- renderPlotly({
    gly.plot_thai.cases
  })
  output$plotthai <- renderPlotly({
    gly.province.thai
  })
  output$thairisk <- renderPlot({
    g.th.risk
  })
  output$thaiage <- renderPlotly({
    gly.data.thai.age
  })
  output$thainationality <- renderPlotly({
    gly.th.nationality
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
  #------------------------ Table ---------------------------------------------
  output$fullTable <- renderDataTable({
    data_n       <- getFullTableData("Country.Region")
    columNames <- c(
      "Country",
      "Total Confirmed",
      "New Confirmed",
      "Total Recovered",
      "New Recovered",
      "Total Deaths",
      "New Deaths",
      "Total Current Confirmed",
      "New Current Confirmed"
      )
    datatable(
      data_n,
      rownames  = FALSE,
      colnames  = columNames,
      escape    = FALSE,
      selection = "none",
      options   = list(
        pageLength     = -1,
        order          = list(1, "desc"),
        scrollX        = TRUE,
        scrollY        = "calc(100vh - 250px)",
        scrollCollapse = TRUE,
        dom            = "ft",
        server         = FALSE,
        columnDefs     = list(
          list(
            targets = c(2, 4, 6, 8),
            render  = JS(
              "function(data, type, row, meta) {
              if (data != null) {
                split = data.split('|')
                if (type == 'display') {
                  return split[1];
                } else {
                  return split[0];
                }
              }
            }"
            )
          ),
          list(className = 'dt-right', targets = 1:ncol(data) - 1),
          list(width = '100px', targets = 0),
          list(visible = FALSE, targets = 9:12)
        )
      )
    ) %>%
      formatStyle(
        columns    = "Country.Region",
        fontWeight = "bold"
      ) %>%
      formatStyle(
        columns         = "confirmed_new",
        valueColumns    = "confirmed_newPer",
        backgroundColor = styleInterval(c(10, 20, 33, 50, 75), c("NULL", "#FFE5E5", "#FFB2B2", "#FF7F7F", "#FF4C4C", "#983232")),
        color           = styleInterval(75, c("#000000", "#FFFFFF"))
      ) %>%
      formatStyle(
        columns         = "deceased_new",
        valueColumns    = "deceased_newPer",
        backgroundColor = styleInterval(c(10, 20, 33, 50, 75), c("NULL", "#FFE5E5", "#FFB2B2", "#FF7F7F", "#FF4C4C", "#983232")),
        color           = styleInterval(75, c("#000000", "#FFFFFF"))
      ) %>%
      formatStyle(
        columns         = "active_new",
        valueColumns    = "active_newPer",
        backgroundColor = styleInterval(c(-33, -20, -10, 10, 20, 33, 50, 75), c("#66B066", "#99CA99", "#CCE4CC", "NULL", "#FFE5E5", "#FFB2B2", "#FF7F7F", "#FF4C4C", "#983232")),
        color           = styleInterval(75, c("#000000", "#FFFFFF"))
      ) %>%
      formatStyle(
        columns         = "recovered_new",
        valueColumns    = "recovered_newPer",
        backgroundColor = styleInterval(c(10, 20, 33), c("NULL", "#CCE4CC", "#99CA99", "#66B066"))
      )
  })
  
  #-------------------------------- Model ----------------------------------------
  
  ## US (SEIRD Model) ##
  
  observe({
    updateSliderInput(
      inputId = "obsday",
      value = input$num_obsday
    )
  })
  
  observe({
    updateSliderInput(
      inputId = "num_obsday",
      value = input$obsday
    )
  })  
  
  inputmodel_us <- reactive({
    calculatemodel_us(input$s_us, input$e_us, input$i_us, input$r_us, input$d_us, input$obsday) 
  })
  
  output$plotmodel_us <- renderPlotly({
    gly.plotmodel_us <- ggplotly(
      ggplot(inputmodel_us(), aes(x = time)) + 
        geom_line(aes(y = S, color = "S (Sensitive)"), size = 1.5) +
        geom_line(aes(y = E, color = "E (Exposed)"), size = 1.5) +
        geom_line(aes(y = I, color = "I (Infected)"), size = 1.5) +
        geom_line(aes(y = R, color = "R (Recovered)"), size = 1.5) +
        geom_line(aes(y = D, color = "D (Deaths)"), size = 1.5) +
        labs(title = " SEIRD model for Covid-19 in US (With Protective)",
             x = "Days from 21 January 2020",
             y = "Estimated of population (Unit: Million)",
             color = "Legend") + 
        theme(plot.title = element_text(face="bold")) +
        scale_color_manual(values = colors_us ,breaks = c("S (Sensitive)", "E (Exposed)", "I (Infected)", "R (Recovered)","D (Deaths)"))
    )
  })
  
  output$select_parameter_us <- renderPlotly({
    choice_us <- switch(input$radio_us,
                        s_us_choice = ggplot(inputmodel_us(), aes(x = time)) +
                          geom_line(aes(y = S, color = "S (Sensitive)"), size = 1.5) + 
                          scale_color_manual(values = colors_us ,
                                             breaks = c("S (Sensitive)", "E (Exposed)", "I (Infected)", "R (Recovered)","D (Deaths)")) +
                          labs(x = "Days from 21 January 2020",
                               y = "S (Sensitive)") ,
                        
                        e_us_choice = ggplot(inputmodel_us(), aes(x = time)) +
                          geom_line(aes(y = E, color = "E (Exposed)"), size = 1.5) +
                          scale_color_manual(values = colors_us ,
                                             breaks = c("S (Sensitive)", "E (Exposed)", "I (Infected)", "R (Recovered)","D (Deaths)")) +
                          labs(x = "Days from 21 January 2020",
                               y = "E (Exposed)"),
                        
                        i_us_choice = ggplot(inputmodel_us(), aes(x = time)) +
                          geom_line(aes(y = I, color = "I (Infected)"), size = 1.5) +
                          scale_color_manual(values = colors_us ,
                                             breaks = c("S (Sensitive)", "E (Exposed)", "I (Infected)", "R (Recovered)","D (Deaths)")) +
                          labs(x = "Days from 21 January 2020",
                               y = "I (Infected)"),
                        
                        r_us_choice = ggplot(inputmodel_us(), aes(x = time)) +
                          geom_line(aes(y = R, color = "R (Recovered)"), size = 1.5) +
                          scale_color_manual(values = colors_us ,
                                             breaks = c("S (Sensitive)", "E (Exposed)", "I (Infected)", "R (Recovered)","D (Deaths)")) +
                          labs(x = "Days from 21 January 2020",
                               y = "R (Recovered)"),
                        
                        d_us_choice = ggplot(inputmodel_us(), aes(x = time)) +
                          geom_line(aes(y = D, color = "D (Deaths)"), size = 1.5) +
                          scale_color_manual(values = colors_us ,
                                             breaks = c("S (Sensitive)", "E (Exposed)", "I (Infected)", "R (Recovered)","D (Deaths)")) +
                          labs(x = "Days from 21 January 2020",
                               y = "D (Deaths)"),
                        
                        s_us_choice
    )
    
  })
  
  output$dataframe_us <- renderDataTable({
    datatable(
      inputmodel_us(), options = list(
        lengthMenu = FALSE,
        lengthChange = FALSE,
        pageLength = 10
      )
    )
  })
  
  #-----------------------------------------------
  
  ## Thai (SEIR Model) ##
  
  observe({
    updateSliderInput(
      inputId = "obsday_th",
      value = input$num_obsday_th
    )
  })
  
  observe({
    updateSliderInput(
      inputId = "num_obsday_th",
      value = input$obsday_th
    )
  })  
  
  inputmodel_th <- reactive({
    calculatemodel_th(input$s_th, input$e_th, input$i_th, input$r_th, input$obsday_th) 
  })
  
  output$plotmodel_th <- renderPlotly({
    gly.plotmodel_th <- ggplotly(
      ggplot(inputmodel_th(), aes(x = time)) + 
        geom_line(aes(y = S, color = "S (Sensitive)"), size = 1.5) +
        geom_line(aes(y = E, color = "E (Exposed)"), size = 1.5) +
        geom_line(aes(y = I, color = "I (Infected)"), size = 1.5) +
        geom_line(aes(y = R, color = "R (Recovered)"), size = 1.5) +
        labs(title = " SEIR model for Covid-19 in Thailand (With Protective)",
             x = "Days from 12 January 2020",
             y = "Estimated of population (Unit: Million)",
             color = "Legend") + 
        theme(plot.title = element_text(face="bold")) +
        scale_color_manual(values = colors_th ,breaks = c("S (Sensitive)", "E (Exposed)", "I (Infected)", "R (Recovered)"))
    )
  })
  
  output$select_parameter_th <- renderPlotly({
    choice_us <- switch(input$radio_th,
                        s_th_choice = ggplot(inputmodel_th(), aes(x = time)) +
                          geom_line(aes(y = S, color = "S (Sensitive)"), size = 1.5) + 
                          scale_color_manual(values = colors_th ,
                                             breaks = c("S (Sensitive)", "E (Exposed)", "I (Infected)", "R (Recovered)")) +
                          labs(x = "Days from 12 January 2020",
                               y = "S (Sensitive)") ,
                        
                        e_th_choice = ggplot(inputmodel_th(), aes(x = time)) +
                          geom_line(aes(y = E, color = "E (Exposed)"), size = 1.5) +
                          scale_color_manual(values = colors_th ,
                                             breaks = c("S (Sensitive)", "E (Exposed)", "I (Infected)", "R (Recovered)")) +
                          labs(x = "Days from 12 January 2020",
                               y = "E (Exposed)"),
                        
                        i_th_choice = ggplot(inputmodel_th(), aes(x = time)) +
                          geom_line(aes(y = I, color = "I (Infected)"), size = 1.5) +
                          scale_color_manual(values = colors_th ,
                                             breaks = c("S (Sensitive)", "E (Exposed)", "I (Infected)", "R (Recovered)")) +
                          labs(x = "Days from 12 January 2020",
                               y = "I (Infected)"),
                        
                        r_th_choice = ggplot(inputmodel_th(), aes(x = time)) +
                          geom_line(aes(y = R, color = "R (Recovered)"), size = 1.5) +
                          scale_color_manual(values = colors_th ,
                                             breaks = c("S (Sensitive)", "E (Exposed)", "I (Infected)", "R (Recovered)")) +
                          labs(x = "Days from 12 January 2020",
                               y = "R (Recovered)"),
                        
                        s_th_choice
    )
    
  })
  
  output$dataframe_th <- renderDataTable({
    datatable(
      inputmodel_th(), options = list(
        lengthMenu = FALSE,
        lengthChange = FALSE,
        pageLength = 10
      )
    )
  })
  
}




#-----------------------------------------------------------------------------------------------------------------

#runApp("app.R")

# Run the app ----
shinyApp(ui = ui, server = server)
