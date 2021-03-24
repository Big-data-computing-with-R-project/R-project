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
library(shinyjs)
source("ui_overview.R", local = TRUE)
source("Thai.R", encoding = "UTF-8", local = TRUE)
source("us.R", encoding = "UTF-8", local = TRUE)
source("model.R", encoding = "UTF-8", local = TRUE)

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
             tabPanel("Overview", page_overview, value = "page-overview"),
             tabPanel("US", page_us, value = "us"),
             tabPanel("Thai", page_thai, value = "Thai"),
             tabPanel("DATA Summary",
                      column(12, style = "padding: 40px;",
                             h1("Worldwide Covid-19 Cases", class = "countryname"),
                             #h3("สหรัฐอเมริกา", class = "countrynameth"),
                             br(), 
                      ),
                      column(12,style='padding:20px;',
                             DT::dataTableOutput("dataTable")
                      ),
             ),
             tabPanel("Model", page_model, value = "Model")
  )
)

packageVersion('flexdashboard')

# Define server logic ----
server <- function(input, output) {
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
  #---------------- summary rable -----------------------
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
      filter(`Country.Region` == selectedCountry) %>%
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
      filter(`Province.State` == selectedCountry) %>%
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
    data$confirmedPerCapita <- data$confirmed / data$population * 100000
    data$activePerCapita    <- data$active / data$population * 100000
    
    leafletProxy("overview_map", data = data) %>%
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
      addCircleMarkers(
        lng          = ~Long,
        lat          = ~Lat,
        radius       = ~log(confirmedPerCapita^(zoomLevel)),
        stroke       = FALSE,
        color        = "maroon",
        fillOpacity  = 0.5,
        label        = ~label,
        labelOptions = labelOptions(textsize = 15),
        group        = "Confirmed (per capita)"
      ) %>%
      addCircleMarkers(
        lng          = ~Long,
        lat          = ~Lat,
        radius       = ~log(recovered^(zoomLevel)),
        stroke       = FALSE,
        color        = "green",
        fillOpacity  = 0.5,
        label        = ~label,
        labelOptions = labelOptions(textsize = 15),
        group = "Estimated Recoveries"
      ) %>%
      addCircleMarkers(
        lng          = ~Long,
        lat          = ~Lat,
        radius       = ~log(deceased^(zoomLevel)),
        stroke       = FALSE,
        color        = "light-blue",
        fillOpacity  = 0.5,
        label        = ~label,
        labelOptions = labelOptions(textsize = 15),
        group        = "Deceased"
      ) %>%
      addCircleMarkers(
        lng          = ~Long,
        lat          = ~Lat,
        radius       = ~log(active^(zoomLevel / 2)),
        stroke       = FALSE,
        color        = "yellow",
        fillOpacity  = 0.5,
        label        = ~label,
        labelOptions = labelOptions(textsize = 15),
        group        = "Active"
      ) %>%
      addCircleMarkers(
        lng          = ~Long,
        lat          = ~Lat,
        radius       = ~log(activePerCapita^(zoomLevel)),
        stroke       = FALSE,
        color        = "#EEEEEE",
        fillOpacity  = 0.5,
        label        = ~label,
        labelOptions = labelOptions(textsize = 15),
        group        = "Active (per capita)"
      )
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
      "confirmed" = HTML(paste(format(data$confirmed, big.mark = " "), sprintf("<h4>(%+.1f %%)</h4>", data_new$new_confirmed))),
      "recovered" = HTML(paste(format(data$recovered, big.mark = " "), sprintf("<h4>(%+.1f %%)</h4>", data_new$new_recovered))),
      "deceased"  = HTML(paste(format(data$deceased, big.mark = " "), sprintf("<h4>(%+.1f %%)</h4>", data_new$new_deceased))),
      "countries" = HTML(paste(format(data$countries, big.mark = " "), "/ 195", sprintf("<h4>(%+d)</h4>", data_new$new_countries)))
    )
    return(keyFigures)
  })
  
  output$valueBox_confirmed <- renderValueBox({
    valueBox(
      key_figures()$confirmed,
      subtitle = "Confirmed",
      icon     = icon("file-medical"),
      color    = "red",
    )
  })
  
  output$valueBox_recovered <- renderValueBox({
    valueBox(
      key_figures()$recovered,
      subtitle = "Estimated Recoveries",
      icon     = icon("heart"),
      color    = "green"
    )
  })
  
  output$valueBox_deceased <- renderValueBox({
    valueBox(
      key_figures()$deceased,
      subtitle = "Deceased",
      icon     = icon("heartbeat"),
      color    = "light-blue",
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
        width = 12,
        style = "margin-left: -20px"
      )
    ),
    #div("Last updated: ", strftime(changed_date, format = "%d.%m.%Y - %R %Z")),
    width = 12
  ))
  #--------- US -------------
  output$confirmUSBox <- renderValueBox({
    valueBox(
      most_us$confirmed,
      subtitle = "Confirmed",
      icon     = icon("file-medical"),
      color    = "blue",
    )
  })
  
  output$deathsUSBox <- renderValueBox({
    valueBox(
      most_us$deaths,
      subtitle = "Deaths",
      icon     = icon("file-medical"),
      color    = "blue",
    )
  })
  
  output$plotuscase <- renderPlotly({
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
  output$plotthaicase <- renderPlotly({
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


#---------- Model --------------

init_us <- reactive({
  initval <- c(input$s_us, input$e_us, input$i_us, input$r_us, input$a_us)
  n <- sum(initval)
  return(initval)
  })

timepoints_us <- reactive({
  req(input$start_us, input$end_us)
  timepoints <- seq(input$start_us, input$end_us, by = 1)
  return(timepoints)
  })

parameter_us <- reactive({
  #Parameters
  #contact_rate = 10                # number of contacts per day
  #transmission_probability = 0.07 #24441852/328200000  # transmission probability
  infectious_period = 14         # infectious period 10-20
  latent_period = 5.2              # latent period
  deaths_period = 406196/24441852

  Ro = 2.5 #1.9-3.3
  
  beta_value = lambda * Ro * (1 / infectious_period)
  #beta_value = Ro * (1 / infectious_period)
  gamma_value = 1 / infectious_period
  delta_value = 1 / latent_period
  sigma_value = deaths_period

  #Compute Ro - Reproductive number.
  #Ro = beta_value / gamma_value
  parameterlist <- c(beta = beta_value, gamma = gamma_value, delta = delta_value, sigma = sigma_value)
  return(parameterlist)
})

createmodel <- reactive({
  model <- lsoda(init_us(), timepoints_us(), seir_modelUS, parameter_us())
  #model <- as.data.frame(model)
  return(model)
})

observeEvent(input$seirdbutton,{
  output$plotmodel_us <- renderTable({
    as.data.frame(createmodel())
  })
  output$printus <- renderPrint({
    output$nsum <- init_us()
  })
})

#output_us <- lsoda(initval_us, timepoints_us, seir_modelUS, parameter_us)
#ouput_us <- as.data.frame(output_us)


#Compute total population.


output$nsum <- renderDataTable({
  
  }) 

#output$plotmodel_us <- renderPlot({
#  ggplot()
#})

#Initial state values for the differential equations.
#initial_values = c (S = W/N, E = X/N, I = Y/N, R = Z/N, D = A/N)

######################################## US ####################################################

# #Parameters
# #contact_rate = 10                # number of contacts per day
# #transmission_probability = 0.07 #24441852/328200000  # transmission probability
# infectious_period = 14         # infectious period 10-20
# latent_period = 5.2              # latent period
# deaths_period = 406196/24441852
# 
# 
# Ro = 2.5 #1.9-3.3
# 
# #Compute values of beta (tranmission rate) and gamma (recovery rate).
# #beta_value = contact_rate * transmission_probability
# lambda = 0.8
# #beta_value = lambda * contact_rate * transmission_probability
# beta_value = lambda * Ro * (1 / infectious_period)
# #beta_value = Ro * (1 / infectious_period)
# gamma_value = 1 / infectious_period
# delta_value = 1 / latent_period
# sigma_value = deaths_period
# 
# #Compute Ro - Reproductive number.
# #Ro = beta_value / gamma_value
# Ro
# 
# #Disease dynamics parameters.
# parameter_list = c (beta = beta_value, gamma = gamma_value, delta = delta_value,sigma = sigma_value)
# 
# #Initial values for sub-populations.
# W = 328200000   # susceptible hosts
# X = 1           # infectious hosts
# Y = 0           # recovered hosts
# Z = 0           # exposed hosts
# A = 0
# 
# ######################## THAI ##################################
# 
# #Initial values for sub-populations.
# W = 69630000        # susceptible hosts
# X = 1           # infectious hosts
# Y = 0           # recovered hosts
# Z = 0           # exposed hosts
# #Compute total population.
# N = W + X + Y + Z
# #Initial state values for the differential equations.
# initial_values = c (S = W/N, E = X/N, I = Y/N, R = Z/N)
# 
# #Parameters
# contact_rate = 10834/366             # number of contacts per day
# transmission_probability =  (100*10834)/69630000     # transmission probability
# infectious_period = 5.2               # infectious period
# latent_period = 14
# 
# #Compute values of beta (tranmission rate) and gamma (recovery rate).
# lambda = 0.8
# #beta_value =  contact_rate * transmission_probability
# beta_value = lambda * contact_rate * transmission_probability
# #beta_value = lambda * Ro * (1 / infectious_period)
# #beta_value = Ro * (1 / infectious_period)
# gamma_value = 1 / infectious_period
# delta_value = 1 / latent_period
# Ro = beta_value / gamma_value
# #Disease dynamics parameters.
# parameter_list = c (beta = beta_value, gamma = gamma_value, delta = delta_value)
# Ro
# delta_value

}

#-----------------------------------------------------------------------------------------------------------------



# Run the app ----
shinyApp(ui = ui, server = server)
