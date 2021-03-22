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
             tabPanel("Overall",
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
                                 fluidRow(
                                   uiOutput("box_keyFigures")
                                 ),
                               ),
                               fluidRow(
                                 column(
                                   box(
                                     width = 12,
                                     leafletOutput("overview_map")
                                   ),
                                   class = "map",
                                   width = 7,
                                   style = 'padding:10px;'
                                 ),
                                 column(
                                   uiOutput("summaryTables"),
                                   class = "summary",
                                   width = 5,
                                   style = 'padding:0px;'
                                 ),
                               ),
                               fluidPage(
                                 sliderInput(
                                   "timeSlider",
                                   label = "select data",
                                   min = min(data_evolution$date),
                                   max = max(data_evolution$date),
                                   value = max(data_evolution$date),
                                   width = "100%",
                                   timeFormat = "%d/%m/%Y",
                                   animate = animationOptions(loop = TRUE)
                                 ),
                                 class = "slider",
                                 width = 12,
                                 style = 'padding:20px'
                               )
                        )
                      )
             ),
             tabPanel("US",
                      fluidRow(#style = "padding: 40px",
                               tags$head(
                                 tags$style(type = 'text/css', ".fluidRow {  
                                                              padding: 500px;"),
                                 tags$style(type = 'text/css',".container {
                                                              background-color: #F0F8FF80;
                                                              padding: 20px; 
                                                              text-align: center; }"),
                                 tags$style(type = 'text/css', "p .format1 { /*color : #4c7093;*/ 
                                                              font-size: 12pt; 
                                                              padding: 10px; }"),
                                 tags$style(type = 'text/css', ".countryname { 
                                                              text-align: center; 
                                                              font: bold; }"),
                                 tags$style(type = 'text/css', ".countrynameth { 
                                                              text-align: center; 
                                                              font: bold; }"),
                                 tags$style(type = 'text/css', ".tab1 { 
                                                              padding-top: 20 px;
                                                              padding-bottom: 20 px;
                                                              padding-left: 2em; 
                                                              padding-right: 2em; }"),
                                 tags$style(type = 'text/css', ".sidebar { 
                                                              height: 450px; 
                                                              text-align: center;
                                                              justify-content: center;}"),
                                 
                               ),
                               column(12, style = "padding: 40px;",
                                      h1("United States", class = "countryname"),
                                      h3("สหรัฐอเมริกา", class = "countrynameth"),
                                      br(), 
                                      fluidRow(style = "align: center;",
                                               #box("confirmUSBox", background = "purple"),  
                                               #box("deathsUSBox"),
                                               #column(12,
                                               #valueBoxOutput("confirmUSBox"),
                                               #valueBoxOutput("deathsUSBox"),
                                               #width = 12,
                                               #style = "margin-left: 100px; align: center;"
                                               #class = "container"
                                               #),
                                      ),
                               ),
                               fluidRow(style = "padding: 40px;",
                                        sidebarLayout(
                                          sidebarPanel( class="sidebar",
                                                        p(strong("US Coronavirus Cases", class = "format1")),
                                                        p("ศูนย์ควบคุมและป้องกันโรคในสหรัฐอเมริกา (CDC) ประกาศตรวจพบผู้ติดเชื้อในสหรัฐอเมริการายแรกในวันที่ 21 มกราคม 2020 ที่รัฐวอชิงตัน โดยผู้ติดเชื้อเดินทางกลับมาจากเมืองอู่ฮั่น ประเทศจีน หลังจากนั้นดอนัลด์ ทรัมป์ ประธานาธิบดีแห่งสหรัฐอเมริกาได้มีการประกาศภาวะฉุกเฉินด้านสาธารณสุขในสหรัฐอเมริกาในวันที่ 3 กุมภาพันธ์ 2020
                                                          ", class = "tab1"),
                                                        p("ข้อมูลโรคติดเชื้อไวรัสโคโรนา 2020 ในสหรัฐอเมริกาที่นำมาใช้วิเคราะห์มีการรวบรวมตั้งแต่วันที่ 21 มกราคม 2020 จนถึงวันที่ 24 ธันวาคม 2020
                                                          ", class = "tab1"),
                                                        #actionButton("cumulative", "Cumulative"),
                                                        #actionButton("daily", "Daily"),
                                                        #hr(),
                                          ),
                                          
                                          mainPanel(
                                            tabsetPanel(
                                              type = "tabs",
                                              tabPanel("Cumulative", br(), plotlyOutput("plotuscase")),
                                              tabPanel("Daily", br(), plotlyOutput("plotdailycaseus"))
                                            )
                                          )
                                        ),
                               ),
                               
                               fluidRow(
                                 column(12,
                                        h3("20 รัฐในสหรัฐอเมริกาที่มีจำนวนผู้ติดเชื้อสูงสุด
                                          ")
                                 )
                               ),
                               
                               fluidRow(
                                 column(6,style = "padding-top: 10px;",
                                        plotlyOutput("plotrateus")
                                 ),
                                 column(6,
                                        style = "height:200px; background-color: white;",
                                        DT::dataTableOutput("result")
                                 )
                               ),
                               
                               fluidRow(
                                 h3("ความถี่ของข้อมูลที่เกี่ยวข้องของ 20 รัฐที่มีผู้ติดเชื้อสูงสุด
                                          ", style = "padding-top: 70px;"),
                               ),
                               
                               fluidRow(style = "padding-top: 10px;",
                                        column(6,
                                               plotlyOutput("plotcorrus")
                                        ),
                                        column(6,
                                               plotOutput("clustering")
                                        )
                               )
                               
                               
                      ),
             ),
             tabPanel("Thailand",(fluidRow(style = "padding: 40px",
                                  column(12, 
                                         h1("Thailand", class = "countryname"),
                                         h3("ประเทศไทย", class = "countrynameth"),
                                         br(), 
                                         fluidRow(style = "padding: 40px;",
                                            sidebarLayout(
                                              sidebarPanel( class="sidebar",
                                                            p(strong("Thailand Coronavirus Cases", class = "format1")),
                                                            p("ประเทศไทยมีการแพร่ระบาดไวรัสโคโรนา 2019 ครั้งแรกเมื่อวันที่ 12 มกราคม 2020 โดยเป็นประเทศที่มีผู้ป่วยยืนยันรายแรกนอกประเทศจีน ซึ่งเป็นนักท่องเที่ยวหญิงวัย 61 ปี สัญชาติจีน มีภูมิลำเนาอยู่ที่เมืองอู่ฮั่น ประเทศจีน ได้เดินทางออกจากเมืองอู่ฮั่นมายังท่าอากาศยานนานาชาติสุวรรณภูมิ 
                                                              ", class = "tab1"),
                                                            p("ต่อมาในวันที่ 31 มกราคม ชายไทยวัย 50 ปี ซึ่งขับแท็กซี่ในกรุงเทพมหานคร ได้รับผลตรวจว่าติดติดเชื้อไวรัสโคโรนา 2019 เพราะรับผู้โดยสารชาวจีนจากเมืองอู่ฮั่นซึ่งมีอาการป่วยไปส่งโรงพยาบาล ถือว่า ชายคนนี้เป็นคนไทยรายแรกที่ติดไวรัสโคโรน่าสายพันธุ์ใหม่ โดยไม่เคยมีประวัติเดินทางไปประเทศจีนมาก่อน นับได้ว่าเป็นจุดเริ่มต้นของการแพร่ระบาดไวรัสโคโรนา 2019 ในประเทศไทย 
                                                              ", class = "tab1"),
                                                            p("ข้อมูลที่นำมาใช้ในการวิเคราะห์รวบรวมตั้งแต่วันที่ 12 มกราคม 2020 – 12 มกราคม 2021
                                                              ", class = "tab1"),
                                                              #actionButton("cumulative", "Cumulative"),
                                                              #actionButton("daily", "Daily"),
                                                              #hr(),
                                                            ),
                                                    
                                                    mainPanel(
                                                      tabsetPanel(
                                                        type = "tabs",
                                                        tabPanel("Cumulative", br(), plotlyOutput("plotthaicase")),
                                                        tabPanel("Daily", br(), plotlyOutput("plotdailycasethai"))
                                                      )
                                                    )
                                                  ),
                                         ),
                                         
                                         fluidRow(
                                           column(12,
                                                  h3("10 จังหวัดที่มีจำนวนผู้ติดเชื้อสูงสุด
                                          ")
                                           )
                                         ),
                                         
                                        fluidRow(
                                          column(6,
                                                 plotlyOutput("plotthai")
                                                 ),
                                          column(6,style = "height:200px; background-color: white;",
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
                                  
                                   
                      )
             ),
             tabPanel("DATA Summary",
                      column(12, style = "padding: 40px;",
                             h1("United States", class = "countryname"),
                             h3("สหรัฐอเมริกา", class = "countrynameth"),
                             br(), 
                      ),
                      column(12,style='padding:20px;',
                             DT::dataTableOutput("dataTable")
                      )
                      
             )
  )
)

packageVersion('flexdashboard')

# Define server logic ----
server <- function(input, output) {
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
        color        = "#00b3ff",
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
        color        = "#000000",
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
        color        = "#EEEEEE",
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
        color        = "#000000",
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
  key_figures <- reactive({
    data           <- sumData(input$timeSlider)
    data_yesterday <- sumData(input$timeSlider - 1)
    
    data_new <- list(
      new_confirmed = ((data$confirmed - data_yesterday$confirmed) / (data_yesterday$confirmed * 100)),
      new_recovered = ((data$recovered - data_yesterday$recovered) / (data_yesterday$recovered * 100)),
      new_deceased  = ((data$deceased - data_yesterday$deceased) / (data_yesterday$deceased * 100)),
      new_countries = (data$countries - data_yesterday$countries)
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
      color    = "yellow",
    )
  })
  
  
  output$valueBox_recovered <- renderValueBox({
    valueBox(
      key_figures()$recovered,
      subtitle = "Estimated Recoveries",
      icon     = icon("heart"),
      color    = "yellow"
    )
  })
  
  output$valueBox_deceased <- renderValueBox({
    valueBox(
      key_figures()$deceased,
      subtitle = "Deceased",
      icon     = icon("heartbeat"),
      color    = "yellow"
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
    div("Last updated: ", strftime(changed_date, format = "%d.%m.%Y - %R %Z")),
    width = 12
  ))
  #--------- US -------------
  output$confirmUSBox <- renderValueBox({
    valueBox(
      most_us$confirmed,
      subtitle = "Confirmed",
      icon     = icon("file-medical"),
      color    = "red",
    )
  })
  
  output$deathsUSBox <- renderValueBox({
    valueBox(
      most_us$deaths,
      subtitle = "Deaths",
      icon     = icon("file-medical"),
      color    = "yellow",
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
        pageLength = 6
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
}

# Run the app ----
shinyApp(ui = ui, server = server)
