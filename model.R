body_model <- dashboardBody(
  fluidRow(style = "padding: 40 px;",
    column(12,
      h2(strong("แบบจำลองทางคณิตศาสตร์สำหรับวางมาตรการป้องกันการแพร่ระบาด Covid-19
         "), class = "countryname")
    ),
    
    column(12,
           h3("สหรัฐอเมริกา
              ", class = "countrynameth")
           ),
    fluidRow(
      sidebarLayout(
        sidebarPanel(class = "sidebar", width = 3, style = "align: center;",
                     p(strong("SEIRD Model", class = "format1")),
                     fluidRow(
                       column(6, numericInput("s_us", label = "S", 328200000, min = 1, max = 328200000)),
                       column(6, numericInput("e_us", label = "E", 0, min = 0, max = 100)),
                       
                     ),
                     fluidRow(
                       column(6, numericInput("i_us", label = "I", 0, min = 0, max = 100)),
                       column(6, numericInput("r_us", label = "R", 0, min = 0, max = 100))
                    
                     ),
                     fluidRow(
                       column(12, numericInput("d_us", label = "D", 0, min = 0, max = 100))
                     ),
                    
                     sliderInput("obsday", "จำนวนวัน
                                 ", value = c(0, 10), min = 0, max = 365),
                     verbatimTextOutput("nsum"),
                     actionButton("seirdbutton", "สร้างโมเดล
                                  ")
                     
                     
        ),
      mainPanel(
        h3("Show model"),
        dataTableOutput("plotmodel_us")
        )
      
      )
    )
  )
)
page_model <- dashboardPage(
  title   = "Model",
  header  = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body    = body_model
)