body_model <- dashboardBody(
  fluidRow(style = "padding: 40 px;",
    column(12,
      h2("แบบจำลองทางคณิตศาสตร์สำหรับวางมาตรการป้องกันการแพร่ระบาด Covid-19
         ", class = "countryname")
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
                       column(6, numericInput("sus", label = "S", 10, min = 10, max = NA)),
                       column(6, numericInput("exposed", label = "E", 0, min = 0, max = 100)),
                       
                     ),
                     fluidRow(
                       column(6, numericInput("infect", label = "I", 0, min = 0, max = 100)),
                       column(6, numericInput("recover", label = "R", 0, min = 0, max = 100))
                    
                     ),
                     fluidRow(
                       column(12, numericInput("recover", label = "R", 0, min = 0, max = 100))
                     ),
                    
                     actionButton("seirdbutton", "สร้างโมเดล
                                  ")
                     
                     
        ),
      mainPanel(
        h3("Show model")
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