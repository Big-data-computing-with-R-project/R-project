body_us <- dashboardBody(
fluidRow(style = "padding-left: 20px; padding-right: 20px; padding-bottom: 20px;",
         tags$head(
           tags$style(type = 'text/css', ".fluidRow { padding: 500px;}"),
           tags$style(type = 'text/css',".container { background-color: #F0F8FF80; padding: 20px; text-align: center; }"),
           tags$style(type = 'text/css', ".topic { text-align: center; font-weight: bold; font-family: 'Kanit', sans-serif;}"),
           tags$style(type = 'text/css', ".tab1 { color : #4c7093; padding-top: 20 px; padding-bottom: 20 px; padding-left: 2em; padding-right: 2em; }"),
           tags$style(type = 'text/css', ".inputtext { color : #000000; text-align: center; justify-content: center; }"),
           tags$style(type = 'text/css', ".normaltext { color : #000000; padding: 50px; height: 470px;  /*justify-content: center; */}"),
         ),
         fluidRow(
           column(12, style = "padding: 20px;",
                  h1("United States of America", class = "topic"),
                  h3("สหรัฐอเมริกา", class = "topic"),
                  br(),
                   fluidRow(
                     column(
                       valueBoxOutput("valueBox_confirmedUS", width = 3),
                       valueBoxOutput("valueBox_recoveredUS", width = 3),
                       valueBoxOutput("valueBox_deceasedUS", width = 3),
                       valueBoxOutput("valueBox_activeUS", width = 3),
                       width = 12,
                       #style = "margin-left: -20px"
                     )         
                   ),
                  br(),
                  ),
           
           fluidRow(style = "padding-top: 10px;",
             sidebarLayout(
               sidebarPanel(class="normaltext",
                            p(strong("US Coronavirus Cases")),
                            p("ศูนย์ควบคุมและป้องกันโรคในสหรัฐอเมริกา (CDC) ประกาศตรวจพบผู้ติดเชื้อในสหรัฐอเมริการายแรกในวันที่ 21 มกราคม 2020 ที่รัฐวอชิงตัน โดยผู้ติดเชื้อเดินทางกลับมาจากเมืองอู่ฮั่น ประเทศจีน หลังจากนั้นดอนัลด์ ทรัมป์ ประธานาธิบดีแห่งสหรัฐอเมริกาได้มีการประกาศภาวะฉุกเฉินด้านสาธารณสุขในสหรัฐอเมริกาในวันที่ 3 กุมภาพันธ์ 2020
                              "),
                            p("ข้อมูลโรคติดเชื้อไวรัสโคโรนา 2020 ในสหรัฐอเมริกาที่นำมาใช้วิเคราะห์มีการรวบรวมตั้งแต่วันที่ 21 มกราคม 2020 จนถึงวันที่ 24 ธันวาคม 2020
                              "),
                            p("ข้อมูลจาก: องค์การอนามัยโลก (World Health Organization : WHO)
                              ", style = "font-style: italic;")
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
           
           fluidRow(style = "padding-top: 10px;",
             column(12,
             h3("20 รัฐในสหรัฐอเมริกาที่มีจำนวนผู้ติดเชื้อสูงสุด
                ", class = "topic")
             )
             ),
           
           br(),
           fluidRow(style = "padding-top: 10px;",
             column(6,
                    plotlyOutput("plotrateus")
                    ),
             
             column(6,
                    style = "height:405px; background-color: white;",
                    DT::dataTableOutput("result")
                    )
             ),
           
           br(),
           fluidRow(style = "padding-top: 10px;",
           h3("ความถี่ของข้อมูลที่เกี่ยวข้องของ 20 รัฐที่มีผู้ติดเชื้อสูงสุด
              ", class = "topic"),
           ),
           
           fluidRow(style = "padding-top: 10px;",
                    column(6,
                           plotlyOutput("plotcorrus")
                           ),
                    
                    column(6,
                           plotOutput("clustering")
                           )
                    )
           )
  )
)
page_us <- dashboardPage(
  title   = "US",
  header  = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body    = body_us
)

