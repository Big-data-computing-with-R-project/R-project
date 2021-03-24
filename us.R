body_us <- dashboardBody(
  fluidRow(style = "padding: 40px",
    tags$head(
      tags$style(type = 'text/css', ".fluidRow {  
                                                              padding: 500px;"),
      tags$style(type = 'text/css',".container {
                                                              background-color: #F0F8FF80;
                                                              padding: 20px; 
                                                              text-align: center; }"),
      tags$style(type = 'text/css', "p {             
                                                              color : #000000;
                                                              font-size: 12pt; 
                                                              padding: 10px; }"),
      tags$style(type = 'text/css', ".countryname { 
                                                              text-align: center; 
                                                              font: bold; }"),
      tags$style(type = 'text/css', ".countrynameth { 
                                                              text-align: center; 
                                                              font: bold; }"),
      tags$style(type = 'text/css', ".tab1 { 
                                                              color : #4c7093;
                                                              padding-top: 20 px;
                                                              padding-bottom: 20 px;
                                                              padding-left: 2em; 
                                                              padding-right: 2em; }"),
      tags$style(type = 'text/css', ".sidebar { 
                                                              height: 450px; 
                                                              text-align: center;
                                                              justify-content: center;}"),
      
    ),
    column(12, style = "align: center;",
           h1("United States", class = "countryname"),
           h3("สหรัฐอเมริกา", class = "countrynameth"),
           br(), 
           fluidRow(
                    column(12,
                           style = "align: center;",
                           valueBoxOutput("confirmUSBox"),
                           valueBoxOutput("deathsUSBox"),
                            
                      
                    ),
                    
           ),br(),
    ),
    fluidRow(#style = "padding: 40px;",
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
      column(6,
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
    
    
  )
)
page_us <- dashboardPage(
  title   = "US",
  header  = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body    = body_us
)
