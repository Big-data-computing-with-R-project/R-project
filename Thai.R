body_thai <- dashboardBody(
  fluidRow(style = "padding: 40px",
           tags$head(tags$style(type = 'text/css', ".fluidRow {padding: 500px;"),
                     tags$style(type = 'text/css',".container {background-color: #F0F8FF80;padding: 20px text-align: center; }"),
                     tags$style(type = 'text/css', "p .format1 {color : #000000;font-size: 12pt;padding: 10px; }"),
                     tags$style(type = 'text/css', ".countryname {text-align: center;font: bold; }"),
                     tags$style(type = 'text/css', ".countrynameth {text-align: center;font: bold; }"),
                     tags$style(type = 'text/css', ".tab1 {color : #4c7093;padding-top: 20 px;padding-bottom: 20 px; padding-left: 2em;padding-right: 2em; }"),
                     tags$style(type = 'text/css', ".sidebar {height: 450px;text-align: center;justify-content: center;}"),
           ),
           column(12, 
                  h1("Thailand", class = "countryname"),
                  h3("ประเทศไทย", class = "countrynameth"),
                  br(), 
                  fluidRow(style = "padding: 40px;",
                           column(
                             valueBoxOutput("valueBox_confirmedTH", width = 3),
                             valueBoxOutput("valueBox_recoveredTH", width = 3),
                             valueBoxOutput("valueBox_deceasedTH", width = 3),
                             valueBoxOutput("valueBox_activeTH", width = 3),
                             width = 12,
                             style = "margin-left: -20px"
                           ),         
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
                           h3("10 จังหวัดที่มีจำนวนผู้ติดเชื้อ")
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
page_thai <- dashboardPage(
  title   = "Thai",
  header  = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body    = body_thai
)
