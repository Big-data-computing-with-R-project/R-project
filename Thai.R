body_thai <- dashboardBody(
  fluidRow(style = "padding-left: 80px; padding-right: 80px; padding-bottom: 80px;",
           tags$head(
             tags$style(type = 'text/css', ".fluidRow { padding: 500px;}"),
             tags$style(type = 'text/css',".container { background-color: #F0F8FF80; padding: 20px; text-align: center; }"),
             tags$style(type = 'text/css', ".topic { text-align: center; font-weight: bold; font-family: 'Kanit', sans-serif;}"),
             tags$style(type = 'text/css', ".tab1 { color : #4c7093; padding-top: 20 px; padding-bottom: 20 px; padding-left: 2em; padding-right: 2em; }"),
             tags$style(type = 'text/css', ".inputtext { color : #000000; text-align: center; justify-content: center; }"),
             tags$style(type = 'text/css', ".normaltext { color : #000000; padding: 50px; height: 470px;  /*justify-content: center; */}"),
           ),
           column(12, style = "padding: 40px;",
                  h1("Kingdom of Thailand", class = "topic"),
                  h3("ราชอาณาจักรไทย", class = "topic"),
                  br(), 
                  fluidRow(style = "padding: 20px;",
                           column(
                             valueBoxOutput("valueBox_confirmedTH", width = 3),
                             valueBoxOutput("valueBox_recoveredTH", width = 3),
                             valueBoxOutput("valueBox_deceasedTH", width = 3),
                             valueBoxOutput("valueBox_activeTH", width = 3),
                             width = 12,
                             style = "margin-left: -20px"
                           ),         
                           sidebarLayout(
                             sidebarPanel( class="normaltext",
                                           p(strong("Thailand Coronavirus Cases")),
                                           p("ประเทศไทยมีการแพร่ระบาดไวรัสโคโรนา 2019 ครั้งแรกเมื่อวันที่ 12 มกราคม 2020 โดยเป็นประเทศที่มีผู้ป่วยยืนยันรายแรกนอกประเทศจีน ซึ่งเป็นนักท่องเที่ยวหญิงวัย 61 ปี สัญชาติจีน มีภูมิลำเนาอยู่ที่เมืองอู่ฮั่น ประเทศจีน ได้เดินทางออกจากเมืองอู่ฮั่นมายังท่าอากาศยานนานาชาติสุวรรณภูมิ 
                                      "),
                                           p("ต่อมาในวันที่ 31 มกราคม ชายไทยวัย 50 ปี ซึ่งขับแท็กซี่ในกรุงเทพมหานคร ได้รับผลตรวจว่าติดติดเชื้อไวรัสโคโรนา 2019 เพราะรับผู้โดยสารชาวจีนจากเมืองอู่ฮั่นซึ่งมีอาการป่วยไปส่งโรงพยาบาล ถือว่า ชายคนนี้เป็นคนไทยรายแรกที่ติดไวรัสโคโรน่าสายพันธุ์ใหม่ โดยไม่เคยมีประวัติเดินทางไปประเทศจีนมาก่อน นับได้ว่าเป็นจุดเริ่มต้นของการแพร่ระบาดไวรัสโคโรนา 2019 ในประเทศไทย 
                                      "),
                                           p("ข้อมูลที่นำมาใช้ในการวิเคราะห์รวบรวมตั้งแต่วันที่ 12 มกราคม 2020 – 12 มกราคม 2021
                                      "),
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
           br(),
           fluidRow(style = "padding-top: 20px;",
             column(12,
                    h3("10 จังหวัดที่มีจำนวนผู้ติดเชื้อสูงสุด", class = "topic")
                    )
             ),
           
           br(),
           fluidRow(
             column(6,
                    plotlyOutput("plotthai")
                    ),
             column(6,style = "height:410px; background-color: white;",
                    DT::dataTableOutput("resultth")
                    )
             ),
           )
  )
)

page_thai <- dashboardPage(
  title   = "Thai",
  header  = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body    = body_thai
)
