body_model <- dashboardBody(
  fluidRow(style = "padding-left: 80px; padding-right: 80px; padding-bottom: 80px;",
           tags$head(
             tags$style(type = 'text/css', ".fluidRow { padding: 500px;}"),
             tags$style(type = 'text/css',".container { background-color: #F0F8FF80; padding: 20px; text-align: center; }"),
             tags$style(type = 'text/css', ".topic { text-align: center; font-weight: bold; font-family: 'Kanit', sans-serif;}"),
             tags$style(type = 'text/css', ".tab1 { color : #4c7093; padding-top: 20 px; padding-bottom: 20 px; padding-left: 2em; padding-right: 2em; }"),
             tags$style(type = 'text/css', ".inputtext { color : #000000; text-align: center; justify-content: center; }"),
             tags$style(type = 'text/css', ".normaltext { color : #000000; padding: 50px; height: 470px;  /*justify-content: center; */}"),
           ),
    
           column(12, style = "padding: 40px",
           h2(strong("แบบจำลองทางคณิตศาสตร์สำหรับวางมาตรการป้องกันการแพร่ระบาด Covid-19
                     "), class = "topic")
           ),
    br(),
    

    tabsetPanel(
      # -------------------------------------- About Model ------------------------------------------------------
      tabPanel("About Model", 
                      h3("About Model", class = "topic"),
                      br(),
                      p("การจำลองสถานการณ์เกี่ยวกับไวรัสผ่านโมเดลทางคณิตศาสตร์ที่จะนำมาใช้นั้นจะเป็นการจำลองการแพร่กระจายในระดับประชากรผ่านสมมติฐาน โดยในที่นี้แบ่งออกเป็น 2 แบบจำลอง ได้แก่ แบบจำลอง SEIRD สำหรับสหรัฐอเมริกา และแบบจำลอง SEIR สำหรับสหราชอาณาจักรไทย
                        ", style = "padding-left: 100px; padding-right: 100px;"),
                      
               fluidRow(style = "padding: 40px;",
                        box(title = "SEIRD Model", solidHeader = TRUE, status = "primary", class = "normaltext",
                            p("แบบจำลอง SEIRD ใช้สมการเชิงเส้นที่ไม่ขึ้นกับเวลาลำดับแรกเพื่อจำลองว่าผู้ป่วยดำเนินไปอย่างไร จะมีความคล้ายกับ SEIR model แต่จะเพิ่มประเภทที่เสียชีวิตจากโรคระบาด เนื่องจากสหรัฐอเมริกามีจำนวนผู้เสียชีวิตสูงมาก จึงนำมาคำนวณด้วย โดยโมเดลจะแบ่งประชากรออกเป็น 5 ประเภท ได้แก่"),
                            
                            fluidRow(
                              column(1),
                              column(11,
                                     p("1)	กลุ่มเสี่ยงที่มีโอกาสติดเชื้อได้ (Susceptible)"),
                                     p("2)	กลุ่มที่ติดเชื้อที่อยู่ในระยะฟักตัว (Exposed) คือผู้ที่ติดเชื้อแล้ว แต่ยังไม่อยู่ในระยะที่สามารถแพร่เชื้อได้"),
                                     p("3)	กลุ่มที่ติดเชื้อที่สามารถแพร่เชื้อได้ (Infectious)"),
                                     p("4)	กลุ่มที่หายจากการติดเชื้อแล้ว และไม่มีโอกาสเป็นซ้ำ รวมถึงไม่สามารถแพร่เชื้อไปยังผู้อื่นได้ (Recovered)"),
                                     p("5)	กลุ่มที่เสียชีวิตจากโรคระบาด  (Deaths)"),
                                     )
                              ),
                            
                            p("โดย SEIRD model นั้น ประชากรแต่ละประเภทนั้นจะมีการเปลี่ยนสถานะอยู่ตลอดเวลา ขึ้นอยู่กับการกำหนดสมมติฐาน ปัจจัย และอัตราในการเพิ่ม หรือลด ของประชากรแต่ละประเภท ")
                        ),
                        
                        box(title =  "SEIR Model", solidHeader = TRUE, status = "primary",  class = "normaltext", 
                            
                            p("แบบจำลอง SEIR model (Susceptible, Exposed, Infectious, Recovered) โดยโมเดลจะแบ่งประชากรออกเป็น 4 ประเภท ได้แก่"),
                            fluidRow(
                              column(1),
                              column(11,
                                     p("1)	กลุ่มเสี่ยงที่มีโอกาสติดเชื้อได้ (Susceptible)"),
                                     p("2)	กลุ่มที่ติดเชื้อที่อยู่ในระยะฟักตัว (Exposed) คือผู้ที่ติดเชื้อแล้ว แต่ยังไม่อยู่ในระยะที่สามารถแพร่เชื้อได้"),
                                     p("3)	กลุ่มที่ติดเชื้อที่สามารถแพร่เชื้อได้ (Infectious)"),
                                     p("4)	กลุ่มที่หายจากการติดเชื้อแล้ว และไม่มีโอกาสเป็นซ้ำ รวมถึงไม่สามารถแพร่เชื้อไปยังผู้อื่นได้ (Recovered)"),
                                     ),
                            ),
                            p("โดย SEIR model นั้น ประชากรแต่ละประเภทนั้นจะมีการเปลี่ยนสถานะอยู่ตลอดเวลา ขึ้นอยู่กับการกำหนดสมมติฐาน ปัจจัย และอัตราในการเพิ่ม หรือลด ของประชากรแต่ละประเภท")
                        )
               )
               
               
               
      ),
      
      # ----------------------------------- Model US -------------------------------------------- 
      tabPanel("US",
               column(12,
               h3("สหรัฐอเมริกา
                  ", class = "topic"),
               br(),
               ),
               
               fluidRow(
                 sidebarLayout(
                   sidebarPanel(class = "inputtext", width = 3, 
                                column(12,
                                       h4(strong("SEIRD Model"), class = "topic"), br()
                                       ),
                                
                                fluidRow(
                                  column(6, numericInput("s_us", label = "S (Sensitive)", 328200000, min = 1, max = 328200000)),
                                  column(6, numericInput("e_us", label = "E (Exposed)", 0, min = 0, max = 100)),
                                  ),
                                
                                fluidRow(
                                  column(6, numericInput("i_us", label = "I (Infected)", 1, min = 0, max = 100)),
                                  column(6, numericInput("r_us", label = "R (Recovered)", 0, min = 0, max = 100))
                                  ),
                                
                                fluidRow(
                                  column(12, numericInput("d_us", label = "D (Deaths)", 0, min = 0, max = 100))
                                  ),
                                  
                                  sliderInput("obsday", "จำนวนวัน
                                              ", value = 1, min = 1, max = 1000),
                                              actionButton("modelbutton_us", "สร้างโมเดล
                                                           ")
                                ),
                   mainPanel(style = "align: center;",
                     plotlyOutput("plotmodel_us"),
                     dataTableOutput("dataframe_us", height = "40em")
                     )
                   )
                 )
               ),
      
      # ----------------------------------- Model Thai -----------------------------------------------------
      tabPanel("Thailand",
               column(12,
               h3("ราชอาณาจักรไทย
                  ", class = "topic"),
               br()
               ),
               
               fluidRow(
                 sidebarLayout(
                   sidebarPanel(class = "inputtext", width = 3, 
                                column(12, h4(strong("SEIR Model"), class = "topic"), br()),
                                
                                fluidRow(
                                  column(6, numericInput("s_th", label = "S (Sensitive)", 69630000, min = 1, max = 69630000)),
                                  column(6, numericInput("e_th", label = "E (Exposed)", 0, min = 0, max = 100)),
                                  ),
                                
                                fluidRow(
                                  column(6, numericInput("i_th", label = "I (Infected)", 1, min = 0, max = 100)),
                                  column(6, numericInput("r_th", label = "R (Recovered)", 0, min = 0, max = 100))
                                  ),
                                  
                                  sliderInput("obsday_th", "จำนวนวัน
                                              ", value = 1, min = 1, max = 1000
                                              ),
                                              actionButton("modelbutton_th", "สร้างโมเดล
                                                           ")
                                ),
                   mainPanel(style = "align: center;",
                     plotlyOutput("plotmodel_th"),
                     dataTableOutput("dataframe_th", height = "40em")
                     )
                   )
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