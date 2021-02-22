library(shiny)
library(DT)
library(markdown)
library(gridExtra)
library(dplyr)
library(lubridate)
library(magrittr)
library(ggplot2)
library(tidyr)
#library(knitr)
library(normalr)
library(ggcorrplot)
library(flexdashboard)
#library(MLRMPA)
#??src_mysql
my_db <- src_mysql(
  dbname = "covid",
  host = "127.0.0.1",
  user = "root",
  password = "1234"
)
my_db
# ------------------------ Define UI -------------------
ui <- fluidPage(
  navbarPage("COVID-19!",
             tabPanel("Over all"
             ),
             tabPanel("US",
                      
             ),
             tabPanel("Thailand",
                      
             ),
             tabPanel("DATA summary",
                      DT::dataTableOutput("dataTable")
                      
             )
  )
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
