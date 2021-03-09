library(shiny)
library(DT)
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
#library(MLRMPA)
#??src_mysql
my_db <- src_mysql(
  dbname = "covid19",
  host = "cvd-database.cmxpnyylhkiw.us-east-2.rds.amazonaws.com",
  user = "admin",
  password = "RprojectCovid19"
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
