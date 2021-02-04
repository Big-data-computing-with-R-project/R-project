library(shiny)
library(markdown)

# Define UI ----
ui <- fluidPage(
  dashboardHeader(title = "Covid-19"),
  navbarPage("Navbar!",
             tabPanel("Over all"
             ),
             tabPanel("US",
                      
             ),
             tabPanel("Thailand",
                      
             )
  )
)

# Define server logic ----
server <- function(input, output) {
 
}

# Run the app ----
shinyApp(ui = ui, server = server)
