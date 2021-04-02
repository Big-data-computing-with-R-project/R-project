library(shiny)
library(DT)
library(fs)
library(wbstats)
library(leaflet)
library(plotly)
library(tidyverse)
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
library(shinydashboard)
library(Metrics)
library(gplots)
library(shinyWidgets)
library("htmltools")
#system("defaults write org.R-project.R force.LANG en_US.UTF-8")
Sys.setlocale("LC_ALL","thai")
source("ui_overview.R", local = TRUE)
source("Thai.R", encoding = "UTF-8", local = TRUE)
source("us.R", encoding = "UTF-8", local = TRUE)
source("model.R", encoding = "UTF-8", local = TRUE)
source("ui_fulltable.R", encoding = "UTF-8", local = TRUE)
# ------------------------ Define UI -------------------
ui <- fluidPage(
  title = "COVID-19 Global Cases",
  #tags$head(
  #  tags$link(rel = "shortcut icon", type = "image/png", href = "logo.png")
  #),
  tags$style(HTML("@import url('https://fonts.googleapis.com/css2?family=Kanit&display=swap');
                  body{
                    font-family: 'Kanit', sans-serif;
                    /*overflow-y: hidden; 
                    width: 100%;*/
                  }
                 ")),
  tags$style(type = "text/css", ".container-fluid {padding-left: 0px; padding-right: 0px !important;}"),
  tags$style(type = "text/css", ".navbar {margin-bottom: 0px;}"),
  tags$style(type = "text/css", ".content {padding: 0px;}"),
  tags$style(type = "text/css", ".row {margin-left: 0px; margin-right: 0px;}"),
  tags$style(HTML(".col-sm-12 { padding: 5px; margin-bottom: -15px; }")),
  tags$style(HTML(".col-sm-6 { padding: 5px; margin-bottom: -15px; }")),
  navbarPage(title = div("COVID-19", style = "padding-left: 10px"),
             inverse = TRUE,
             collapsible = TRUE,
             fluid       = TRUE,
             tabPanel("Overview", page_overview, value = "page-overview"),
             tabPanel("US", page_us, value = "us"),
             tabPanel("Thai", page_thai, value = "Thai"),
             tabPanel("Table", page_fullTable, value = "page-fullTable" ),
             tabPanel("Model", page_model, value = "Model")
  )
)