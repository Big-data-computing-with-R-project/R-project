library(tidyr)
library(knitr)
library(normalr)
library(ggcorrplot)
library(leaflet)
library(plotly)
library(RColorBrewer)
library(readr)
library(magrittr)

#??src_mysql
my_db <- src_mysql(
  dbname = "covid19",
  host = "cvd-database.cmxpnyylhkiw.us-east-2.rds.amazonaws.com",
  user = "admin",
  password = "RprojectCovid19"
)
my_db
ui <- fluidPage(
  navbarPage("COVID-19!",
             tabPanel("Over all",
                      fluidRow(
                        column(12,"Fluid 12",
                               fluidRow(
                                 column(8,
                                        leafletOutput("plot1")
                                 ),
                                 column(width = 4,"Fluid 6"
                                        )
                               )
                        )
                      )
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
  output$dataTable = DT::renderDataTable({
    data.latest.all
  })
  output$plot1 <- renderLeaflet({
    map.confirmed 
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
#update.packages()

df_conf <- tbl(my_db, sql("select * from covid19_confirmed"))
df_conf <- as.data.frame(df_conf)
df_deaths <- tbl(my_db, sql("select * from covid19_deaths"))
df_deaths <- as.data.frame(df_deaths)
df_recover <- tbl(my_db, sql("select * from covid19_recovered"))
df_recover <- as.data.frame(df_recover)
##check the time frame of the data
n.col <- ncol(df_conf)
dates <- names(df_conf)[5:n.col]%>% mdy()
range(dates)
min.date <- min(dates)
max.date <- max(dates)
min.date.txt <- min.date %>% format('%d %b %Y')
max.date.txt <- max.date %>% format('%d %b %Y')
#clean data
cleanData <- function(data) {
  ## remove some columns
  data %<>% select(-c(Province.State, Lat, Long)) %>% rename(country=Country.Region)
  ## convert from wide to long format
  data %<>% gather(key=date, value=count, -country)
  ## convert from character to date
  data %<>% mutate(date = date %>% mdy())
  ## aggregate by country
  data %<>% group_by(country, date) %>% summarise(count=sum(count, na.rm=T)) %>% as.data.frame()
  return(data)
}
## clean the three data sets
data.confirmed <- df_conf %>% cleanData() %>% rename(confirmed=count)
data.deaths <- df_deaths %>% cleanData() %>% rename(deaths=count)
data.recovered <- df_recover %>% cleanData() %>% rename(recovered=count)
data <- data.confirmed %>% merge(data.deaths, all=T) %>% merge(data.recovered, all=T)
## countries/regions with confirmed cases, excl. cruise ships
countries <- data %>% pull(country) %>% setdiff('Cruise Ship')
data.world <- data %>% group_by(date) %>%
  summarise(country='World',
            confirmed = sum(confirmed, na.rm=T),
            deaths = sum(deaths, na.rm=T),
            recovered = sum(recovered, na.rm=T))
data %<>% rbind(data.world)
data %<>% mutate(current.confirmed = confirmed - deaths - recovered)
# World confirmed cases map
world.location <- df_conf
#View(world.location )
#*--------------------plot1----------------------------------------------------
world.location$confirmed <- world.location [, ncol(world.location )]
world.location  %<>% select(c(Country.Region, Province.State, Lat, Long, confirmed)) %>%
  mutate(txt=paste0(Country.Region, ' - ', Province.State, ': ', confirmed))
map.confirmed <- leaflet(width=1200, height=800) %>% addTiles()
# circle marker (units in pixels)
map.confirmed %<>% addCircleMarkers(world.location$Long, world.location$Lat,
                                    #radius=2+log2(x$confirmed),
                                    radius=0.02*sqrt(world.location$confirmed),
                                    stroke=F,
                                    color='red', fillOpacity=0.3,
                                    label=world.location$txt)
# world
map.confirmed
data %<>% arrange(country, date)
n <- nrow(data)
day1 <- min(data$date)
data %<>% mutate(new.confirmed = ifelse(date == day1, NA, confirmed - lag(confirmed, n=1)),
                 new.deaths = ifelse(date == day1, NA, deaths - lag(deaths, n=1)),
                 new.recovered = ifelse(date == day1, NA, recovered - lag(recovered, n=1)))
data %<>% mutate(new.confirmed = ifelse(new.confirmed < 0, 0, new.confirmed),
                 new.deaths = ifelse(new.deaths < 0, 0, new.deaths),
                 new.recovered = ifelse(new.recovered < 0, 0, new.recovered))
## death rate based on total deaths and recovered cases
data %<>% mutate(rate.upper = (100 * deaths / (deaths + recovered)) %>% round(1),
                 rate.upper = ifelse(is.nan(rate.upper), 0, rate.upper))

## lower bound: death rate based on total confirmed cases
data %<>% mutate(rate.lower = (100 * deaths / confirmed) %>% round(1),
                 rate.lower = ifelse(is.nan(rate.lower), 0, rate.lower))

## death rate based on the number of death/recovered on every single day
data %<>% mutate(rate.daily = (100 * new.deaths / (new.deaths + new.recovered)) %>% round(1),
                 rate.daily = ifelse(is.nan(rate.daily), 0, rate.daily))
## convert from wide to long format
data.long <- data %>%
  select(c(country, date, confirmed, current.confirmed, recovered, deaths)) %>%
  gather(key=type, value=count, -c(country, date))
## set factor levels to show them in a desirable order
data.long %<>% mutate(type=recode_factor(type, confirmed='Total Confirmed',
                                         current.confirmed='Current Confirmed',
                                         recovered='Recovered',
                                         deaths='Deaths'))
rates.long <- data %>%
  select(c(country, date, rate.upper, rate.lower, rate.daily)) %>%
  gather(key=type, value=count, -c(country, date))
# set factor levels to show them in a desirable order
rates.long %<>% mutate(type=recode_factor(type, rate.daily='Daily',
                                          rate.upper='Upper bound',
                                          rate.lower = 'Lower bound')) 



## ranking by confirmed cases
data.latest.all <- data %>% filter(date == max(date)) %>%
  select(country, date,confirmed, new.confirmed, current.confirmed,
         recovered, deaths, new.deaths, death.rate=rate.lower) %>%
  mutate(ranking = dense_rank(desc(confirmed))) %>%
  arrange(ranking)
data.latest <- data.latest.all %>% filter(!is.na(country)) %>%
  mutate(country=ifelse(ranking <= k + 1, as.character(country), 'Others')) %>%
  mutate(country=country %>% factor(levels=c(top.countries, 'Others')))
data.latest %<>% group_by(country) %>%
  summarise(confirmed=sum(confirmed), new.confirmed=sum(new.confirmed),
            current.confirmed=sum(current.confirmed),
            recovered=sum(recovered), deaths=sum(deaths), new.deaths=sum(new.deaths)) %>%
  mutate(death.rate=(100 * deaths/confirmed) %>% round(1)) 
data.latest %<>% select(c(country, confirmed, deaths, death.rate,
                          new.confirmed, new.deaths, current.confirmed,recovered)) %>%
  mutate(recover.rate=(100 * recovered/confirmed) %>% round(1))
mostconfirm <- data.latest %>% select(c(country,confirmed)) %>% filter(country=="World")
mostrecover <- data.latest %>% select(c(country,recovered)) %>% filter(country=="World")
mostdeath <- data.latest %>% select(c(country,deaths)) %>% filter(country=="World")
View(mostrecover)
View(mostdeath)
