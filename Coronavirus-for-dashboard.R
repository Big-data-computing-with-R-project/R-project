library(tidyr)
library(knitr)
library(normalr)
library(ggcorrplot)
library(leaflet)
library(plotly)
library(RColorBrewer)
library(readr)

#??src_mysql
my_db <- src_mysql(
  dbname = "covid19",
  host = "cvd-database.cmxpnyylhkiw.us-east-2.rds.amazonaws.com",
  user = "admin",
  password = "RprojectCovid19"
)
my_db
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


