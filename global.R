library("shiny")
library("shinydashboard")
library("tidyverse")
library("leaflet")
library("plotly")
library("DT")
library("fs")
library("wbstats")
library("gridExtra")
library("dplyr")
library("lubridate")
library("magrittr")
library("ggplot2")
library("tidyr")
#library(knitr)
library("normalr")
library("ggcorrplot")
 
my_db <- src_mysql(
  dbname = "covid19",
  host = "cvd-database.cmxpnyylhkiw.us-east-2.rds.amazonaws.com",
  user = "admin",
  password = "RprojectCovid19"
)
my_db
data_confirmed <- tbl(my_db, sql("select * from covid19_confirmed"))
data_confirmed <- as.data.frame(data_confirmed)
data_confirmed
data_deceased <- tbl(my_db, sql("select * from covid19_deaths"))
data_deceased <- as.data.frame(data_deceased)
data_deceased
data_recovered <- tbl(my_db, sql("select * from covid19_recovered"))
data_recovered <- as.data.frame(data_recovered)
data_recovered
# TODO: Still throws a warning but works for now
#data_confirmed <- read_csv("data/time_series_covid19_confirmed_global.csv")
#data_deceased  <- read_csv("data/time_series_covid19_deaths_global.csv")
# data_recovered <- read_csv("data/time_series_covid19_recovered_global.csv")

# Get latest data
current_date <- as.Date(names(data_confirmed)[ncol(data_confirmed)], format = "%m/%d/%y")
#dates <- names(data_confirmed)[5:n.col]%>% mdy()

#changed_date <- file_info("data/covid19_data.zip")$change_time
#View(data_confirmed)
# Get evolution data by country
#data_confirmed_sub <- data_confirmed %>% 
#  pivot_longer(names_to = "date", cols = 5:ncol(data_confirmed)) %>%
#  group_by(`Province.State`, `Country.Region`, date, Lat, Long) %>%
#  summarise("confirmed" = sum(value, na.rm = T))

data_confirmed_sub <- data_confirmed %>% 
  gather(key=date, value=count, -c(Country.Region,Province.State, Lat, Long))%>% 
  mutate(date = date %>% mdy())%>% 
  group_by(`Province.State`, `Country.Region`, date, Lat, Long) %>% 
  summarise("confirmed"=sum(count, na.rm=T)) %>% 
  as.data.frame()
#View(data_confirmed_sub)

#data_recovered_sub <- data_recovered %>%
#   pivot_longer(names_to = "date", cols = 5:ncol(data_recovered)) %>%
#   group_by(`Province.State`, `Country.Region`, date, Lat, Long) %>%
#   summarise("recovered" = sum(value, na.rm = T))
data_recovered_sub <- data_recovered %>% 
  gather(key=date, value=count, -c(Country.Region,Province.State, Lat, Long))%>% 
  mutate(date = date %>% mdy())%>% 
  group_by(`Province.State`, `Country.Region`, date, Lat, Long) %>% 
  summarise("recovered"=sum(count, na.rm=T)) %>% 
  as.data.frame()
#View(data_recovered_sub)

#data_deceased_sub <- data_deceased %>%
#  pivot_longer(names_to = "date", cols = 5:ncol(data_deceased)) %>%
#  group_by(`Province.State`, `Country.Region`, date, Lat, Long) %>%
#  summarise("deceased" = sum(value, na.rm = T))
data_deceased_sub <- data_deceased %>% 
  gather(key=date, value=count, -c(Country.Region,Province.State, Lat, Long))%>% 
  mutate(date = date %>% mdy())%>% 
  group_by(`Province.State`, `Country.Region`, date, Lat, Long) %>% 
  summarise("deceased"=sum(count, na.rm=T)) %>% 
  as.data.frame()

data_evolution <- data_confirmed_sub %>%
  full_join(data_deceased_sub) %>%
  ungroup() %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  arrange(date) %>%
  group_by(`Province.State`, `Country.Region`, Lat, Long) %>%
  mutate(
    recovered = lag(confirmed, 14, default = 0) - deceased,
    recovered = ifelse(recovered > 0, recovered, 0),
    active = confirmed - recovered - deceased
  ) %>%
  pivot_longer(names_to = "var", cols = c(confirmed, recovered, deceased, active)) %>%
  ungroup()

# Calculating new cases
data_evolution <- data_evolution %>%
  group_by(`Province.State`, `Country.Region`) %>%
  mutate(value_new = value - lag(value, 4, default = 0)) %>%
  ungroup()


rm(data_confirmed, data_confirmed_sub, 
   data_recovered, data_recovered_sub, 
   data_deceased, data_deceased_sub)

# ---- Download population data ----
population <- wb(country = "countries_only", indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2020) %>%
  select(country, value) %>%
  rename(population = value)
countryNamesPop <- c("Brunei Darussalam", "Congo, Dem. Rep.", "Congo, Rep.", 
                     "Czech Republic","Egypt, Arab Rep.", "Iran, Islamic Rep.", 
                     "Korea, Rep.", "St. Lucia", "West Bank and Gaza", "Russian Federation",
                     "Slovak Republic", "United States", "St. Vincent and the Grenadines", "Venezuela, RB")
countryNamesDat <- c("Brunei", "Congo (Kinshasa)", "Congo (Brazzaville)", "Czechia", "Egypt", 
                     "Iran", "Korea, South","Saint Lucia", "occupied Palestinian territory", 
                     "Russia", "Slovakia", "US", "Saint Vincent and the Grenadines", "Venezuela")
population[which(population$country %in% countryNamesPop), "country"] <- countryNamesDat


# Data from wikipedia
noDataCountries <- data.frame(
  country    = c("Cruise Ship", "Guadeloupe", "Guernsey", "Holy See", "Jersey", "Martinique", "Reunion", "Taiwan*"),
  population = c(3700, 395700, 63026, 800, 106800, 376480, 859959, 23780452)
)
population <- bind_rows(population, noDataCountries)

data_evolution <- data_evolution %>%
  left_join(population, by = c("Country.Region" = "country"))
rm(population, countryNamesPop, countryNamesDat, noDataCountries)

data_atDate <- function(inputDate) {
  data_evolution[which(data_evolution$date == inputDate),] %>%
    distinct() %>%
    pivot_wider(id_cols = c("Province.State", "Country.Region", "date", "Lat", "Long", "population"), names_from = var, values_from = value) %>%
    filter(confirmed > 0 | recovered > 0 | deceased > 0 |  active > 0)
}

data_latests <- data_atDate(max(data_evolution$date))


top5_countries <- data_evolution %>%
  filter(var == "active", date == current_date) %>%
  group_by(`Country.Region`) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  arrange(desc(value)) %>%
  top_n(5) %>%
  select(`Country.Region`) %>%
  pull()




