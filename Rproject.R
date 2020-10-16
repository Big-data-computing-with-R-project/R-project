#install.packages("magrittr") #pipe operation
#install.packages("lubridate") #date operations
#install.packages("tidyverse") #ggplot2,tidyr,dplyr
#install.packages("gridExtra") #multiple grid-based plots on a page
#install.packages("ggforce") #accelerating ggplot2
#install.packages("kableExtra") #complex tables
#install.packages("leaflet") #map
#install.packages("dbplyr")
#install.packages("dplyr")
#install.packages("RMySQL")
#install.packages("DBI")
#packageVersion("dplyr")

library(magrittr)
library(lubridate)
library(tidyverse)
library(gridExtra)
library(ggforce)
library(kableExtra)
library(leaflet)
library(dbplyr)
library(dplyr)
library(RMySQL)
library(DBI)

??src_mysql
my_db <- src_mysql(
  dbname = "full_data",
  host = "localhost",
  user = "root",
  password = "1234"
)

my_db


r <- tbl(my_db,
         sql("select * from full_data where total_cases > 10000 "))
r

View(r)

r <- tbl(my_db,
         sql("select date,location,total_cases,total_deaths from full_data
             "))
r
barplot(r$total_cases,names.arg = r&total_deaths)
d<-as.data.frame(r)
barplot(d$total_cases,names.arg = r&total_deaths)

View(r)

r <- tbl(my_db,
         sql("select location,total_cases,count(*) as total_deaths,avg(total_cases) as avg_total_cases
         from full_data group by location
         order by avg_total_case desc
             "))
r
barplot(r$avg_total_cases,names.arg = r&total_cases)
d<-as.data.frame(r)
barplot(d$avg_total_cases,names.arg = r&total_cases)
View(r)

rdate <- as.Date(full_data$date,"%m/%d/%y")
