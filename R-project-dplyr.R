#install.packages("magrittr") #pipe operation
#install.packages("lubridate") #date operations
#install.packages("tidyverse") #ggplot2,tidyr,dplyr
#install.packages("gridExtra") #multiple grid-based plots on a page
#install.packages("ggforce") #accelerating ggplot2
#install.packages("kableExtra") #complex tables
#install.packages("leaflet") #map

library(magrittr)
library(lubridate)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(ggforce)
library(kableExtra)
library(leaflet)

##load data into R
raw.data.full_data <- read.csv("~/R-project/full_datas.csv")
View(raw.data.full_data)
dim(raw.data.full_data) # row 31978 column 10
raw.data.full_data %>% ggplot(aes(total_cases))+geom_histogram()

raw.data.full_data[1:10,1:10] %>%
  kable('latex',booktabs=T,caption='Raw Data (first 10 columns only)') %>%
  kable_styling(font_size=5,latex_options = c('striped','hold_position','repeat_header'))
View(raw.data.full_data)

##get dates from column names
rdate <- as.Date(raw.data.full_data$date,"%Y-%m-%d")
range(rdate)
##search data in column
data <- raw.data.full_data %>% filter(date == "9/3/2020")

min.date <- min(rdate)
max.date <- max(rdate)
min.date.txt <- min.date %>% format('%d %b %Y')
max.date.txt <- max.date %>% format('%d %b %Y') %>% paste('UTC')

##find missing values
is.na(raw.data.full_data)
##count missing values
sum(is.na(raw.data.full_data))
##Omit missing values
na.omit(raw.data.full_data)
##Remove missing values
mean(raw.data.full_data$date,na.rm = TRUE)

#type data
str(raw.data.full_data)
##convert from character to date
data <- raw.data.full_data$date %>% as.Date(raw.data.full_data$date,"%Y-%m-%d")
str(data)
#View(data)
#remove some columns
data <- raw.data.full_data %>% select(-c(weekly_cases,weekly_deaths,biweekly_cases,biweekly_deaths))
View(data)

#Average of total_cases by location
test_totalcase <- aggregate(total_cases~location,raw.data.full_data,mean)
test_totalcase
barplot(test_totalcase$total_cases,names.arg = test_totalcase$location)


raw.data.full_data %>% group_by(date(date)) %>%
  summarize(avg_totalcase = total_cases) %>%
  ggplot(aes(avg_totalcase))+geom_histogram()


raw.data.full_data %>%
  group_by(location) %>%
  summarize(mean(total_cases),mean(total_deaths))

##ranking by total_cases top 10 country
data <- raw.data.full_data %>% filter(date == max(date))%>%
  select(location,date,new_cases,new_deaths,total_cases,total_deaths) %>%
  mutate(ranking = dense_rank(desc(total_cases)))
#view(data)
n <- 9
top_country <- data %>% filter(ranking <= n+1) %>%
  arrange(ranking) %>% pull(location) %>% as.character()
top_country %>% setdiff('World') %>% print()
#view(top_country)

data.latest <- data %>% filter(!is.na(location)) %>%
  mutate(location=ifelse(ranking <= n+1,as.character(location),'0ther')) %>%
  mutate(location = location %>% factor(levels=c(top_country),'0ther'))
#view(data.latest)

data.latest %<>% group_by(location) %>%
  summarise(total_cases = sum(total_cases),new_cases = sum(new_cases), 
            total_deaths = sum(total_deaths),new_deaths = sum(new_deaths))%>%
  mutate(death.rate = (100*total_deaths/total_cases) %>% round(1))
data.latest %<>% select(c(location,total_cases,new_cases,total_deaths,new_deaths ))
view(data.latest)



#current tatal cases
data.world <- raw.data.full_data %>% filter(location=='World') 
n <- nrow(data.world)
## current confirmed and daily new confirmed
plot1 <- ggplot(data.world, aes(x=date, y=total_cases)) + geom_point() + geom_smooth() +
  xlab('') + ylab('Count') + labs(title='total Cases') + theme(axis.text.x=element_text(angle=45, hjust=1))
plot2 <- ggplot(data.world, aes(x=date, y=new_cases)) +
  geom_point() + geom_smooth() +
  xlab('') + ylab('Count') + labs(title='Daily New  Cases') + theme(axis.text.x=element_text(angle=45, hjust=1))
## show two plots side by side
grid.arrange(plot1, plot2, ncol=2)



order(raw.data.full_data$date)
sorted <- raw.data.full_data[order(raw.data.full_data$date),]
view(raw.data.full_data)

