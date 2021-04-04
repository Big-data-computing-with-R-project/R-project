#install.packages("knitr")
#install.packages("grid")
#install.packages("MLRMPA")
#install.packages("dprep")
#install.packages("normalr")
#install.packages("ggcorrplot")
#install.packages("RColorBrewer")
#install.packages("rgdal")
#install.packages("jsonlite")
#install.packages("RColorBrewer")
#install.packages("readr")
# Sys.setlocale("LC_ALL", "English")
# Sys.setenv("LANGUAGE"="En")

library(gridExtra)
library(dplyr)
library(lubridate)
library(magrittr)
library(ggplot2)
library(tidyr)
library(knitr)
library(normalr)
library(ggcorrplot)
library(leaflet)
library(plotly)
library(RColorBrewer)
library(readr)
library(gplots)

#??src_mysql
my_db <- src_mysql(
  dbname = "covid19",
  host = "rproject.cmxpnyylhkiw.us-east-2.rds.amazonaws.com",
  user = "admin",
 password = "kmitlprojectCE-14")

my_db

##import data
df_conf <- tbl(my_db, sql("select * from covid19_confirmed"))
df_conf <- as.data.frame(df_conf)
df_conf
df_deaths <- tbl(my_db, sql("select * from covid19_deaths"))
df_deaths <- as.data.frame(df_deaths)
df_deaths
df_recover <- tbl(my_db, sql("select * from covid19_recovered"))
df_recover <- as.data.frame(df_recover)
df_recover

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
data
## countries/regions with confirmed cases, excl. cruise ships
countries <- data %>% pull(country) %>% setdiff('Cruise Ship')
data

data.world <- data %>% group_by(date) %>%
  summarise(country='World',
            confirmed = sum(confirmed, na.rm=T),
            deaths = sum(deaths, na.rm=T),
            recovered = sum(recovered, na.rm=T))
data %<>% rbind(data.world)
data
data %<>% mutate(current.confirmed = confirmed - deaths - recovered)
#View(data)

# World confirmed cases map
world.location <- df_conf
#View(world.location )
world.location$confirmed <- world.location [, ncol(world.location )]
world.location  %<>% select(c(Country.Region, Province.State, Lat, Long, confirmed)) %>%
  mutate(txt=paste0(Country.Region, ' - ', Province.State, ': ', confirmed))
map.confirmed <- leaflet(width=1200, height=800) %>% addTiles()
# circle marker (units in pixels)
map.confirmed %<>% addCircleMarkers(world.location$Long, world.location$Lat,
                        # radius=2+log2(x$confirmed),
                        radius=0.02*sqrt(world.location$confirmed),
                        stroke=F,
                        color='red', fillOpacity=0.3,
                        label=world.location$txt)
# world
map.confirmed

## China
map.confirmed %>% setView(95, 35, zoom=4)
## Australia and New Zealand
map.confirmed %>% setView(135, -27, zoom=4)
## US and Canada
map.confirmed %>% setView(-105, 40, zoom=4)
## Europe
map.confirmed %>% setView(10, 50, zoom=4)

# new.confirmed new.deaths new.recovered
data %<>% arrange(country, date)
n <- nrow(data)
day1 <- min(data$date)
data %<>% mutate(new.confirmed = ifelse(date == day1, NA, confirmed - dplyr::lag(confirmed, n=1)),
                 new.deaths = ifelse(date == day1, NA, deaths - dplyr::lag(deaths, n=1)),
                 new.recovered = ifelse(date == day1, NA, recovered - dplyr::lag(recovered, n=1)))
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

#View(data)

## convert from wide to long format
data.long <- data %>%
  select(c(country, date, confirmed, current.confirmed, recovered, deaths)) %>%
  gather(key=type, value=count, -c(country, date))
## set factor levels to show them in a desirable order
data.long %<>% mutate(type=recode_factor(type, confirmed='Total Confirmed',
                                         current.confirmed='Current Confirmed',
                                         recovered='Recovered',
                                         deaths='Deaths'))
#View(data.long)

##Number of case World
world <- dplyr::filter(data.long,country == 'World')
world
plot_world.area <- world %>% dplyr::filter(type != 'Total Confirmed') %>%
  ggplot(aes(x=date, y=count)) +
  geom_area(aes(fill=type), alpha=0.5) +
  labs(title=paste0('Numbers of Cases Worldwide - ', max.date.txt)) +
  scale_fill_manual(values=c('red', 'green', 'black')) +
  theme(legend.title=element_blank(), legend.position='bottom',
        plot.title = element_text(size=15),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.key.size=unit(0.8, 'cm'),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.text.x=element_text(angle=45, hjust=1))
plot_world.line <- world %>%
  ggplot(aes(x=date, y=count,color = type)) +
  geom_line(aes(color=type)) +
  labs(title=paste0('Numbers of Cases Worldwide (log scale) - ', max.date.txt)) +
  scale_color_manual(values=c('purple', 'red', 'green', 'black')) +
  theme(legend.title=element_blank(), legend.position='bottom',
        plot.title = element_text(size=15),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.key.size=unit(1, 'cm'),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.text.x=element_text(angle=45, hjust=1)) +
  scale_y_continuous(trans='log10')
## show two plots side by side
grid.arrange(plot_world.area, plot_world.line, ncol=2)

plot_world.area
plot_world.line
#gly.plot_world.area <- ggplotly(plot_world.area)
#gly.plot_world.area

#gly.plot_world.line <- ggplotly(plot_world.line)
#gly.plot_world.line

data.world.all <- data %>% dplyr::filter(country == "World")
## a scatter plot with a smoothed line and vertical x-axis labels
plot_acc.deaths <- ggplot(data.world.all, aes(x=date, y=deaths)) +
  geom_point() + geom_smooth() +
  xlab('') + ylab('Count') + labs(title='Accumulative Deaths') +
  theme(axis.text.x=element_text(angle=45, hjust=1))
plot_recovered <- ggplot(data.world.all, aes(x=date, y=recovered)) +
  geom_point() + geom_smooth() +
  xlab('') + ylab('Count') + labs(title='Accumulative Recovered Cases') +
  theme(axis.text.x=element_text(angle=45, hjust=1))
plot_new.deaths <- ggplot(data.world.all, aes(x=date, y=new.deaths)) +
  geom_point() + geom_smooth() +
  xlab('') + ylab('Count') + labs(title='New Deaths') +
  theme(axis.text.x=element_text(angle=45, hjust=1))
plot_new.recovered <- ggplot(data.world.all, aes(x=date, y=new.recovered)) +
  geom_point() + geom_smooth() +
  xlab('') + ylab('Count') + labs(title='New Recovered Cases') +
  theme(axis.text.x=element_text(angle=45, hjust=1))
## show four plots together, with 2 plots in each row
grid.arrange(plot_acc.deaths, plot_recovered, plot_new.deaths, plot_new.recovered, nrow=2)


## convert from wide to long format, for drawing area plots
rates.long <- data %>%
  select(c(country, date, rate.upper, rate.lower, rate.daily)) %>%
  gather(key=type, value=count, -c(country, date))
# set factor levels to show them in a desirable order
rates.long %<>% mutate(type=recode_factor(type, rate.daily='Daily',
                             rate.upper='Upper bound',
                             rate.lower = 'Lower bound')) 
#View(rates.long) 

plot_rates <- rates.long %>% dplyr::filter(country == "World") %>% 
  ggplot(aes(x = date, y = count, color = type)) + 
  geom_line() +
  labs(title = "World Death Rate (%)") +
  xlab("") + ylab("Death Rate (%)")

plot_rates

gly.rates <- ggplotly(plot_rates)
gly.rates

## ranking by confirmed cases
data.latest.all <- data %>% dplyr::filter(date == max(date)) %>%
  select(country, date,confirmed, new.confirmed, current.confirmed,
         recovered, deaths, new.deaths, death.rate=rate.lower) %>%
  mutate(ranking = dense_rank(desc(confirmed))) %>%
  arrange(ranking)
#View(data.latest.all)

k <- 20
## top 20 countries: 21 incl. 'World'
top.countries <- data.latest.all %>% dplyr::filter(ranking <= k + 1) %>%
  arrange(ranking) %>% pull(country) %>% as.character()
top.countries %>% setdiff('World') %>% print()

df <- data.long %>% dplyr::filter(country %in% top.countries) %>%
  mutate(country=country %>% factor(levels=c(top.countries))) %>% 
  dplyr::filter(country != 'World' & type != 'Total Confirmed') %>%
  ggplot(aes(x=date, y=count, color=type)) +
  geom_line()+
  #geom_area(alpha=0.5) +
# xlab('') + ylab('') +
  labs(title=paste0('Numbers of COVID-19 Cases in Top 20 Countries - ',
                    max.date.txt)) +
  scale_color_manual(values=c('red', 'green', 'black')) +
  theme(legend.title=element_blank(), legend.position='bottom',
        plot.title = element_text(size=15),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.key.size=unit(0.8, 'cm'),
        legend.text=element_text(size=13),
        strip.text.x=element_text(size=13),
        axis.text=element_text(size=13),
        axis.text.x=element_text(angle=45, hjust=1)) +
  facet_wrap(~country, ncol=4, scales='free_y') +
  scale_y_continuous(trans='log10')
df

data.latest <- data.latest.all %>% dplyr::filter(!is.na(country)) %>%
  mutate(country=ifelse(ranking <= k + 1, as.character(country), 'Others')) %>%
  mutate(country=country %>% factor(levels=c(top.countries, 'Others')))
data.latest %<>% group_by(country) %>%
  summarise(confirmed=sum(confirmed), new.confirmed=sum(new.confirmed),
            current.confirmed=sum(current.confirmed),
            recovered=sum(recovered), deaths=sum(deaths), new.deaths=sum(new.deaths)) %>%
  mutate(death.rate=(100 * deaths/confirmed) %>% round(1)) 
data.latest
data.latest %<>% select(c(country, confirmed, deaths, death.rate,
                          new.confirmed, new.deaths, current.confirmed,recovered)) %>%
  mutate(recover.rate=(100 * recovered/confirmed) %>% round(1))
data.latest
mostconfirm <- data.latest %>% select(c(country,confirmed)) %>% dplyr::filter(country=="World")
mostrecover <- data.latest %>% select(c(country,recovered)) %>% dplyr::filter(country=="World")
mostdeath <- data.latest %>% select(c(country,deaths)) %>% dplyr::filter(country=="World")

df_pop <- tbl(my_db, sql("select * from population "))
df_pop <- as.data.frame(df_pop)
df_pop <- rename(df_pop,"country"="Country")

# Add World Population
world_pop <- sum(df_pop$`Population (2020)`)
df_pop[nrow(df_pop) + 1,] = c("World", world_pop)

# Add Other Countries Population
top_pop <- dplyr::filter(df_pop, df_pop$country %in% top.countries & df_pop$country != "World")
top_pop <- sum(top_pop$`Population (2020)` %>% as.numeric())
others_pop <- (world_pop - top_pop) 
df_pop[nrow(df_pop) + 1,] = c("Others", others_pop)
#View(df_pop)

data.latest <- merge(x = data.latest, y = df_pop, by = "country", all.x = TRUE) 
data.latest
data.latest <- rename(data.latest,"population" = "Population (2020)")
data.latest$population <- data.latest$population %>% as.numeric()
data.latest  <- data.latest %>%
  select(c(country, confirmed, deaths, death.rate,
                          new.confirmed, new.deaths,
                          current.confirmed, recovered, recover.rate, population)) %>%
  mutate(confirm.rate = (100 * confirmed / population) %>% round(1))
data.latest

## % of death
data.latest %>% mutate(death.rate=death.rate %>% format(nsmall=1) %>% paste0('%'))

## convert from wide to long format, for drawing area plots
data.latest.long <- data.latest %>% dplyr::filter(country!='World') %>%
  gather(key=type, value=count, -country)
## set factor levels to show them with proper text and in a desirable order
data.latest.long %<>% mutate(type=recode_factor(type,
                                                confirmed='Total Confirmed',
                                                deaths='Total Deaths',
                                                death.rate='Death Rate (%)',
                                                new.confirmed='New Confirmed (compared with one day before)',
                                                new.deaths='New Deaths (compared with one day before)',
                                                current.confirmed='Current Confirmed',
                                                recover.rate = 'Recovered Rate(%)',
                                                confirm.rate = 'Confirmed Rate(%)'))
#View(data.latest.long)
data.one.dem <- dplyr::filter(data.latest.long,type=='Total Confirmed'
                       | type=='Total Deaths'
                       | type=='Current Confirmed')
data.two.dem <- dplyr::filter(data.latest.long,type=='Death Rate (%)'
                     #  | type=='New Confirmed (compared with one day before)'
                    #   | type=='New Deaths (compared with one day before)'
                       | type=='Recovered Rate(%)'
                       | type=='Confirmed Rate(%)')
data.two.dem

## bar chart
data.one.dem %>% ggplot(aes(x=country, y=count, fill=country, group=country)) +
  geom_bar(stat='identity') +
  geom_text(aes(label=count, y=count), size=3, vjust=0) +
  xlab('') + ylab('') +
  labs(title=paste0('Top 20 Countries with Most Confirmed Cases - ', max.date.txt)) +
  scale_fill_discrete(name='Country', labels=aes(count)) +
  theme(legend.title=element_blank(),
        legend.position='none',
        plot.title=element_text(size=13),
        axis.text=element_text(size=8),
        axis.text.x=element_text(angle=45, hjust=1)) +
  facet_wrap(~type, ncol=1, scales='free_y')

data.two.dem$facet <- factor(data.two.dem$type, levels = c('Confirmed Rate(%)', 'Recovered Rate(%)','Death Rate (%)'))
data.two.dem %>% 
  ggplot(aes(x=country, y=count, fill=country, group=country)) +
  geom_bar(stat='identity') +
  geom_text(aes(label=count, y=count), size=4, vjust=0) +
  xlab('') + ylab('') +
  labs(title=paste0('Top 20 Countries with Most Confirmed Cases - ', max.date.txt)) +
  scale_fill_discrete(name='Country', labels=aes(count)) +
  theme(legend.title=element_blank(),
        legend.position='none',
        plot.title=element_text(size=15),
        axis.text=element_text(size=13),
        axis.text.x=element_text(size=13,angle=45, hjust=1)) +
  facet_wrap(~facet, ncol=1, scales='free_y')

##GDP
df_gdp2019 <- tbl(my_db, sql("select * from gdp19"))
df_gdp2019 <- as.data.frame(df_gdp2019)
df_gdp2019

#healthranking data
df_healt <- tbl(my_db, sql("select * from healthranking"))
df_healt <- as.data.frame(df_healt)
df_healt <- select(df_healt,c("country","healthCareIndex"))
#View(df_healt)

#Top20Pornhub data
#df_pornhub <- tbl(my_db, sql("select * from Pornhub"))
#df_pornhub <- as.data.frame(df_pornhub)
#df_pornhub

# temp data
df_temp <- tbl(my_db, sql("select * from world_temp"))
df_temp <- as.data.frame(df_temp) 
df_temp$Country[df_temp$Country == "United States"] <- "US"

df_city <- select(df_temp,c("Country","City")) %>%
  rename(country=Country) %>% 
  rename(city=City)

numofcity <- aggregate(city ~ country, data = df_city, length)

#clean data
df_temp <- select(df_temp,c("Country","Avg_Year")) %>%
  rename(country=Country)
#View(df_temp)

#df_temp <- data.frame(country=df_temp[,1],avg=rowMeans(df_temp[,-1]))
df_temp <- df_temp %<>% group_by(country) %>% summarise(avg_temp = mean(Avg_Year,na.rm = TRUE)%>% round(1))
df_temp

##temp bar
df_temp.all <- df_temp %>% merge(data.latest.all)
#View(df_temp.all)
df_temp_top.all <- df_temp.all %>% dplyr::filter(country %in% top.countries) %>%
  mutate(ranking = ranking - 1) %>%
  arrange(ranking)
#View(df_temp_top.all)
g_temp_top <- df_temp_top.all %>%
  ggplot(aes(x = reorder(country, ranking), y = avg_temp, fill = avg_temp)) +
  labs(title=paste0("Temperature in Top  20 countries"), subtitle = "Average Temperature in Top 20 countries with most confirmed cases (°C) (2020)") +
  scale_color_gradient(low = "#93DBFF", high = "#FF7771") +
  geom_text(aes(label=avg_temp, y=avg_temp), size=4, vjust=-0.5) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(
        legend.title=element_blank(),
        legend.position='none',
        plot.title=element_text(size = 15, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.text=element_text(size=8),
        axis.text.x=element_text(size = 9, angle=45, hjust=1)) +
  scale_x_discrete(name = "Country") +
  scale_y_discrete(name = "Average Temperature")

           #labs(title = "Temperature in Top  20 countries", subtitle = "Temperature in Top 20 countries with most confirmed cases (°C)")
         
#g_temp_top         
g_temp_top 

#data.latest.all
lat.long <- rename(df_conf, "country" = "Country.Region", "city" = "Province.State") %>% 
  select("country", "Lat", "Long") %>% 
  merge(df_temp.all[c("country","confirmed", "recovered", "deaths", "avg_temp", "ranking")], by = "country") %>%
  distinct(country, .keep_all = TRUE) %>%
  mutate(ranking = ranking - 1) %>%
  arrange(ranking)
#View(lat.long)

label_world <- lat.long 
label_world$avg_temp <- as.numeric(label_world[, names(label_world) %in% c("avg_temp")])
label_world <- label_world %>%  
  mutate(txt=paste0('<b>',ranking, '</b>',
                    '<br/>','<b>',country, '</b>',
                    '<br/>', "Temperature:  ",avg_temp, ' °C',
                    '<br/>', "Confirmed:  ", confirmed, 
                    '<br/>', "Deaths: ", deaths,
                    '<br/>', "Recovered: ", recovered
                    )) 

label_world$txt <- label_world$txt %>% lapply(htmltools::HTML)
label_world 

label_top <- label_world %>% dplyr::filter(ranking < 21)
label_top

# Temperature Map
wpal <- colorNumeric("YlOrRd", label_world$avg_temp, n = 4)

topIcon <- makeIcon("star.png",
  #iconUrl = "https://static.vecteezy.com/system/resources/previews/001/189/063/non_2x/star-rounded-png.png",
  iconWidth = 10, iconHeight = 10
  #iconAnchorX = 20, iconAnchorY = 20
  
)

label_world <- label_world %>% dplyr::filter(ranking > 20) 
  
m <- leaflet(width=1200, height=800) %>% addTiles()  
m %<>%  addCircleMarkers(label_world$Long, label_world$Lat,
                        # radius=2+log2(x$confirmed),
                        radius=10,#*log2(m.world$avg.temp),
                        stroke=F,
                        #color='red',
                        color = wpal(label_world$avg_temp), 
                        fillOpacity=0.5,
                        #popup=label.top$txt
                        label= label_world$txt,
                        group = "World"
                        ) %>%
  
  addCircleMarkers(label_top$Long, label_top$Lat,
                        # radius=2+log2(x$confirmed),
                        radius=10,#*log2(m.world$avg.temp),
                        stroke=F,
                        #color='red',
                        color = wpal(label_top$avg_temp), 
                        fillOpacity=0.5,
                        #popup=label.top$txt
                        label= label_top$txt,
                        group = "Top 20 Countries"
                        ) %>%
  
  addLabelOnlyMarkers(label_top$Long, label_top$Lat, label = label_top$ranking,
                      labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, 
                                                  direction = "head", 
                                                  offset = c(5,4)),
                      group = "Top 20 Countries") %>%
  
  addLegend("bottomright", pal = wpal, values = label_world$avg_temp, opacity = 1,
            labFormat = labelFormat(suffix = " °C"),
            title = "Temperature") %>% 
  
  addLayersControl(
    #baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    overlayGroups = c("Top 20 Countries", "World"),
    options = layersControlOptions(collapsed = FALSE)
  )
m

gdp.top20 <- df_gdp2019 %>%
  select(c("rank", "country", "GDP (millions of US dollars)")) %>%
  merge(data.latest.all %>% 
          select(country, ranking, confirmed, recovered, deaths) %>% 
          dplyr::filter(country %in% top.countries & country != "World"), by = "country") %>%
  arrange(ranking) %>% 
  mutate(ranking = ranking - 1) 
gdp.top20 %<>% rename("GDP" = "GDP (millions of US dollars)")
gdp.top20

g <- ggplot(gdp.top20, aes(x = GDP, y = reorder(country, -ranking))) +
  geom_histogram(stat = "identity", aes(fill = GDP))+ 
  scale_fill_gradient("GDP", low = "#FF4038", high = "#50E952") + 
  labs(title=paste0("GDP  of Top 20 Countries in 2019 (millions of US dollars)")) +
  geom_text(aes(label=GDP, x = GDP), size=3.5, hjust=-0.2) +
  xlab("GDP (millions of US dollars)") +
  ylab("") +
  theme(legend.title=element_blank())
g

gly.top.gdp <- ggplotly(g)
gly.top.gdp

# Pornhub
df_pornhub <- tbl(my_db, sql("select * from pornhub"))
df_pornhub <- as.data.frame(df_pornhub)
df_pornhub

# Sars
df_sars <- tbl(my_db, sql("select * from sars_2003"))
df_sars <- as.data.frame(df_sars)
#View(df_sars)

dates.s <- df_sars[,1]%>% mdy()
range(dates.s)
min.date.s <- min(dates.s)
max.date.s <- max(dates.s)
min.date.txt.s <- min.date.s %>% format('%d %b %Y')
max.date.txt.s <- max.date.s %>% format('%d %b %Y')
day1.sars <- min.date.s

# clean data Sars 
data.sars <- df_sars %>% rename(c("date" = "Date", "confirmed" = "Cumulative_number"  ,"deaths" = "Number_deaths", "recovered" = "Number_recovered")) %>%
  mutate(date = date %>% mdy()) %>%
  group_by(country, date) %>% as.data.frame() 

# Add World's Sars cases 
world.sars <- data.sars %>% group_by(date) %>% 
  summarise(country='World',
            confirmed = sum(confirmed, na.rm=T),
            deaths = sum(deaths, na.rm=T),
            recovered = sum(recovered, na.rm=T))

data.sars %<>% rbind(world.sars)
data.sars %<>% mutate(current.confirmed = confirmed - deaths - recovered)
#View(world.sars) 
data.sars

#rate
data.sars %<>% arrange(country, date)
n <- nrow(data.sars)
day1.sars <- min(data.sars$date)
data.sars %<>% mutate(new.confirmed = ifelse(date == day1.sars, NA, confirmed - dplyr::lag(confirmed, n=1)),
                 new.deaths = ifelse(date == day1.sars, NA, deaths - dplyr::lag(deaths, n=1)),
                 new.recovered = ifelse(date == day1.sars, NA, recovered - dplyr::lag(recovered, n=1)))
data.sars %<>% mutate(new.confirmed = ifelse(new.confirmed < 0, 0, new.confirmed),
                 new.deaths = ifelse(new.deaths < 0, 0, new.deaths),
                 new.recovered = ifelse(new.recovered < 0, 0, new.recovered))
## death rate based on total deaths and recovered cases
data.sars %<>% mutate(rate.upper = (100 * deaths / (deaths + recovered)) %>% round(1),
                 rate.upper = ifelse(is.nan(rate.upper), 0, rate.upper))

## lower bound: death rate based on total confirmed cases
data.sars %<>% mutate(rate.lower = (100 * deaths / confirmed) %>% round(1),
                 rate.lower = ifelse(is.nan(rate.lower), 0, rate.lower))

## death rate based on the number of death/recovered on every single day
data.sars %<>% mutate(rate.daily = (100 * new.deaths / (new.deaths + new.recovered)) %>% round(1),
                 rate.daily = ifelse(is.nan(rate.daily), 0, rate.daily))

#View(data.sars)

## convert from wide to long format
data.sars.long <- data.sars %>%
  select(c(country, date, confirmed, current.confirmed, recovered, deaths)) %>%
  gather(key=type, value=count, -c(country, date))
## set factor levels to show them in a desirable order
data.sars.long %<>% mutate(type=recode_factor(type, confirmed='Total Confirmed',
                                         current.confirmed='Current Confirmed',
                                         recovered='Recovered',
                                         deaths='Deaths'))
#View(data.sars.long)

# World sars' long data 
world.sars.long <- data.sars.long %>%
  dplyr::filter(country == "World")
#View(world.sars.long)

g <- ggplot(world.sars.long, aes(date, count, color = type)) +
  geom_line()+
  labs(title = "Number of Cases Worldwide: SARs")+
  xlab("")+
  ylab("")
g

gly.g <- ggplotly(g)
gly.g
gly.plot_world.line

df_sars_lastdate <- data.sars %>%
  dplyr::filter(date == max.date.s)

df_sars_lastdate

## Current Confirmed Cases
data.sars.world <- data.sars %>% dplyr::filter(country=='World')
#View(data.sars.world)
n <- nrow(data.sars.world)
#View(data.sars.world)
plot_sars.currconf <- ggplot(data.sars.world, aes(x=date, y=current.confirmed)) +
  geom_point() + geom_smooth() +
  xlab('') + ylab('Count') + labs(title='Current Confirmed Cases') +
  theme(axis.text.x=element_text(angle=45, hjust=1))
plot_sars.newconf <- ggplot(data.sars.world, aes(x=date, y=new.confirmed)) +
  geom_point() + geom_smooth() +
  xlab('') + ylab('Count') + labs(title='Daily New Confirmed Cases') +
  theme(axis.text.x=element_text(angle=45, hjust=1))
## show two plots side by side
grid.arrange(plot_sars.currconf, plot_sars.newconf, ncol=2)

## a scatter plot with a smoothed line and vertical x-axis labels
plot_sars.accdeaths <- ggplot(data.sars.world, aes(x=date, y=deaths)) +
  geom_point() + geom_smooth() +
  xlab('') + ylab('Count') + labs(title='Accumulative Deaths') +
  theme(axis.text.x=element_text(angle=45, hjust=1))
plot_sars.accrecov <- ggplot(data.sars.world, aes(x=date, y=recovered)) +
  geom_point() + geom_smooth() +
  xlab('') + ylab('Count') + labs(title='Accumulative Recovered Cases') +
  theme(axis.text.x=element_text(angle=45, hjust=1))
plot_sars.newdeaths <- ggplot(data.sars.world, aes(x=date, y=new.deaths)) +
  geom_point() + geom_smooth() +
  xlab('') + ylab('Count') + labs(title='New Deaths') +
  theme(axis.text.x=element_text(angle=45, hjust=1))
plot_sars.newrecov <- ggplot(data.sars.world, aes(x=date, y=new.recovered)) +
  geom_point() + geom_smooth() +
  xlab('') + ylab('Count') + labs(title='New Recovered Cases') +
  theme(axis.text.x=element_text(angle=45, hjust=1))
## show four plots together, with 2 plots in each row
grid.arrange(plot_sars.accdeaths, plot_sars.accrecov, plot_sars.newdeaths, plot_sars.newrecov, nrow=2)

#Top 20 with gdp
data.longGDP <- df_gdp2019 %>% gather(key=year, value=GDP, -c(country))
data.top <- data.latest %>% dplyr::filter(!country %in% c('World', 'Others'))
data.top <- head(data.top,20)
#View(data.latest)
data.gdp <- dplyr::filter(data.longGDP,year=='2020')
df_sars_lastdate_confirmed <- df_sars_lastdate %>%
  select("country", "confirmed") %>%
  rename(sars = "confirmed")
#View(df_sars_lastdate_confirmed)
#merge
mergcountry = function(data1,data2){
  data <- merge(x = data1, y = data2, by = "country", all.x = TRUE) 
  return(data)
}
data.top.world <- merge(x = data.top, y = df_gdp2019, by = "country", all.x = TRUE) %>% 
  select(-c(code,rank,new.confirmed,new.deaths,current.confirmed,population)) %>% 
  rename(GDP="GDP (millions of US dollars)")

data.top.world <- merge(x = data.top.world, y = df_healt, by = "country", all.x = TRUE) %>%
  rename(healthcare="healthCareIndex")
#data.top.world <- mergcountry(data.top.world, df_temp)

data.top.world <- merge(x = data.top.world, y = df_pornhub, by = "country", all.x = TRUE) %>%
  rename(Pornhub = "PornhubIndex(%)")

data.top.world <- merge(x = data.top.world, y = df_sars_lastdate_confirmed, by = "country", all.x = TRUE) 


data.top.world <- mergcountry(data.top.world, df_temp)
index <- is.na(data.top.world)
data.top.world[index] <- 0
data.top.world
#View(data.top.world)


normalize = function(data){
  #return ((data - min(data,na.rm = TRUE))/(max(data,na.rm = TRUE) - min(data,na.rm = TRUE)))
  z <- scale(data);
  tanh(z/2)
}

norm_data = as.data.frame(apply(data.top.world[,2:13],2,normalize))
corr_data <- norm_data
norm_data$country <- c("Argentina","Bangladesh","Brazil","Chile","Colombia","France","Germany","India","Iran","Italy","Mexico","Pakistan","Peru","Russia","saudi Arabia","South Africa","Spain","Turkey","United Kingdom","US")
norm_data <- norm_data[,c(ncol(norm_data),1:(ncol(norm_data)-1))]
rownames(norm_data) <- norm_data[,1]
norm_data[,1] <- NULL

#heatmap(as.matrix(norm_data[, -1]),scale="column",col= colorRampPalette(brewer.pal(8, "Blues"))(25))
#legend(x="bottomright", legend=c("-1", "0", "1"), 
#     fill=colorRampPalette(brewer.pal(8, "Blues"))(3))

# install.packages("gplots")
library("gplots")
library("stringr")
norm_data_plot <- select(norm_data,"confirm.rate","death.rate","recover.rate","healthcare","Pornhub","GDP","avg_temp", "sars")
norm_data_plot
world_clust <- as.matrix(norm_data_plot)
#heatmap.clust <- 
  heatmap.2(as.matrix(norm_data_plot),
                    scale="none", 
                    col = colorRampPalette(c("#6D9EC1","white","#E46726"))(n = 200),
                    margins=c(10,6),trace="column") + 
    theme(axis.text.x = element_text(angle = 90,vjust = 1))
  
#gly.heatmap2 <- plotly_build(heatmap.clust)


norm_data2 = as.data.frame(apply(data.top.world[,2:13],2,normalize))
norm_data2$country <- c("Argentina","Bangladesh","Brazil","Chile","Colombia","France","Germany","India","Iran","Italy","Mexico","Pakistan","Peru","Russia","saudi Arabia","South Africa","Spain","Turkey","United Kingdom","US")
#View(norm_data2)
norm_data_plot2 <- select(norm_data2,"country","confirm.rate","death.rate","recover.rate","healthcare","Pornhub","GDP","avg_temp", "sars")
#View(norm_data_plot2)
norm_data_plot2 %<>% gather(key=type, value=count, -c(country))
level_order <- factor(norm_data_plot2$type,
                      level =c("sars","Pornhub","GDP","avg_temp","healthcare","recover.rate","death.rate","confirm.rate"))
#View(level_order)
ggplot(data = norm_data_plot2, aes(x=country, y=level_order, fill=count)) + 
  geom_tile() +
  scale_fill_gradient(low = "pink", high = "blue") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,vjust = 1))+
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    #legend.position = "none"
    axis.text.y=element_text(size=13),
    axis.text.x=element_text(size=13,angle=45, hjust=1)
  )

#correlation
corr_data %<>% select(c(GDP,confirm.rate,death.rate,recover.rate,healthcare,avg_temp,Pornhub, sars))
head(corr_data)
cor(corr_data)
plotcorr <- ggcorrplot(cor(corr_data),hc.order = TRUE,
           outline.color = "white",
           colors = c("#6D9EC1","white","#E46726"),
           lab = TRUE)

plotcorr

## Data in US

df_us <- tbl(my_db, sql("select * from covidus"))
df_us <- as.data.frame(df_us) 
df_us <- select(df_us, date, state, cases, deaths)

df_us_pop <- tbl(my_db, sql("select * from data_population")) 
df_us_pop <- as.data.frame(df_us_pop)

df_us_gender <- tbl(my_db, sql("select * from data_gender")) 
df_us_gender <- as.data.frame(df_us_gender)

df_us_ethnic <- tbl(my_db, sql("select * from data_ethnic")) 
df_us_ethnic <- as.data.frame(df_us_ethnic)

df_us_lockdown <- tbl(my_db, sql("select * from data_lockdown")) 
df_us_lockdown <- as.data.frame(df_us_lockdown)

df_us_health <- tbl(my_db, sql("select * from data_health")) 
df_us_health <- as.data.frame(df_us_health)

df_us_testing <- tbl(my_db, sql("select * from data_testing")) 
df_us_testing <- as.data.frame(df_us_testing)


df_us <- df_us  %>%
  mutate(date = date %>% mdy()) %>%
  rename("confirmed" = "cases")
#data.us
dates.us <- df_us[,1]
range(dates.us)
min.date.us <- min(dates.us)
max.date.us <- max(dates.us)
min.date.txt.us <- min.date.us %>% format('%d %b %Y')
max.date.txt.us <- max.date.us %>% format('%d %b %Y')
day1.us <- min(df_us$date)

data.us.total <- df_us %>% group_by(date) %>%
  summarise(state='US',
            confirmed = sum(confirmed, na.rm=T),
            deaths = sum(deaths, na.rm=T))
#View(data.us.total)

data.us <- df_us
data.us %<>% rbind(data.us.total)
#View(data.us)

data.us.long <- data.us %>% 
  gather(key = type, value = count, -c(date, state)) 
#View(data.us.long)

us.total <- data.us.total %>%
  mutate(new.confirmed = ifelse(date == day1, 0, confirmed - dplyr::lag(confirmed, n=1)),
                new.deaths = ifelse(date == day1, 0, deaths - dplyr::lag(deaths, n=1)))

us.total %<>% mutate(new.confirmed = ifelse(new.confirmed < 0, 0, new.confirmed),
                 new.deaths = ifelse(new.deaths < 0, 0, new.deaths))
us.total

most_us <- us.total %>% dplyr::filter(date == max.date.us)

us.total.long <- us.total %>% 
  gather(key = type, value = count, -c(date, state))


plot_us.cases <- data.us.long %>% dplyr::filter(state == "US") %>%
  ggplot(aes(x = date, y = count)) +
  geom_area(aes(fill=type), alpha=0.5) +
#  labs(title = paste0("Cumulative cases in US : ", min.date.txt.us, '-', max.date.txt.us, " (Stack, Log Scale)")) +
  scale_y_continuous(trans='log10') +
  scale_fill_manual(values=c('red', 'black'))+
  ylab("") +
  theme(axis.title = element_text(size=8))
#View(us.total.long )
plot_us.newconf <- us.total.long %>%
  dplyr::filter(type %in% c("new.confirmed","new.deaths")) %>%
  ggplot(aes(x = date, y = count, color = type)) + 
  geom_line() + 
  #labs(title = paste0("Daily confirmed cases in US : ", min.date.txt.us, '-', max.date.txt.us)) +
  xlab("Date") +
  ylab("Confirmed cases")

gly.plot_us.cases <- ggplotly(plot_us.cases)
gly.plot_us.cases <- gly.plot_us.cases %>% 
  layout(title = paste0("<b>Cumulative cases in US : ", min.date.txt.us, '-', max.date.txt.us, " (Stack, Log Scale)"), 
         font=list(size = 10))
gly.plot_us.newconf <- ggplotly(plot_us.newconf) %>% 
  layout(title = paste0("<b>Daily confirmed cases in US : ", min.date.txt.us, '-', max.date.txt.us), 
         font=list(size = 10))

gly.plot_us.cases  
gly.plot_us.newconf


data.us %<>% mutate(new.confirmed = ifelse(date == day1, NA, confirmed - dplyr::lag(confirmed, n=1)),
                 new.deaths = ifelse(date == day1, NA, deaths - dplyr::lag(deaths, n=1)))

data.us %<>% mutate(new.confirmed = ifelse(new.confirmed < 0, 0, new.confirmed),
                 new.deaths = ifelse(new.deaths < 0, 0, new.deaths))
#View(data.us)

#data.us.daily <- data.us %>% dplyr::filter(state == "US")
#View(data.us.daily)

data.us.pop <- df_us_pop %>% select(State, Population) %>%
  rename("state" = "State") 
#data.us.pop

data.us.gender <- df_us_gender %>% select(State, Male, Female) %>%
  rename("state" = "State") 
data.us.gender

data.us.latest <- data.us %>%
  dplyr::filter(date == max.date.us) %>% 
  merge(data.us.pop, by = "state", all.x = T) %>%
  merge(data.us.gender, by = "state", all.x = T)
#View(data.us.latest)

data.us.latest$Population[data.us.latest$state == "US"] <- sum(data.us.pop$Population)
data.us.latest %<>% mutate(ranking = dense_rank(desc(confirmed)),
                           confirmed.rate = (100 * confirmed / Population) %>% round(2),
                           death.rate = (100 * deaths / confirmed) %>% round(2),
                           Male.confirmed = (Male * confirmed) %>% round(0),
                           Female.confirmed = Female * confirmed %>% round(0),
                           Male.deaths = Male * deaths,
                           Female.deaths = Female * deaths) %>%
  arrange(ranking) 

top.us <- data.us.latest[,1]
top.us

#View(top.us)

data.us.latest.show <- data.us.latest %>% select(state,date,confirmed,deaths, confirmed.rate, death.rate)
#View(data.us.latest.show)

# List of top 20 state
k <- 20
data.us.top <- data.us.latest %>%
  dplyr::filter(ranking <= k+1) %>% 
  arrange(ranking) 
#View(data.us.top)

us.state.top <- data.us.top %>% pull(state) %>% as.character()
us.state.top  %>% setdiff('US') %>% print()

# confirmed rate & death rate of top 20 state
g.rate <- data.us.latest %>% dplyr::filter(state %in% us.state.top & state != "US") %>%
  select(state, confirmed.rate, death.rate, ranking) %>%
  gather(key = Type, value = Percent, -c(state, ranking)) %>%
  ggplot(aes(x=reorder(state, -desc(ranking)), y=Percent, fill = Percent)) +
  geom_bar(stat='identity') +
  scale_fill_gradient(low = "#ebbc62", high = "#b42006") +
  geom_text(aes(label=Percent, y=Percent), size=4, vjust=0) +
  xlab('') + ylab('') +
  labs(title=paste0('Confirmed Rate & Death Rate of Top 20 State in US')) +
  #scale_fill_continuous(name='State', labels=aes(Percent)) +
  theme(legend.title=element_blank(),
        legend.position='none',
        plot.title=element_text(size=13),
        axis.text=element_text(size=12),
        axis.text.x=element_text(size = 12, angle=45, hjust=1)) +
  facet_wrap(~Type, ncol=1, scales='free_y') 

g.rate

us.confirmed.num <- data.us.top$confirmed[data.us.top$state == "US"] %>% as.numeric()
us.confirmed.num

data.us.top %<>% mutate(confirmed.per.us = (confirmed * 100 / us.confirmed.num) %>% round(1))
data.us.top

g.us.top1 <- data.us.top %>%
  dplyr::filter(state != "US") %>%
  select(state, confirmed, confirmed.per.us, ranking) %>%
  gather(key = Type, value = count, -c(state, ranking, confirmed.per.us)) %>%
  arrange(ranking) %>%
  
  ggplot(aes(x = reorder(state, ranking), y = count, fill = count)) + 
  geom_bar(stat = "identity") +
  #labs(title = "20 state in US with most confirmed cases") +
  scale_color_gradient(low = "#93DBFF", high = "#FF7771") +
  xlab("") + 
  ylab("Confirmed Cases") +
  geom_text(aes(label=paste0(confirmed.per.us, "%")), size=3, vjust=-0.5) +
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5))+
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    #legend.position = "none"
  )

g.us.top1

gly.us.top1 <- ggplotly(g.us.top1)
gly.us.top1

data.us.latest.corr <- df_us %>%dplyr::filter(date==max.date.us)
#View(data.us.latest.corr)

data.us.latest.corr %<>% mutate(ranking = dense_rank(desc(confirmed))) %>% arrange(ranking)
top.us <- data.us.latest.corr[,2]
#top.us

# List of top 20 state
k <- 19
data.us.latest.top <- data.us.latest.corr %>%
    dplyr::filter(ranking <= k+1) %>% 
    arrange(ranking) 
#View(data.us.latest.top)
us.state.top <- data.us.latest.top %>% pull(state) %>% as.character()
us.state.top  %>% setdiff('US') %>% print()
#View(us.state.top)

#gender in us
df_us_gender <- tbl(my_db, sql("select * from data_gender"))
df_us_gender <- as.data.frame(df_us_gender)
df_us_gender <- select(df_us_gender,c("State","Male","Female"))
df_us_gender <- rename(df_us_gender,"state"="State")
df_us_gender

#population in us
df_us_pop <- tbl(my_db, sql("select * from data_population"))
df_us_pop <- as.data.frame(df_us_pop)
df_us_pop <- select(df_us_pop,c("State","Population"))
df_us_pop <- rename(df_us_pop,"state"="State")
df_us_pop

#lockdown in us
df_us_lockdown <- tbl(my_db, sql("select * from data_lockdown"))
df_us_lockdown <- as.data.frame(df_us_lockdown)
df_us_lockdown <- select(df_us_lockdown,c("State","Day lockdown"))
df_us_lockdown <- rename(df_us_lockdown,"state"="State")
df_us_lockdown

#GDP in us
df_us_gdp <- tbl(my_db, sql("select * from us_gdp"))
df_us_gdp  <- as.data.frame(df_us_gdp )
df_us_gdp  <- select(df_us_gdp ,c("State","GDPs"))
df_us_gdp  <- rename(df_us_gdp ,"state"="State")
df_us_gdp 

#homeless in us
df_us_homeless <- tbl(my_db, sql("select * from us_homeless"))
df_us_homeless <- as.data.frame(df_us_homeless)
df_us_homeless <- select(df_us_homeless,c("State","Homeless"))
df_us_homeless <- rename(df_us_homeless,"state"="State")
df_us_homeless

#View(df_us)


#merge
mergcountry = function(data1,data2){
  data <- merge(x = data1, y = data2, by = "state", all.x = TRUE) 
  return(data)
}

data.top.state <- data.us.latest.top %>% dplyr::filter(state != "US") %>%
  select(state, confirmed, deaths)
#View(data.top.state)

df_Allus <- mergcountry(data.top.state, df_us_gender)

df_Allus <- mergcountry(df_Allus, df_us_pop) 

df_Allus <- mergcountry(df_Allus, df_us_lockdown) 

df_Allus <- mergcountry(df_Allus, df_us_gdp)

df_Allus <- mergcountry(df_Allus, df_us_homeless)

#View(df_Allus)

index <- is.na(df_Allus)
df_Allus[index] <- 0
#View(df_Allus)

normalize = function(data){
  #return ((data - min(data,na.rm = TRUE))/(max(data,na.rm = TRUE) - min(data,na.rm = TRUE)))
  z <- scale(data);
  tanh(z/2)
}
Allus = as.data.frame(apply(df_Allus[,2:9],2,normalize))
corr_dataUS <- Allus 

#View(corr_dataUS)

Allus$state <- c(df_Allus$state)
#View(Allus)

corr_dataUS <- rename(corr_dataUS ,"Daylockdown" = "Day lockdown")

Allus_plot <- select(Allus,"state","confirmed","deaths","Male","Female","Population","Day lockdown","GDPs","Homeless")
Allus_plot %<>% gather(key=type, value=count, -c(state))
level_order <- factor(Allus_plot$type, 
                      level = c("confirmed","deaths","Male","Female","Population","Day lockdown","GDPs","Homeless"))

Allus_Heat <- Allus[,c(ncol(Allus),1:(ncol(Allus)-1))]
rownames(Allus_Heat) <- Allus_Heat[,1]
Allus_Heat[,1] <- NULL
heatmap.2(as.matrix(Allus_Heat),
                    scale="none", 
                    col = colorRampPalette(c("#6D9EC1","white","#E46726"))(n = 200),
                    margins=c(10,6),trace="column")

heatmapus <- ggplot(data = Allus_plot, aes(x=state, y=level_order, fill=count)) + 
  geom_tile() +
  scale_fill_gradient(low = "pink", high = "blue") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,vjust = 1))+
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    #legend.position = "none"
  ) +labs(title='The heatmap of COVID-19 infections in US')

heatmapus

#correlation
corr_dataUS %<>% select(c(confirmed,deaths,Male,Female,Population,Daylockdown,GDPs,Homeless))
head(corr_dataUS)
cor(corr_dataUS)
ggcorrplot(cor(corr_dataUS),hc.order = TRUE,
           outline.color = "white",
           colors = c("#6D9EC1","white","#E46726"),
           lab = TRUE)



#Covid_Thailand
df_thai <- tbl(my_db, sql("select * from covid19_thailand"))
df_thai <- as.data.frame(df_thai)
#View(df_thai)

#clean Covid_Thailand
dates.th <- df_thai[,2]%>% mdy()
range(dates.th)
min.date.th <- min(dates.th)
max.date.th <- max(dates.th)
min.date.txt.th <- min.date.th %>% format('%d %b %Y')
max.date.txt.th <- max.date.th %>% format('%d %b %Y')

df_thai$announce_date <- mdy(df_thai$announce_date)
df_thai$notification_date <- mdy(df_thai$notification_date)
df_thai

df_thai <- df_thai %>% select(!No.) %>% select(!notification_date) %>% 
  group_by(announce_date)
df_thai
#View(df_thai)

# Total confirmed cases in Thailand
data.thai.count <- df_thai %>%
  select(announce_date) %>%
  summarise(confirmed = n()) %>% as.data.frame()
#View(data.thai.count)
data.thai.count$cumulative_confirmed <- cumsum(data.thai.count[, 2])

data.thai.count <- data.thai.count %>% rename(new.confirmed=confirmed) %>% rename(confirmed=cumulative_confirmed)
thai.total.long <- data.thai.count %>% 
  gather(key = type, value = count, -c(announce_date))
thai.total.long

plot_thailand.conf <- thai.total.long %>%
  dplyr::filter(type %in% c("new.confirmed")) %>%
  ggplot(aes(x = announce_date, y = count, color = type)) + 
  geom_line() + 
  xlab("Date") +
  ylab("cases")

gly.plot_thai.cases <- ggplotly(plot_thailand.conf)
gly.plot_thai.cases <- gly.plot_thai.cases %>% 
  layout(title = paste0("<b>Daily Thai Confirmed Cases in Thailand : Jan 2020 - Jan 2021"), 
         font=list(size = 10))

gly.plot_thai.cases

plot_thailand.cumulative <- thai.total.long %>% 
  dplyr::filter(type %in% c("confirmed")) %>%
  ggplot(aes(x = announce_date, y = count)) +
  geom_area(aes(fill=type), alpha=0.5) +

  scale_y_continuous(trans='log10') +
  scale_fill_manual(values=c('red'))+
  ylab("") +
  theme(axis.title = element_text(size=8))

gly.plot_thai.cumulative <- ggplotly(plot_thailand.cumulative)
gly.plot_thai.cumulative <- gly.plot_thai.cumulative %>% 
  layout(title = paste0("<b>Cumulative Cases in Thailand : Jan 2020 - Jan 2021 (Stack,Log scale)"), 
         font=list(size = 10))

gly.plot_thai.cumulative

## Thai Confirmed Cases (Jan 2020 - Jan 2021
plot1 <- ggplot(data.thai.count, aes(x=announce_date, y=confirmed)) +
  geom_point() + geom_smooth() +
  xlab(" ") + ylab("Count") + labs(title='Thai Cumulative Confirmed Cases (Jan 2020 - Jan 2021)') +
  theme(axis.text.x=element_text(size=12,angle=45, hjust=1),
        axis.text.y=element_text(size=12,angle=45, hjust=1)
        )
plot2 <- ggplot(data.thai.count, aes(x=announce_date, y=new.confirmed)) +
  geom_point() + geom_smooth() +
    xlab(" ") + ylab("Count")+ labs(title='Thai Confirmed Cases (Jan 2020 - Jan 2021 log scale)') +
   theme(axis.text.x=element_text(size=12,angle=45, hjust=1),
         axis.text.y=element_text(size=12,angle=45, hjust=1))+
scale_y_continuous(trans='log10')
## show two plots side by side
grid.arrange(plot1, plot2, ncol=1)

# Confirmed cases divided by sex (gender)
df_thai$sex[df_thai$sex == ""] <- "Unknown"
data.thai.gender <- df_thai %>%
  group_by(sex) %>%
  summarise(count = n()) %>%
  mutate(percent = (count / sum(count) * 100) %>% round(2)) %>%
  dplyr::filter(percent>1)%>%
  #mutate(pos = cumsum(percent) - 0.5*percent) %>%
  arrange(desc(percent))

data.thai.gender

data.thai.gender$sex <- factor(data.thai.gender$sex, levels = as.character(data.thai.gender$sex))
data.thai.gender$sex
g.th.gender <- data.thai.gender %>% 
  ggplot(aes(x = "", y = percent, fill = sex)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  labs(title='Gender of Thai Confirmed Cases (Jan 2020 - Jan 2021)')+
  geom_text(aes(label = paste0(percent, "%")), color = "white", size = 5, position = position_stack(vjust = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE)) 
g.th.gender

# Confirmed cases divided by province_of_onset
data.thai.onset <- df_thai %>%
    group_by(province_of_onset) %>%
    summarise(count = n()) %>%
    arrange(desc(count))%>%
    rename(onset="count")%>%
    rename(province="province_of_onset")
data.thai.onset$province[data.thai.onset$province == ""] <- "Unknown"
data.thai.onset

# Confirmed cases divided by province_of_isolation 
data.thai.isolation <- df_thai %>%
    group_by(province_of_isolation) %>%
    summarise(count = n()) %>%
    arrange(desc(count))%>%
    rename(isolation ="count")%>%
    rename(province="province_of_isolation")

data.thai.isolation$province[data.thai.isolation$province == ""] <- "Unknown"
data.thai.isolation

data.province <- merge(x = data.thai.onset, y = data.thai.isolation, by = "province", all.x = TRUE) 
data.province <- data.province%>%dplyr::filter(onset>125)
data.province

data.province.long <- data.province %>%
    gather(key=type, value=count, -c(province))
data.province.long

province.thai <- ggplot(data.province.long,aes(x=province,y=count))+
    geom_bar(stat = "identity",position = "dodge",aes(fill=type))+
    labs(title = "Province of onset and Province of isolation in Thailand")+
    theme(legend.title=element_blank(),
                  #legend.position='none',
                  plot.title=element_text(size=15),
                  axis.text=element_text(size=12),
                  axis.text.x=element_text(angle=45, hjust=1))+ xlab('') + ylab('Count')
gly.province.thai <- ggplotly(province.thai)
gly.province.thai

# Confirmed cases divided by risk
data.thai.risk <- df_thai %>%
  group_by(risk) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
data.thai.risk

# Confirmed cases divided by risk
data.thai.risk <- df_thai %>%
  group_by(risk) %>%
  summarise(count = n()) %>%
  mutate(percent = (count / sum(count) * 100) %>% round(2)) %>%
  dplyr::filter(percent>4) %>%
  arrange(desc(percent)) 
data.thai.risk

data.thai.risk$risk[data.thai.risk$risk == "C"] <- "Close contact with the patient"
data.thai.risk$risk[data.thai.risk$risk == "CGustDr Samut SaGGAF"] <- "Cluster Samut Sakhon"
data.thai.risk$risk[data.thai.risk$risk == "F"] <- "State Quarantine"
data.thai.risk$risk[data.thai.risk$risk == "G"] <- "Go to a crowded place"
data.thai.risk$risk[data.thai.risk$risk == "A"] <- "People travelling from abroad"
data.thai.risk$risk[data.thai.risk$risk == "UFGFAwF"] <- "Unknown"
data.thai.risk$risk[data.thai.risk$risk == "D"] <- "Career at risk"
data.thai.risk$risk[data.thai.risk$risk == "CGustDr RayAFg"] <- "Cluster Rayong"
data.thai.risk$risk[data.thai.risk$risk == "CGustDr Pattaya CasCFA"] <- "Cluster Pattaya Casino"
data.thai.risk$risk[data.thai.risk$risk == "CGustDr AFg TGAFg"] <- "Cluster Ang Thong"
data.thai.risk

data.thai.risk$risk <- factor(data.thai.risk$risk, levels = as.character(data.thai.risk$risk))
data.thai.risk$risk
g.th.risk <- data.thai.risk %>% 
  ggplot(aes(x = "", y = percent, fill = risk)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_polar("y") +
  theme_void() +
  labs(title='Risk of Thai Confirmed Cases(Jan 2020 - Jan 2021)')+
  geom_text(aes(label = paste0(percent, "%")), color = "Black", size = 3.5, position = position_stack(vjust = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE)) 
g.th.risk
gly.th.risk <- ggplotly(g.th.risk)

# Confirmed cases divided by age
data.thai.age <- df_thai %>%
  group_by(age,sex) %>% 
  dplyr::filter(age != 0)%>%
  summarise(count = n()) %>%
  arrange(desc(count))
data.thai.age

data.thai.age <- data.thai.age %>% dplyr::filter(sex != "Unknown")
g.data.thai.age <- ggplot(data.thai.age,aes(x=age,y=count,fill=sex))+geom_bar(stat = "identity")+
      labs(title='Age of Thai Confirmed Cases (Stack)')+guides(fill=guide_legend(reverse = T))
gly.data.thai.age <- ggplotly(g.data.thai.age)

# Confirmed cases divided by nationality
data.thai.nationality <- df_thai

data.thai.nationality$nationality[data.thai.nationality$nationality == "????????"] <- "Unknown"
data.thai.nationality$nationality[data.thai.nationality$nationality == ""] <- "Unknown"
data.thai.nationality$nationality[data.thai.nationality$nationality == "Burma"] <- "Myanmar"

data.thai.nationality <- data.thai.nationality %>%
  group_by(nationality) %>%
  summarise(count = n()) %>%
  dplyr::filter(count > 11)%>%
  arrange(desc(count))

data.thai.nationality

data.thai.nationality$count[data.thai.nationality$nationality == "Unknown"]
g.th.nationality <- ggplot(data.thai.nationality, aes(x=reorder(nationality, count),y=count,fill = count))+ 
  geom_bar(stat = "identity",show.legend = F)+
    ggtitle('Nationality of Confirmed Cases in Thailand (log scale)')+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.text = element_text(size = 10),
            axis.title = element_text(size = 10),
            plot.title = element_text(size = 11))+
    xlab("")+ylab("Number of Nationality")+
   scale_y_continuous(trans='log10')+coord_flip()+scale_fill_gradient(low="blue",high = "red")
gly.th.nationality <- ggplotly(g.th.nationality)
gly.th.nationality
















