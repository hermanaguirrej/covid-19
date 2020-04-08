# Covid Time Series
library(tidyverse)
library(lubridate)
library(zoo)
library(readr)

myfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
sample_d <- read.csv(myfile)
sample_d <- sample_d %>% group_by(Country.Region)
sample_d <- sample_d %>% summarise_if(is.numeric,sum)
sample_d <- sample_d %>% select(-2,-3) %>% pivot_longer(-Country.Region,names_to = "date", values_to = "deaths")

sample_d$newdeaths <- 0
i = 2
for (i in 2:length(sample_d$deaths)){
  if(sample_d$Country.Region[i] == sample_d$Country.Region[i-1]){
    sample_d$newdeaths[i] <- sample_d$deaths[i] - sample_d$deaths[i-1]
  } else {
    sample_d$newdeaths[i] <-0
  }
}

sample_d$date <- as.character(sample_d$date)
sample_d$date <-str_sub(sample_d$date,2,8)
sample_d$date <- str_replace_all(sample_d$date, "[.]", "/")

# This snipt requires lubridate and zoo:
sample_d$date <- parse_date(sample_d$date, "%m/%d/%y")
sample_d$day <- as.numeric(format(sample_d$date,"%d"))
sample_d$month <- as.numeric(format(sample_d$date,"%m"))
sample_d$year <- as.numeric(format(sample_d$date,"%Y"))
unique(sample_d$Country.Region)

deaths <- sample_d %>% group_by(Country.Region) %>% summarise(date = last(date), deaths = last(deaths)) %>% arrange(desc(deaths)) %>%  slice(1:15) %>% mutate(Country.Region=factor(Country.Region, levels=Country.Region))

ggplot(data = deaths, aes(x= Country.Region, y=deaths)) + geom_col(color="steelblue", fill="white") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggtitle(paste0("Covid-19 Deaths, Date: ",last(deaths$date),"\nHerman Aguirre-Jofre")) + geom_text(aes(label = deaths),vjust = -0.3, hjust=0.5, angle=0,check_overlap = TRUE,size = 3, colour="steelblue") 
View(deaths)


###
library(tidyverse)
library(lubridate)
library(zoo)
library(readr)
myfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
sample_d <- read.csv(myfile)
sample_d <- sample_d %>% group_by(Country.Region)
sample_d <- sample_d %>% summarise_if(is.numeric,sum)
sample_d <- sample_d %>% select(-2,-3) %>% pivot_longer(-Country.Region,names_to = "date", values_to = "deaths")

sample_d$newdeaths <- 0
i = 2
for (i in 2:length(sample_d$deaths)){
  if(sample_d$Country.Region[i] == sample_d$Country.Region[i-1]){
    sample_d$newdeaths[i] <- sample_d$deaths[i] - sample_d$deaths[i-1]
  } else {
    sample_d$newdeaths[i] <-0
  }
}

sample_d$date <- as.character(sample_d$date)
sample_d$date <-str_sub(sample_d$date,2,8)
sample_d$date <- str_replace_all(sample_d$date, "[.]", "/")

# This snipt requires lubridate and zoo:
sample_d$date <- parse_date(sample_d$date, "%m/%d/%y")
sample_d$day <- as.numeric(format(sample_d$date,"%d"))
sample_d$month <- as.numeric(format(sample_d$date,"%m"))
sample_d$year <- as.numeric(format(sample_d$date,"%Y"))


sample_d %>% group_by(date) %>% summarise(newdeaths=sum(newdeaths)) %>% arrange(desc(date)) %>% slice(1:30) %>% ggplot(aes(x= date, y=newdeaths)) + geom_col(color="steelblue", fill="white") + theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) + ggtitle("Covid-19: Daily deaths over the last 30 days worldwide \nHerman Aguirre-Jofre") + geom_text(aes(label = newdeaths),vjust = -0.3, hjust=0.5, angle=0,check_overlap = TRUE,size = 3, colour="steelblue") 

sample_d <- sample_d %>% group_by(date) %>% summarise(newdeaths=sum(newdeaths)) %>% arrange(desc(date)) %>% slice(1:30)

ggplot(sample_d,aes(x = date, y = newdeaths)) + 
geom_col(color="steelblue", fill="white") + 
theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) + 
ggtitle("Covid-19: Daily deaths over the last 30 days worldwide \nHerman Aguirre-Jofre") + 
geom_text(aes(label = newdeaths),
vjust = -0.3, 
hjust=0.5, 
angle=0,check_overlap = TRUE,size = 3, 
colour="steelblue") 

warning()
newdeaths_lastdays <- sample_d %>% group_by(date) %>% summarise(newdeaths=sum(newdeaths)) %>% arrange(desc(date))
sum(newdeaths_lastdays$newdeaths)



sample_d %>% group_by(date) %>% summarise(deaths=sum(deaths)) %>% arrange(desc(date)) %>% slice(1:30) %>% ggplot(aes(x = date, y = deaths)) + geom_col(color="steelblue", fill="white") + theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) + ggtitle("Covid-19: Total deaths over the last 30 days worldwide \nHerman Aguirre-Jofre") + geom_text(aes(label = deaths),vjust = -0.3, hjust=0.5, angle=0,check_overlap = TRUE,size = 2.2, colour="steelblue") 

deaths_lastday <- sample_d %>% group_by(date) %>% summarise(deaths = sum(deaths)) %>% arrange(desc(date))

deaths_lastday$deaths[1]-deaths_lastday$deaths[2]
