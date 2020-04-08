# Covid Time Series
library(tidyverse)
library(lubridate)
library(zoo)
library(readr)

myfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
sample <- read.csv(myfile)
sample <- sample %>% group_by(Country.Region)
sample <- sample %>% summarise_if(is.numeric,sum)
sample <- sample %>% select(-2,-3) %>% pivot_longer(-Country.Region,names_to = "date", values_to = "confirmed")

sample$newConfirmed <- 0
i = 2
for (i in 2:length(sample$confirmed)){
  if(sample$Country.Region[i] == sample$Country.Region[i-1]){
    sample$newConfirmed[i] <- sample$confirmed[i] - sample$confirmed[i-1]
  } else {
    sample$newConfirmed[i] <-0
  }
}

sample$date <- as.character(sample$date)
sample$date <-str_sub(sample$date,2,8)
sample$date <- str_replace_all(sample$date, "[.]", "/")

# This snipt requires lubridate and zoo:
sample$date <- parse_date(sample$date, "%m/%d/%y")
sample$day <- as.numeric(format(sample$date,"%d"))
sample$month <- as.numeric(format(sample$date,"%m"))
sample$year <- as.numeric(format(sample$date,"%Y"))
#unique(sample$Country.Region)
#unique(sample$date)

confirmed <- sample %>% group_by(Country.Region) %>% summarise(date = last(date), confirmed = last(confirmed)) %>% arrange(desc(confirmed)) %>%  slice(1:15) %>% mutate(Country.Region=factor(Country.Region, levels=Country.Region))

ggplot(data = confirmed, aes(x= Country.Region, y=confirmed)) + geom_col(color="steelblue",fill = "white") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggtitle(paste0("Covid-19 Confirmed Cases, Date: ",last(confirmed$date),"\nHerman Aguirre-Jofre")) + geom_text(aes(label = confirmed),vjust = -0.5, color="steelblue")


###
# Covid Time Series
library(tidyverse)
library(lubridate)
library(zoo)
library(readr)

myfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
sample <- read.csv(myfile)
sample <- sample %>% group_by(Country.Region)
sample <- sample %>% summarise_if(is.numeric,sum)
sample <- sample %>% select(-2,-3) %>% pivot_longer(-Country.Region,names_to = "date", values_to = "confirmed")

sample$newConfirmed <- 0
i = 2
for (i in 2:length(sample$confirmed)){
  if(sample$Country.Region[i] == sample$Country.Region[i-1]){
    sample$newConfirmed[i] <- sample$confirmed[i] - sample$confirmed[i-1]
  } else {
    sample$newConfirmed[i] <-0
  }
}

sample$date <- as.character(sample$date)
sample$date <-str_sub(sample$date,2,8)
sample$date <- str_replace_all(sample$date, "[.]", "/")

# This snipt requires lubridate and zoo:
sample$date <- parse_date(sample$date, "%m/%d/%y")
sample$day <- as.numeric(format(sample$date,"%d"))
sample$month <- as.numeric(format(sample$date,"%m"))
sample$year <- as.numeric(format(sample$date,"%Y"))

sample %>% group_by(date) %>% summarise(newConfirmed = sum(newConfirmed)) %>% arrange(desc(date)) %>% slice(1:30) %>% ggplot(aes(x = date, y = newConfirmed)) + geom_col(color = "steelblue", fill = "white") + theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) + ggtitle("Covid-19: Daily Confirmed over the last 30 days wordlwide \nHerman Aguirre-Jofre") + geom_text(aes(label = newConfirmed),vjust = -0.3, hjust= 0.5, angle = 0,check_overlap = TRUE,size = 3, colour ="steelblue") 

newConfirmed_lastdays <- sample %>% group_by(date) %>% summarise(newConfirmed = sum(newConfirmed)) %>% arrange(desc(date))
sum(newConfirmed_lastdays$newConfirmed)


sample %>% group_by(date) %>% summarise(confirmed = sum(confirmed)) %>% arrange(desc(date)) %>% slice(1:30) %>% ggplot(aes(x = date, y = confirmed)) + geom_col(color = "steelblue", fill = "white") + theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) + ggtitle("Covid-19: Confirmed cases over the last 30 days wordlwide \nHerman Aguirre-Jofre") + geom_text(aes(label = confirmed),vjust = -0.3, hjust= 0.5, angle = 0,check_overlap = TRUE,size = 3, colour ="steelblue") 

confirmed_lastdays <- sample %>% group_by(date) %>% summarise(newConfirmed=sum(newConfirmed)) %>% arrange(desc(date))
