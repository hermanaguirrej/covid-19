# Covid Time Series
library(tidyverse)
library(lubridate)
library(zoo)
library(readr)

myfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
sample <- read.csv(myfile)
country <- sample %>% filter(Country.Region == "US")
country <- country %>% group_by(Province.State)
country <- country %>% summarise_if(is.numeric,sum)
country <- country %>% select(-2,-3) %>% pivot_longer(-Province.State,names_to = "date", values_to = "confirmed")

i=1
country$newcases <-1
for (i in 1:length(country$confirmed)){
  country$newcases[i+1] <- country$confirmed[i+1] - country$confirmed[i] 
}

country$date <- as.character(country$date)
country$date <-str_sub(country$date,2,8)
country$date <- str_replace_all(country$date, "[.]", "/")

# This snipt requires lubridate and zoo:
country$date <- parse_date(country$date, "%m/%d/%y")
country$day <- as.numeric(format(country$date,"%d"))
country$month <- as.numeric(format(country$date,"%m"))
country$year <- as.numeric(format(country$date,"%Y"))
unique(country$Province.State)


# Script to plot a selected country:
pais1 = "Florida"
pais <- country %>% filter(Province.State == pais1 & confirmed > 0) 
#pais <- pais %>% slice(1:15)
pais$days <- c(1:length(pais$date))
ggplot(pais, aes(days,confirmed)) + geom_col() + ggtitle(paste0(pais1,": Confirmed Cases after ",length(pais$date)," days\nHerman Aguirre-Jofre"))
View(pais)
