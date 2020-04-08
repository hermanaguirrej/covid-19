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

sample_d$newdeaths <- 1
i = 2
for (i in 2:length(sample_d$deaths)){
  sample_d$newdeaths[i] <- sample_d$deaths[i] - sample_d$deaths[i-1] 
}

sample_d$date <- as.character(sample_d$date)
sample_d$date <-str_sub(sample_d$date,2,8)
sample_d$date <- str_replace_all(sample_d$date, "[.]", "/")

# This snipt requires lubridate and zoo:
sample_d$date <- parse_date(sample_d$date, "%m/%d/%y")
sample_d$day <- as.numeric(format(sample_d$date,"%d"))
sample_d$month <- as.numeric(format(sample_d$date,"%m"))
sample_d$year <- as.numeric(format(sample_d$date,"%Y"))
unique(sample_d$country_d.Region)


# Script to plot a selected country_d:
#country_d_var = "Ukraine"
#country_d_var = "Chile"
#country_d_var = "United Kingdom"
country_d_var = "Italy"
#country_d_var = "China"
#country_d_var = "Spain"
#country_d_var = "US"
#country_d_var = "Germany"
#country_d_var = "France"
#country_d_var = "Korea, South"
#country_d_var = "Singapore"
#country_d_var = "Mexico"
country_d <- sample_d %>% filter(Country.Region == country_d_var & deaths > 0) 
country_d$days <- c(1:length(country_d$date))

#
ggplot(country_d, aes(days,deaths)) + geom_col(color ="steelblue",fill="white") + ggtitle(paste0(country_d_var,": Cumulative Deaths after ",length(country_d$date)," days\nHerman Aguirre-Jofre")) + geom_text(aes(label = deaths),vjust = -0.2, hjust=0.5, angle=0,check_overlap = TRUE,size = 3, colour="blue") 
View(country_d)


ggplot(country_d, aes(days,newdeaths)) + geom_col(color ="steelblue",fill="white") + ggtitle(paste0(country_d_var,": Deaths per day ",length(country_d$date)," days\nHerman Aguirre-Jofre")) + geom_text(aes(label = newdeaths),vjust = -0.2, hjust=0.5, angle=0,check_overlap = TRUE,size = 3, colour="blue") 
