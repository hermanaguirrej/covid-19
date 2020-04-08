# Covid Time Series
library(tidyverse)
library(lubridate)
library(zoo)
library(readr)
#HUB
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

# Script to plot a selected country:
#country_var = "Ukraine"
#country_var = "Chile"
#country_var = "United Kingdom"
#country_var = "Italy"
country_var = "China"
#country_var = "Spain"
#country_var = "US"
#country_var = "Germany"
#country_var = "France"
#country_var = "Korea, South"
#country_var = "Singapore"
#country_var = "Mexico"

#database definition
country <- sample %>% filter(Country.Region == country_var & confirmed > 100) 
country$days <- c(1:length(country$date))

ggplot(data = country, aes(x = days,y = confirmed)) + geom_col(color = "steelblue", fill ="White") + ggtitle(paste0(country_var,": Confirmed Cases after ",length(country$date)," days\nHerman Aguirre-Jofre")) + geom_text(aes(label = confirmed),vjust = -0.2, hjust=0.5, angle=0,check_overlap = TRUE,size = 3, colour="steelblue") 
View(country)

ggplot(data = country, aes(x = days,y = newcases)) + geom_col(color = "steelblue", fill ="White") + ggtitle(paste0(country_var,": Confirmed Cases after ",length(country$date)," days\nHerman Aguirre-Jofre")) + geom_text(aes(label = newcases),vjust = -0.2, hjust=0.5, angle=0,check_overlap = TRUE,size = 3, colour="steelblue") 
View(country)



country_var2 = "US"
#database definition
country2 <- sample %>% filter(Country.Region == country_var2 & confirmed > 0) 
country2$days <- c(1:length(country2$date))
country_comp <- rbind(country,country2)


#Geom_plot_column
ggplot(data = country_comp, aes(x = days,y = confirmed, colour = Country.Region)) + geom_col(fill ="white",position = "dodge") + ggtitle(paste0(country_var,": Confirmed Cases after ",length(country$date)," days\nHerman Aguirre-Jofre")) + geom_text(aes(label = confirmed),vjust = 0.1, hjust=-0.2, angle=90,check_overlap = TRUE,size = 3)
View(country)


#Geom_plot_line
ggplot(data = country_comp, aes(x = days,y = confirmed, colour = Country.Region)) + geom_line() + ggtitle(paste0(country_var," and ",country_var2,": Confirmed Cases after ",length(country$date)," days\nHerman Aguirre-Jofre")) + geom_text(aes(label = confirmed),vjust = 0.1, hjust=-0.2, angle = 0,check_overlap = TRUE,size = 3)

