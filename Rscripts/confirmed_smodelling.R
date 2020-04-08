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

sample$newcases <-1
i = 1
for (i in 1:length(sample$confirmed)){
  sample$newcases[i+1] <- sample$confirmed[i+1] - sample$confirmed[i] 
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
#country1 = "Ukraine"
country1 = "Chile"
#country1 = "United Kingdom"
#country1 = "Italy"
#country1 = "China"
#country1 = "Spain"
#country1 = "US"
#country1 = "Germany"
#country1 = "France"
#country1 = "Korea, South"
#country1 = "Singapore"
#country1 = "Colombia"

country <- sample %>% filter(Country.Region == country1 & confirmed > 0) 
country$days <- c(1:length(country$date))
ggplot(country, aes(days,confirmed)) + geom_col() + ggtitle(paste0(country1,": Confirmed Cases after ",length(country$date)," days\nHerman Aguirre-Jofre")) + geom_text(aes(label = confirmed),vjust = -0.2, hjust= 0.5, angle=0,check_overlap = TRUE,size = 3)
View(country)





country$porcentage <- 1
i=1
for (i in 1:length(country$date)) {
  country$porcentage[i+1] <- ((country$confirmed[i+1] - country$confirmed[i])/country$confirmed[i])*100
}

CGR <- (((country$confirmed[length(country$date)] - country$confirmed[1])^(1/(length(country$date)-1)))-1)*100
CGR



## linear model
fit <- lm(confirmed ~ days, country)
summary(fit)

hist(fit$residuals)
par(mfrow=c(2, 2))
plot(fit, pch=19, col='darkgrey')

## produce predicted longevity for thorax = 0.8mm
newdata <- data.frame(days = 10)
predict(fit, newdata)

newdata <- data.frame(days=seq(min(country$days), max(country$days), length.out=50))
newdata <- cbind(newdata, confirmed=predict(fit, newdata))

plot(confirmed ~ days, data = country, pch = 19, col='darkgrey', main = paste0(country1,":Confirmed cases and linear model"))
points(confirmed ~ days, data = newdata, pch = 19, col='red')
(gradient = (newdata$confirmed[50]-newdata$confirmed[1])/(newdata$days[50]-newdata$days[1]))



##
fit <- glm(confirmed ~ days, data=country, family=poisson(link=log))
summary(fit)
par(mfrow=c(2, 2))
plot(fit, pch=19, col='red')

newdata <- expand.grid(days=seq(min(country$day), max(country$day), length.out=200))
newdata <- cbind(newdata, confirmed=predict(fit, newdata, type='response'))

ggplot(mapping=aes(x=days, y=confirmed)) + geom_col(data=country, fill="white", color="steelblue") + geom_line(data=newdata, colour = "red") + ggtitle(paste0(country1,": Generalised Lienear Model\nHerman Aguirre-Jofre")) + geom_text(data=country,aes(label = confirmed),vjust = -0.2, hjust= 0.5, angle=0,check_overlap = TRUE,size = 3, color = "steelblue")

