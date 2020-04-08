library(tidyverse)
library(leaflet)
library(viridis)
library(plotly)
library(rworldmap)
library(dplyr)
library(ggmap)

myfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-24-2020.csv"
sample <- read.csv(myfile)

world  <- map_data("world")
g <- sample %>% filter(Confirmed >5) %>% arrange(Confirmed) %>% mutate(name=factor(Country_Region, unique(Country_Region))) %>% ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) + geom_point(aes(x=Long_, y=Lat, size=Confirmed, color=Confirmed), alpha=0.4) + scale_size_continuous(range=c(1,10)) +scale_color_viridis (trans="log") + coord_map() +  ggtitle("Confirmed Cases Worldwide\nHerman Aguirre-Jofre")
g


# Covid
sample$Country_Region <- as.factor(sample$Country_Region)
sample1<- sample %>% group_by(Country_Region) %>% summarise(
  Last.Update = first(Last_Update),
  Confirmed = sum(Confirmed),
  Deaths = sum(Deaths),
  Recovered = sum(Recovered),
  Latitude = mean(Lat),
  Longitude = mean(Long_)
) %>% arrange(desc(Confirmed)) %>% slice(1:20) %>% mutate(Country_Region=factor(Country_Region, levels=Country_Region)) # This trick update the factor levels

g1 <- ggplot(data = sample1, aes(x= Country_Region, y=Confirmed)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Top 20 Confirmed Cases Worldwide\nHerman Aguirre-Jofre")
g1
