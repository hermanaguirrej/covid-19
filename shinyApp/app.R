library(shiny)
library(tidyverse)
library(lubridate)
library(zoo)
library(readr)
#com
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


###
myfile1 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
sample_d <- read.csv(myfile1)
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
unique(sample_d$Country.Region)


ui <- fluidPage(
  tabsetPanel(
    tabPanel(title = "Confirmed",
             fluidRow(
               titlePanel(h1("Covid-19 Confirmed cases",style = 'padding-left: 15px')),
               sidebarPanel(selectInput("country_var", "Choose country:", choices = c("Argentina","Brazil","Bolivia","Chile","Colombia","Ecuador","Peru","Venezuela","Panama","Mexico","US","United Kingdom","Italy","Spain","China","France","Iran","Singapore","Korea, South", "Germany", "Finland", "Denmark", "Sweden", "Norway","Ukraine")))
             ),
             fluidRow(
               plotOutput("distPlot1"),
               plotOutput("distPlot2")
             ),
             hr(),
             fluidRow(
               titlePanel(h1("Covid-19 confirmed cases. top 15 countries",style = 'padding-left: 15px')),plotOutput("distPlot3"),plotOutput("distPlot4"),plotOutput("distPlot5")
             )
    ),
    tabPanel(title = "Deaths",
             fluidRow(
               titlePanel(h1("Total covid-19 Death cases worldwide",style = 'padding-left:15px')),
               sidebarPanel(selectInput("country_d_var","Choose country:", choices = c("Argentina","Brazil","Bolivia","Chile","Colombia","Ecuador","Peru","Venezuela","Panama","Mexico","US","United Kingdom","Italy","Spain","China","France","Iran","Singapore","Korea, South", "Germany", "Finland", "Denmark", "Sweden", "Norway","Ukraine")))
             ),
             fluidRow(
               plotOutput("distPlot_a"),
               plotOutput("distPlot_b")
             ),
             hr(),
             fluidRow(
               titlePanel(h1("Covid-19 Death cases worldwide", style = 'padding-left:15px')),plotOutput("distPlot_c"),plotOutput("distPlot_d"),plotOutput("distPlot_e")
             )
             
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot1 <- renderPlot({ 
    country <- sample %>% filter(Country.Region == input$country_var & confirmed > 100) 
    country$days <- c(1:length(country$date))
    
    ggplot(country, aes(days,confirmed)) + geom_col(color="steelblue", fill="white") + ggtitle(paste0(input$country_var,": Confirmed Cases after ",length(country$date)," days")) + geom_text(aes(label = confirmed),vjust = -0.2, hjust=0.5, angle=0,check_overlap = TRUE,size = 3, colour="steelblue")
  })
  
  
  output$distPlot2 <- renderPlot({ 
    country <- sample %>% filter(Country.Region == input$country_var & confirmed > 100) 
    country$days <- c(1:length(country$date))
    
    ggplot(data = country, aes(x = days,y = newConfirmed)) + geom_col(color = "steelblue", fill ="White") + ggtitle(paste0(input$country_var,": Confirmed Cases after ",length(country$date)," days\nHerman Aguirre-Jofre")) + geom_text(aes(label = newConfirmed),vjust = -0.2, hjust=0.5, angle=0,check_overlap = TRUE,size = 3, colour="steelblue") 
  })
  
  
  output$distPlot3 <- renderPlot({
    confirmed <- sample %>% group_by(Country.Region) %>% summarise(date = last(date), confirmed = last(confirmed)) %>% arrange(desc(confirmed)) %>%  slice(1:15) %>% mutate(Country.Region=factor(Country.Region, levels=Country.Region))
    
    ggplot(data = confirmed, aes(x= Country.Region, y=confirmed)) + geom_col(color="steelblue",fill = "white") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggtitle(paste0("Covid-19 Confirmed Cases, Date: ",last(confirmed$date),"\nHerman Aguirre-Jofre")) + geom_text(aes(label = confirmed),vjust = -0.5, color="steelblue")
  })
  
  
  output$distPlot4 <- renderPlot({
    sample %>% group_by(date) %>% summarise(confirmed = sum(confirmed)) %>% arrange(desc(date)) %>% slice(1:30) %>% ggplot(aes(x = date, y = confirmed)) + geom_col(color = "steelblue", fill = "white") + theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) + ggtitle("Covid-19: Confirmed cases over the last 30 days wordlwide \nHerman Aguirre-Jofre") + geom_text(aes(label = confirmed),vjust = -0.3, hjust= 0.5, angle = 0,check_overlap = TRUE,size = 3, colour ="steelblue") 
  })
  
  
  output$distPlot5 <- renderPlot({
    sample %>% group_by(date) %>% summarise(newConfirmed = sum(newConfirmed)) %>% arrange(desc(date)) %>% slice(1:30) %>% ggplot(aes(x = date, y = newConfirmed)) + geom_col(color = "steelblue", fill = "white") + theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) + ggtitle("Covid-19: Daily Confirmed over the last 30 days wordlwide \nHerman Aguirre-Jofre") + geom_text(aes(label = newConfirmed),vjust = -0.3, hjust= 0.5, angle = 0,check_overlap = TRUE,size = 3, colour ="steelblue") 
  })
  
  
  output$distPlot_a <- renderPlot({
    country_d <- sample_d %>% filter(Country.Region == input$country_d_var & deaths > 0) 
    country_d$days <- c(1:length(country_d$date))
    ggplot(country_d, aes(days,deaths)) + geom_col(color ="steelblue",fill="white") + ggtitle(paste0(input$country_d_var,": Deaths per day ",length(country_d$date)," days\nHerman Aguirre-Jofre")) + geom_text(aes(label = deaths),vjust = -0.2, hjust=0.5, angle=0,check_overlap = TRUE,size = 3, colour="steelblue") 
  })
  
  output$distPlot_b <- renderPlot({
    country_d <- sample_d %>% filter(Country.Region == input$country_d_var & deaths > 0) 
    country_d$days <- c(1:length(country_d$date))
    
    ggplot(country_d, aes(days,newdeaths)) + geom_col(color ="steelblue",fill="white") + ggtitle(paste0(input$country_d_var,": Deaths per day ",length(country_d$date)," days\nHerman Aguirre-Jofre")) + geom_text(aes(label = newdeaths),vjust = -0.2, hjust=0.5, angle=0,check_overlap = TRUE,size = 3, colour="steelblue")  
  })
  
  output$distPlot_c <- renderPlot({
    deaths <- sample_d %>% group_by(Country.Region) %>% summarise(date = last(date), deaths = last(deaths)) %>% arrange(desc(deaths)) %>%  slice(1:15) %>% mutate(Country.Region=factor(Country.Region, levels=Country.Region))
    
    ggplot(data = deaths, aes(x= Country.Region, y=deaths)) + geom_col(color="steelblue", fill="white") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggtitle(paste0("Covid-19 Deaths, Date: ",last(deaths$date),"\nHerman Aguirre-Jofre")) + geom_text(aes(label = deaths),vjust = -0.3, hjust=0.5, angle=0,check_overlap = TRUE,size = 3, colour="steelblue") 
  })
  
  output$distPlot_d <- renderPlot({
    sample_d %>% group_by(date) %>% summarise(deaths=sum(deaths)) %>% arrange(desc(date)) %>% slice(1:30) %>% ggplot(aes(x = date, y = deaths)) + geom_col(color="steelblue", fill="white") + theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) + ggtitle("Covid-19: Total deaths over the last 30 days worldwide \nHerman Aguirre-Jofre") + geom_text(aes(label = deaths),vjust = -0.3, hjust=0.5, angle=0,check_overlap = TRUE,size = 2.2, colour="steelblue") 
  })
  
  
  output$distPlot_e <- renderPlot({
    sample_d %>% group_by(date) %>% summarise(newdeaths=sum(newdeaths)) %>% arrange(desc(date)) %>% slice(1:30) %>% ggplot(aes(x= date, y=newdeaths)) + geom_col(color="steelblue", fill="white") + theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) + ggtitle("Covid-19: Daily deaths over the last 30 days worldwide \nHerman Aguirre-Jofre") + geom_text(aes(label = newdeaths),vjust = -0.3, hjust=0.5, angle=0,check_overlap = TRUE,size = 3, colour="steelblue") 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

