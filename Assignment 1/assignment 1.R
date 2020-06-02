#Season
#1=winter, 
#2=spring, 
#3=summer, 
#4=fall
#yr
#0=2011, 
#1=2012
#weather	
#1=clear/partly cloudy, 
#2=mist/cloudy/partly cloudy, 
#3=light rain/snow,  
#4=heavy rain/snow/storm
#temp	normalized air temperature, ranging 0-1 where 0 is coldest and 1 is hottest
#cnt	number of bike rentals

##For each Variable
#1. Identify the variable type (quantitative (continuous or discrete), categorical (nominal, binary, or ordinal). Explain your reasoning.
                               
#2. Using R, create an appropriate graphic showing the distribution of the data for the respective variable. Remember labels and titles.
                               
#3.	Discuss the distribution for each variable based on the graphs in #2. 
# •	For categorical, compare counts or proportions between categories. Do they look as you’d expect, given the definitions of the variables? 
# •	For quantitative, discuss the approximate center, range, shape, and outliers (if any).
                               
#4.	Compute appropriate summary statistics for each variable.
                               
#Season plot, Categorial graph is barplot

season.barplot <- ggplot(data = Bikeshare) + geom_bar(mapping = aes(x = season))

# INCLUDE THIS AT THE START OF EVERY MARKDOWN FILE
#```{r setup, include=FALSE}
#knitr::opts_chunk$set(echo = TRUE)
#library(tidyverse)
#library(readr)
#Bikeshare <- read.csv("Bikeshare.csv")
#``` #


#Table set up for categorical variables

season.setup <- table(Bikeshare$season)
season.table = cbind(season.setup, round(prop.table(season.setup)*100,2))
colnames(season.table) = c("Count", "Percentage")
rownames(season.table) = c("Winter", "Spring", "Summer", "Fall")
season.table

#Year barplot

yr.barplot <- ggplot(data = Bikeshare) + geom_bar(mapping = aes(x = yr)) + labs(title = "Days in Each Year") + scale_x_continuous(breaks = c(0, 1), labels = c("2011", "2012"))
plot(yr.barplot)

#year table

yr.setup <- table(Bikeshare$yr)
yr.table = cbind(yr.setup, round(prop.table(yr.setup)*100,2))
colnames(yr.table) = c("Count", "Percentage")
rownames(yr.table) = c("2011", "2012")
writeLines("Summary Statistics for Years (Days)")
print(yr.table)

#weather barplot

weather.barplot <- ggplot(data = Bikeshare) + geom_bar(mapping = aes(x = weather)) + labs(title = "Weather") + scale_x_continuous(breaks = c(1, 2, 3, 4), labels = c("Clear/partly cloudy", "mist/cloudy/partly cloudy", "light rain/snow", "Storm"), drop = FALSE)
plot(weather.barplot)

#weather table

weather.setup <- table(Bikeshare$weather)
weather.table = cbind(weather.setup, round(prop.table(weather.setup)*100,2))
colnames(weather.table) = c("Count", "Percentage")
rownames(weather.table) = c("clear/partly cloudy", "mist/cloudy/partly cloudy", "light rain/snow", "heavy rain/snow/storm")
writeLines("Summary Statistics for Types of Weather Across 2011-2012")
print(weather.table)

#dumb way to include 0 obs for 4th level of weather

meme <- matrix(c(463,63.34, 247,33.79, 21,2.87, 0,0), nrow = 4, ncol = 2, byrow = TRUE,
               dimnames = list(c("clear/partly cloudy", "mist/cloudy/partly cloudy", "light rain/snow", "heavy rain/snow/storm"),
                               c("Count", "Percentage")))

#temperature plot
#bin width calculated using Freedman-Diaconis rule

temp.histogram <- ggplot(Bikeshare, aes(x=temp)) + geom_histogram(breaks=seq(0,1, by = 0.07), color = "darkblue", fill = "lightblue")
temp.histogram

#updated weather plot to include 0 count factor level
weather.barplot <- ggplot(data = Bikeshare) + geom_bar(mapping = aes(x = weather)) + labs(title = "Weather") + scale_x_discrete(breaks = c(1, 2, 3, 4), labels = c("Clear/partly cloudy", "mist/cloudy/partly cloudy", "light rain/snow", "Storm"), drop = FALSE)

#cnt histogram

cnt.histogram <- ggplot(Bikeshare, aes(x=cnt)) + geom_histogram(breaks=seq(0,10000, by = 622), color = "darkblue", fill = "lightblue") + labs(title = "Number of Bike rentals" ) + xlab("Number of Bike Rentals") + ylab("Total Days")
cnt.histogram
