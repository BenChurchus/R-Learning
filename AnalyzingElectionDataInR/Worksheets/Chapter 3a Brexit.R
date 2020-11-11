library(lubridate)
library(tidyverse)
library(zoo)
library(tidyr)
library(choroplethr)
library(choroplethrMaps)
library(gridExtra)
library(magrittr)


#Import Data
brexit_polls = read.csv("D:/CodingDocs/DataCamp/AnalyzingElectionDataInR/brexit_polls.csv")
brexit_results = read.csv("D:/CodingDocs/DataCamp/AnalyzingElectionDataInR/brexit_results.csv")

# Filter the dataset to polls only released after June 16th, 2016, and mutate a variable for the Remain campaign's lead
brexit_average <- brexit_polls %>%
  filter(mdy(Date)>ymd("2016-06-16") )%>%
  mutate(RemainLead = Remain - Leave)  

# Average the last seven days of polling
mean(brexit_average$RemainLead)



# Familiarize yourself with the data using the head() function
head(brexit_results)

# Chart the counstituency-by-constituency relationship between voting for the Labour Party and voting to leave the EU
ggplot(brexit_results,aes(x=lab_2015,y=leave_share)) + 
  geom_point()

# Show the relationship between UKIP and Leave vote share with points and a line representing the linear relationship between the variables
ggplot(brexit_results,aes(x=ukip_2015,y=leave_share)) + 
  geom_point()  +
  geom_smooth(method = "lm")

# predict leave's share with the percentage of a constituency that holds a college degree and its 2015 UKIP vote share
model.multivar <- lm(leave_share ~ ukip_2015 + degree, brexit_results)

summary(model.multivar)