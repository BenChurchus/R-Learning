#import packages
library(tidyverse)
library(lubridate)
library(zoo)


#importing gallup polling
gallup = read.csv("D:/CodingDocs/DataCamp/AnalyzingElectionDataInR/gallup_approval_polls.csv")

#Seperating Trump Approvals 
TrumpApproval <- gallup %>% 
  select(President,Date,Approve) %>%
  filter(President == "Trump") %>%
  pull(Approve)


#Trying to do the same with a different president
ObamaApproval <- gallup %>% 
  select(President,Date,Approve) %>%
  filter(President == "Obama") %>%
  pull(Approve)

#Displaying
mean(ObamaApproval)
mean(TrumpApproval)


#creating an all approval rating of presidents
AllApproval <- gallup %>%
  group_by(President) %>%
  mutate(AvgApprove = rollmean(Approve, 10, na.pad=TRUE, align = "right"))


# Graph an moving average of each president's approval rating

ggplot(data = AllApproval, aes(x=Days, y=AvgApprove, col=President))  + 
    geom_line() 
