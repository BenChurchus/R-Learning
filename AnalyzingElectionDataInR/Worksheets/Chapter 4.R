# Import dplyr and lubridate
library(dplyr)
library(lubridate)

#Import Data

# average all of the generic ballot polls that have been taken since June
polls_2018 %>% 
  filter(month(end_date) > 6) %>% 
  mutate(Dem.Margin = Democrat - Republican) %>%
  pull(Dem.Margin) %>% 
  mean()

# Filter the dataset to include polls from August and September
# Mutate a variable for the Democratic vote margin in that year
polls_predict <- generic_ballot %>%
  filter(month(mdy(Date)) %in% c(8,9)) %>%
  mutate(Dem.Poll.Margin = Democrats - Republicans,
         Dem.Vote.Margin = DemVote - RepVote)

# Group by election year
polls_predict <- polls_predict %>%
  group_by(ElecYear) %>%
  summarise(Dem.Poll.Margin  = mean(Dem.Poll.Margin),
            Dem.Vote.Margin = mean(Dem.Vote.Margin))

library(lubridate)
library(tidyverse)
library(zoo)

#Import Data
generic_ballot = read.csv("D:/CodingDocs/DataCamp/AnalyzingElectionDataInR/generic_ballot.csv")

ggplot(generic_ballot, aes(x=mdy(Date), y=Democrats)) + 
  geom_point()

#graph trends over time

#grabbing head
head(generic_ballot)


#filtering to 2016 only
generic_ballot %>%
  filter(ElecYear == 2016) %>%
  select(Date, Democrats, Republicans)

# Mutate a new variable called "Democratic.Margin" equal to the difference between Democrats' vote share and Republicans'
democratic_lead <- generic_ballot %>%
  mutate(Democratic.Margin = Democrats - Republicans)

# Mutate a new variable called "Repuiblican" equal to the difference between Democrats' vote share and Republicans'
republican_lead  <- generic_ballot %>%
  mutate(Republican.Margin = Republicans - Democrats)


#mutating a new variables to represent Democratic value over republicans


ggplot(republican_lead, aes(x=mdy(Date), y=Republican.Margin)) + 
  geom_point()



ggplot(democratic_lead, aes(x=mdy(Date), y=Democratic.Margin)) + 
  geom_point()




#tracking using the support rather than the lead
#creating our summarise

over_timeD <- democratic_lead %>% 
  group_by(ElecYear)  %>% 
  summarise(Democratic.Margin = mean(Democratic.Margin))

over_timeR <- republican_lead %>% 
  group_by(ElecYear)  %>% 
  summarise(Republican.Margin = mean(Republican.Margin))

head(over_timeD)
head(over_timeR)

# Create a month and year variable for averaging polls by approximate date
timeseriesD <- democratic_lead %>%
  mutate(Date = mdy(Date),
         month = month(Date),
         yr = year(Date))

# Now group the polls by their month and year, then summarise
timeseriesD <- timeseriesD %>%
  group_by(yr,month) %>%
  summarise(Democratic.Margin = mean(Democratic.Margin))

# Create a month and year variable for averaging polls by approximate date
timeseriesR <- republican_lead %>%
  mutate(Date = mdy(Date),
         month = month(Date),
         yr = year(Date))

# Now group the polls by their month and year, then summarise
timeseriesR <- timeseriesR %>%
  group_by(yr,month) %>%
  summarise(Republican.Margin = mean(Republican.Margin))

# Mutate a new variable to use a date summary for the monthly average
timeseries_plotD <- timeseriesD %>%
  mutate(time = sprintf("%s-%s-%s",yr,month,"01"))

# Mutate a new variable to use a date summary for the monthly average
timeseries_plotR <- timeseriesR %>%
  mutate(time = sprintf("%s-%s-%s",yr,month,"01"))


# Plot the line over time
ggplot(timeseries_plotD,aes(x=ymd(time),y=Democratic.Margin)) +
  geom_line()  + 
  geom_smooth(span=0.2)


# Plot the line over time
ggplot(timeseries_plotR,aes(x=ymd(time),y=Republican.Margin)) +
  geom_line()  + 
  geom_smooth(span=0.2)

# Mutate two variables for the Democrats' margin in polls and election day votes
poll_error <- generic_ballot %>%
  mutate(Dem.Poll.Margin = Democrats - Republicans,
         Dem.Vote.Margin = DemVote - RepVote)

# Average those two variables per year and mutate the "error" variable
poll_error <- poll_error %>%
  group_by(ElecYear) %>%
  summarise(Dem.Poll.Margin = mean(Dem.Poll.Margin),
            Dem.Vote.Margin = mean(Dem.Vote.Margin)) %>%
  mutate(error = Dem.Poll.Margin - Dem.Vote.Margin)

# Calculate the room-mean-square error of the error variable
rmse <- sqrt(mean(poll_error$error^2))

# Multiply the RMSE by 1.96 to get the 95% confidence interval, or "margin of error"
CI <- rmse * 1.96

# Add variables to our dataset for the upper and lower bound of the `Dem.Poll.Margin` variable
by_year <- poll_error %>%
  mutate(upper = Dem.Poll.Margin + CI,
         lower = Dem.Poll.Margin - CI)

# Plot estimates for Dem.Poll.Margin and Dem.Vote.Margin on the y axis for each year on the x axis with geom_point
ggplot(by_year) + 
  geom_point(aes(x=ElecYear, y=Dem.Poll.Margin, col="Poll")) +
  geom_point(aes(x=ElecYear, y=Dem.Vote.Margin, col="Vote")) +
  geom_errorbar(aes(x=ElecYear, ymin=lower, ymax=upper))

# Fit a model predicting Democratic vote margin with Democratic poll margin
model <- lm(Dem.Vote.Margin ~ Dem.Poll.Margin, data=by_year)

# Evaluate the model
summary(model)

# Make a new data.frame that has our prediction variable and value
predictdata <- data.frame("Dem.Poll.Margin" = 5)

# Make the prediction with the coefficients from our model
predict(model, predictdata)

# Fit a model to predict Democrats' November vote margin with the Democratic poll margin and party in power variable
model <- lm(Dem.Vote.Margin ~ Dem.Poll.Margin + party_in_power, data=polls_predict)

# Evaluate the model
summary(model)

# Make a prediction for November if Democrats are up 7.5 points in the generic ballot and the party_in_power is the Republicans (-1)
predict(model, data.frame(Dem.Poll.Margin = 7.5, party_in_power=-1))

# Multiply the root-mean-square error by 1.96
sqrt(mean(c(model$fitted.values - polls_predict$Dem.Vote.Margin)^2)) * 1.96

# Import ggplot2
library(ggplot2)

#  Make a plot with points representing a year's presidential approval and vote share and a line running through them to show the linear relationship
ggplot(pres_elecs,aes(x=pres_approve, y=vote_share, label=Year)) + 
  geom_text() + 
  geom_smooth(method='lm')

# Make a model that predict the vote_share variable with pres_approve, q2_gdp, and two_plus_terms
fit <- lm(vote_share ~ pres_approve + q2_gdp + two_plus_terms, pres_elecs)

# Evaluate the model
summary(fit)

# Save the predicted vote shares to a variable called predict
pres_elecs$predict <- predict(fit, pres_elecs)

# Graph the predictions and vote shares with a label for each election year
ggplot(pres_elecs,aes(x=predict, y=vote_share, label=Year)) + 
  geom_abline() +
  geom_text()

# Calculate the model's root-mean-square error
sqrt(mean(c(pres_elecs$predict - pres_elecs$vote_share)^2)) * 1.96

# Make a prediction for hypothetical data
predict(fit, data.frame(pres_approve=-10, q2_gdp=2, two_plus_terms=0))