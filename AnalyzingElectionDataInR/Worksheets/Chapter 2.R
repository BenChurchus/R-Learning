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

library(readxl)
read_excel()
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
