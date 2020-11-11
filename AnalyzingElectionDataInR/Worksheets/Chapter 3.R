library(lubridate)
library(tidyverse)
library(zoo)
library(tidyr)
library(choroplethr)
library(choroplethrMaps)
library(gridExtra)
library(magrittr)

#THE INSTALL ZONE
#install.packages("jbmisc") # package installations are only needed the first time you use it



#Import Data
uspres_results = read.csv("D:/CodingDocs/DataCamp/AnalyzingElectionDataInR/us_pres_2016_by_county.csv")

# Deselect the is.national.winner, national.count, and national.party.percent variables
uspres_results.slim <- uspres_results %>%
  select(-c(is.national.winner, national.count, national.party.percent))

# Spread party and votes to their own columns
uspres_county <- uspres_results.slim %>%
  spread(key=party,value=vote.count)

# Add a variable to the uspres_county dataset to store the Democrat's percentage of votes
uspres_county <- uspres_county %>%
  mutate(Dem.pct = D/county.total.count) %>%
  mutate(Rep.pct = R/county.total.count)

# Load the county demographic data
data(df_county_demographics)

# Look at the demographic data
head(df_county_demographics)

# Rename the 'region' variable in df_county_demographics to "county.fips"
df_county_demographics <- df_county_demographics %>%
  rename("county.fips" = region)

# Join county demographic with vote share data via its FIPS code
county_merged <- left_join(df_county_demographics,
                           uspres_county, by = "county.fips")

# plot percent_white and Dem.pct on the x and y axes. add points and a trend line
ggplot(county_merged, aes(x=percent_white, y=Dem.pct)) +
  geom_point() +
  geom_smooth(method="lm")

# plot percent_white and Dem.pct on the x and y axes. add points and a trend line
ggplot(county_merged, aes(x=percent_black, y=Dem.pct)) +
  geom_point() +
  geom_smooth(method="lm")

# plot percent_white and Dem.pct on the x and y axes. add points and a trend line
ggplot(county_merged, aes(x=percent_hispanic, y=Dem.pct)) +
  geom_point() +
  geom_smooth(method="lm")

# plot percent_white and Dem.pct on the x and y axes. add points and a trend line
ggplot(county_merged, aes(x=per_capita_income, y=Dem.pct)) +
  geom_point() +
  geom_smooth(method="lm")

# plot percent_white and Dem.pct on the x and y axes. add points and a trend line
ggplot(county_merged, aes(x=percent_white, y=Rep.pct)) +
  geom_point() +
  geom_smooth(method="lm")

democratic_map <- county_merged %>%
  dplyr::rename("region" = county.fips,
                "value" = Dem.pct)

republican_map <- county_merged %>%
  dplyr::rename("region" = county.fips,
                "value" = Rep.pct)

white_map <- county_merged %>%
  dplyr::rename("region" = county.fips,
                "value" = percent_white)

hispanic_map <- county_merged %>%
  dplyr::rename("region" = county.fips,
                "value" = percent_hispanic)


black_map <- county_merged %>%
  dplyr::rename("region" = county.fips,
                "value" = percent_black)


county_choropleth(white_map)
county_choropleth(black_map)
county_choropleth(hispanic_map)

ggplot(whiteplot)


# Fit a linear model to predict Dem.pct dependent on percent_white in each county
fit <- lm(Dem.pct ~ percent_white, data=county_merged)

# Evaluate the model
summary(fit)

# Fit a linear model to predict Dem.pct dependent on percent_white and per_capita_income in each county
fit <- lm(Dem.pct ~ percent_white + per_capita_income, data=county_merged)
 e model
summary(fit)
