library(dplyr)
library(readr)
library(lubridate)
#library(zoo)

# Remove all objects from the R environment
rm(list = ls())

fransistown_airport_obs <- read_csv("data/csv/Obs_rainfall_data/033-FRAN-FRANCISTOWN AIRPORT.csv")%>%
  rename(daily_date =1, Rainfall = 2)
head(fransistown_airport_obs)
str(fransistown_airport_obs$daily_date)

# Convert the numeric date to character, then to Date format
fransistown_airport_obs$daily_date <- as.Date(as.character(fransistown_airport_obs$daily_date), format = "%Y%m%d")
str(fransistown_airport_obs$daily_date)


# Aggregate daily data to monthly data
monthly_data <- fransistown_airport_obs %>%
  mutate(year_month = floor_date(daily_date, "month")) %>%
  group_by(year_month) %>%
  summarize(Rainfall = sum(Rainfall, na.rm = TRUE)) %>%
  ungroup()

# View the first few rows of the monthly dataset
head(monthly_data)

