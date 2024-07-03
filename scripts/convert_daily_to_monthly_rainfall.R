# Remove all objects from the R environment
rm(list = ls())

library(dplyr)
library(readr)
library(lubridate)
#library(zoo)

rain_obs <- read_csv("data/csv/Obs_rainfall_data/033-FRAN-FRANCISTOWN AIRPORT.csv")%>%
  rename(daily_date =1, Rainfall = 2)
head(rain_obs)
str(rain_obs$daily_date)

# Convert the numeric date to character, then to Date format
rain_obs$daily_date <- as.Date(as.character(rain_obs$daily_date), format = "%Y%m%d")
str(rain_obs$daily_date)


# Aggregate daily data rainfall to monthly data
monthly_stn_data <- rain_obs %>%
  mutate(year_month = floor_date(daily_date, "month")) %>%
  group_by(year_month) %>%
  summarise(days_with_na = sum(is.na(Rainfall)),
            monthly_rainfall = sum(Rainfall, na.rm = TRUE)) %>%
  ungroup()



# Import CHIRPS satellite data
fransistown_airport_chirps <- read_csv("data/csv/csv_proc/FRANCISTOWN AIRPORT.csv",skip = 1)%>%
  rename(daily_date_chirps =1, Rainfall_chirps = 2)

# Convert "No data" and set it to numeric to perform monthly computation
fransistown_airport_chirps$Rainfall_chirps[fransistown_airport_chirps$Rainfall_chirps == "No data"] <- NA
fransistown_airport_chirps$Rainfall_chirps <- as.numeric(fransistown_airport_chirps$Rainfall_chirps)


# Aggregate daily data rainfall to monthly data
monthly_data_chirps <- fransistown_airport_chirps %>%
  mutate(year_month = floor_date(daily_date_chirps, "month")) %>%
  group_by(year_month) %>%
  summarize(monthly_chirps_rainfall = sum(Rainfall_chirps, na.rm = TRUE))



