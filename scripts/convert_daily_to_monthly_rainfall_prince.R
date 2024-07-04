# import Libraries
rm(list = ls())
library(openxlsx)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
#library(zoo)

# Notes for Fransistown Airport data
# obs 1960-01-01 to 2023-12-31.   1960-01-01 to 1990-12-31 nodata
# Prince data is monthly from 1948 to 2016 
# Cut data to 1992 to 2016

rain_obs <- read_csv("data/csv/Obs_rainfall_data/033-FRAN-FRANCISTOWN AIRPORT.csv")%>%
  rename(daily_date_obs =1, Rainfall_obs = 2)
head(rain_obs)
str(rain_obs$daily_date_obs)

# Convert the numeric date to character, then to Date format
rain_obs$daily_date_obs <- as.Date(as.character(rain_obs$daily_date_obs), format = "%Y%m%d")
str(rain_obs$daily_date_obs)


# Aggregate daily data rainfall to monthly data
monthly_obs_data <- rain_obs %>%
  mutate(year_month = floor_date(daily_date_obs, "month")) %>%
  group_by(year_month) %>%
  summarise(days_with_na_obs = sum(is.na(Rainfall_obs)),
            monthly_rainfall_obs = sum(Rainfall_obs, na.rm = TRUE)) %>%
  ungroup()



# Import Prince satellite data 
rain_prince <- read_csv("data/csv/csv_proc/FRANCISTOWN AIRPORT_prince.csv",skip = 1)%>%
  rename(year_month =1, Rainfall_prince = 2)

# Convert "No data" and set it to numeric to perform monthly computation
rain_prince$Rainfall_prince[rain_prince$Rainfall_prince == "No data"] <- NA
rain_prince$Rainfall_prince <- as.numeric(rain_prince$Rainfall_prince)

# Match the monthly obs data by adding a "01" for day at the end
rain_prince <- rain_prince %>%
  mutate(year_month_day = as.Date(paste0(year_month , "-01")))

# Join the obs and prince data
# Rename date_column in data2 to match daily_date_chirps in data1
names(monthly_obs_data)[1] <- "year_month_day"

obs_prince_joined <- monthly_obs_data %>%
  left_join(rain_prince, by = "year_month_day")

# Drop the Redundant columns
obs_prince_joined <- obs_prince_joined %>%
  select(-year_month, -days_with_na_obs)


# Filter data to include only rows within the specified date range 1992 to 2016
# obs 1960-01-01 to 2023-12-31.   1960-01-01 to 1990-12-31 nodata
# Prince data is monthly from 1948 to 2016 
# Cut data to 1992 to 2016

start_date <- "1992-01-01"
end_date <- "2016-12-01"

obs_prince_joined_filtered <- obs_prince_joined %>%
  filter(year_month_day >= start_date & year_month_day <= end_date)

# Remove NA values in the obs data
obs_prince_joined_filtered_clean<-obs_prince_joined_filtered[!is.na(obs_prince_joined_filtered$monthly_rainfall_obs),]
print(obs_prince_joined_filtered_clean)

#write.xlsx(obs_prince_joined_filtered_clean, "out/monthly_obs_prince_data_cleaned_fransistown_airpot.xlsx")

##### Perform Correlation analysis ######
# Perform correlation analysis on numeric columns
correlation_matrix <- cor(obs_prince_joined_filtered_clean[, sapply(obs_prince_joined_filtered_clean, is.numeric)], use = "complete.obs")
print(correlation_matrix)

#write.xlsx(as.data.frame(correlation_matrix), "out/correlation_matrix_Prince_fransistown_airpot.xlsx")

# Plot correlation scatter plot
ggplot(obs_prince_joined_filtered_clean, aes(x = monthly_rainfall_obs , y = Rainfall_prince)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red")

# Save the plot
ggsave("scatter_plot_with_line_graph_and_r_squared.png")
