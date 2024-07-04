# import Libraries
rm(list = ls())
library(openxlsx)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
#library(zoo)

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
            monthly_rainfall_chirps = sum(Rainfall_obs, na.rm = TRUE)) %>%
  ungroup()



# Import CHIRPS satellite data
rain_chirps <- read_csv("data/csv/csv_proc/FRANCISTOWN AIRPORT.csv",skip = 1)%>%
  rename(daily_date_chirps =1, Rainfall_chirps = 2)

# Convert "No data" and set it to numeric to perform monthly computation
rain_chirps$Rainfall_chirps[rain_chirps$Rainfall_chirps == "No data"] <- NA
rain_chirps$Rainfall_chirps <- as.numeric(rain_chirps$Rainfall_chirps)


# Aggregate daily data rainfall to monthly data
monthly_chirps_data <- rain_chirps %>%
  mutate(year_month = floor_date(daily_date_chirps, "month")) %>%
  group_by(year_month) %>%
  summarise(days_with_na_chirps = sum(is.na(Rainfall_chirps)),
            monthly_rainfall_chirps = sum(Rainfall_chirps, na.rm = TRUE))
  
# Join the obs and chirps data
# Rename date_column in data2 to match daily_date_chirps in data1
#names(rain_obs)[1] <- "daily_date_chirps"


# Rename date_column in data2 to match daily_date_chirps in data1
names(rain_obs)[1] <- "daily_date_chirps"

obs_chirp_joined <- rain_obs %>%
  left_join(rain_chirps, by = "daily_date_chirps")

# Option without renaming
#obs_chirps_joined <- rain_obs %>%
#  left_join(rain_chirps, by = c("daily_date_obs"="daily_date_chirps"))

######## Prepare data for correlation -Remove all columns with missing data from obs col ########
obs_chirps_joined_clean<-obs_chirp_joined[!is.na(obs_chirp_joined$Rainfall_obs),]
print(obs_chirps_joined_clean)

# Calculate monthly obs and chirps rainfall

monthly_obs_chirps_data_cleaned <- obs_chirps_joined_clean %>%
  mutate(year_month = floor_date(daily_date_chirps, "month"))%>%
  group_by(year_month) %>%
  summarise(monthly_rainfall_obs = sum(Rainfall_obs, na.rm = TRUE),
            monthly_rainfall_chirps = sum(Rainfall_chirps, na.rm = TRUE))

# write.xlsx(monthly_obs_chirps_data_cleaned, "monthly_obs_chirps_data_cleaned.xlsx")

##### Perform Correlation analysis ######
# Perform correlation analysis on numeric columns
correlation_matrix <- cor(monthly_obs_chirps_data_cleaned[, sapply(monthly_obs_chirps_data_cleaned, is.numeric)], use = "complete.obs")
print(correlation_matrix)
#write.xlsx(as.data.frame(correlation_matrix), "out/correlation_matrix_Francistown_Airport.xlsx")


# Plot correlation scatter plot
ggplot(monthly_obs_chirps_data_cleaned, aes(x = monthly_rainfall_obs , y = monthly_rainfall_chirps)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red")

# Save the plot
ggsave("scatter_plot_with_line_graph_and_r_squared.png")
