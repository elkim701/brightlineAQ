# ------------------------------------------------------------------------------
# Title: Calculate Geo Traffic Metrics 
# Author: Eleanor Kim
# Last Updated: 24 Sept 2025
# Description: This script calculates the average traffic metric at a specified point 
#              (AQ sensor) and hour for a fixed radius for a "typical" traffic day
#              of a fixed month.
# ------------------------------------------------------------------------------
# Load required packages
library(dplyr)
library(tidyr)
library(jsonlite)
library(purrr)
library(lubridate)
library(geosphere)

# Set-up
dir <- "/Users/johnkim/Desktop/brightlineAQ/"

# Read in files
sensor_locs <- read.csv(paste0(dir,"CLEAN/Brightline_AQ_Sensor_Locs.csv"))[,-c(1:2)] # 10 sensors, 4 vars
traffic_locs <- read.csv(paste0(dir,"CLEAN/SF_Segment_Traffic.csv")) # 3225632 points, 17 vars
aq <- read.csv(paste0(dir,"CLEAN/cleanBC_use.csv"))

# Review parameters in traffic data
table(traffic_locs$month) # Dec 2024 - Mar 2025
table(traffic_locs$hour) # 24 hours
length(unique(traffic_locs$cmp_name)) # 190 traffic segments


# Define parameter lists
radii <- c(100, 200, 300)
months <- c(12,1,2,3)
hours <- (0:23)
cont_traffic_vars <- c("avg_speed","tti80","base_speed","spd_diff","pct_diff","spd_diff")


# Function to compute mean traffic metric
calc_means <- function(df, lat, lon, radius_m, target_month, target_hour, vars) {
  df %>%
    filter(month == target_month, hour == target_hour) %>%
    mutate(dist = distHaversine(cbind(longitude, latitude),
                                c(lon, lat))) %>%
    filter(dist <= radius_m) %>%
    summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)))
}

# Expand all parameter combinations
param_grid <- expand_grid(
  sensor_locs,
  radius_m = radii,
  month = months,
  hour = hours
)

# Apply function rowwise and unnest
results <- param_grid %>%
  rowwise() %>%
  mutate(
    stats = list(calc_means(traffic_locs, latitude, longitude, radius_m, month, hour, cont_traffic_vars))
  ) %>%
  ungroup() %>%
  unnest_wider(stats)
names(aq)

# Prep AQ data to relevate averages
aq_filtered <- aq %>% filter(Datetime >= '2024-12-01' & Datetime <= '2025-03-31' & Device_Name %in% sensor_locs$device_name,BC_AllSources_Hour_Calibrated>0 ) %>% 
  mutate(Datetime = ymd_hms(Datetime),
         month = month(Datetime),
         hour  = hour(Datetime),
         wday  = wday(Datetime)) %>%
  filter(wday %in% c(3, 4, 5)) %>% # Filter Tues, Wed, Thurs
  select(Datetime, Device_Name, month, hour, Temperature, Humidity, BC_AllSources_Hour_Calibrated, `PM2.5_Hour_MassConc_Calibrated`, NO2_Hour_MassConc_Calibrated) %>%
  group_by(Device_Name, month, hour) %>%
  summarise(
    avg_Temperature = mean(Temperature, na.rm = TRUE)*9/5+32,
    avg_Humidity = mean(Humidity, na.rm = TRUE),
    avg_BC = mean(BC_AllSources_Hour_Calibrated, na.rm = TRUE)/1000,
    avg_PM25 = mean(PM2.5_Hour_MassConc_Calibrated, na.rm = TRUE),
    avg_NO2 = mean(NO2_Hour_MassConc_Calibrated, na.rm = TRUE),
    .groups = "drop"
  )

# Merge with AQ data
merged <- results %>% left_join(aq_filtered, by = c("device_name" = "Device_Name", "month","hour"))

# Save
write.csv(merged, paste0(dir,"/CLEAN/Agg_Traffic_AQ.csv"))
