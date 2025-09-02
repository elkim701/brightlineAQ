# ------------------------------------------------------------------------------
# Title: Data Cleaning Traffic Data
# Author: Eleanor Kim
# Last Updated: 10 June 2025
# Description: This script performs data cleaning for raw Clarity data,
#              including the addition of device names and neighborhood
#              classifications, and preparing the data for further analysis.
# ------------------------------------------------------------------------------
# Load required packages
library(dplyr)
library(tidyr)
library(jsonlite)
library(purrr)
library(lubridate)

# Set-up
dir <- "/Users/johnkim/Desktop/brightlineAQ/"
# dir <- "C:/Users/Eleanor-Kim/OneDrive - MMC/Documents/Projects/Brightline/brightlineAQ/"

# Read in files from Brightline
## Victor mapped closest street segments to sensors
seg_sensor <- read.csv(paste0(dir,"RAW/closest_segments_results.csv")) # 60 x 8 segment-sensor
## Brightline clean AQ data
aq <- read.csv(paste0(dir,"CLEAN/cleanBCPM_19aug20_30apr25.csv")) # 740 x 7 segment

# Read in raw files from SFCTA
avg_speed_block <- read.csv(paste0(dir,"RAW/speed_by_hourblock.csv")) # 11575 x 4 segment-month-time of day
avg_speed_hour <- read.csv(paste0(dir,"RAW/filtered_file.csv")) # 68686 x 11 segment-month-hour
seg_loc <- read.csv(paste0(dir,"RAW/sfcta_cmp_exp_segments - sfcta_cmp_exp_segments.csv")) # 740 x 7 segment


# -----------------------
# Define date range (Dec 2024 – Mar 2025)
# -----------------------
start_date <- as.Date("2024-12-01")
end_date   <- as.Date("2025-03-31")

# -----------------------
# Scrub AQ data
# -----------------------
aq_filtered <- aq %>%
  mutate(Datetime = ymd_hms(Datetime)) %>%
  filter(
    Datetime >= start_date & Datetime <= end_date,
    BC_AllSources_Hour_Calibrated > 0
  ) %>%
  select(Datetime, ID, Device_Name, Latitude, Longitude, BC_AllSources_Hour_Calibrated) %>%
  mutate(
    Hour  = hour(Datetime),
    Month = month(Datetime),
    Year  = year(Datetime)
  ) %>%
  group_by(Year, Month, Hour, ID, Device_Name, Latitude, Longitude) %>%
  summarize(Average_BC = mean(BC_AllSources_Hour_Calibrated, na.rm = TRUE), .groups = "drop") %>%
  rename_with(tolower)

# -----------------------
# Function to extract coords from GeoJSON
# -----------------------
extract_coords <- function(geojson_str) {
  coords <- fromJSON(geojson_str)$coordinates
  if (is.matrix(coords)) {
    coords <- split(coords, seq(nrow(coords)))
  }
  coords
}

# -----------------------
# Transform segment geometry
# -----------------------
seg_loc_long <- seg_loc %>%
  mutate(coords = map(geometry, extract_coords)) %>%
  unnest(coords) %>%
  mutate(
    longitude = map_dbl(coords, 1),
    latitude  = map_dbl(coords, 2)
  ) %>%
  group_by(cmp_segid) %>%
  mutate(point_index = row_number()) %>%
  ungroup() %>%
  select(cmp_segid, point_index, longitude, latitude, cmp_name, direction, length) %>%
  filter(latitude > 0)

# -----------------------
# Filter speed data for the same date range
# -----------------------
speed_filtered <- avg_speed_hour %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= start_date & date <= end_date)

# -----------------------
# Merge segments with speed data
# -----------------------
segments <- merge(seg_loc_long, speed_filtered, by = "cmp_segid", all = TRUE) %>%
  mutate(
    month = month(date),
    year  = year(date),
    hour  = period
  ) %>%
  select(-period, -date)

# -----------------------
# Merge with AQ data
# -----------------------
aq_segments <- merge(
  segments, 
  aq_filtered, 
  by = c("latitude","longitude","year","month","hour"), 
  all = TRUE
)

dim(aq_segments)
head(aq_segments)

# -----------------------
# Save  datasets
# -----------------------

# Dec 2024 - Mar 2025
write.csv(aq_segments, paste0(dir, "CLEAN/bc_sfcta_seg_dec24_mar25.csv"), row.names = FALSE)

# March only version
write.csv(aq_segments %>% filter(month == 3), paste0(dir, "CLEAN/bc_sfcta_seg_mar25.csv"), row.names = FALSE)
