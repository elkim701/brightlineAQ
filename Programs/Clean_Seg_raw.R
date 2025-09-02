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
dir <- "C:/Users/Eleanor-Kim/OneDrive - MMC/Documents/Projects/Brightline/brightlineAQ/"


# Read in raw files from Clarity
seg_sensor <- read.csv(paste0(dir,"RAW/closest_segments_results.csv")) # 60 x 8 segment-sensor
avg_speed_block <- read.csv(paste0(dir,"RAW/speed_by_hourblock.csv")) # 11575 x 4 segment-month-time of day
avg_speed_hour <- read.csv(paste0(dir,"RAW/filtered_file.csv")) # 68686 x 11 segment-month-hour
seg_loc <- read.csv(paste0(dir,"RAW/sfcta_cmp_exp_segments - sfcta_cmp_exp_segments.csv")) # 740 x 7 segment
aq <- read.csv(paste0(dir,"CLEAN/cleanBCPM_19aug20_30apr25.csv")) # 740 x 7 segment

# scrub aq data
aq_filtered <- aq %>%
  filter(grepl("2025-03", Datetime) & BC_AllSources_Hour_Calibrated>0) %>% # March analysis only
  select(Datetime, ID, Device_Name, Latitude, Longitude, BC_AllSources_Hour_Calibrated) %>%
  mutate(
    Datetime = ymd_hms(Datetime),  # Convert to datetime format
    Hour = hour(Datetime),          # Extract hour
    Month = month(Datetime),        # Extract month
    Year = year(Datetime)           # Extract year
  ) %>%
  group_by(Year, Month, Hour, ID, Device_Name, Latitude, Longitude) %>%
  summarize(Average_BC = mean(BC_AllSources_Hour_Calibrated, na.rm = TRUE), .groups = 'drop')
names(aq_filtered) = tolower(names(aq_filtered))


# Function to extract coords in appropriate format
extract_coords <- function(geojson_str) {
  coords <- fromJSON(geojson_str)$coordinates
  # If it's a matrix (LineString), wrap it in a list of rows
  if (is.matrix(coords)) {
    coords <- split(coords, seq(nrow(coords)))
  }
  return(coords)}

# Apply transformation
seg_loc_long <- seg_loc %>%
  mutate(coords = map(geometry, extract_coords)) %>%
  unnest(coords) %>%
  mutate(
    longitude = map_dbl(coords, 1),
    latitude = map_dbl(coords, 2)
  ) %>%
  group_by(cmp_segid) %>%
  mutate(point_index = row_number()) %>%
  ungroup() %>%
  select(cmp_segid, point_index, longitude, latitude, cmp_name, direction, length) %>% filter(latitude>0)

# Merge geo segments with speed data
segments = merge(seg_loc_long, avg_speed_hour %>% filter(date == "2025-03-01"), by = "cmp_segid", all.x = TRUE, all.y = TRUE)
segments2 <- segments %>% mutate(month = 3, hour = period) %>% select(-period, -date)

# Merge segments with air quality# Merge segments with air qualselect()ity
aq_segments <- merge(segments2, aq_filtered, by = c('latitude','longitude', 'year', 'month', 'hour'), all=TRUE)

write.csv(aq_segments,paste0(dir,"CLEAN/march_bc_sfcta_seg.csv"))
