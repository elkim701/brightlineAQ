# ------------------------------------------------------------------------------
# Title: Data Cleaning for Raw BC Clarity Data
# Author: Eleanor Kim
# Last Updated: 2 August 2025
# Description: This script performs data cleaning for raw Clarity data,
#              including the addition of device names and neighborhood
#              classifications, and preparing the data for further analysis.
# ------------------------------------------------------------------------------
# ----------------------------------------------------
# Load packages
# ----------------------------------------------------
library(dplyr)
library(lubridate)

# ----------------------------------------------------
# Set file paths
# ----------------------------------------------------
dir <- "/Users/johnkim/Desktop/brightlineAQ/"
# dir <- "C:/Users/Eleanor-Kim/OneDrive - MMC/Documents/brightlineAQ/"

# ----------------------------------------------------
# Import raw + old cleaned data
# ----------------------------------------------------
new_data_raw <- read.csv(paste0(dir,"RAW/BC_7jul25_22aug25.csv"))
old_data_raw <- read.csv(paste0(dir,"CLEAN/cleanBC_12dec24_7jul25.csv"))[,2:20]  # remove index col

# ----------------------------------------------------
# Define device metadata (Device_Name + Neighborhood)
# ----------------------------------------------------
device_lookup <- tribble(
  ~alt.ID,     ~Device_Name,                          ~Neighborhood,
  "DVCKP7679", "Carroll Ave. & 3rd St. (BC)",         "BVHP",
  "DPANQ2934", "Air District Reference Site",         "Potrero Hill",
  "DNHZP3586", "9th St. and Brannan St. (BC)",        "SoMa",
  "DGVIU8498", "Kirkwood Ave. & Earl St. (BC)",       "BVHP",
  "DBUFQ1648", "Newcomb Ave. & 3rd St. (BC)",         "Potrero Hill",
  "DEVXA9067", "Hyde St. & Golden Gate Ave. (BC)",    "Tenderloin",
  "DCDBK0901", "Howard St. & 6th St. (BC)",           "SoMa",
  "DSARJ4044", "Eddy St. & Jones St. (BC)",           "Tenderloin",
  "DAJXS2653", "Clay St. & Kearny St. (BC)",          "Chinatown",
  "DFPAI0612", "Broadway St. & Stockton St. (BC)",    "Chinatown",
  "DSUSD5545", "Broadway St. & Stockton St. (BC)",    "Chinatown"
)

# ----------------------------------------------------
# Clean old data (attach Device_Name + Neighborhood)
# ----------------------------------------------------
old_data <- old_data_raw %>%
  left_join(device_lookup, by = "alt.ID") %>%
  mutate(
    Device_Name  = coalesce(Device_Name.x, Device_Name.y, Device_Name),
    Neighborhood = coalesce(Neighborhood.x, Neighborhood.y, Neighborhood)
  ) %>%
  select(-Device_Name.x, -Device_Name.y, -Neighborhood.x, -Neighborhood.y)

# ----------------------------------------------------
# Select + rename relevant columns in new data
# ----------------------------------------------------
new_data <- new_data_raw %>%
  select(
    alt.ID           = datasourceId,
    ID               = locationId,
    Latitude         = latitude,
    Longitude        = longitude,
    BC_AllSources_Hour_Calibrated      = bc1HourMean.value,
    BC_Biomass_Hour_Calibrated         = bcBiomass1HourMean.value,
    BC_FossilFuel_Hour_Calibrated      = bcFossil1HourMean.value,
    BC_SpectralB1_Hour_Calibrated      = bcB1HourMean.value,
    BC_SpectralG1_Hour_Calibrated      = bcG1HourMean.value,
    BC_SpectralIR1_Hour_Calibrated     = bcIR1HourMean.value,
    BC_SpectralR1_Hour_Calibrated      = bcR1HourMean.value,
    BC_SpectralUV1_Hour_Calibrated     = bcUV1HourMean.value,
    Datetime                           = startOfPeriod,
    Temperature                        = temperatureInternal1HourMean.value,
    Humidity                           = relHumidInternal1HourMean.value,
    PM2.5_Hour_MassConc_Calibrated     = pm2_5ConcMass1HourMean.value,
    NO2_Hour_MassConc_Calibrated       = o2Conc1HourMean.value
  ) %>%
  mutate(
    Datetime = ymd_hms(Datetime, tz = Sys.timezone())
  ) %>%
  left_join(device_lookup, by = "alt.ID")

# ----------------------------------------------------
# Make sure datetime formats align
# ----------------------------------------------------
old_data <- old_data %>%
  mutate(Datetime = ymd_hms(Datetime, tz = Sys.timezone()))

# ----------------------------------------------------
# Combine old + new, drop duplicates
# ----------------------------------------------------
combined <- bind_rows(old_data, new_data) %>%
  distinct()

# ----------------------------------------------------
# Save updated dataset
# ----------------------------------------------------
write.csv(combined, paste0(dir,"CLEAN/cleanBC_use.csv"), row.names = FALSE)

