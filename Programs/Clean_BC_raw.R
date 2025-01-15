# ------------------------------------------------------------------------------
# Title: Data Cleaning for Raw BC Clarity Data
# Author: Eleanor Kim
# Last Updated: 14 January 2025
# Description: This script performs data cleaning for raw Clarity data, 
#              including the addition of device names and neighborhood 
#              classifications, and preparing the data for further analysis.
# ------------------------------------------------------------------------------

# Load required packages
library(dplyr)
library(lubridate)

# Set-up
dir <- "/Users/johnkim/Desktop/brightlineAQ/"

# Read in raw files from Clarity
new_data0 <- read.csv(paste0(dir,"RAW/BC_12dec24_14jan25.csv"))
# old_data0 <- read.csv("BC_start_end.csv")

# Drop unnecessary columns and select relevant ones
data <- new_data0 %>%
  select(contains("sourceId") |
           contains("location") |
           (contains("bc") & contains("1HourMean.value")) |# which bc fields to keep?
           contains("start") | 
           contains("temperatureInternal1HourMean.value") | # any other atmospheric data to keep?
           contains("relHumidInternal1HourMean.value") |
           contains("pm2_5ConcMass1HourMean.value") | # which pm2.5 fields to keep?
           contains("o2Conc1HourMean.value")) # any other pollutant measures to keep?

# Check variable names
names(data)

# Convert to datetime standard and Pacific Time
data$startOfPeriod <- ymd_hms(data$startOfPeriod, tz = Sys.timezone())
#old_data0$Datetime <- ymd_hms(old_data0$Datetime, tz = Sys.timezone())

# Create a column for Neighborhood
data <- data %>%
  mutate(Neighborhood = case_when(
    datasourceId == "DVCKP7679" ~ "BVHP",
    datasourceId == "DPANQ2934" ~ "Potrero Hill",
    datasourceId == "DNHZP3586" ~ "SoMa"
  ))
    

# Create a column for Device Name
data <- data %>%
  mutate(Device_Name = case_when(
    datasourceId == "DVCKP7679" ~ "BVHP Foundation",
    datasourceId == "DPANQ2934" ~ "Air District Reference Site",
    datasourceId == "DNHZP3586" ~ "Fitness SF SOMA"
  ))

# Rename columns
names(data) = c('alt.ID','ID','Latitude','Longitude',
                'BC_AllSources_Hour_Calibrated','BC_Biomass_Hour_Calibrated','BC_FossilFuel_Hour_Calibrated',
                'BC_SpectralB1_Hour_Calibrated','BC_SpectralG1_Hour_Calibrated','BC_SpectralIR1_Hour_Calibrated',
                'BC_SpectralR1_Hour_Calibrated','BC_SpectralUV1_Hour_Calibrated',
                'Datetime', 'Temperature','Humidity','PM2.5_Hour_MassConc_Calibrated', 'NO2_Hour_MassConc_Calibrated', 
                'Neighborhood','Device_Name')

# Check data
head(data)

# Save to csv
write.csv(data,paste0(dir,"CLEAN/cleanBC_12dec24_14jan25.csv"))

