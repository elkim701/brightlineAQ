# ------------------------------------------------------------------------------
# Title: Data Cleaning for Raw Pm2.5 Clarity Data (old extended network)
# Author: Eleanor Kim
# Last Updated: 2 March 2025
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
new_data0a <- read.csv(paste0(dir,"RAW/PM_1oct24_31dec24_JBW47CD1FA.csv"))
new_data0b <- read.csv(paste0(dir,"RAW/PM_1oct24_31dec24_A1LDYFNJ.csv")) # looks like b is just a subset of a
new_data0c <- read.csv(paste0(dir,"RAW/PM_1sep24_31dec24.csv"))

new_data0 <- read.csv(paste0(dir, "RAW/tmp.csv"))
old_data0 <- read.csv(paste0(dir,"CLEAN/cleanPM_19aug20_31dec24.csv"))[,2:12]

# Combine raw data
new_data0 <- rbind(new_data0a, new_data0b, new_data0c)
new_data1 <- new_data0[!duplicated(new_data0), ]
names(new_data1)
# Drop unnecessary columns and select relevant ones
data <- new_data1 %>%
  select(contains("sourceId") |
           contains("location") |
           contains("start") | 
           contains("temperatureInternal1HourMean.value") | 
           contains("relHumidInternal1HourMean.value") |
           contains("pm2_5ConcMass1HourMean.value") | 
           contains("no2Conc1HourMean.value")) 
# Check variable names
names(data)

# Convert to datetime standard and Pacific Time
data$startOfPeriod <- ymd_hms(data$startOfPeriod, tz = Sys.timezone())
#old_data0$Datetime <- ymd_hms(old_data0$Datetime, tz = Sys.timezone())

# Create a column for Neighborhood
data <- data %>%
  mutate(Neighborhood = case_when(
    sourceId %in% c("A1LDYFNJ", "A924XW2N") ~ "Chinatown",
    sourceId %in% c("AVRWKTCY", "AQWPK7D9","AHSXGFGQ") ~ "BVHP",
    sourceId %in% c("AYHT7CF7") ~ "SoMa",
    sourceId %in% c("AWL3N4NT") ~ "Tenderloin",
    sourceId %in% c("ALZ4PSJB") ~ "Potrero Hill"))

# Create a column for Device Name
data <- data %>%
  mutate(Device_Name = case_when(
    sourceId == "A1LDYFNJ" ~ "CAA Building",
    sourceId == "AVRWKTCY" ~ "Our Planet Recycling",
    sourceId == "AYHT7CF7" ~ "Fitness SF",
    sourceId == "AQWPK7D9" ~ "BVHP Foundation",
    sourceId == "AWL3N4NT" ~ "Turk & Jones",
    sourceId == "AHSXGFGQ" ~ "Cesar Chavez and Bryant",
    sourceId == "A924XW2N" ~ "Stockton and Broadway",
    sourceId == "ALZ4PSJB" ~ "BAAQMD Co-Location",
  ))

# Rename columns
names(data) <- c('alt.ID','ID','Latitude','Longitude',
                'Datetime', 'Temperature','Humidity','PM2.5_Hour_MassConc_Calibrated', 'NO2_Hour_MassConc_Calibrated', 
                'Neighborhood','Device_Name')

# Combine old and new
data_combined <- rbind(data, old_data0)
head(data_combined)
data_combined2 <- data_combined[!duplicated(data_combined),]

# Save to csv
write.csv(data_combined,paste0(dir,"CLEAN/cleanPM_19aug20_31dec24.csv"))
