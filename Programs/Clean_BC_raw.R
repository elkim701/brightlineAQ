# ------------------------------------------------------------------------------
# Title: Data Cleaning for Raw BC Clarity Data
# Author: Eleanor Kim
# Last Updated: 21 March 2025
# Description: This script performs data cleaning for raw Clarity data,
#              including the addition of device names and neighborhood
#              classifications, and preparing the data for further analysis.
# ------------------------------------------------------------------------------

# Load required packages
library(dplyr)
library(lubridate)

# Set-up
dir <- "/Users/johnkim/Desktop/brightlineAQ/"
#dir <- "C:/Users/Eleanor-Kim/OneDrive - MMC/Documents/brightlineAQ/"


# Read in raw files from Clarity
new_data0 <- read.csv(paste0(dir,"RAW/BC_7jul25_22aug25.csv"))
old_data0 <- read.csv(paste0(dir,"CLEAN/cleanBC_12dec24_7jul25.csv"))[,2:20] # rm index col
max(old_data0$Datetime)

# clean up old data device names
old_data0 <- old_data0 %>%
  mutate(Device_Name = case_when(
    alt.ID == "DVCKP7679" ~ "Carroll Ave. & 3rd St. (BC)", # bvhp foundation
    alt.ID == "DPANQ2934" ~ "Air District Reference Site",
    alt.ID == "DNHZP3586" ~ "9th St. and Brannan St. (BC)", # fitness sf
    alt.ID == "DGVIU8498" ~ "Kirkwood Ave. & Earl St. (BC)",
    alt.ID == "DBUFQ1648" ~ "Newcomb Ave. & 3rd St. (BC)",
    alt.ID == "DEVXA9067" ~ "Hyde St. & Golden Gate Ave. (BC)",
    alt.ID == "DCDBK0901" ~ "Howard St. & 6th St. (BC)",
    alt.ID == "DSARJ4044" ~ "Eddy St. & Jones St. (BC)",
    alt.ID == "DAJXS2653" ~ "Clay St. & Kearny St. (BC)",
    alt.ID == "DFPAI0612" ~ "Broadway St. & Stockton St. (BC)",
    alt.ID == "DSUSD5545" ~ "Broadway St. & Stockton St. (BC)",
    
    TRUE ~ Device_Name
  ))
table(old_data0$Neighborhood)
old_data0 <- old_data0 %>%
  mutate(Neighborhood = case_when(
    alt.ID %in% c("DVCKP7679","DGVIU8498") ~ "BVHP",
    alt.ID %in% c("DPANQ2934", "DBUFQ1648") ~ "Potrero Hill",
    alt.ID %in% c("DNHZP3586", "DCDBK0901") ~ "SoMa",
    alt.ID %in% c("DSARJ4044" ,"DEVXA9067") ~ "Tenderloin",
    alt.ID %in% c("DAJXS2653","DFPAI0612","DSUSD5545" ) ~ "Chinatown",
    TRUE ~ Neighborhood
  ))

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
old_data0$Datetime <- ymd_hms(old_data0$Datetime, tz = Sys.timezone())

# Create a column for Neighborhood
data <- data %>%
  mutate(Neighborhood = case_when(
    datasourceId %in% c("DVCKP7679","DGVIU8498","DSUSD5545") ~ "BVHP",
    datasourceId %in% c("DPANQ2934", "DBUFQ1648") ~ "Potrero Hill",
    datasourceId %in% c("DNHZP3586", "DCDBK0901") ~ "SoMa",
    datasourceId %in% c("DSARJ4044" ,"DEVXA9067") ~ "Tenderloin",
    datasourceId %in% c("DAJXS2653","DFPAI0612" ) ~ "Chinatown",
  ))


# Create a column for Device Name
data <- data %>%
  mutate(Device_Name = case_when(
    datasourceId == "DVCKP7679" ~ "Carroll Ave. & 3rd St. (BC)", # bvhp foundation
    datasourceId == "DPANQ2934" ~ "Air District Reference Site",
    datasourceId == "DNHZP3586" ~ "9th St. and Brannan St. (BC)", # fitness sf
    datasourceId == "DGVIU8498" ~ "Kirkwood Ave. & Earl St. (BC)",
    datasourceId == "DBUFQ1648" ~ "Newcomb Ave. & 3rd St. (BC)",
    datasourceId == "DEVXA9067" ~ "Hyde St. & Golden Gate Ave. (BC)",
    datasourceId == "DCDBK0901" ~ "Howard St. & 6th St. (BC)",
    datasourceId == "DSARJ4044" ~ "Eddy St. & Jones St. (BC)",
    datasourceId == "DAJXS2653" ~ "Clay St. & Kearny St. (BC)",
    datasourceId == "DFPAI0612" ~ "Broadway St. & Stockton St. (BC)"
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

# combine with old
combined <- rbind(old_data0, data)
updated <- combined[!duplicated(combined), ] # drop dups if any

# Save to csv
write.csv(updated,paste0(dir,"CLEAN/cleanBC_use.csv"))
