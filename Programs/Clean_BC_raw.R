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


# Read in raw files from Clarity
new_data0 <- read.csv(paste0(dir,"RAW/BC_21mar25_30apr25.csv"))
old_data0 <- read.csv(paste0(dir,"CLEAN/cleanBC_12dec24_21mar25.csv"))[,2:20] # rm index col

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
    datasourceId %in% c("DVCKP7679","DGVIU8498") ~ "BVHP",
    datasourceId %in% c("DPANQ2934", "DBUFQ1648") ~ "Potrero Hill",
    datasourceId %in% c("DNHZP3586", "DCDBK0901") ~ "SoMa",
    datasourceId %in% c("DSARJ4044" ,"DEVXA9067") ~ "Tenderloin",
    datasourceId %in% c("DAJXS2653","DFPAI0612" ) ~ "Chinatown",
  ))
    

# Create a column for Device Name
data <- data %>%
  mutate(Device_Name = case_when(
    datasourceId == "DVCKP7679" ~ "BVHP Foundation",
    datasourceId == "DPANQ2934" ~ "Air District Reference Site",
    datasourceId == "DNHZP3586" ~ "Fitness SF SOMA",
    datasourceId == "DGVIU8498" ~ "Wu Yee Kirkwood",
    datasourceId == "DBUFQ1648" ~ "Joseph Lee Rec Center",
    datasourceId == "DEVXA9067" ~ "126 Hyde St",
    datasourceId == "DCDBK0901" ~ "United Playaz",
    datasourceId == "DSARJ4044" ~ "Drake Hotel",
    datasourceId == "DAJXS2653" ~ "665 Clay St",
    datasourceId == "DFPAI0612" ~ "Bayside Elderly"
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
write.csv(updated,paste0(dir,"CLEAN/cleanBC_12dec24_21mar25.csv"))


# combine with old PM data
old_dataPM <- read.csv(paste0(dir,"CLEAN/cleanPM_19aug20_31dec24.csv"))[,2:12] # rm index col
dataPM <- old_dataPM %>%
  mutate(
    BC_AllSources_Hour_Calibrated = NA,
    BC_Biomass_Hour_Calibrated = NA,
    BC_FossilFuel_Hour_Calibrated = NA,
    BC_SpectralB1_Hour_Calibrated = NA,
    BC_SpectralG1_Hour_Calibrated = NA,
    BC_SpectralIR1_Hour_Calibrated = NA,
    BC_SpectralR1_Hour_Calibrated = NA,
    BC_SpectralUV1_Hour_Calibrated = NA
  ) %>% select(names(updated))
dataPM$Datetime <- ymd_hms(dataPM$Datetime, tz = Sys.timezone())

combined2 <- rbind(dataPM, updated)
updated2 <- combined2[!duplicated(combined2), ]
updated2$Datetime <- ymd_hms(updated2$Datetime, tz = Sys.timezone())
max(updated2$Datetime)
write.csv(updated2,paste0(dir,"CLEAN/cleanBCPM_19aug20_30apr25.csv"))
tmp <- read.csv(paste0(dir,"CLEAN/cleanBCPM_19aug20_21mar25.csv"))
max(tmp$Datetime)
