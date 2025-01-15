# ------------------------------------------------------------------------------
# Title: Data Cleaning for Raw BC Clarity Data
# Author: Eleanor Kim
# Last Updated: 14 January 2025
# Description: This script performs data cleaning for raw Clarity data, 
#              including the addition of device names and neighborhood 
#              classifications, and preparing the data for further analysis.
# ------------------------------------------------------------------------------
file.choose()
# Load required packages
library(dplyr)
library(lubridate)

# Set-up
dir <- 
# Load new data (June-August 2024)
new_data0 <- read.csv("summer24_1.csv")

# Load last updated data
# old_data0 <- read.csv("Cleaned_Cumulative_524.csv")

# Drop unnecessary columns and select relevant ones
data <- new_data0 %>%
  select(
    contains("sourceId"),
    contains("location"),
    contains("start"),
    contains("bc"),  # Which bc fields to keep?
    contains("temperatureInternal1HourMean.value"),  # Any other atmospheric data to keep?
    contains("relHumidInternal1HourMean.value"),
    contains("pm2_5ConcMass1HourMean.value"),  # Which pm2.5 fields to keep?
    contains("o2Conc1HourMean.value")  # Any other pollutant measures to keep?
  )

# Check variable names
names(data)

# Convert to datetime standard and Pacific Time
data$startOfPeriod <- ymd_hms(data$startOfPeriod, tz = Sys.timezone())
old_data0$Datetime <- ymd_hms(old_data0$Datetime, tz = Sys.timezone())

# Create a column for Device Name
data <- data %>%
  mutate(Device_Name = case_when(
    (endsWith(sourceId, "AKTH9M8W") & startOfPeriod > '2021-05-19 13:00:00') ~ "S Park",
    (endsWith(sourceId, "AKTH9M8W") & startOfPeriod < '2021-05-19 13:00:00') ~ "Rizal St",
    (endsWith(sourceId, "ASB31LYW") & startOfPeriod > '2021-08-30 14:00:00' & startOfPeriod < '2022-08-17 00:00:00') ~ "Fitness SF",
    (endsWith(sourceId, "AYHT7CF7") & startOfPeriod > '2021-08-18 14:00:00') ~ "Fitness SF",
    (endsWith(sourceId, "ASB31LYW") & startOfPeriod < '2021-08-30 14:00:00') ~ "Richmond District",
    (endsWith(sourceId, "AHZCJNC1") & startOfPeriod < '2021-10-05 00:00:00') ~ "Camputee Press",
    (endsWith(sourceId, "AHZCJNC1") & startOfPeriod > '2021-10-05 00:00:00') ~ "Bryant & 7th",
    (endsWith(sourceId, "AX3F35TN") & startOfPeriod < '2022-02-17 00:00:00') ~ "Boeddeker Park",
    (endsWith(sourceId, "AG2TTZH0") & startOfPeriod > '2022-08-12 00:00:00') ~ "Boeddeker Park",
    (endsWith(sourceId, "AX3F35TN") & startOfPeriod > '2022-03-01 00:00:00') ~ "Remove",
    (endsWith(sourceId, "A5G4JM95") & startOfPeriod < '2022-06-29 00:00:00') ~ "BAAQMD Co-Location",
    (endsWith(sourceId, "ALZ4PSJB") & startOfPeriod > '2022-06-30 00:00:00') ~ "BAAQMD Co-Location",
    (endsWith(sourceId, "AKCJ9QPK") & startOfPeriod < '2022-08-11 00:00:00') ~ "Tenderloin Housing Clinic",
    (endsWith(sourceId, "AX7346WV") & startOfPeriod > '2022-08-12 00:00:00' & startOfPeriod < '2022-10-11 00:00:00') ~ "Tenderloin Housing Clinic",
    (endsWith(sourceId, "AX7346WV") & startOfPeriod > '2022-10-11 00:00:00') ~ "Remove",
    endsWith(sourceId, "AW59TBQR") ~ "Hart Hotel",
    endsWith(sourceId, "A9VHK6HM") ~ "Hart Hotel",
    (endsWith(sourceId, "A0JFW5Q8") & startOfPeriod < '2022-08-11 00:00:00') ~ "St. Claire Residence",
    (endsWith(sourceId, "AMNW46MQ") & startOfPeriod > '2022-08-12 00:00:00') ~ "St. Claire Residence",
    (endsWith(sourceId, "ARBVNYYS") & startOfPeriod < '2022-09-09 00:00:00') ~ "CAA Building",
    (endsWith(sourceId, "A1LDYFNJ") & startOfPeriod > '2022-09-10 00:00:00') ~ "CAA Building",
    (endsWith(sourceId, "ALR9Y4HY") & startOfPeriod < '2022-08-11 00:00:00') ~ "Macaulay Park",
    (endsWith(sourceId, "ABRRQN36") & startOfPeriod > '2022-08-12 00:00:00') ~ "Macaulay Park",
    (endsWith(sourceId, "AC95QG6X") & startOfPeriod < '2022-09-07 00:00:00') ~ "Bessie Carmichael Elementary",
    endsWith(sourceId, "A924XW2N") ~ "Stockton and Broadway",
    endsWith(sourceId, "AYVSW72T") ~ "Ping Yuen Center",
    endsWith(sourceId, "A73MGQG3") ~ "Mission & 9th",
    endsWith(sourceId, "ARRJWXYL") ~ "Market & 7th",
    endsWith(sourceId, "AZSLPLLN") ~ "Turk & Jones",
    endsWith(sourceId, "AWL3N4NT") ~ "Turk & Jones",
    endsWith(sourceId, "AKC3DRG9") ~ "Tenderloin Rec Center",
    endsWith(sourceId, "A42WWWH9") ~ "CYC Main Office",
    endsWith(sourceId, "AN3Q2PXB") ~ "Gene Friend Rec Center",
    endsWith(sourceId, "AVRWKTCY") ~ "Our Planet Recycling",
    (endsWith(sourceId, "AKVHMJYG") & startOfPeriod < '2022-08-03 00:00:00') ~ "Howard & 9th",
    (endsWith(sourceId, "ATWJ3V74") & startOfPeriod > '2022-08-03 15:00:00') ~ "Howard & 9th",
    endsWith(sourceId, "APQV6PFW") ~ "Florence Fang Community Farm",
    endsWith(sourceId, "AFT4PL9M") ~ "Wu Yee Children's Services",
    endsWith(sourceId, "A2BPKVSX") ~ "The Box Shop",
    endsWith(sourceId, "AQWPK7D9") ~ "BVHP Foundation",
    endsWith(sourceId, "A0QDSP1M") ~ "Cesar Chavez and Hampshire",
    endsWith(sourceId, "AHSXGFGQ") ~ "Cesar Chavez and Bryant",
    endsWith(sourceId, "A88H1PS0") ~ "Bessie Carmichael Elementary",
    endsWith(sourceId, "AVRWKTCY") ~ "Our Planet Recycling",
    endsWith(sourceId, "A6QPXPGM") ~ "Bryant & 7th"
  ))

# Create a column with Neighborhood
data <- data %>%
  mutate(Neighborhood = case_when(
    endsWith(Device_Name, "S Park") ~ "SoMa",
    endsWith(Device_Name, "Rizal St") ~ "SoMa",
    endsWith(Device_Name, "Fitness SF") ~ "SoMa",
    endsWith(Device_Name, "Richmond District") ~ "Richmond",
    endsWith(Device_Name, "Florence Fang Community Farm") ~ "BVHP",
    endsWith(Device_Name, "Wu Yee Children's Services") ~ "BVHP",
    endsWith(Device_Name, "BVHP Foundation") ~ "BVHP",
    endsWith(Device_Name, "The Box Shop") ~ "BVHP",
    endsWith(Device_Name, "Our Planet Recycling") ~ "BVHP",
    endsWith(Device_Name, "Cesar Chavez and Hampshire") ~ "BVHP",
    endsWith(Device_Name, "Cesar Chavez and Bryant") ~ "BVHP",
    endsWith(Device_Name, "Stockton and Broadway") ~ "Chinatown",
    endsWith(sourceId, "AHZCJNC1") ~ "SoMa",
    endsWith(Device_Name, "Boeddeker Park") ~ "Tenderloin",
    endsWith(Device_Name, "Tenderloin Housing Clinic") ~ "Tenderloin",
    endsWith(Device_Name, "Hart Hotel") ~ "SoMa",
    endsWith(Device_Name, "St. Claire
