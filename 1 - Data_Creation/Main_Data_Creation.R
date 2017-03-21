setwd("D:/Study_Material/DataAnalytics/Cricket/ODIs/1 - Data_Creation/DC_Code/")

options(warn=-1)

library(reshape2)
library(dplyr)
library(sqldf)

# Parse all csv files for ODI matches since 2005 and create Team winning records
source("1 - ParseCSV_CreateAllTeamWinningRecords.R")


# Update the ODI Matches Team winning records with Venue Country names
source("2 - Update_TeamAllTeamWinningRecordsWithVenueCountry.R")


# Parse all csv files for ODI matches since 2005 and create Batting Records
source("3 - ParseCSV_CreateBattingRecords.R")


# Parse all csv files for ODI matches since 2005 and create Bowling Records
source("4 - ParseCSV_CreateBowlingRecords.R")