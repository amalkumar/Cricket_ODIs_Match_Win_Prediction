setwd("D:/Study_Material/DataAnalytics/Cricket/ODIs/1 - Data_Creation/DC_Code/")

options(warn=-1)

library(reshape2)
library(dplyr)
library(sqldf)

inputFileNameCountryName <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/Common_Reference_Data/venue_country_mapping.csv"
df_venue_country <- read.csv(file = inputFileNameCountryName,as.is=TRUE,header = TRUE)


inputFileNameTeamWinRecords <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/1 - Data_Creation/DC_Output/DC-1-TeamWinningRecord.csv"
df_TeamWinRecords <- read.csv(file = inputFileNameTeamWinRecords,as.is=TRUE,header = TRUE)



for (i in 1:nrow(df_TeamWinRecords)){
  vn <- df_TeamWinRecords[i,"Venue"]
  if (vn %in% df_venue_country$Venue){
    Venue <- (df_venue_country[df_venue_country$Venue==vn,])$Country_Name
    df_TeamWinRecords$Country_Name[i] <- Venue
  }
  
}
print("Completed")


result <- df_TeamWinRecords[,c("Match_Number","Match_Date",	"City",	"Country_Name",	"Venue",	"Team1",	"Team2",	"Toss_Winner",	"Toss_Decision",	"Winner",	"Looser")]

outputFileName <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/1 - Data_Creation/DC_Output/DC-2-TeamWinningRecord_UpdatedCountry.csv"
write.csv(result,file = outputFileName,row.names = FALSE)


