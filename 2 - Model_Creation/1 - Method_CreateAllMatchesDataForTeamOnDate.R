setwd("D:/Study_Material/DataAnalytics/Cricket/ODIs/2 - Model_Creation/MC_Code/")

options(warn=-1)

library(reshape2)
library(dplyr)
library(sqldf)



CreateAllMatchesDataForTeamOnDate <- function(teamName,OnDate){
  
  print(teamName)
  print(OnDate)
  
  inputFileName <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/1 - Data_Creation/DC_Output/DC-2-TeamWinningRecord_UpdatedCountry.csv"
  df_All_Team_Data <- read.csv(file = inputFileName,as.is=TRUE,header = TRUE)
  df_All_Team_Data$Match_Date <- as.Date(df_All_Team_Data$Match_Date)
  
  # Get All the records for teamName
  df_Team_Data <- df_All_Team_Data %>% filter(Team1==teamName|Team2==teamName,Match_Date<=OnDate)
  df_Team_Data$Year <- format(as.Date(as.Date(df_Team_Data$Match_Date), format="%d/%m/%Y"),"%Y")
  #print(nrow(df_Team_Data))
  #print(head(df_Team_Data,2))
  

  df_Team_Data <- df_Team_Data[,c("Year","City","Country_Name","Venue","Match_Date","Team1","Team2","Toss_Winner","Toss_Decision","Winner","Looser")]
  
  names(df_Team_Data) <- c("Year","City","Country_Name","Venue","Date","Team1","Team2","Toss","Decision","Winner","Looser")
  
  df_Team_Data <- df_Team_Data[order(df_Team_Data$Date, decreasing = TRUE),]
  
  
  
  
  # Creating Team and Date Specific Directory
  outputDirectoryPath <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/2 - Model_Creation/MC_OutPut_Data/"
  teamNameDirectoryonDate <- paste(outputDirectoryPath,"MC-",teamName,"/",sep = "")
  dir.create(path = teamNameDirectoryonDate,showWarnings = FALSE)
  
  fileName <- paste("MC-1-AllMatchesDataFor_",teamName,"_",OnDate,".csv",sep = "")
  outputFileName <- paste(teamNameDirectoryonDate,fileName,sep = "")
  print(nrow(df_Team_Data))
  print(head(df_Team_Data,2))
  print(outputFileName)
  write.csv(df_Team_Data,file = outputFileName,row.names = FALSE)
}

