setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/2 - Model_Creation/MC_Code/")

options(warn=-1)

library(reshape2)
library(dplyr)
library(sqldf)

inputFileName <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/1 - Data_Creation/DC_Output/DC-1-TeamWinningRecord.csv"
outputDirectoryPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/2 - Model_Creation/MC_Output/"

CreateAllMatchesDataForTeamOnDate <- function(teamName,OnDate,startDate){
  
  print(teamName)
  print(OnDate)
  
  
  df_All_Team_Data <- read.csv(file = inputFileName,as.is=TRUE,header = TRUE)
  df_All_Team_Data$Match_Date <- as.Date(df_All_Team_Data$Match_Date)
  

  # Get All the records for teamName
  df_Team_Data <- df_All_Team_Data %>% filter(Team1==teamName|Team2==teamName,Match_Date>=startDate,Match_Date<=OnDate)
  df_Team_Data$Year <- format(as.Date(as.Date(df_Team_Data$Match_Date), format="%d/%m/%Y"),"%Y")
  
  
  
  df_Team_Data <- df_Team_Data[,c("Match_Number","Year","Country_Name","Venue","Match_Date","Team1","Team2","Batting_First","Bowling_First","Winner","Looser")]
  
  names(df_Team_Data) <- c("Src_Match_No","Year","Country_Name","Venue","Date","Team1","Team2","Batting_First","Bowling_First","Winner","Looser")
  
  df_Team_Data <- df_Team_Data[order(df_Team_Data$Date, decreasing = TRUE),]
  
  
  # Creating Team and Date Specific Directory
  teamNameDirectoryonDate <- paste(outputDirectoryPath,"MC-",teamName,"/",sep = "")
  dir.create(path = teamNameDirectoryonDate,showWarnings = FALSE)
  
  fileName <- paste("MC-1-AllMatchesDataFor_",teamName,"_",OnDate,".csv",sep = "")
  outputFileName <- paste(teamNameDirectoryonDate,fileName,sep = "")
  print(nrow(df_Team_Data))
  print(head(df_Team_Data,2))
  print(outputFileName)
  write.csv(df_Team_Data,file = outputFileName,row.names = FALSE)
}

