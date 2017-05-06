setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/2 - Model_Creation/MC_Code/")

options(warn=-1)

library(reshape2)
library(dplyr)
library(sqldf)



GetTeamWinRatioOnDateAtVenueCountry <- function(teamName,onDate,countryName){
  
  inputFileName <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/1 - Data_Creation/DC_Output/DC-1-TeamWinningRecord.csv"
  
  columnDataType <- c("integer","Date","character","character","character","character","character","character","character","character")
  
  df <- read.csv(file = inputFileName,as.is=TRUE,header = TRUE,colClasses = columnDataType)
  
  df$Match_Date <- as.Date(df$Match_Date,"%m/%d/%Y")
  
  #print("debug 1")
  #print(onDate)
  #print(teamName)
  #print(countryName)
  df_team_records <- df %>% filter(Match_Date<onDate,Team1==teamName | Team2==teamName,Country_Name==countryName)
  #print("debug 2")
  Total_Matches <- df_team_records %>% nrow()
  #####print(Total_Matches)
  
  if (Total_Matches==0){
    Won <- 0
    ######print(Won)
    
    Lost <- 0
    ######print(Lost)
    
    Winning_Ratio <- 0
    ######print(Winning_Ratio)
    
  } else {
    Won <- df_team_records %>% filter(Winner==teamName) %>% nrow()
    ######print(Won)
    
    Lost <- df_team_records %>% filter(Looser==teamName) %>% nrow()
    ######print(Lost)
    
    Winning_Ratio <- ifelse(is.infinite((Won/Total_Matches)*100),0,(Won/Total_Matches)*100)
    ######print(Winning_Ratio)
    
  }
  
  
  
  result <- data.frame(Date = onDate,Team = teamName,Total_Matches_Played = Total_Matches,Won = Won,Lost = Lost,Winning_Ratio = Winning_Ratio)
  
  return(result)
  
}



GetTeamWinRatioOnDateAtVenueCountry_Last5Matches <- function(teamName,onDate,countryName){
  
  inputFileName <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/1 - Data_Creation/DC_Output/DC-1-TeamWinningRecord.csv"
  
  columnDataType <- c("integer","Date","character","character","character","character","character","character","character","character")
  
  df <- read.csv(file = inputFileName,as.is=TRUE,header = TRUE,colClasses = columnDataType)
  
  df$Match_Date <- as.Date(df$Match_Date,"%m/%d/%Y")
  
  df_team_records <- df %>% filter(Match_Date<onDate,Team1==teamName | Team2==teamName,Country_Name==countryName)
  
  #Total_Matches <- df_team_records %>% nrow()
  #####print(Total_Matches)
  
  if (nrow(df_team_records)==0){
    Total_Matches <- 0
    
    Won <- 0
    ######print(Won)
    
    Lost <- 0
    ######print(Lost)
    
    Winning_Ratio <- 0
    ######print(Winning_Ratio)
    
  } else {
    
    df_team_records <- df_team_records[order(df_team_records$Match_Date, decreasing = TRUE),]
    
    ####print(nrow(df_team_records))
    ####print("--2")
    ####print(head(df_team_records,2))
    
    df_team_records$Match_Seq <- 1:nrow(df_team_records)
    
    ####print("--3")
    
    noOfMatchesToCalc <- ifelse(nrow(df_team_records)<5,nrow(df_team_records),5)
    
    ####print("--4")
    
    df_team_records <- df_team_records %>% filter(Match_Seq<=noOfMatchesToCalc)
    
    ####print("--5")
    
    
    Total_Matches <- df_team_records %>% nrow()
    
    
    Won <- df_team_records %>% filter(Winner==teamName) %>% nrow()
    ######print(Won)
    
    Lost <- df_team_records %>% filter(Looser==teamName) %>% nrow()
    ######print(Lost)
    
    Winning_Ratio <- ifelse(is.infinite((Won/Total_Matches)*100),0,(Won/Total_Matches)*100)
    ######print(Winning_Ratio)
    
  }
  
  
  
  result <- data.frame(Date = onDate,Team = teamName,Total_Matches_Played = Total_Matches,Won = Won,Lost = Lost,Winning_Ratio = Winning_Ratio)
  
  return(result)
  
}