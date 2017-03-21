setwd("D:/Study_Material/DataAnalytics/Cricket/ODIs/2 - Model_Creation/MC_Code/")

options(warn=-1)

library(reshape2)
library(dplyr)
library(sqldf)



GetTeamWinningRecordsOnDate <- function(teamName,onDate){
  
  inputFileName <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/1 - Data_Creation/DC_Output/DC-1-TeamWinningRecord.csv"
  
  columnDataType <- c("integer","character","Date","character","character","character","character","character","character","character","character")
  
  df <- read.csv(file = inputFileName,as.is=TRUE,header = TRUE,colClasses = columnDataType)
  
  df_team_records <- df %>% filter(Match_Date<onDate,Team1==teamName | Team2==teamName)
  
  Total_Matches <- df_team_records %>% nrow()
  ##print(Total_Matches)
  
  if (Total_Matches==0){
    Won <- 0
    ###print(Won)
    
    Lost <- 0
    ###print(Lost)
    
    Winning_Ratio <- 0
    ###print(Winning_Ratio)
    
  } else {
    Won <- df_team_records %>% filter(Winner==teamName) %>% nrow()
    ###print(Won)
    
    Lost <- df_team_records %>% filter(Looser==teamName) %>% nrow()
    ###print(Lost)
    
    Winning_Ratio <- ifelse(is.infinite((Won/Total_Matches)*100),0,(Won/Total_Matches)*100)
    ###print(Winning_Ratio)
    
  }
  
  
  
  result <- data.frame(Date = onDate,Team = teamName,Total_Matches_Played = Total_Matches,Won = Won,Lost = Lost,Winning_Ratio = Winning_Ratio)
  
  return(result)

}




GetTeamWinningRecordsOnDate_Last5Matches <- function(teamName,onDate){
  
  inputFileName <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/1 - Data_Creation/DC_Output/DC-1-TeamWinningRecord.csv"
  
  columnDataType <- c("integer","character","Date","character","character","character","character","character","character","character","character")
  
  df <- read.csv(file = inputFileName,as.is=TRUE,header = TRUE,colClasses = columnDataType)
  
  #print("---Start")
  #print(paste("Team Name : ",teamName," :::onDate : ",onDate,sep = ""))
  df_team_records <- df %>% filter(Match_Date<onDate,Team1==teamName | Team2==teamName)
  
  if (nrow(df_team_records)==0){
    Total_Matches <- 0
    #print(Total_Matches)
    
    Won <- 0
    #print(Won)
    
    Lost <- 0
    #print(Lost)
    
    Winning_Ratio <- 0
    #print(Winning_Ratio)
    
  } else {
    
    #print(nrow(df_team_records))
    #print(head(df_team_records,2))
    #print("--1")
    
    df_team_records <- df_team_records[order(df_team_records$Match_Date, decreasing = TRUE),]
    
    #print(nrow(df_team_records))
    #print("--2")
    #print(head(df_team_records,2))
    
    df_team_records$Match_Seq <- 1:nrow(df_team_records)
    
    #print("--3")
    
    noOfMatchesToCalc <- ifelse(nrow(df_team_records)<5,nrow(df_team_records),5)
    
    #print("--4")
    
    df_team_records <- df_team_records %>% filter(Match_Seq<=noOfMatchesToCalc)
    
    #print("--5")
    
    
    
    Total_Matches <- df_team_records %>% nrow()
    #print(Total_Matches)
    
    Won <- df_team_records %>% filter(Winner==teamName) %>% nrow()
    #print(Won)
    
    Lost <- df_team_records %>% filter(Looser==teamName) %>% nrow()
    #print(Lost)
    
    Winning_Ratio <- ifelse(is.infinite((Won/Total_Matches)*100),0,(Won/Total_Matches)*100)
    #print(Winning_Ratio)
    
  }
  
  
  
  
  result <- data.frame(Date = onDate,Team = teamName,Total_Matches_Played = Total_Matches,Won = Won,Lost = Lost,Winning_Ratio = Winning_Ratio)
  
  return(result)
  
}