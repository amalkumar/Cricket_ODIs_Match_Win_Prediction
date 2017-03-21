setwd("D:/Study_Material/DataAnalytics/Cricket/ODIs/3 - New_Match_Prediction/NM_Code/")



options(warn=-1)

library(reshape2)
library(dplyr)
library(yaml)

source("Method_GetTeamWinRatioOnDate.R")
source("Method_GetTeamWinRatioOnDate_vs_Opponent.R")
source("Method_GetTeamWinRatioOnDateAtVenueCountry.R")
source("Method_GetTeamBattingPerfScoreOnDate.R")
source("Method_GetTeamBattingPerfScoreOnDate_vs_Opponent.R")
source("Method_GetTeamBowlingPerfScoreOnDate.R")
source("Method_GetTeamBowlingPerfScoreOnDate_vs_Opponent.R")




CreateAggregatedDataForTeamOnDate <- function(teamName,onDate){
  
  inputDirectoryPath <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/3 - New_Match_Prediction/NM_OutPut_Data/"
  teamNameDirectoryonDate <- paste(inputDirectoryPath,"MC-",teamName,"/",sep = "")
  
  fileName <- paste("NM-1-NewMatchRecord_",teamName,".csv",sep = "")
  inputFileName <- paste(teamNameDirectoryonDate,fileName,sep = "")
  #print(teamNameDirectoryonDate)
  #print(inputFileName)
  
  
  if(!file.exists(inputFileName)){
    
    print("File Doesnt exists")
    stop()
    
  } else{
    #print(inputFileName)
    df <- read.csv(file = inputFileName,as.is=TRUE,header = TRUE)
    df$Match_Date <- as.Date(df$Match_Date)
    #print(nrow(df))
    df_onDate <- df %>% filter(Team1==teamName|Team2==teamName,Match_Date<=onDate)
    #print(nrow(df_onDate))
    
    count <- 0
    for (i in 1:nrow(df_onDate)) {
      count <- count + 1
      team1 <- df_onDate[i,"Team1"]
      team2 <- df_onDate[i,"Team2"]
      Match_Date <- df_onDate[i,"Match_Date"]
      countryName <- df_onDate[i,"Country_Name"]
      ####print(countryName)
      opponentTeam <- ifelse(team1==teamName,team2,team1)
      
      print(paste("row : ",count,sep = ""))
      print(paste("Team 1 : ",team1,"::::Team2:",team2,"::::Date:",Match_Date,"::::teamName:",teamName,"::::Opponent:",opponentTeam,sep = ""))
      
      
      df_onDate$TeamName_Team_Winning_Ratio_All[i] <- ((GetTeamWinningRecordsOnDate(teamName,Match_Date))$Winning_Ratio)
      #print("---------------------1")
      df_onDate$TeamName_Team_Winning_Ratio_Last5Matches[i] <- ((GetTeamWinningRecordsOnDate_Last5Matches(teamName,Match_Date))$Winning_Ratio)
      ##print("---------------------2")
      df_onDate$TeamName_Batting_Performance_all[i] <- ((TeamBattingPerformanceOnDate(teamName,Match_Date))$Batting_Performance)
      ##print("---------------------3")
      df_onDate$TeamName_Batting_Performance_Last5Matches[i] <- ((TeamBattingPerformanceOnDate_Last5Matches(teamName,Match_Date))$Batting_Performance)
      #print("---------------------4")
      df_onDate$TeamName_Bowling_Performance_all[i] <- ((TeamBowlingPerformanceOnDate(teamName,Match_Date))$Bowling_Performance)
      ##print("---------------------5")
      df_onDate$TeamName_Bowling_Performance_Last5Matches[i] <- ((TeamBowlingPerformanceOnDateLast5Matches(teamName,Match_Date))$Bowling_Performance)
      ##print("---------------------6")
      df_onDate$TeamName_Winning_Ratio_At_Venue[i] <- ((GetTeamWinRatioOnDateAtVenueCountry(teamName,Match_Date,countryName))$Winning_Ratio)
      df_onDate$TeamName_Winning_Ratio_At_Venue_Last5Matches[i] <- ((GetTeamWinRatioOnDateAtVenueCountry_Last5Matches(teamName,Match_Date,countryName))$Winning_Ratio)
      #print("---------------------6b")
      
      # Adding new Variables for perf againts the other team playing the match
      df_onDate$TeamName_Team_Winning_Ratio_vs_Other_Team_All_Matches[i] <- ((GetTeamWinningRecordsOnDate_vs_Opponent(teamName,opponentTeam,Match_Date))$Winning_Ratio)
      #print("---------------------7")
      df_onDate$TeamName_Team_Winning_Ratio_vs_Other_Team_Last5Matches[i] <- ((GetTeamWinningRecordsOnDate_vs_Opponent_Last5Matches(teamName,opponentTeam,Match_Date))$Winning_Ratio)
      
      df_onDate$TeamName_Batting_Performance_vs_Other_Team_All_Matches[i] <- ((TeamBattingPerformanceOnDate_vs_Opponent(teamName,opponentTeam,Match_Date))$Batting_Performance)
      df_onDate$TeamName_Batting_Performance_vs_Other_Team_Last5Matches[i] <- ((TeamBattingPerformanceOnDate_vs_Opponent_Last5Matches(teamName,opponentTeam,Match_Date))$Batting_Performance)
      
      df_onDate$TeamName_Bowling_Performance_vs_Other_Team_All_Matches[i] <- ((TeamBowlingPerformanceOnDate_vs_Opponent(teamName,opponentTeam,Match_Date))$Bowling_Performance)
      df_onDate$TeamName_Bowling_Performance_vs_Other_Team_Last5Matches[i] <- ((TeamBowlingPerformanceOnDate_vs_Opponent_Last5Matches(teamName,opponentTeam,Match_Date))$Bowling_Performance)
      #print("---------------------8")
      
      df_onDate$TeamName_Team_Perf_vs_Opponent[i] <- (df_onDate$TeamName_Team_Winning_Ratio_vs_Other_Team_All_Matches[i]+df_onDate$TeamName_Team_Winning_Ratio_vs_Other_Team_Last5Matches[i]+df_onDate$TeamName_Batting_Performance_vs_Other_Team_All_Matches[i]+df_onDate$TeamName_Batting_Performance_vs_Other_Team_Last5Matches[i]+df_onDate$TeamName_Bowling_Performance_vs_Other_Team_All_Matches[i]+df_onDate$TeamName_Bowling_Performance_vs_Other_Team_Last5Matches[i])/6
      
      
      #print("Stage4")
      df_onDate$Opponent_Team_Winning_Ratio_All[i] <- ((GetTeamWinningRecordsOnDate(opponentTeam,Match_Date))$Winning_Ratio)
      #print("---------------------7")
      df_onDate$Opponent_Team_Winning_Ratio_Last5Matches[i] <- ((GetTeamWinningRecordsOnDate_Last5Matches(opponentTeam,Match_Date))$Winning_Ratio)
      #print("---------------------8")
      df_onDate$Opponent_Batting_Performance_all[i] <- ((TeamBattingPerformanceOnDate(opponentTeam,Match_Date))$Batting_Performance)
      #print("---------------------9")
      df_onDate$Opponent_Batting_Performance_Last5Matches[i] <- ((TeamBattingPerformanceOnDate_Last5Matches(opponentTeam,Match_Date))$Batting_Performance)
      ###print("---------------------10")
      df_onDate$Opponent_Bowling_Performance_all[i] <- ((TeamBowlingPerformanceOnDate(opponentTeam,Match_Date))$Bowling_Performance)
      ###print("---------------------11")
      df_onDate$Opponent_Bowling_Performance_Last5Matches[i] <- ((TeamBowlingPerformanceOnDateLast5Matches(opponentTeam,Match_Date))$Bowling_Performance)
      ###print("---------------------12")
      df_onDate$Opponent_Winning_Ratio_At_Venue[i] <- ((GetTeamWinRatioOnDateAtVenueCountry(opponentTeam,Match_Date,countryName))$Winning_Ratio)
      ###print("---------------------13")
      
      df_onDate$Opponent_Winning_Ratio_At_Venue_Last5Matches[i] <- ((GetTeamWinRatioOnDateAtVenueCountry_Last5Matches(opponentTeam,Match_Date,countryName))$Winning_Ratio) 
      
      df_onDate$OpponentTeam[i] <- opponentTeam
      
      
    }
    colNamee <- paste(gsub(" ", "", teamName, fixed = TRUE),"_Team_Winning_Ratio_All",sep = "")
    #print(colNamee)
    
    #print("-----------------------------------------------------------------------------------------------")
    #print("-----------------------------------------------------------------------------------------------")
    ##print(df_onDate[,c("City",	"Country_Name",	"Venue",	"Match_Date",	"Team1",	"Team2",	"TeamName_Team_Winning_Ratio_All",	"TeamName_Team_Winning_Ratio_Last5Matches",	"TeamName_Batting_Performance_all",	"TeamName_Batting_Performance_Last5Matches",	"TeamName_Bowling_Performance_all",	"TeamName_Bowling_Performance_Last5Matches",	"TeamName_Winning_Ratio_At_Venue",	"TeamName_Winning_Ratio_At_Venue_Last5Matches",	"Opponent_Team_Winning_Ratio_All",	"Opponent_Team_Winning_Ratio_Last5Matches",	"Opponent_Batting_Performance_all",	"Opponent_Batting_Performance_Last5Matches",	"Opponent_Bowling_Performance_all",	"Opponent_Bowling_Performance_Last5Matches",	"Opponent_Winning_Ratio_At_Venue",	"Opponent_Winning_Ratio_At_Venue_Last5Matches",	"OpponentTeam","TeamName_Team_Perf_vs_Opponent")])
    #print("-----------------------------------------------------------------------------------------------")
    #print("-----------------------------------------------------------------------------------------------")
    
    dff <- df_onDate[,c("City",	"Country_Name",	"Venue",	"Match_Date",	"Team1",	"Team2",	"TeamName_Team_Winning_Ratio_All",	"TeamName_Team_Winning_Ratio_Last5Matches",	"TeamName_Batting_Performance_all",	"TeamName_Batting_Performance_Last5Matches",	"TeamName_Bowling_Performance_all",	"TeamName_Bowling_Performance_Last5Matches",	"TeamName_Winning_Ratio_At_Venue",	"TeamName_Winning_Ratio_At_Venue_Last5Matches",	"Opponent_Team_Winning_Ratio_All",	"Opponent_Team_Winning_Ratio_Last5Matches",	"Opponent_Batting_Performance_all",	"Opponent_Batting_Performance_Last5Matches",	"Opponent_Bowling_Performance_all",	"Opponent_Bowling_Performance_Last5Matches",	"Opponent_Winning_Ratio_At_Venue",	"Opponent_Winning_Ratio_At_Venue_Last5Matches",	"OpponentTeam","TeamName_Team_Perf_vs_Opponent")]
    
    names(dff) <- c("City",	"Country_Name",	"Venue",	"Match_Date",	"Team1",	"Team2",	paste(gsub(" ", "", teamName, fixed = TRUE),"_Team_Winning_Ratio_All",sep = ""),	paste(gsub(" ", "", teamName, fixed = TRUE),"_Team_Winning_Ratio_Last5Matches",sep = ""),	paste(gsub(" ", "", teamName, fixed = TRUE),"_Batting_Performance_all",sep = ""),	paste(gsub(" ", "", teamName, fixed = TRUE),"_Batting_Performance_Last5Matches",sep = ""),	paste(gsub(" ", "", teamName, fixed = TRUE),"_Bowling_Performance_all",sep = ""),	paste(gsub(" ", "", teamName, fixed = TRUE),"_Bowling_Performance_Last5Matches",sep = ""),	paste(gsub(" ", "", teamName, fixed = TRUE),"_Winning_Ratio_At_Venue",sep = ""),	paste(gsub(" ", "", teamName, fixed = TRUE),"_Winning_Ratio_At_Venue_Last5Matches",sep = ""),	"Opponent_Team_Winning_Ratio_All",	"Opponent_Team_Winning_Ratio_Last5Matches",	"Opponent_Batting_Performance_all",	"Opponent_Batting_Performance_Last5Matches",	"Opponent_Bowling_Performance_all",	"Opponent_Bowling_Performance_Last5Matches",	"Opponent_Winning_Ratio_At_Venue",	"Opponent_Winning_Ratio_At_Venue_Last5Matches",	"OpponentTeam",paste(gsub(" ", "", teamName, fixed = TRUE),"_Team_Perf_vs_Opponent",sep = ""))
    
    #print(dff)
    
    
    
    
    
    OutPutfileName <- paste("NM-2-NewMatch_Aggregated_",teamName,".csv",sep = "")
    OutPutfileNameWithDir <- paste(teamNameDirectoryonDate,OutPutfileName,sep = "")
    #print(OutPutfileNameWithDir)
    write.csv(dff,file = OutPutfileNameWithDir,row.names = FALSE)
    
    #outputFileName <- paste("MC-2-AllMatchesDataFor_",teamName,"_",onDate,"_NM_","_Aggregated_FORNOW",".csv",sep = "")
    #outputFileNameWithPath <- paste(teamNameDirectoryonDate,outputFileName,sep = "")
    #write.csv(dff,file = outputFileNameWithPath,row.names = FALSE)
    
    
    
  }
  
  
}