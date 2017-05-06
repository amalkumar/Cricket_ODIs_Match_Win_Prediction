setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/3 - New_Match/NM_Code/")
options(warn=-1)

library(dplyr)

source("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/2 - Model_Creation/MC_Code/Method_GetTeamWinRatioOnDate.R")
source("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/2 - Model_Creation/MC_Code/Method_GetTeamWinRatioOnDate_vs_Opponent.R")
source("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/2 - Model_Creation/MC_Code/Method_GetTeamWinRatioOnDateAtVenueCountry.R")
source("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/2 - Model_Creation/MC_Code/Method_GetTeamBattingPerfScoreOnDate.R")
source("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/2 - Model_Creation/MC_Code/Method_GetTeamBattingPerfScoreOnDate_vs_Opponent.R")
source("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/2 - Model_Creation/MC_Code/Method_GetTeamBowlingPerfScoreOnDate.R")
source("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/2 - Model_Creation/MC_Code/Method_GetTeamBowlingPerfScoreOnDate_vs_Opponent.R")


CreateAggregatedDataForTeamOnDate <- function(newMatchDir="D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/3 - New_Match/",newMatchFile="New_Match.csv"){
  
  # creating the file name with path
  newMatchFileWithPath <- paste(newMatchDir,newMatchFile,sep = "")

  if(!file.exists(newMatchFileWithPath)){
    
    print("File Doesnt exists")
    stop()
    
  } else{
    print(newMatchFileWithPath)
    
    # Reading the new match file for prediction
    df <- read.csv(file = newMatchFileWithPath,as.is=TRUE,header = TRUE)
    df$Date <- as.Date(df$Date)
    
    print(df)
    
    count <- 0
    for (i in 1:nrow(df)) {
      count <- count + 1
      
      print(paste("Processing New Match file record : ",count))
      
      # Getting the team names
      team1 <- df[i,"Team1"]
      team2 <- df[i,"Team2"]
      Match_Date <- df[i,"Date"]
      countryName <- df[i,"Country_Name"]
      
      print(paste("Team 1 : ",team1," :::: Team 2 : ",team2,sep = ""))
      
      # treating each team as main team and other as opponent
      teams <- c(team1,team2)
      for (teamName in teams){
        
        opponentTeam <- ifelse(team1==teamName,team2,team1)
        print(paste("Team is : ",teamName," and Opponent is : ",opponentTeam))
        
        
        
        df$TeamName_Team_Winning_Ratio_All[i] <- ((GetTeamWinningRecordsOnDate(teamName,Match_Date))$Winning_Ratio)
        ##print("---------------------1")
        df$TeamName_Team_Winning_Ratio_Last5Matches[i] <- ((GetTeamWinningRecordsOnDate_Last5Matches(teamName,Match_Date))$Winning_Ratio)
        ##print("---------------------2")
        df$TeamName_Batting_Performance_all[i] <- ((TeamBattingPerformanceOnDate(teamName,Match_Date))$Batting_Performance)
        ##print("---------------------3")
        df$TeamName_Batting_Performance_Last5Matches[i] <- ((TeamBattingPerformanceOnDate_Last5Matches(teamName,Match_Date))$Batting_Performance)
        ##print("---------------------4")
        df$TeamName_Bowling_Performance_all[i] <- ((TeamBowlingPerformanceOnDate(teamName,Match_Date))$Bowling_Performance)
        ##print("---------------------5")
        df$TeamName_Bowling_Performance_Last5Matches[i] <- ((TeamBowlingPerformanceOnDateLast5Matches(teamName,Match_Date))$Bowling_Performance)
        ##print("---------------------6")
        df$TeamName_Winning_Ratio_At_Venue[i] <- ((GetTeamWinRatioOnDateAtVenueCountry(teamName,Match_Date,countryName))$Winning_Ratio)
        df$TeamName_Winning_Ratio_At_Venue_Last5Matches[i] <- ((GetTeamWinRatioOnDateAtVenueCountry_Last5Matches(teamName,Match_Date,countryName))$Winning_Ratio)
        ##print("---------------------6b")
        
        # Adding new Variables for perf againts the other team playing the match
        df$TeamName_Team_Winning_Ratio_vs_Other_Team_All_Matches[i] <- ((GetTeamWinningRecordsOnDate_vs_Opponent(teamName,opponentTeam,Match_Date))$Winning_Ratio)
        df$TeamName_Team_Winning_Ratio_vs_Other_Team_Last5Matches[i] <- ((GetTeamWinningRecordsOnDate_vs_Opponent_Last5Matches(teamName,opponentTeam,Match_Date))$Winning_Ratio)
        
        df$TeamName_Batting_Performance_vs_Other_Team_All_Matches[i] <- ((TeamBattingPerformanceOnDate_vs_Opponent(teamName,opponentTeam,Match_Date))$Batting_Performance)
        df$TeamName_Batting_Performance_vs_Other_Team_Last5Matches[i] <- ((TeamBattingPerformanceOnDate_vs_Opponent_Last5Matches(teamName,opponentTeam,Match_Date))$Batting_Performance)
        
        df$TeamName_Bowling_Performance_vs_Other_Team_All_Matches[i] <- ((TeamBowlingPerformanceOnDate_vs_Opponent(teamName,opponentTeam,Match_Date))$Bowling_Performance)
        df$TeamName_Bowling_Performance_vs_Other_Team_Last5Matches[i] <- ((TeamBowlingPerformanceOnDate_vs_Opponent_Last5Matches(teamName,opponentTeam,Match_Date))$Bowling_Performance)
        
        df$TeamName_Team_Perf_vs_Opponent[i] <- (df$TeamName_Team_Winning_Ratio_vs_Other_Team_All_Matches[i]+df$TeamName_Team_Winning_Ratio_vs_Other_Team_Last5Matches[i]+df$TeamName_Batting_Performance_vs_Other_Team_All_Matches[i]+df$TeamName_Batting_Performance_vs_Other_Team_Last5Matches[i]+df$TeamName_Bowling_Performance_vs_Other_Team_All_Matches[i]+df$TeamName_Bowling_Performance_vs_Other_Team_Last5Matches[i])/6
        
        
        #####print("Stage4")
        df$Opponent_Team_Winning_Ratio_All[i] <- ((GetTeamWinningRecordsOnDate(opponentTeam,Match_Date))$Winning_Ratio)
        ##print("---------------------7")
        df$Opponent_Team_Winning_Ratio_Last5Matches[i] <- ((GetTeamWinningRecordsOnDate_Last5Matches(opponentTeam,Match_Date))$Winning_Ratio)
        ##print("---------------------8")
        df$Opponent_Batting_Performance_all[i] <- ((TeamBattingPerformanceOnDate(opponentTeam,Match_Date))$Batting_Performance)
        ##print("---------------------9")
        df$Opponent_Batting_Performance_Last5Matches[i] <- ((TeamBattingPerformanceOnDate_Last5Matches(opponentTeam,Match_Date))$Batting_Performance)
        ##print("---------------------10")
        df$Opponent_Bowling_Performance_all[i] <- ((TeamBowlingPerformanceOnDate(opponentTeam,Match_Date))$Bowling_Performance)
        ##print("---------------------11")
        df$Opponent_Bowling_Performance_Last5Matches[i] <- ((TeamBowlingPerformanceOnDateLast5Matches(opponentTeam,Match_Date))$Bowling_Performance)
        ##print("---------------------12")
        df$Opponent_Winning_Ratio_At_Venue[i] <- ((GetTeamWinRatioOnDateAtVenueCountry(opponentTeam,Match_Date,countryName))$Winning_Ratio)
        ##print("---------------------13")
        
        df$Opponent_Winning_Ratio_At_Venue_Last5Matches[i] <- ((GetTeamWinRatioOnDateAtVenueCountry_Last5Matches(opponentTeam,Match_Date,countryName))$Winning_Ratio) 
        
        df$OpponentTeam[i] <- opponentTeam
        
        dff <- df[,c("Country_Name",	"Venue",	"Date",	"Team1",	"Team2","TeamName_Team_Winning_Ratio_All",	"TeamName_Team_Winning_Ratio_Last5Matches",	"TeamName_Batting_Performance_all",	"TeamName_Batting_Performance_Last5Matches",	"TeamName_Bowling_Performance_all",	"TeamName_Bowling_Performance_Last5Matches",	"TeamName_Winning_Ratio_At_Venue",	"TeamName_Winning_Ratio_At_Venue_Last5Matches",	"Opponent_Team_Winning_Ratio_All",	"Opponent_Team_Winning_Ratio_Last5Matches",	"Opponent_Batting_Performance_all",	"Opponent_Batting_Performance_Last5Matches",	"Opponent_Bowling_Performance_all",	"Opponent_Bowling_Performance_Last5Matches",	"Opponent_Winning_Ratio_At_Venue",	"Opponent_Winning_Ratio_At_Venue_Last5Matches",	"OpponentTeam","TeamName_Team_Perf_vs_Opponent")]
        
        names(dff) <- c("Country_Name",	"Venue",	"Date",	"Team1",	"Team2",paste(gsub(" ", "", teamName, fixed = TRUE),"_Team_Winning_Ratio_All",sep = ""),	paste(gsub(" ", "", teamName, fixed = TRUE),"_Team_Winning_Ratio_Last5Matches",sep = ""),	paste(gsub(" ", "", teamName, fixed = TRUE),"_Batting_Performance_all",sep = ""),	paste(gsub(" ", "", teamName, fixed = TRUE),"_Batting_Performance_Last5Matches",sep = ""),	paste(gsub(" ", "", teamName, fixed = TRUE),"_Bowling_Performance_all",sep = ""),	paste(gsub(" ", "", teamName, fixed = TRUE),"_Bowling_Performance_Last5Matches",sep = ""),	paste(gsub(" ", "", teamName, fixed = TRUE),"_Winning_Ratio_At_Venue",sep = ""),	paste(gsub(" ", "", teamName, fixed = TRUE),"_Winning_Ratio_At_Venue_Last5Matches",sep = ""),	"Opponent_Team_Winning_Ratio_All",	"Opponent_Team_Winning_Ratio_Last5Matches",	"Opponent_Batting_Performance_all",	"Opponent_Batting_Performance_Last5Matches",	"Opponent_Bowling_Performance_all",	"Opponent_Bowling_Performance_Last5Matches",	"Opponent_Winning_Ratio_At_Venue",	"Opponent_Winning_Ratio_At_Venue_Last5Matches","OpponentTeam",paste(gsub(" ", "", teamName, fixed = TRUE),"_Team_Perf_vs_Opponent",sep = ""))
        
        
        teamNameOutPutDir <- paste(newMatchDir,"MC-",teamName,"/",sep = "")
        outputFileName <- paste("MC-1-AllMatchesDataFor_",teamName,"_Aggregated",".csv",sep = "")
        outputFileNameWithPath <- paste(teamNameOutPutDir,outputFileName,sep = "")
        write.csv(dff,file = outputFileNameWithPath,row.names = FALSE)
        
        
      }
      
    }
    
    
    
    
    
    
  }
  
    
}
