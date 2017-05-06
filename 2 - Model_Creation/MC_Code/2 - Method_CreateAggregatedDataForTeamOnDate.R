setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/2 - Model_Creation/MC_Code/")



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
  
  inputDirectoryPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/2 - Model_Creation/MC_Output/"
  teamNameDirectoryonDate <- paste(inputDirectoryPath,"MC-",teamName,"/",sep = "")
  
  fileName <- paste("MC-1-AllMatchesDataFor_",teamName,"_",onDate,".csv",sep = "")
  inputFileName <- paste(teamNameDirectoryonDate,fileName,sep = "")
  

  if(!file.exists(inputFileName)){
    
    print("File Doesnt exists")
    stop()
    
  } else{
    
    df <- read.csv(file = inputFileName,as.is=TRUE,header = TRUE)
    df$Date <- as.Date(df$Date)
    print(nrow(df))
    df_onDate <- df %>% filter(Team1==teamName|Team2==teamName,Date<=onDate)
    print(nrow(df_onDate))
    
    count <- 0
    for (i in 1:nrow(df_onDate)) {
      count <- count + 1
      #src_match_no <- df_onDate[i,"Src_Match_No"]
      team1 <- df_onDate[i,"Team1"]
      team2 <- df_onDate[i,"Team2"]
      Match_Date <- df_onDate[i,"Date"]
      countryName <- df_onDate[i,"Country_Name"]
      ###print(countryName)
      opponentTeam <- ifelse(team1==teamName,team2,team1)
      
      print(paste("row : ",count,sep = ""))
      print(paste("Team 1 : ",team1,"::::Team2:",team2,"::::Date:",Match_Date,"::::teamName:",teamName,"::::Opponent:",opponentTeam,sep = ""))

      
      df_onDate$TeamName_Team_Winning_Ratio_All[i] <- ((GetTeamWinningRecordsOnDate(teamName,Match_Date))$Winning_Ratio)
      ##print("---------------------1")
      df_onDate$TeamName_Team_Winning_Ratio_Last5Matches[i] <- ((GetTeamWinningRecordsOnDate_Last5Matches(teamName,Match_Date))$Winning_Ratio)
      ##print("---------------------2")
      df_onDate$TeamName_Batting_Performance_all[i] <- ((TeamBattingPerformanceOnDate(teamName,Match_Date))$Batting_Performance)
      ##print("---------------------3")
      df_onDate$TeamName_Batting_Performance_Last5Matches[i] <- ((TeamBattingPerformanceOnDate_Last5Matches(teamName,Match_Date))$Batting_Performance)
      ##print("---------------------4")
      df_onDate$TeamName_Bowling_Performance_all[i] <- ((TeamBowlingPerformanceOnDate(teamName,Match_Date))$Bowling_Performance)
      ##print("---------------------5")
      df_onDate$TeamName_Bowling_Performance_Last5Matches[i] <- ((TeamBowlingPerformanceOnDateLast5Matches(teamName,Match_Date))$Bowling_Performance)
      ##print("---------------------6")
      df_onDate$TeamName_Winning_Ratio_At_Venue[i] <- ((GetTeamWinRatioOnDateAtVenueCountry(teamName,Match_Date,countryName))$Winning_Ratio)
      df_onDate$TeamName_Winning_Ratio_At_Venue_Last5Matches[i] <- ((GetTeamWinRatioOnDateAtVenueCountry_Last5Matches(teamName,Match_Date,countryName))$Winning_Ratio)
      ##print("---------------------6b")
      
      # Adding new Variables for perf againts the other team playing the match
      df_onDate$TeamName_Team_Winning_Ratio_vs_Other_Team_All_Matches[i] <- ((GetTeamWinningRecordsOnDate_vs_Opponent(teamName,opponentTeam,Match_Date))$Winning_Ratio)
      df_onDate$TeamName_Team_Winning_Ratio_vs_Other_Team_Last5Matches[i] <- ((GetTeamWinningRecordsOnDate_vs_Opponent_Last5Matches(teamName,opponentTeam,Match_Date))$Winning_Ratio)
      
      df_onDate$TeamName_Batting_Performance_vs_Other_Team_All_Matches[i] <- ((TeamBattingPerformanceOnDate_vs_Opponent(teamName,opponentTeam,Match_Date))$Batting_Performance)
      df_onDate$TeamName_Batting_Performance_vs_Other_Team_Last5Matches[i] <- ((TeamBattingPerformanceOnDate_vs_Opponent_Last5Matches(teamName,opponentTeam,Match_Date))$Batting_Performance)
      
      df_onDate$TeamName_Bowling_Performance_vs_Other_Team_All_Matches[i] <- ((TeamBowlingPerformanceOnDate_vs_Opponent(teamName,opponentTeam,Match_Date))$Bowling_Performance)
      df_onDate$TeamName_Bowling_Performance_vs_Other_Team_Last5Matches[i] <- ((TeamBowlingPerformanceOnDate_vs_Opponent_Last5Matches(teamName,opponentTeam,Match_Date))$Bowling_Performance)
      
      df_onDate$TeamName_Team_Perf_vs_Opponent[i] <- (df_onDate$TeamName_Team_Winning_Ratio_vs_Other_Team_All_Matches[i]+df_onDate$TeamName_Team_Winning_Ratio_vs_Other_Team_Last5Matches[i]+df_onDate$TeamName_Batting_Performance_vs_Other_Team_All_Matches[i]+df_onDate$TeamName_Batting_Performance_vs_Other_Team_Last5Matches[i]+df_onDate$TeamName_Bowling_Performance_vs_Other_Team_All_Matches[i]+df_onDate$TeamName_Bowling_Performance_vs_Other_Team_Last5Matches[i])/6
      
      
      #####print("Stage4")
      df_onDate$Opponent_Team_Winning_Ratio_All[i] <- ((GetTeamWinningRecordsOnDate(opponentTeam,Match_Date))$Winning_Ratio)
      ##print("---------------------7")
      df_onDate$Opponent_Team_Winning_Ratio_Last5Matches[i] <- ((GetTeamWinningRecordsOnDate_Last5Matches(opponentTeam,Match_Date))$Winning_Ratio)
      ##print("---------------------8")
      df_onDate$Opponent_Batting_Performance_all[i] <- ((TeamBattingPerformanceOnDate(opponentTeam,Match_Date))$Batting_Performance)
      ##print("---------------------9")
      df_onDate$Opponent_Batting_Performance_Last5Matches[i] <- ((TeamBattingPerformanceOnDate_Last5Matches(opponentTeam,Match_Date))$Batting_Performance)
      ##print("---------------------10")
      df_onDate$Opponent_Bowling_Performance_all[i] <- ((TeamBowlingPerformanceOnDate(opponentTeam,Match_Date))$Bowling_Performance)
      ##print("---------------------11")
      df_onDate$Opponent_Bowling_Performance_Last5Matches[i] <- ((TeamBowlingPerformanceOnDateLast5Matches(opponentTeam,Match_Date))$Bowling_Performance)
      ##print("---------------------12")
      df_onDate$Opponent_Winning_Ratio_At_Venue[i] <- ((GetTeamWinRatioOnDateAtVenueCountry(opponentTeam,Match_Date,countryName))$Winning_Ratio)
      ##print("---------------------13")
      
      df_onDate$Opponent_Winning_Ratio_At_Venue_Last5Matches[i] <- ((GetTeamWinRatioOnDateAtVenueCountry_Last5Matches(opponentTeam,Match_Date,countryName))$Winning_Ratio) 
      

      

      df_onDate$TeamName_Win[i] <- ifelse(df_onDate$Winner[i]==teamName,1,0)
      df_onDate$OpponentTeam[i] <- opponentTeam
      
      #print("--BREAK---")
      #break


    }
    colNamee <- paste(gsub(" ", "", teamName, fixed = TRUE),"_Team_Winning_Ratio_All",sep = "")
    print(colNamee)
    
    print(head(df_onDate))
    
    
    dff <- df_onDate[,c("Src_Match_No", "Year",	"Country_Name",	"Venue",	"Date",	"Team1",	"Team2",	"Batting_First",	"Bowling_First",	"Winner",	"Looser",	"TeamName_Team_Winning_Ratio_All",	"TeamName_Team_Winning_Ratio_Last5Matches",	"TeamName_Batting_Performance_all",	"TeamName_Batting_Performance_Last5Matches",	"TeamName_Bowling_Performance_all",	"TeamName_Bowling_Performance_Last5Matches",	"TeamName_Winning_Ratio_At_Venue",	"TeamName_Winning_Ratio_At_Venue_Last5Matches",	"Opponent_Team_Winning_Ratio_All",	"Opponent_Team_Winning_Ratio_Last5Matches",	"Opponent_Batting_Performance_all",	"Opponent_Batting_Performance_Last5Matches",	"Opponent_Bowling_Performance_all",	"Opponent_Bowling_Performance_Last5Matches",	"Opponent_Winning_Ratio_At_Venue",	"Opponent_Winning_Ratio_At_Venue_Last5Matches","TeamName_Win",	"OpponentTeam","TeamName_Team_Perf_vs_Opponent")]
    
    names(dff) <- c("Src_Match_No","Year",	"Country_Name",	"Venue",	"Date",	"Team1",	"Team2",	"Batting_First",	"Bowling_First",	"Winner",	"Looser",	paste(gsub(" ", "", teamName, fixed = TRUE),"_Team_Winning_Ratio_All",sep = ""),	paste(gsub(" ", "", teamName, fixed = TRUE),"_Team_Winning_Ratio_Last5Matches",sep = ""),	paste(gsub(" ", "", teamName, fixed = TRUE),"_Batting_Performance_all",sep = ""),	paste(gsub(" ", "", teamName, fixed = TRUE),"_Batting_Performance_Last5Matches",sep = ""),	paste(gsub(" ", "", teamName, fixed = TRUE),"_Bowling_Performance_all",sep = ""),	paste(gsub(" ", "", teamName, fixed = TRUE),"_Bowling_Performance_Last5Matches",sep = ""),	paste(gsub(" ", "", teamName, fixed = TRUE),"_Winning_Ratio_At_Venue",sep = ""),	paste(gsub(" ", "", teamName, fixed = TRUE),"_Winning_Ratio_At_Venue_Last5Matches",sep = ""),	"Opponent_Team_Winning_Ratio_All",	"Opponent_Team_Winning_Ratio_Last5Matches",	"Opponent_Batting_Performance_all",	"Opponent_Batting_Performance_Last5Matches",	"Opponent_Bowling_Performance_all",	"Opponent_Bowling_Performance_Last5Matches",	"Opponent_Winning_Ratio_At_Venue",	"Opponent_Winning_Ratio_At_Venue_Last5Matches",paste(gsub(" ", "", teamName, fixed = TRUE),"_Win",sep = ""),	"OpponentTeam",paste(gsub(" ", "", teamName, fixed = TRUE),"_Team_Perf_vs_Opponent",sep = ""))

    
    outputFileName <- paste("MC-2-AllMatchesDataFor_",teamName,"_",onDate,"_Aggregated",".csv",sep = "")
    outputFileNameWithPath <- paste(teamNameDirectoryonDate,outputFileName,sep = "")
    write.csv(dff,file = outputFileNameWithPath,row.names = FALSE)
    


  }

  
}