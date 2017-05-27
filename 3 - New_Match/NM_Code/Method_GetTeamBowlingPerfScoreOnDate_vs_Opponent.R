setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/3 - New_Match/NM_Code/")

options(warn=-1)

library(reshape2)
library(dplyr)
library(sqldf)


TeamBowlingRecordsOnDate_vs_Opponent <- function(teamName,opponentTeam,onDate){
  
  #####print(teamName)
  #####print("stage1")
  
  
  inputFileName <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/3 - New_Match/DC-3-bowling_record.csv"
  
  df <- read.csv(file = inputFileName,as.is=TRUE,header = TRUE)
  df$Match_Date <- as.Date(df$Match_Date)
  
  
  
  # Getting All Matches for Team Name and Match Nos
  df_teamName_Matches <- df %>% filter(Match_Date<=onDate,bowling_team==teamName)
  teamMatch_Match_no <- unique(df_teamName_Matches$Match_Number)
  #print("-------Debug-3")
  # Getting All Matches for Opponent Team and Match Nos
  df_opponentTeam_Matches <- df %>% filter(Match_Date<=onDate,bowling_team==opponentTeam)
  opponentTeam_Match_no <- unique(df_opponentTeam_Matches$Match_Number)
  #print("-------Debug-4")
  
  # Getting the Match no, teamName Played against Opponent
  Common_Match_no <- Reduce(intersect, list(opponentTeam_Match_no,teamMatch_Match_no))
  
  
  
  
  #####print("stage2")
  # bowling record of the Input Team since start
  df_team_bowling_records <- df %>% filter(Match_Date<=onDate,bowling_team==teamName)
  #####print("stage3")
  
  # Players playing record of the Input Team on that date
  df_team_bowling_records_onDate <- df_team_bowling_records %>% filter(Match_Date==onDate)
  #####print("stage4")
  
  #####print(df_team_bowling_records_onDate)
  
  datalist = list()
  ct <- 0
  
  
  
  # Getting each player past record using the bowler name playing on that date
  for (i in 1:nrow(df_team_bowling_records_onDate)) {
    
    ct <- ct + 1
    #####print(paste("---Repeatition ---",as.character(ct),sep = ""))
    
    bowlerToCalc <- df_team_bowling_records_onDate[i, "bowler"]
    #####print(bowlerToCalc)
    
    # Getting all the matchs of teamName vs Opponent
    df_Teamteam_bowling_records_vs_Opponent <- df %>% filter(Match_Number %in% Common_Match_no)
    
    # bowler all the record before the date
    df_bowler_records <- df_Teamteam_bowling_records_vs_Opponent %>% filter(bowler==bowlerToCalc,Match_Date<onDate)
    #####print("stage5")
    numberOfMatches <-df_bowler_records %>% nrow()
    #####print(paste("Number of Matches : ",as.character(numberOfMatches),sep = ""))
    
    if(as.integer(numberOfMatches)==0){
      df_bowler_summary <- data.frame(bowler=bowlerToCalc,Matches=0,Balls=0,Runs=0,Wickets=0,Strike_Rate=0,Average=0,Economy=0,WicketsPerMatch=0)
    } else{
      df_bowler_summary <- df_bowler_records %>% group_by(bowler) %>% summarise(Matches = n(),Balls = sum(Balls),Runs = sum(Runs), Wickets = sum(Wickets,na.rm = TRUE))
      #####print("stage6")
      
      df_bowler_summary$Strike_Rate <- ifelse(is.infinite(df_bowler_summary$Balls/df_bowler_summary$Wickets),0,(df_bowler_summary$Balls/df_bowler_summary$Wickets))
      #####print("stage7")
      df_bowler_summary$Average <- ifelse(is.infinite(df_bowler_summary$Runs/df_bowler_summary$Wickets),0,(df_bowler_summary$Runs/df_bowler_summary$Wickets))
      #####print("stage8")
      df_bowler_summary$Economy <- ifelse(is.infinite(df_bowler_summary$Runs*6)/(df_bowler_summary$Balls),0,(df_bowler_summary$Runs*6)/(df_bowler_summary$Balls))
      df_bowler_summary$WicketsPerMatch <- ifelse(is.infinite(df_bowler_summary$Wickets)/(df_bowler_summary$Matches),0,(df_bowler_summary$Wickets)/(df_bowler_summary$Matches))
      
    }
    
    
    
    
    df_bowler_summary$Ondate <- onDate
    #####print("stage9")
    
    df_bowler_summary <- df_bowler_summary[,c(10,1,2,3,4,5,6,7,8,9)]
    #####print("stage10")
    #####print(df_bowler_summary)
    datalist[[ct]] <- df_bowler_summary
    #####print("stage11")
    
  }
  result <- do.call(rbind, datalist)
  #####print(result)
  return(result)
  
  
}


TeamBowlingPerformanceOnDate_vs_Opponent <- function(teamName,opponentTeam,onDate,Matches_wt=1,Strike_Rate_wt=1,Average_wt=1,Economy_wt=1,WicketsPerMatch_wt=1){
  
  rs <- TeamBowlingRecordsOnDate_vs_Opponent(teamName,opponentTeam,onDate)
  
  rs$Matches_Norm <- ifelse(is.nan((rs$Matches-min(rs$Matches))/(max(rs$Matches)-min(rs$Matches))),0,((rs$Matches-min(rs$Matches))/(max(rs$Matches)-min(rs$Matches))))
  rs$Strike_Rate_Norm <- ifelse(is.nan((rs$Strike_Rate-max(rs$Strike_Rate))/(min(rs$Strike_Rate)-max(rs$Strike_Rate))),0,((rs$Strike_Rate-max(rs$Strike_Rate))/(min(rs$Strike_Rate)-max(rs$Strike_Rate))))
  rs$Average_Norm <- ifelse(is.nan((rs$Average-max(rs$Average))/(min(rs$Average)-max(rs$Average))),0,((rs$Average-max(rs$Average))/(min(rs$Average)-max(rs$Average))))
  rs$Economy_Norm <- ifelse(is.nan((rs$Economy-max(rs$Economy))/(min(rs$Economy)-max(rs$Economy))),0,((rs$Economy-max(rs$Economy))/(min(rs$Economy)-max(rs$Economy))))
  rs$WicketsPerMatch_Norm <- ifelse(is.nan((rs$WicketsPerMatch-min(rs$WicketsPerMatch))/(max(rs$WicketsPerMatch)-min(rs$WicketsPerMatch))),0,((rs$WicketsPerMatch-min(rs$WicketsPerMatch))/(max(rs$WicketsPerMatch)-min(rs$WicketsPerMatch))))
  
  rs$Matches_Weight <- rep(Matches_wt,nrow(rs))
  rs$Strike_Rate_Weight <- rep(Strike_Rate_wt,nrow(rs))
  rs$Average_Weight <- rep(Average_wt,nrow(rs))
  rs$Economy_Weight <- rep(Economy_wt,nrow(rs))
  rs$WicketsPerMatch_Weight <- rep(WicketsPerMatch_wt,nrow(rs))
  
  rs$Points <- (rs$Matches_Norm*rs$Matches_Weight + rs$Strike_Rate_Norm*rs$Strike_Rate_Weight + rs$Average_Norm*rs$Average_Weight + rs$Economy_Norm*rs$Economy_Weight + rs$WicketsPerMatch_Norm*rs$WicketsPerMatch_Weight)/5
  
  #rs$Points_Norm <- (rs$Points-min(rs$Points))/(max(rs$Points)-min(rs$Points))
  
  Performance <- mean(rs$Points)
  
  result <- data.frame(Match_Date=onDate,Team=teamName,Bowling_Performance=Performance)
  
  return(result)
  
  
}





###-----------------------------------------###


TeamBowlingRecordsOnDate_vs_Opponent_Last5Matches <- function(teamName,opponentTeam,onDate){
  
  #####print(teamName)
  #####print("stage1")
  
  
  inputFileName <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/3 - New_Match/DC-3-bowling_record.csv"
  
  df <- read.csv(file = inputFileName,as.is=TRUE,header = TRUE)
  df$Match_Date <- as.Date(df$Match_Date)
  
  
  # Getting All Matches for Team Name and Match Nos
  df_teamName_Matches <- df %>% filter(Match_Date<=onDate,bowling_team==teamName)
  teamMatch_Match_no <- unique(df_teamName_Matches$Match_Number)
  #print("-------Debug-3")
  # Getting All Matches for Opponent Team and Match Nos
  df_opponentTeam_Matches <- df %>% filter(Match_Date<=onDate,bowling_team==opponentTeam)
  opponentTeam_Match_no <- unique(df_opponentTeam_Matches$Match_Number)
  #print("-------Debug-4")
  
  # Getting the Match no, teamName Played against Opponent
  Common_Match_no <- Reduce(intersect, list(opponentTeam_Match_no,teamMatch_Match_no))
  
  
  
  #####print("stage2")
  # bowling record of the Input Team since start
  df_team_bowling_records <- df %>% filter(Match_Date<=onDate,bowling_team==teamName)
  #####print("stage3")
  
  # Players playing record of the Input Team on that date
  df_team_bowling_records_onDate <- df_team_bowling_records %>% filter(Match_Date==onDate)
  #####print("stage4")
  
  #####print(df_team_bowling_records_onDate)
  
  datalist = list()
  ct <- 0
  
  
  
  # Getting each player past record using the bowler name playing on that date
  for (i in 1:nrow(df_team_bowling_records_onDate)) {
    
    ct <- ct + 1
    #####print(paste("---Repeatition ---",as.character(ct),sep = ""))
    
    bowlerToCalc <- df_team_bowling_records_onDate[i, "bowler"]
    ####print(bowlerToCalc)
    
    
    # Getting all the matchs of teamName vs Opponent
    df_Teamteam_bowling_records_vs_Opponent <- df %>% filter(Match_Number %in% Common_Match_no)
    
    # bowler all the record before the date
    df_bowler_records <- df_Teamteam_bowling_records_vs_Opponent %>% filter(bowler==bowlerToCalc,Match_Date<onDate)
    
    
    #####print("stage5")
    numberOfMatches <-df_bowler_records %>% nrow()
    #####print(paste("Number of Matches : ",as.character(numberOfMatches),sep = ""))
    
    if(as.integer(numberOfMatches)==0){
      df_bowler_summary <- data.frame(bowler=bowlerToCalc,Matches=0,Balls=0,Runs=0,Wickets=0,Strike_Rate=0,Average=0,Economy=0,WicketsPerMatch=0)
    } else{
      
      df_bowling_records_Ordered <- df_bowler_records[order(df_bowler_records$Match_Date, decreasing = TRUE),]
      
      df_bowling_records_Ordered$Match_Seq <- 1:nrow(df_bowling_records_Ordered)
      
      noOfMatchesToCalc <- ifelse(nrow(df_bowling_records_Ordered)<5,nrow(df_bowling_records_Ordered),5)
      
      df_bowling_five_matches <- df_bowling_records_Ordered %>% filter(Match_Seq<=noOfMatchesToCalc)
      
      df_bowler_summary <- df_bowling_five_matches %>% group_by(bowler) %>% summarise(Matches = n(),Balls = sum(Balls),Runs = sum(Runs), Wickets = sum(Wickets,na.rm = TRUE))
      #####print("stage6")
      
      df_bowler_summary$Strike_Rate <- ifelse(is.infinite(df_bowler_summary$Balls/df_bowler_summary$Wickets),0,(df_bowler_summary$Balls/df_bowler_summary$Wickets))
      #####print("stage7")
      df_bowler_summary$Average <- ifelse(is.infinite(df_bowler_summary$Runs/df_bowler_summary$Wickets),0,(df_bowler_summary$Runs/df_bowler_summary$Wickets))
      #####print("stage8")
      df_bowler_summary$Economy <- ifelse(is.infinite(df_bowler_summary$Runs*6)/(df_bowler_summary$Balls),0,(df_bowler_summary$Runs*6)/(df_bowler_summary$Balls))
      df_bowler_summary$WicketsPerMatch <- ifelse(is.infinite(df_bowler_summary$Wickets)/(df_bowler_summary$Matches),0,(df_bowler_summary$Wickets)/(df_bowler_summary$Matches))
      
    }
    
    
    
    
    df_bowler_summary$Ondate <- onDate
    #####print("stage9")
    
    df_bowler_summary <- df_bowler_summary[,c(10,1,2,3,4,5,6,7,8,9)]
    #####print("stage10")
    #####print(df_bowler_summary)
    datalist[[ct]] <- df_bowler_summary
    #####print("stage11")
    
  }
  result <- do.call(rbind, datalist)
  #####print(result)
  return(result)
  
  
}


TeamBowlingPerformanceOnDate_vs_Opponent_Last5Matches <- function(teamName,opponentTeam,onDate,Matches_wt=1,Strike_Rate_wt=1,Average_wt=1,Economy_wt=1,WicketsPerMatch_wt=1){
  
  rs <- TeamBowlingRecordsOnDate_vs_Opponent_Last5Matches(teamName,opponentTeam,onDate)
  
  rs$Matches_Norm <- ifelse(is.nan((rs$Matches-min(rs$Matches))/(max(rs$Matches)-min(rs$Matches))),0,((rs$Matches-min(rs$Matches))/(max(rs$Matches)-min(rs$Matches))))
  rs$Strike_Rate_Norm <- ifelse(is.nan((rs$Strike_Rate-max(rs$Strike_Rate))/(min(rs$Strike_Rate)-max(rs$Strike_Rate))),0,((rs$Strike_Rate-max(rs$Strike_Rate))/(min(rs$Strike_Rate)-max(rs$Strike_Rate))))
  rs$Average_Norm <- ifelse(is.nan((rs$Average-max(rs$Average))/(min(rs$Average)-max(rs$Average))),0,((rs$Average-max(rs$Average))/(min(rs$Average)-max(rs$Average))))
  rs$Economy_Norm <- ifelse(is.nan((rs$Economy-max(rs$Economy))/(min(rs$Economy)-max(rs$Economy))),0,((rs$Economy-max(rs$Economy))/(min(rs$Economy)-max(rs$Economy))))
  rs$WicketsPerMatch_Norm <- ifelse(is.nan((rs$WicketsPerMatch-min(rs$WicketsPerMatch))/(max(rs$WicketsPerMatch)-min(rs$WicketsPerMatch))),0,((rs$WicketsPerMatch-min(rs$WicketsPerMatch))/(max(rs$WicketsPerMatch)-min(rs$WicketsPerMatch))))
  
  rs$Matches_Weight <- rep(Matches_wt,nrow(rs))
  rs$Strike_Rate_Weight <- rep(Strike_Rate_wt,nrow(rs))
  rs$Average_Weight <- rep(Average_wt,nrow(rs))
  rs$Economy_Weight <- rep(Economy_wt,nrow(rs))
  rs$WicketsPerMatch_Weight <- rep(WicketsPerMatch_wt,nrow(rs))
  
  rs$Points <- (rs$Matches_Norm*rs$Matches_Weight + rs$Strike_Rate_Norm*rs$Strike_Rate_Weight + rs$Average_Norm*rs$Average_Weight + rs$Economy_Norm*rs$Economy_Weight + rs$WicketsPerMatch_Norm*rs$WicketsPerMatch_Weight)/5
  
  
  
  #rs$Points_Norm <- (rs$Points-min(rs$Points))/(max(rs$Points)-min(rs$Points))
  
  Performance <- mean(rs$Points)
  
  result <- data.frame(Match_Date=onDate,Team=teamName,Bowling_Performance=Performance)
  
  return(result)
  
  
}
