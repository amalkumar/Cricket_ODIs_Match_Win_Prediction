setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/2 - Model_Creation/MC_Code/")

options(warn=-1)

library(reshape2)
library(dplyr)
library(sqldf)


TeamBattingRecordsOnDate_vs_Opponent <- function(teamName,opponentTeam,onDate){
  
  ######print(teamName)
  #print("-------Debug-1")
  
  inputFileName <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/1 - Data_Creation/DC_Output/DC-2-batting_record.csv"
  
  df <- read.csv(file = inputFileName,as.is=TRUE,header = TRUE)
  df$Match_Date <- as.Date(df$Match_Date)
  #print("-------Debug-2")
  
  # Getting All Matches for Team Name and Match Nos
  df_teamName_Matches <- df %>% filter(Match_Date<=onDate,batting_team==teamName)
  teamMatch_Match_no <- unique(df_teamName_Matches$Match_Number)
  #print("-------Debug-3")
  # Getting All Matches for Opponent Team and Match Nos
  df_opponentTeam_Matches <- df %>% filter(Match_Date<=onDate,batting_team==opponentTeam)
  opponentTeam_Match_no <- unique(df_opponentTeam_Matches$Match_Number)
  #print("-------Debug-4")
  
  # Getting the Match no, teamName Played against Opponent
  Common_Match_no <- Reduce(intersect, list(opponentTeam_Match_no,teamMatch_Match_no))
  #print("-------Debug-5")
  
  
  ###print("stage2")
  ###print(nrow(df))
  ###print(onDate)
  # Batting record of the Input Team since start
  df_team_batting_records <- df %>% filter(Match_Date<=onDate,batting_team==teamName)
  
  #print("-------Debug-6")
  ###print(nrow(df_team_batting_records))
  
  # Players playing record of the Input Team on that date
  df_team_batting_records_onDate <- df_team_batting_records %>% filter(Match_Date==onDate)
  #print("-------Debug-7")
  
  #print(nrow(df_team_batting_records_onDate))
  
  datalist = list()
  ct <- 0
  
  
  
  # Getting each player past record using the Batsman name playing on that date
  for (i in 1:nrow(df_team_batting_records_onDate)) {
    #print("-------Debug-8")
    ct <- ct + 1
    #print(paste("---Repeatition ---",as.character(ct),sep = ""))
    
    batsmanToCalc <- df_team_batting_records_onDate[i, "batsman"]
    #print(batsmanToCalc)
    
    # Getting all the matchs of teamName vs Opponent
    df_Teamteam_batting_records_vs_Opponent <- df %>% filter(Match_Number %in% Common_Match_no)
    
    # Batsman all the record before the date
    df_batsman_records <- df_Teamteam_batting_records_vs_Opponent %>% filter(batsman==batsmanToCalc,Match_Date<onDate)
    
    # removing records where batsman played, but did not get batting
    df_batsman_records <- df_batsman_records[!is.na(df_batsman_records$Balls),]
    
    ###print("stage5")
    numberOfMatches <-df_batsman_records %>% nrow()
    ######print(paste("Number of Matches : ",as.character(numberOfMatches),sep = ""))
    
    if(as.integer(numberOfMatches)==0){
      df_batsman_summary <- data.frame(batsman=batsmanToCalc,Matches=0,Balls=0,Runs=0,Out=0,Strike_Rate=0,Average=0)
    } else{
      df_batsman_summary <- df_batsman_records %>% group_by(batsman) %>% summarise(Matches = n(),Balls = sum(Balls),Runs = sum(Runs), Out = sum(Got_Out,na.rm = TRUE))
      ####print("stage6")
      
      df_batsman_summary$Strike_Rate <- ifelse(is.infinite((df_batsman_summary$Runs/df_batsman_summary$Balls)*100),0,((df_batsman_summary$Runs/df_batsman_summary$Balls)*100))
      ######print("stage7")
      df_batsman_summary$Average <- ifelse(is.infinite(df_batsman_summary$Runs/df_batsman_summary$Out),0,(df_batsman_summary$Runs/df_batsman_summary$Out))
      ######print("stage8")
      
    }
    
    
    
    
    df_batsman_summary$Ondate <- onDate
    ######print("stage9")
    
    df_batsman_summary <- df_batsman_summary[,c(8,1,2,3,4,5,6,7)]
    ######print("stage10")
    ######print(df_batsman_summary)
    datalist[[ct]] <- df_batsman_summary
    ######print("stage11")
    
  }
  result <- do.call(rbind, datalist)
  ######print(result)
  return(result)
  
  
}


TeamBattingPerformanceOnDate_vs_Opponent <- function(teamName,opponentTeam,onDate,Matches_wt=5,Strike_Rate_wt=1,Average_wt=5){
  
  rs <- TeamBattingRecordsOnDate_vs_Opponent(teamName,opponentTeam,onDate)
  
  rs$Matches_Norm <- ifelse(is.nan((rs$Matches-min(rs$Matches))/(max(rs$Matches)-min(rs$Matches))),0,((rs$Matches-min(rs$Matches))/(max(rs$Matches)-min(rs$Matches))))
  rs$Strike_Rate_Norm <- ifelse(is.nan((rs$Strike_Rate-min(rs$Strike_Rate))/(max(rs$Strike_Rate)-min(rs$Strike_Rate))),0,((rs$Strike_Rate-min(rs$Strike_Rate))/(max(rs$Strike_Rate)-min(rs$Strike_Rate))))
  rs$Average_Norm <- ifelse(is.nan((rs$Average-min(rs$Average))/(max(rs$Average)-min(rs$Average))),0,((rs$Average-min(rs$Average))/(max(rs$Average)-min(rs$Average))))
  
  rs$Matches_Weight <- rep(Matches_wt,nrow(rs))
  rs$Strike_Rate_Weight <- rep(Strike_Rate_wt,nrow(rs))
  rs$Average_Weight <- rep(Average_wt,nrow(rs))
  
  rs$Points <- (rs$Matches_Norm*rs$Matches_Weight + rs$Strike_Rate_Norm*rs$Strike_Rate_Weight + rs$Average_Norm*rs$Average_Weight)/3
  
  #rs$Points_Norm <- (rs$Points-min(rs$Points))/(max(rs$Points)-min(rs$Points))
  
  Performance <- mean(rs$Points)
  
  result <- data.frame(Match_Date=onDate,Team=teamName,Batting_Performance=Performance)
  
  return(result)
  
  
}



TeamBattingRecordsOnDate_vs_Opponent_Last5Matches <- function(teamName,opponentTeam,onDate){
  
  ###print("Stage1")
  
  
  inputFileName <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/1 - Data_Creation/DC_Output/DC-2-batting_record.csv"
  
  df <- read.csv(file = inputFileName,as.is=TRUE,header = TRUE)
  df$Match_Date <- as.Date(df$Match_Date)
  
  
  # Getting All Matches for Team Name and Match Nos
  df_teamName_Matches <- df %>% filter(Match_Date<=onDate,batting_team==teamName)
  teamMatch_Match_no <- unique(df_teamName_Matches$Match_Number)
  
  # Getting All Matches for Opponent Team and Match Nos
  df_opponentTeam_Matches <- df %>% filter(Match_Date<=onDate,batting_team==opponentTeam)
  opponentTeam_Match_no <- unique(df_opponentTeam_Matches$Match_Number)
  
  
  # Getting the Match no, teamName Played against Opponent
  Common_Match_no <- Reduce(intersect, list(opponentTeam_Match_no,teamMatch_Match_no))
  
  ###print("stage2")
  
  # Batting record of the Input Team since start
  df_team_batting_records <- df %>% filter(Match_Date<=onDate,batting_team==teamName)
  ###print("stage3")
  ###print(nrow(df_team_batting_records))
  ###print(head(df_team_batting_records,2))
  
  # Players playing record of the Input Team on that date
  df_team_batting_records_onDate <- df_team_batting_records %>% filter(Match_Date==onDate)
  ###print("stage4")
  ###print(nrow(df_team_batting_records_onDate))
  ###print(df_team_batting_records_onDate)
  
  datalist = list()
  ct <- 0
  
  
  
  # Getting each player past record using the Batsman name playing on that date
  for (i in 1:nrow(df_team_batting_records_onDate)) {
    
    ct <- ct + 1
    #####print(paste("---Repeatition ---",as.character(ct),sep = ""))
    
    batsmanToCalc <- df_team_batting_records_onDate[i, "batsman"]
    ###print(batsmanToCalc)
    
    
    # Getting all the matchs of teamName vs Opponent
    df_Teamteam_batting_records_vs_Opponent <- df %>% filter(Match_Number %in% Common_Match_no)
    
    # Batsman all the record before the date
    df_batsman_records <- df_Teamteam_batting_records_vs_Opponent %>% filter(batsman==batsmanToCalc,Match_Date<onDate)
    
    
    
    
    # Batsman all the record before the date
    #df_batsman_records <- df_team_batting_records %>% filter(batsman==batsmanToCalc,Match_Date<onDate)
    #####print("a")
    
    ######print("stage5")
    numberOfMatches <-df_batsman_records %>% nrow()
    ######print(paste("Number of Matches : ",as.character(numberOfMatches),sep = ""))
    #####print("f")
    
    if(as.integer(numberOfMatches)==0){
      df_batsman_summary <- data.frame(batsman=batsmanToCalc,Matches=0,Balls=0,Runs=0,Out=0,Strike_Rate=0,Average=0)
    } else{
      
      df_batsman_records_Ordered <- df_batsman_records[order(df_batsman_records$Match_Date, decreasing = TRUE),]
      #####print("b")
      
      df_batsman_records_Ordered$Match_Seq <- 1:nrow(df_batsman_records_Ordered)
      #####print("c")
      
      
      
      noOfMatchesToCalc <- ifelse(nrow(df_batsman_records_Ordered)<5,nrow(df_batsman_records_Ordered),5)
      #####print("d")
      
      df_batsman_five_matches <- df_batsman_records_Ordered %>% filter(Match_Seq<=noOfMatchesToCalc)
      #####print("e")
      
      
      
      df_batsman_summary <- df_batsman_five_matches %>% group_by(batsman) %>% summarise(Matches = n(),Balls = sum(Balls),Runs = sum(Runs), Out = sum(Got_Out,na.rm = TRUE))
      ######print("stage6")
      
      df_batsman_summary$Strike_Rate <- ifelse(is.infinite((df_batsman_summary$Runs/df_batsman_summary$Balls)*100),0,((df_batsman_summary$Runs/df_batsman_summary$Balls)*100))
      ######print("stage7")
      df_batsman_summary$Average <- ifelse(is.infinite(df_batsman_summary$Runs/df_batsman_summary$Out),0,(df_batsman_summary$Runs/df_batsman_summary$Out))
      ######print("stage8")
      
    }
    
    
    
    
    df_batsman_summary$Ondate <- onDate
    ######print("stage9")
    
    df_batsman_summary <- df_batsman_summary[,c(8,1,2,3,4,5,6,7)]
    ######print("stage10")
    ######print(df_batsman_summary)
    datalist[[ct]] <- df_batsman_summary
    ######print("stage11")
    
  }
  result <- do.call(rbind, datalist)
  ######print(result)
  return(result)
  
  
}


TeamBattingPerformanceOnDate_vs_Opponent_Last5Matches <- function(teamName,opponentTeam,onDate,Matches_wt=5,Strike_Rate_wt=1,Average_wt=5){
  
  rs <- TeamBattingRecordsOnDate_vs_Opponent_Last5Matches(teamName,opponentTeam,onDate)
  
  
  rs$Matches_Norm <- ifelse(is.nan((rs$Matches-min(rs$Matches))/(max(rs$Matches)-min(rs$Matches))),0,((rs$Matches-min(rs$Matches))/(max(rs$Matches)-min(rs$Matches))))
  rs$Strike_Rate_Norm <- ifelse(is.nan((rs$Strike_Rate-min(rs$Strike_Rate))/(max(rs$Strike_Rate)-min(rs$Strike_Rate))),0,((rs$Strike_Rate-min(rs$Strike_Rate))/(max(rs$Strike_Rate)-min(rs$Strike_Rate))))
  rs$Average_Norm <- ifelse(is.nan((rs$Average-min(rs$Average))/(max(rs$Average)-min(rs$Average))),0,((rs$Average-min(rs$Average))/(max(rs$Average)-min(rs$Average))))  
  
  rs$Matches_Weight <- rep(Matches_wt,nrow(rs))
  rs$Strike_Rate_Weight <- rep(Strike_Rate_wt,nrow(rs))
  rs$Average_Weight <- rep(Average_wt,nrow(rs))
  
  rs$Points <- (rs$Matches_Norm*rs$Matches_Weight + rs$Strike_Rate_Norm*rs$Strike_Rate_Weight + rs$Average_Norm*rs$Average_Weight)/3
  
  #rs$Points_Norm <- (rs$Points-min(rs$Points))/(max(rs$Points)-min(rs$Points))
  
  Performance <- mean(rs$Points)
  
  result <- data.frame(Match_Date=onDate,Team=teamName,Batting_Performance=Performance)
  
  return(result)
  
  
}
