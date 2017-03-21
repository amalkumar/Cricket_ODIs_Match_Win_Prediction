setwd("D:/Study_Material/DataAnalytics/Cricket/ODIs/1 - Data_Creation/DC_Code/")

options(warn=-1)

library(reshape2)
library(dplyr)
library(sqldf)


sourceFileDirectory <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/Common_Reference_Data/odi_csv_male/"


listOfFiles <- list.files(sourceFileDirectory)
count <- length(listOfFiles)


datalist = list()
ct <- 0

df_data <- data.frame(match_no=integer(0),season= character(0), match_date = character(0),match_city = character(0),match_venue = character(0),Team1= character(0), Team2= character(0), toss_winner = character(0),toss_decision = character(0), winner = character(0), looser=character(0))




for (file in listOfFiles){
  df <- data.frame()
  df_info <- data.frame()
  df_innings <- data.frame()
  batting_record <- data.frame()
  
  
  ct <- ct + 1
  print(paste("Number of Files Pending for Processing : ",as.character(count)))
  print(paste("Currently Processing : ",file))
  
  fileFullPath <- paste(sourceFileDirectory,file,sep = "")
  
  
  df <- read.csv(fileFullPath,row.names=NULL,col.names=c("ball","inning_no","over_n_ball","batting_team","batsman","non_striker","bowler","runs_of_bat","extras","kind_of_wicket","Dismissed_player"))
  
  
  df_info <- sqldf("select * from df where ball='info'")
  colnames(df_info) <- c("info","key","value","NA1","NA2","NA3","NA4","NA5","NA6","NA7","NA8")
  
  
  
  result <- (df_info %>% filter(key=="outcome"))[1,]["value"]
  
  
  if ( !(is.na(as.character(result[1,]))) & (as.character(result[1,])==as.character("no result") || as.character(result[1,])==as.character("tie"))){
    print("NOOOOOOOO")
    count <- count - 1
    next
  }
  
  
  match_no <- ct
  season <- (df_info %>% filter(key=="season"))[1,]["value"]
  match_date <- (df_info %>% filter(key=="date"))[1,]["value"]
  match_venue <- (df_info %>% filter(key=="venue"))[1,]["value"]
  match_city <- (df_info %>% filter(key=="city"))[1,]["value"]
  team1 <- (df_info %>% filter(key=="team"))[1,]["value"]
  team2 <- (df_info %>% filter(key=="team"))[2,]["value"]
  toss_winner <- (df_info %>% filter(key=="toss_winner"))[1,]["value"]
  toss_decision <- (df_info %>% filter(key=="toss_decision"))[1,]["value"]
  winner <- (df_info %>% filter(key=="winner"))[1,]["value"]
  if (winner == team1) {
    looser <- team2 
  }
  else {
    looser <- team1
  }  
  print(paste(match_no,season,match_date,match_city,match_venue,team1,team2,toss_winner,toss_decision,winner,looser))
  df_data <- rbind(df_data, data.frame(match_no,season,match_date,match_city,match_venue,team1,team2,toss_winner,toss_decision,winner,looser))
  
  
  count <- count - 1
}



colnames(df_data) <- c("Match_Number","Season","Match_Date","City","Venue","Team1","Team2","Toss_Winner","Toss_Decision","Winner","Looser")

#View(df_data)

outputFileName <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/1 - Data_Creation/DC_Output/DC-1-TeamWinningRecord.csv"
write.csv(df_data,file = outputFileName,row.names = FALSE)