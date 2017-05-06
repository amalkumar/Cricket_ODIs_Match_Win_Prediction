setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/0 - ScrapWebData/SW_Code/Full_Load/")
library(rvest)
library(dplyr)
library(XML)
library(tidyr)

outputPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/0 - ScrapWebData/Full_Load/"
commonRefPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Common_Reference_Data/"

allODIFileWithPath <- paste(outputPath,"All_ODIs.csv",sep = "")
df <- read.csv(file = allODIFileWithPath,header = TRUE,as.is = TRUE)

#df <- head(df,2)

for (i in 1:nrow(df)){
  
  #print(df$Winner)
  #print(as.character(df$Winner))
  
  # skipping the matches where the match had NO results
  if (as.character(df$Winner[i]) %in% c("No result","Match abandoned","Match Tied","Match cancelled")){
    print(as.character(df$Winner[i]))
    next
  }
  
  print(paste("Match No : ",df$Match_Seq[i]," ::: ScoreCard : ",df$ScoreCard[i]))
  
  match_no <- as.character(df$Match_Seq[i])
  
  # read in HTML data
  tbls_xml <- readHTMLTable(as.character(df$ScoreCard[i]))
  
  # Reading Match Info
  info_df <- data.frame(tbls_xml[[7]])
  info_df <- data.frame(apply(info_df,2,function(x) gsub("[^0-9A-Za-z///' ]","",x)))
  colnames(info_df) <- c("Key","Value") 
  
  #print(info_df)
  if(length(tbls_xml)<12){
    next
  }
  
  
  # Reading Batting records
  df_Batting_Tab <- tbls_xml[[6]]
  print("stage1")
  #print(df_Batting_Tab)
  start_bat <- which(df_Batting_Tab$V4=="4s")
  end_bat <- which(df_Batting_Tab$V1=="Extras")
  if (length(start_bat)!=2 | length(end_bat)!=2)
  {
   next 
  }
  
  # Reading First Team Batting Record
  Team1_Name <- as.character(df_Batting_Tab[start_bat[1],1])
  Team1_Bat <- data.frame(apply(df_Batting_Tab[seq(start_bat[1]+1,end_bat[1]-1),],2,function(x) gsub("[^0-9A-Za-z///' ]","",x)))
  Team1_Bat <- Team1_Bat[,1:7]
  Team1_Bat$Team_Name <- Team1_Name
  colnames(Team1_Bat) <- c("batsman","dismissal","Runs","Balls","4s","6s","Strike_Rate","Team_Name")
  Team1_Bat$Strike_Rate <- as.integer(as.character(Team1_Bat$Strike_Rate))/100
  Team1_Bat <- Team1_Bat[,c("Team_Name","batsman","dismissal","Runs","Balls","4s","6s","Strike_Rate")]
  
  
  # Reading Second Team Batting Record
  Team2_Name <- as.character(df_Batting_Tab[start_bat[2],1])
  Team2_Bat <- data.frame(apply(df_Batting_Tab[seq(start_bat[2]+1,end_bat[2]-1),],2,function(x) gsub("[^0-9A-Za-z///' ]","",x)))
  Team2_Bat <- Team2_Bat[,1:7]
  Team2_Bat$Team_Name <- Team2_Name
  colnames(Team2_Bat) <- c("batsman","dismissal","Runs","Balls","4s","6s","Strike_Rate","Team_Name")
  Team2_Bat$Strike_Rate <- as.integer(as.character(Team2_Bat$Strike_Rate))/100
  Team2_Bat <- Team2_Bat[,c("Team_Name","batsman","dismissal","Runs","Balls","4s","6s","Strike_Rate")]
  
  # Reading Second Team Bowling Record
  Team2_Ball <- data.frame(tbls_xml[[10]])
  Team2_Ball <- data.frame(apply(Team2_Ball,2,function(x) gsub("[^0-9A-Za-z///' ]","",x)))
  Team2_Ball$Team_Name <- Team2_Name
  colnames(Team2_Ball) <- c("bowler","Overs","Balls","Maiden","Runs","Wickets","Economy","Extra","Team_Name")
  Team2_Ball$Economy <- as.integer(as.character(Team2_Ball$Economy))/100
  Team2_Ball <- Team2_Ball[,c("Team_Name","bowler","Overs","Balls","Maiden","Runs","Wickets","Economy")]
  
  # Reading First Team Bowling Record
  Team1_Ball <- data.frame(tbls_xml[[12]])
  Team1_Ball <- data.frame(apply(Team1_Ball,2,function(x) gsub("[^0-9A-Za-z///' ]","",x)))
  Team1_Ball$Team_Name <- Team1_Name
  colnames(Team1_Ball) <- c("bowler","Overs","Balls","Maiden","Runs","Wickets","Economy","Extra","Team_Name")
  Team1_Ball$Economy <- as.integer(as.character(Team1_Ball$Economy))/100
  Team1_Ball <- Team1_Ball[,c("Team_Name","bowler","Overs","Balls","Maiden","Runs","Wickets","Economy")]
  
  Match_Batting <- rbind(Team1_Bat,Team2_Bat)
  Match_Bowling <- rbind(Team1_Ball,Team2_Ball)
  

  
  match_info <- paste(outputPath,"Info/","Match_Info_Match_Number_",match_no,".csv",sep = "")
  batting_file_name <- paste(outputPath,"Batting/","Batting_Record_Match_Number_",match_no,".csv",sep = "")
  bowling_file_name <- paste(outputPath,"Bowling/","Bowling_Record_Match_Number_",match_no,".csv",sep = "")
  print(paste(match_no,Team1_Name,Team2_Name,sep = ":::"))
  
  
  write.csv(info_df,file = match_info,row.names = FALSE)
  write.csv(Match_Batting,file = batting_file_name,row.names = FALSE)
  write.csv(Match_Bowling,file = bowling_file_name,row.names = FALSE)
   
  
  #break
}

#print(Match_Batting)
#print(Match_Bowling)


