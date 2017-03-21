setwd("D:/Study_Material/DataAnalytics/Cricket/ODIs/1 - Data_Creation/DC_Code/")

options(warn=-1)

library(reshape2)
library(dplyr)
library(sqldf)
library(Jmisc)

sourceFileDirectory <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/Common_Reference_Data/odi_csv_male/"
#sourceFileDirectory <- "D:/Study_Material/DataAnalytics/Cricket/Generic/Common_Reference_Data/Sample/"

listOfFiles <- list.files(sourceFileDirectory)
count <- length(listOfFiles)

datalist = list()
ct <- 0

for (file in listOfFiles){
  df <- data.frame()
  df_info <- data.frame()
  df_innings <- data.frame()
  batting_record <- data.frame()
  #match_date <- data.frame()
  venue <- data.frame()
  city <- data.frame()
  
  ct <- ct + 1
  print(paste("Number of Files Pending for Processing : ",as.character(count)))
  print(paste("Currently Processing : ",file))
  
  fileFullPath <- paste(sourceFileDirectory,file,sep = "")
  
  print("step1")
  
  df <- read.csv(fileFullPath,row.names=NULL,col.names=c("ball","inning_no","over_n_ball","batting_team","batsman","non_striker","bowler","runs_of_bat","extras","kind_of_wicket","Dismissed_player"))
  
  print("step2")
  
  
  df_info <- sqldf("select * from df where ball='info'")
  colnames(df_info) <- c("info","key","value","NA1","NA2","NA3","NA4","NA5","NA6","NA7","NA8")
  
  #print("step3")
  
  df_innings <- sqldf("select * from df where ball='ball'")
  
  player_dismissed <- sqldf("select Dismissed_player,count(1) as Got_Out from df_innings where TRIM(Dismissed_player)>5 group by Dismissed_player")
  
  #print("step4")
  
  batting_record <- sqldf("select batting_team,batsman, count(batsman) as Balls,sum(runs_of_bat) as Runs,avg(runs_of_bat) as Strike_Rate from df_innings group by batting_team,batsman")
  
  
  batting_record <- merge(x = batting_record, y = player_dismissed, by.x = "batsman",by.y = "Dismissed_player", all.x = TRUE)
  
  
  
  
  
  #print("step5")
  
  match_date <- sqldf("select value from df_info where key='date'")
  #batting_record <- addCol(batting_record,Match_Date=as.character(match_date$value))
  batting_record$Match_Date <- rep(as.character(match_date$value[1]),nrow(batting_record))
  
  season <- sqldf("select value from df_info where key='season'")
  #batting_record <- addCol(batting_record,Match_Date=as.character(match_date$value))
  batting_record$Season <- rep(as.integer(substr(as.character(season$value[1]),1,4)),nrow(batting_record))
  
  
  batting_record <- batting_record[order(batting_record[,8], batting_record[,7], batting_record[,2]),]
  
  #print("step6")
  
  #venue <- sqldf("select value from df_info where key='venue'")
  #batting_record <- addCol(batting_record,Venue=as.character(venue$value))
  #batting_record$Venue <- rep(as.character(venue$value[1]),nrow(batting_record))
  
  #print("step6")
  
  #city <- sqldf("select value from df_info where key='city'")
  #batting_record <- addCol(batting_record,City=as.character(city$value))
  #batting_record$City <- rep(as.character(city$value[1]),nrow(batting_record))
  
  batting_record$Match_Number <- rep(as.character(ct),nrow(batting_record))
  
  
  datalist[[ct]] <- batting_record
  
  count <- count - 1
  
  #print("step7")
  
  
  
  
  
  
  
}

big_data = do.call(rbind, datalist)
big_data <- big_data[,c(9,8,7,2,1,3,4,5,6)]

outputFileName <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/1 - Data_Creation/DC_Output/DC-3-batting_record.csv"
write.csv(big_data,file = outputFileName,row.names = FALSE)


