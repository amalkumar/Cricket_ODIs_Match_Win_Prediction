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
  bowling_record <- data.frame()
  #match_date <- data.frame()
  venue <- data.frame()
  city <- data.frame()
  
  ct <- ct + 1
  print(paste("Number of Files Pending for Processing : ",as.character(count)))
  print(paste("Currently Processing : ",file))
  
  fileFullPath <- paste(sourceFileDirectory,file,sep = "")
  
  #print("step1")
  
  df <- read.csv(fileFullPath,row.names=NULL,col.names=c("ball","inning_no","over_n_ball","batting_team","batsman","non_striker","bowler","runs_of_bat","extras","kind_of_wicket","Dismissed_player"))
  
  #print("step2")
  
  
  df_info <- sqldf("select * from df where ball='info'")
  colnames(df_info) <- c("info","key","value","NA1","NA2","NA3","NA4","NA5","NA6","NA7","NA8")
  
  #print("step3")
  
  df_innings <- sqldf("select * from df where ball='ball'")
  
  team1 <- (df_info %>% filter(key=="team"))[1,]["value"]
  team2 <- (df_info %>% filter(key=="team"))[2,]["value"]
  
  df_innings$bowling_team <- ifelse(df_innings$batting_team==as.character(team1[1,]),as.character(team2[1,]),as.character(team1[1,]))
  df_bowling_record <- sqldf("select bowling_team,bowler, count(over_n_ball) as Balls,sum(runs_of_bat+extras) as Runs from df_innings group by bowling_team,bowler")
  
  wicket_record <- sqldf("select bowling_team,bowler,count(1) as Wickets from df_innings where length(Dismissed_player)>0 group by bowling_team,bowler")
  
  bowling_record <- merge(x = df_bowling_record, y = wicket_record, all.x = TRUE)
  
  match_date <- sqldf("select value from df_info where key='date'")
  bowling_record$Match_Date <- rep(as.character(match_date$value[1]),nrow(bowling_record))
  
  season <- sqldf("select value from df_info where key='season'")
  bowling_record$Season <- rep(as.integer(substr(as.character(season$value[1]),1,4)),nrow(bowling_record))
  
  #print(class(bowling_record$Season))
  
  bowling_record <- bowling_record[order(rev(bowling_record[,7]), rev(bowling_record[,6]), rev(bowling_record[,1])),]
  
  bowling_record$Match_Number <- rep(as.character(ct),nrow(bowling_record))
  
  datalist[[ct]] <- bowling_record
  count <- count - 1
  
}

big_data = do.call(rbind, datalist)


big_data <- big_data[,c(8,7,6,1,2,3,4,5)]
outputFileName <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/1 - Data_Creation/DC_Output/DC-4-bowling_record.csv"
write.csv(big_data,file = outputFileName,row.names = FALSE)
