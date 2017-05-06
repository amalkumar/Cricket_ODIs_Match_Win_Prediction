setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/1 - Data_Creation/DC_Code/")


commonRefDataPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Common_Reference_Data/"
webScrappedDataPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/0 - ScrapWebData/Full_Load/"
outputDataPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/1 - Data_Creation/DC_Output/"


# reading all ODI Match Winning Record csv file
allMatchFile <- paste(outputDataPath,"DC-1-TeamWinningRecord.csv",sep = "")
df_All_Matches_Win_Rec <- read.csv(file = allMatchFile,header = TRUE,as.is = TRUE)

# declaring empty data frame
df_result <- data_frame()
df_result <- data.frame(Match_Number=integer(0),Match_Date=character(0),bowling_team=character(0),	bowler=character(0),	Balls=integer(0),	Runs=integer(0),	Wickets=integer(0), Economy=numeric(0))

# total matches
totMatches <- nrow(df_All_Matches_Win_Rec)

# Iterating through all the matches win records
for (i in 1:nrow(df_All_Matches_Win_Rec)){

  print(paste("Number of Matches Pending for processing : ",(totMatches-i),sep = ""))
  
  # reading columnss from winning records file
  match_no <- as.integer(df_All_Matches_Win_Rec$Match_Number[i])
  match_date <- as.Date(df_All_Matches_Win_Rec$Match_Date[i])
  team1 <- df_All_Matches_Win_Rec$Team1[i]
  team2 <- df_All_Matches_Win_Rec$Team2[i]
  
  # bowling file name for each record
  bowling_file_name <- paste(webScrappedDataPath,"Bowling/","Bowling_Record_Match_Number_",match_no,".csv",sep = "")
  print(bowling_file_name)
  
  # raise error if batting file doesnt exists
  if(!file.exists(bowling_file_name)){
    print("Batting File Doesnt exists")
    stop()
  }

  # reading batting file csv for each match
  df_bowling_file <- read.csv(file = bowling_file_name,header = TRUE,as.is = TRUE)
  
  # Subsituting NA values to 0 for Balls
  df_bowling_file$Balls[is.na(df_bowling_file$Balls)] <- 0 
  
  # Calc Total Balls
  df_bowling_file$Total_Balls <- df_bowling_file$Overs*6 + df_bowling_file$Balls
  
  # adding columnss from winning records file
  df_bowling_file$Match_Number <- match_no
  df_bowling_file$Match_Date <- match_date
  df_bowling_file$bowling_team <- ifelse(grepl(team1, df_bowling_file$Team_Name),team1,team2)
  
  # selecting the desired columns
  df_bowling_file <- df_bowling_file[,c("Match_Number","Match_Date","bowling_team",	"bowler",	"Total_Balls",	"Runs",	"Wickets","Economy")]
  
  colnames(df_bowling_file) <- c("Match_Number","Match_Date","bowling_team",	"bowler",	"Balls",	"Runs",	"Wickets","Economy")
  
  
  df_result <- rbind(df_result,df_bowling_file)

  
  #break
  
}

print("All the batting file processed")
#print(head(df_result))

# saving final data frame into csv
outputFileName <- paste(outputDataPath,"DC-3-bowling_record.csv",sep = "")
write.csv(df_result,file = outputFileName,row.names = FALSE)




