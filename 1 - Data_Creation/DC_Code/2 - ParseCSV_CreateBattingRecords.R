setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/1 - Data_Creation/DC_Code/")


commonRefDataPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Common_Reference_Data/"
webScrappedDataPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/0 - ScrapWebData/Full_Load/"
outputDataPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/1 - Data_Creation/DC_Output/"


# reading all ODI Match Winning Record csv file
allMatchFile <- paste(outputDataPath,"DC-1-TeamWinningRecord.csv",sep = "")
df_All_Matches_Win_Rec <- read.csv(file = allMatchFile,header = TRUE,as.is = TRUE)


# declaring empty data frame
df_result <- data_frame()
df_result <- data.frame(Match_Number=integer(0),Match_Date=character(0),batting_team=character(0),	batsman=character(0),	Balls=integer(0),	Runs=integer(0),	Strike_Rate=numeric(0),Fours=integer(0),Sixes=integer(0),	Got_Out=integer(0))

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
  
  # batting file name for each record
  batting_file_name <- paste(webScrappedDataPath,"Batting/","Batting_Record_Match_Number_",match_no,".csv",sep = "")
  #print(batting_file_name)
  
  
  # raise error if batting file doesnt exists
  if(!file.exists(batting_file_name)){
    print("Batting File Doesnt exists")
    stop()
  }
  
  # reading batting file csv for each match
  df_batting_file <- read.csv(file = batting_file_name,header = TRUE,as.is = TRUE)
  
  # removing players name who did not get batting
  df_batting_file <- df_batting_file[!(nchar(gsub("^\\s+|\\s+$", "", df_batting_file$dismissal))==0),]
  
  # adding a column to flag if batsman got out
  df_batting_file$Got_Out <- ifelse(gsub("^\\s+|\\s+$", "", df_batting_file$dismissal)!="not out",1,NA)
  
  # adding columnss from winning records file
  df_batting_file$Match_Number <- match_no
  df_batting_file$Match_Date <- match_date
  df_batting_file$batting_team <- ifelse(grepl(team1, df_batting_file$Team_Name),team1,team2)
  
  # selecting the desired columns
  df_batting_file <- df_batting_file[,c("Match_Number","Match_Date","batting_team",	"batsman",	"Balls",	"Runs",	"Strike_Rate","X4s","X6s",	"Got_Out")]
  
  colnames(df_batting_file) <- c("Match_Number","Match_Date","batting_team",	"batsman",	"Balls",	"Runs",	"Strike_Rate","Fours","Sixes",	"Got_Out")
  
  df_result <- rbind(df_result,df_batting_file)
  
  #break
  
  
}

print("All the batting file processed")
#print(head(df_result))

# saving final data frame into csv
outputFileName <- paste(outputDataPath,"DC-2-batting_record.csv",sep = "")
write.csv(df_result,file = outputFileName,row.names = FALSE)


