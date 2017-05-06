setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/1 - Data_Creation/DC_Code/")


commonRefDataPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Common_Reference_Data/"
webScrappedDataPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/0 - ScrapWebData/Full_Load/"
outputDataPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/1 - Data_Creation/DC_Output/"


# reading all ODI Match csv file
allMatchFile <- paste(webScrappedDataPath,"All_ODIs.csv",sep = "")
df_All_Matches <- read.csv(file = allMatchFile,header = TRUE,as.is = TRUE)

# removing matches with No results
df_All_Matches <- df_All_Matches[!(df_All_Matches$Winner%in% c("No result","Match abandoned","Match Tied","Match cancelled")), ]

# adding new column "Batting First"
df_All_Matches$Batting_First <- ifelse(grepl("wicket", df_All_Matches$Win_Style),ifelse(df_All_Matches$Team1==df_All_Matches$Winner,df_All_Matches$Team2,df_All_Matches$Team1),ifelse(grepl("run", df_All_Matches$Win_Style),ifelse(df_All_Matches$Team1==df_All_Matches$Winner,df_All_Matches$Team1,df_All_Matches$Team2),"NA"))

# removing matches with Batting First as NA
df_All_Matches <- df_All_Matches[!(df_All_Matches$Batting_First=="NA"), ]

# adding new column "Bowling First"
df_All_Matches$Bowling_First <- ifelse(df_All_Matches$Team1==df_All_Matches$Batting_First,df_All_Matches$Team2,df_All_Matches$Team1)

# adding new column "Looser"
df_All_Matches$Looser <- ifelse(df_All_Matches$Team1==df_All_Matches$Winner,df_All_Matches$Team2,df_All_Matches$Team1)


# reading Venue COuntry Mapping file
inputFileNameCountryName <- paste(commonRefDataPath,"venue_country_mapping.csv",sep = "")
df_venue_country <- read.csv(file = inputFileNameCountryName,as.is=TRUE,header = TRUE)

# Adding the column Country_Name, updating on basis of venue
for (i in 1:nrow(df_All_Matches)){
  vn <- df_All_Matches[i,"Ground"]
  if (vn %in% df_venue_country$Venue){
    Venue <- (df_venue_country[df_venue_country$Venue==vn,])$Country_Name
    df_All_Matches$Country_Name[i] <- Venue
  }
  
}

# selecting columns for final data frame
df_All_Matches <- df_All_Matches[,c("Match_Seq","Date","Country_Name","Ground","Team1","Team2","Batting_First","Bowling_First","Winner","Looser")]

# updating column names of final data frame
colnames(df_All_Matches) <- c("Match_Number","Match_Date","Country_Name","Venue","Team1","Team2","Batting_First","Bowling_First","Winner","Looser")

# saving final data frame into csv
outputFileName <- paste(outputDataPath,"DC-1-TeamWinningRecord.csv",sep = "")
write.csv(df_All_Matches,file = outputFileName,row.names = FALSE)


