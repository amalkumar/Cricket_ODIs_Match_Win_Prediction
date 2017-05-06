setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/3 - New_Match/NM_Code/")
options(warn=-1)

library(dplyr)

No_Binning_AutomatedForTeam <- function(newMatchDir="D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/3 - New_Match/",newMatchFile="New_Match.csv"){
  
  
  # creating the file name with path
  newMatchFileWithPath <- paste(newMatchDir,newMatchFile,sep = "")
  
  if(!file.exists(newMatchFileWithPath)){
    
    print("File Doesnt exists")
    stop()
    
  } else{
    print(newMatchFileWithPath)
    
    # Reading the new match file for prediction
    new_df <- read.csv(file = newMatchFileWithPath,as.is=TRUE,header = TRUE)
    new_df$Date <- as.Date(new_df$Date)
    
    count <- 0
    for (i in 1:nrow(new_df)) {
      count <- count + 1
      
      print(paste("Processing New Match file record : ",count))
      
      # Getting the team names
      team1 <- new_df[i,"Team1"]
      team2 <- new_df[i,"Team2"]
      Match_Date <- new_df[i,"Date"]
      countryName <- new_df[i,"Country_Name"]
      
      print(paste("Team 1 : ",team1," :::: Team 2 : ",team2,sep = ""))
      
      # treating each team as main team and other as opponent
      teams <- c(team1,team2)
      for (teamName in teams){
        
        opponentTeam <- ifelse(team1==teamName,team2,team1)
        print(paste("Team is : ",teamName," and Opponent is : ",opponentTeam))
        
        teamNameInputDir <- paste(newMatchDir,"MC-",teamName,"/",sep = "")
        inputFileName <- paste("MC-1-AllMatchesDataFor_",teamName,"_Aggregated",".csv",sep = "")
        inputFileNameWithPath <- paste(teamNameInputDir,inputFileName,sep = "")
        
        if(!file.exists(inputFileNameWithPath)){
          
          print("File Doesnt exists")
          stop()
          
        } else{
          print(inputFileNameWithPath)
          
          # Reading aggregated data
          df <- read.csv(file = inputFileNameWithPath,as.is=TRUE,header = TRUE)
          
          rulesInputDirectoryPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/2 - Model_Creation/MC_Output/"
          rulesDirForTeam <- paste(rulesInputDirectoryPath,"MC-",teamName,"/",sep = "")
          No_Binning_Bin_Rules <- paste(rulesDirForTeam,"No-Binning_Bin_Rules/",sep = "")
          
          # Updating Country ID for Country Name
          countryNameRuleFileName <- paste(No_Binning_Bin_Rules,"Country_Name.csv",sep = "")
          df_country <- read.csv(file = countryNameRuleFileName,as.is=TRUE,header = TRUE)
          df <- merge(df,df_country)
          
          # removing Country Name column
          country_index <- colnames(df)=="Country_Name"
          df <- df[,!country_index]
          
          # Updating Country ID for Country Name
          opponentNameRuleFileName <- paste(No_Binning_Bin_Rules,"OpponentTeam.csv",sep = "")
          df_OpponentTeam <- read.csv(file = opponentNameRuleFileName,as.is=TRUE,header = TRUE)
          
          # Merging with country
          df <- merge(df,df_OpponentTeam)
          
          # removing Country Name column
          OpponentTeam_index <- colnames(df)=="OpponentTeam"
          df <- df[,!OpponentTeam_index]
          
          # Getting the index of all the numeric columns
          nums <- sapply(df, is.numeric)
          df_Num <- df[,nums]
          
          teamNameOutPutDir <- paste(newMatchDir,"MC-",teamName,"/",sep = "")
          outputFileName <- paste("MC-2-No-Binning-AllMatchesDataFor_",teamName,"_Binned",".csv",sep = "")
          outputFileNameWithPath <- paste(teamNameOutPutDir,outputFileName,sep = "")
          write.csv(df_Num,file = outputFileNameWithPath,row.names = FALSE)
          
          
          
          
        }
        
      }
      
    }
    
  }
  
  
  
}