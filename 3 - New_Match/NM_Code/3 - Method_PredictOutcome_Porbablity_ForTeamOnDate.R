setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/3 - New_Match/NM_Code/")
options(warn=-1)

library(dplyr)

PredictOutcome_Probability_ForTeamOnDate <- function(newMatchDir="D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/3 - New_Match/",newMatchFile="New_Match.csv",BinningParam="3Levels"){
  
  
  # creating the file name with path
  newMatchFileWithPath <- paste(newMatchDir,newMatchFile,sep = "")
  
  if(!file.exists(newMatchFileWithPath)){
    
    print("File Doesnt exists--1")
    stop()
    
  } else{
    print(newMatchFileWithPath)
    
    # Reading the new match file for prediction
    new_df <- read.csv(file = newMatchFileWithPath,as.is=TRUE,header = TRUE)
    new_df$Date <- as.Date(new_df$Date)
    
    print(new_df)
    
    count <- 0
    for (i in 1:nrow(new_df)) {
      count <- count + 1
      print("---------------------------------------------------------------")
      print(paste("Processing New Match file record : ",count))
      
      
      # Getting the team names
      team1 <- new_df[i,"Team1"]
      team2 <- new_df[i,"Team2"]
      Match_Date <- new_df[i,"Date"]
      countryName <- new_df[i,"Country_Name"]
      
      print(paste("Team 1 : ",team1," :::: Team 2 : ",team2,sep = ""))
      print(paste("Prediction using the Model : ",BinningParam))
      
      # treating each team as main team and other as opponent
      teams <- c(team1,team2)
      for (teamName in teams){
        
        opponentTeam <- ifelse(team1==teamName,team2,team1)
        #print(paste("Team is : ",teamName," and Opponent is : ",opponentTeam))
        
        
        teamNameInputDir <- paste(newMatchDir,"MC-",teamName,"/",sep = "")
        inputFileName <- paste("MC-2-",BinningParam,"-AllMatchesDataFor_",teamName,"_Binned",".csv",sep = "")
        inputFileNameWithPath <- paste(teamNameInputDir,inputFileName,sep = "")
        
        
        inputDirectoryPathMode <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/2 - Model_Creation/MC_Output/"
        teamNameDirectoryonDateModel <- paste(inputDirectoryPathMode,"MC-",teamName,"/",sep = "")
        model_name <- paste("MC-5-",BinningParam,"-",teamName,"_final_model.rda",sep = "")
        inputModelFileNameWithPath <- paste(teamNameDirectoryonDateModel,model_name,sep = "")
        
        
        #print(inputModelFileNameWithPath)
        if(!file.exists(inputFileNameWithPath) | !file.exists(inputModelFileNameWithPath)){
          
          print("File Doesnt exists--2")
          stop()
          
        } else{
          
          
          # Loading Model
          load(inputModelFileNameWithPath)
          
          # Reading data
          df <- read.csv(file = inputFileNameWithPath,as.is=TRUE,header = TRUE)
          
          # Prediction
          pred <- predict(mod,type="response",newdata = df)
          
          print(paste("Probability for Team  :: ",teamName," :: winning Against Opponent :: ",opponentTeam," :: is = ",pred,sep = ""))
          
          
        }
        
      }
      
    }
    
  }
  
  
  
}