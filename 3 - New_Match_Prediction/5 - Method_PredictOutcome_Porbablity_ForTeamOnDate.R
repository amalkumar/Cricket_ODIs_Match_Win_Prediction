setwd("D:/Study_Material/DataAnalytics/Cricket/ODIs/3 - New_Match_Prediction/NM_Code/")



options(warn=-1)

library(reshape2)
library(dplyr)

PredictOutcome_Probability_ForTeamOnDate <- function(teamName,onDate,BinningCriteria="No-Binning"){
  
  inputDirectoryPathMode <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/2 - Model_Creation/MC_OutPut_Data/"
  teamNameDirectoryonDateModel <- paste(inputDirectoryPathMode,"MC-",teamName,"/",sep = "")

  model_name <- paste("MC-5-",BinningCriteria,"-",teamName,"_",onDate,"_final_model.rda",sep = "")
  inputModelFileNameWithPath <- paste(teamNameDirectoryonDateModel,model_name,sep = "")
  #print(inputModelFileNameWithPath)
  
  
  if(!file.exists(inputModelFileNameWithPath)){
    
    print("Model File Doesnt exists")
    stop()
    
  } else{
    
    #onDate <- "2017-02-19"
    
    inputDirectoryPath <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/3 - New_Match_Prediction/NM_OutPut_Data/"
    teamNameDirectoryonDate <- paste(inputDirectoryPath,"MC-",teamName,"/",sep = "")

    InputFileName <- paste("NM-4-NewMatch_",BinningCriteria,"_",teamName,".csv",sep = "")
    InputFileNameWithPath <- paste(teamNameDirectoryonDate,InputFileName,sep = "")
    
    #print(InputFileNameWithPath)
    
    if(!file.exists(InputFileNameWithPath)){
      
      print("Input File Doesnt exists")
      stop()
      
    } else {
      
      load(inputModelFileNameWithPath)
      
      df <- read.csv(file = InputFileNameWithPath,as.is=TRUE,header = TRUE)
      
      
      pred <- predict(mod,type="response",newdata = df)
      
      print(paste("Match Date - ",df$Match_Date,"  ::: Team Name - ",teamName,"  ::: Winning Probablity - ",pred,sep = ""))
      
    }
    
    
    
    
    
  }
  
  
}
