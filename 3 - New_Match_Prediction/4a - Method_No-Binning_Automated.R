setwd("D:/Study_Material/DataAnalytics/Cricket/ODIs/3 - New_Match_Prediction/NM_Code/")
options(warn=-1)

library(reshape2)
library(dplyr)
library(yaml)
library(ggplot2)
library(rpart)



No_Binning_AutomatedForTeam <- function(teamName,onDate){
  
  inputDirectoryPath <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/3 - New_Match_Prediction/NM_OutPut_Data/"
  teamNameDirectoryonDate <- paste(inputDirectoryPath,"MC-",teamName,"/",sep = "")
  
  fileName <- paste("NM-3-NewMatch_RemovedCorrelatedVars_",teamName,".csv",sep = "")
  inputFileName <- paste(teamNameDirectoryonDate,fileName,sep = "")
  print(inputFileName)
  
  if(!file.exists(inputFileName)){
    
    print("File Doesnt exists")
    stop()
    
  } else{
    
    df <- read.csv(file = inputFileName,as.is=TRUE,header = TRUE)
    
    
    rulesInputDirectoryPath <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/2 - Model_Creation/MC_OutPut_Data/"
    rulesDirectoryonDate <- paste(rulesInputDirectoryPath,"MC-",teamName,"/",sep = "")
    No_Binning_Bin_Rules <- paste(rulesDirectoryonDate,"No-Binning_Bin_Rules/",sep = "")
    
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
    
    outputFileName <- paste("NM-4-NewMatch_No-Binning_",teamName,".csv",sep = "")
    outputFileNameWithPath <- paste(teamNameDirectoryonDate,outputFileName,sep = "")
    write.csv(df,file = outputFileNameWithPath,row.names = FALSE)
    
   
  }
}