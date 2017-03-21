setwd("D:/Study_Material/DataAnalytics/Cricket/ODIs/2 - Model_Creation/MC_Code/")
options(warn=-1)

library(reshape2)
library(dplyr)
library(yaml)
library(ggplot2)
library(rpart)



No_Binning_AutomatedForTeam <- function(teamName,onDate){
  
  inputDirectoryPath <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/2 - Model_Creation/MC_OutPut_Data/"
  teamNameDirectoryonDate <- paste(inputDirectoryPath,"MC-",teamName,"/",sep = "")
  
  fileName <- paste("MC-3-AllMatchesDataFor_",teamName,"_",onDate,"_RemovedCorrelatedVars",".csv",sep = "")
  inputFileName <- paste(teamNameDirectoryonDate,fileName,sep = "")
  
  
  No_Binning_Bin_Rules <- paste(teamNameDirectoryonDate,"No-Binning_Bin_Rules/",sep = "")
  dir.create(path = No_Binning_Bin_Rules,showWarnings = FALSE)
  
  
  if(!file.exists(inputFileName)){
    
    print("File Doesnt exists")
    stop()
    
  } else{
    
    df <- read.csv(file = inputFileName,as.is=TRUE,header = TRUE)
    
    # Creating country name ids
    df_country <- data.frame(unique(df$Country_Name))
    df_country$Id <- rownames(df_country)
    colnames(df_country) <- c("Country_Name","Country_Id")
    
    # Saving the countr Id
    outputFileNameWithPath <- paste(No_Binning_Bin_Rules,"Country_Name.csv",sep = "")
    write.csv(df_country,file = outputFileNameWithPath,row.names = FALSE)
    
    # Merging with country
    df <- merge(df,df_country)
    
    # removing Country Name column
    country_index <- colnames(df)=="Country_Name"
    df <- df[,!country_index]
    
    
    # Creating Opponent Team name ids
    df_OpponentTeam <- data.frame(unique(df$OpponentTeam))
    df_OpponentTeam$Id <- rownames(df_OpponentTeam)
    colnames(df_OpponentTeam) <- c("OpponentTeam","OpponentTeam_Id")
    
    # Saving the countr Id
    outputFileNameWithPath <- paste(No_Binning_Bin_Rules,"OpponentTeam.csv",sep = "")
    write.csv(df_OpponentTeam,file = outputFileNameWithPath,row.names = FALSE)
    
    # Merging with country
    df <- merge(df,df_OpponentTeam)
    
    # removing Country Name column
    OpponentTeam_index <- colnames(df)=="OpponentTeam"
    df <- df[,!OpponentTeam_index]
    
    #df <- df[,c(4,	7,	8,	46,	17,	25,	18,	26,	47,	13,	21,	14,	22,	15,	23,	16,	24,	11,	19,	12,	20,	34,	35,	36,	37,	38,	39,	40,	41,	42,	43,	44,	45,	29,	28,	27,	30,	31,	32,	33)]
    
    outputFileName <- paste("MC-4-No-Binning-AllMatchesDataFor_",teamName,"_",onDate,"_Binned",".csv",sep = "")
    outputFileNameWithPath <- paste(teamNameDirectoryonDate,outputFileName,sep = "")
    write.csv(df,file = outputFileNameWithPath,row.names = FALSE)
    
  }
}