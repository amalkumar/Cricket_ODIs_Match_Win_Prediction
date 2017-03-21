setwd("D:/Study_Material/DataAnalytics/Cricket/ODIs/2 - Model_Creation/MC_Code/")



options(warn=-1)

library(reshape2)
library(dplyr)
library(yaml)

RemoveCorrelatedVarsAndCombinedVars <- function(teamName,onDate){
  
  inputDirectoryPath <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/2 - Model_Creation/MC_OutPut_Data/"
  teamNameDirectoryonDate <- paste(inputDirectoryPath,"MC-",teamName,"/",sep = "")
  
  fileName <- paste("MC-2-AllMatchesDataFor_",teamName,"_",onDate,"_Aggregated",".csv",sep = "")
  inputFileName <- paste(teamNameDirectoryonDate,fileName,sep = "")
  
  
  if(!file.exists(inputFileName)){
    
    print("Input File Doesnt exists")
    stop()
    
  } else{
    # Reading aggregated data file
    df <- read.csv(file = inputFileName,as.is=TRUE,header = TRUE)
    
    #print(length(names(df)))
    
    # Getting the index of all the numeric columns
    nums <- sapply(df, is.numeric)
    
    df_Num <- df[,nums]
    df_Cat <- df[,!nums]
    print(paste("before Processing", " df_Num Vars : ",length(names(df_Num)),":::df_Cat Vars : ",length(names(df_Cat)),sep = ""))
    #print(names(df_Num))
    
    # Reading file containing Only relevant variables after removing the correlated variables
    teamNameSpaceRemoved <- gsub(" ", "", teamName, fixed = TRUE)
    directoryPathImpVars <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/2 - Model_Creation/MC_Reference_Data/"
    impVarsfileName <- paste(teamNameSpaceRemoved,"_Variables",".csv",sep = "")
    impVarsinputFileName <- paste(directoryPathImpVars,impVarsfileName,sep = "")
    
    
    
    
    if(!file.exists(impVarsinputFileName)){
      
      print("Variables File Doesnt exists")
      var_to_keep <- names(df_Num)
       
      
    } else{
      print("Variables File exists")
      df_imp_vars <- read.csv(file = impVarsinputFileName,as.is=TRUE,header = FALSE)
      var_to_keep <- df_imp_vars[,1]
      
      
    }
    # Keeping only the relevant numeric variables
    df_Num <- df_Num[,(names(df_Num) %in% var_to_keep)]
    

    print(paste("After Processing", " df_Num Vars : ",length(names(df_Num)),":::df_Cat Vars : ",length(names(df_Cat)),sep = ""))
    #print(names(df_Num))
    
    
    
    
    df_result <- cbind(df_Cat, df_Num)
    
    #print(length(names(df_result)))
    #print(head(df_result,2))
    
    outputFileName <- paste("MC-3-AllMatchesDataFor_",teamName,"_",onDate,"_RemovedCorrelatedVars",".csv",sep = "")
    outputFileNameWithPath <- paste(teamNameDirectoryonDate,outputFileName,sep = "")
    write.csv(df_result,file = outputFileNameWithPath,row.names = FALSE)
    
  }
  
}