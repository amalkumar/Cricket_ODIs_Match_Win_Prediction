setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/2 - Model_Creation/MC_Code/")
options(warn=-1)


GetImportantVariablesForTeamAutomated <- function(teamName,onDate,BinningCriteria="No-Binning"){
  
  inputDirectoryPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/2 - Model_Creation/MC_Output/"
  teamNameDirectoryonDate <- paste(inputDirectoryPath,"MC-",teamName,"/",sep = "")
  
  fileName <- paste("MC-4-",BinningCriteria,"-AllMatchesDataFor_",teamName,"_",onDate,"_Binned",".csv",sep = "")
  inputFileName <- paste(teamNameDirectoryonDate,fileName,sep = "")
  
  if(!file.exists(inputFileName)){
    
    print("File Doesnt exists")
    stop()
    
  } else{
    
    # Reading the Data for feature selection using RFE (Recursive Feature Elimination )
    df <- read.csv(file = inputFileName,as.is=TRUE,header = TRUE)
    
    # remving rows with NA's
    df <- df[complete.cases(df),]
    
    # Getting Only the Numeric column for modeling
    nums <- sapply(df, is.numeric)
    df <- df[,nums]
    
    # Dropping Few Variables for now, may use in future
    drops <- c("Src_Match_No","Year")
    df <- df[ , !(names(df) %in% drops)]
    
    
    # Target variable
    target_variable <- paste(gsub(" ", "", teamName, fixed = TRUE),"_Win",sep = "")
    print(target_variable)
    
    print(paste("Variable before RFE : ",length(colnames(df))))
    
    
    source("Methon_FeatureSelection_Automated_RFE.R")
    impVariables <- FeatureSelection_Automated_RFE(df,target_variable)
    
    
    # Keeping only the important variables in data for modeling
    df <- df[,c(impVariables,target_variable)]
    
    print(paste("Variable after RFE : ",length(colnames(df))))
    
    
    # Saving the resulant data with Selected Variables 
    outputFileImpVariables <- paste("MC-4-",BinningCriteria,"-AllMatchesDataFor_",teamName,"_",onDate,"_Selected_Variables",".csv",sep = "")
    outputFileNameWithPathImpVariables <- paste(teamNameDirectoryonDate,outputFileImpVariables,sep = "")
    write.csv(df,file = outputFileNameWithPathImpVariables,row.names = FALSE)
    
    
    
    
    
  }
  
}