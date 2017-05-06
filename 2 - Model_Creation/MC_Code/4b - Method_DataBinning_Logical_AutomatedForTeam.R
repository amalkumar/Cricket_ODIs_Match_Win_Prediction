setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/2 - Model_Creation/MC_Code/")
options(warn=-1)

library(reshape2)
library(dplyr)

source("Method_Bin_20Perc_DF_Continuous_Variables.R")
source("Method_Bin_3Levels_DF_Continuous_Variables.R")

DataBinning_Logical_AutomatedForTeam <- function(teamName,onDate,BinningParam="3Levels"){
  
  inputDirectoryPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/2 - Model_Creation/MC_Output/"
  teamNameDirectoryonDate <- paste(inputDirectoryPath,"MC-",teamName,"/",sep = "")
  
  
  fileName <- paste("MC-4-No-Binning-AllMatchesDataFor_",teamName,"_",onDate,"_Binned",".csv",sep = "")
  inputFileName <- paste(teamNameDirectoryonDate,fileName,sep = "")
  print(inputFileName)

  if(!file.exists(inputFileName)){
    
    print("File Doesnt exists")
    stop()
    
  } else{
    # Target variable
    win_var <- paste(gsub(" ", "", teamName, fixed = TRUE),"_Win",sep = "")
    
    df <- read.csv(file = inputFileName,as.is=TRUE,header = TRUE)
    
    # Dropping Few Extra Numerical Variables , not required
    drops <- c("Src_Match_No","Year")
    df <- df[ , !(names(df) %in% drops)]
    
    # Getting the index of all the numeric columns
    nums <- sapply(df, is.numeric)
    
    df_Num <- df[,nums]
    df_Cat <- df[,!nums]
    
    # Getting index of every Numeric column to seperate INT and NUM columns
    index <- col(df_Num)
    index <- index[1,]
    
    # Get the Target variable index
    win_var_index <- which( colnames(df_Num)==win_var )
    
    ctt <- 0
    print(paste("Total Number of columns Before binning : ",length(names(df))))
    for (i in 1:length(names(df_Num))){
      var_name <- colnames(df_Num)[index[i]]
      if(!(win_var==var_name | "Year"==var_name | "Country_Id"==var_name | "OpponentTeam_Id"==var_name)){
        if(BinningParam=="20Perc"){
          ctt <- ctt + 1
          print(paste("Binning Param : ",BinningParam," :::: Variable Name : ",var_name," :::: Target Variable Name : ",win_var))
          df_Num <- Bin_DF_20Perc_Continuous_Variables(df_Num,var_name,win_var,teamNameDirectoryonDate)
          
          
        } else if (BinningParam=="3Levels") {
          ctt <- ctt + 1
          print(paste("Binning Param : ",BinningParam," :::: Variable Name : ",var_name," :::: Target Variable Name : ",win_var))
          df_Num <- Bin_DF_3Levels_Continuous_Variables(df_Num,var_name,win_var,teamNameDirectoryonDate)
          
        } else{
          print("Binning Parameter Not Correct")
          stop()
        }
        
      }
      
    }
    
    
    
    #print("----Cat variables--")
    #print(head(df_Cat,2))
    print("-----------") 
    
 
    
    
    
    
    
    print(paste("Total Numeric variable : ",ctt, sep = ""))
    print("::::::::::::::::::::::::::::::::::")
    #print(head(df_Num,2))
    
    
    
    #Taking out only binned variables
    dummy_var_list <- grep("dummy_", names(df_Num), value = TRUE)
    df_Num <- df_Num[,which(colnames(df_Num) %in% c(dummy_var_list,win_var,"Country_Id","OpponentTeam_Id"))]
    
    
    
    
    
    # Combining Both Categorical DF and the Num DF with Binned values
    df_result <- cbind(df_Cat, df_Num)
    print(paste("Total Number of columns After binning : ",length(names(df_result))))
    
    
    # Re-arrange the columns and taking out the relavant ones
    #df_result <- df_result[,c(3,	6,	7,	10,	19,	27,	20,	28,	11,	15,	23,	16,	24,	17,	25,	18,	26,	13,	21,	14,	22,	35,	36,	37,	38,	39,	40,	41,	42,	43,	44,	45,	46,	31,	30,	29,	32,	33,	34,	12)]
    
    #print(colSums(is.na(df_result)))
    
    df_result <- na.omit(df_result)
    
    outputFileName <- paste("MC-4-",BinningParam,"-AllMatchesDataFor_",teamName,"_",onDate,"_Binned",".csv",sep = "")
    outputFileNameWithPath <- paste(teamNameDirectoryonDate,outputFileName,sep = "")
    write.csv(df_result,file = outputFileNameWithPath,row.names = FALSE)
    
    
  }

  
  
}

