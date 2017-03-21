setwd("D:/Study_Material/DataAnalytics/Cricket/ODIs/3 - New_Match_Prediction/NM_Code/")
options(warn=-1)

library(reshape2)
library(dplyr)
library(sqldf)

DataBinning_Logical_AutomatedForTeam <- function(teamName,onDate,BinningParam="3Levels"){
  
  inputDirectoryPath <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/3 - New_Match_Prediction/NM_OutPut_Data/"
  teamNameDirectoryonDate <- paste(inputDirectoryPath,"MC-",teamName,"/",sep = "")
  
  fileName <- paste("NM-4-NewMatch_No-Binning_",teamName,".csv",sep = "")
  inputFileName <- paste(teamNameDirectoryonDate,fileName,sep = "")
  print(inputFileName)
  
  if(!file.exists(inputFileName)){
    
    print("File Doesnt exists")
    stop()
    
  } else{
    
    df <- read.csv(file = inputFileName,as.is=TRUE,header = TRUE)
    
    # Getting the index of all the numeric columns
    nums <- sapply(df, is.numeric)
    
    df_Num <- df[,nums]
    df_Cat <- df[,!nums]
    
    # Getting index of every Numeric column to seperate INT and NUM columns
    index <- col(df_Num)
    index <- index[1,]
    
    ctt <- 0
    print(paste("Total Number of columns Before binning : ",length(names(df_Num))))
    
    
    df_Num_res <- df_Num
    for (i in 1:length(names(df_Num))){
      var_name <- colnames(df_Num)[index[i]]
      if(!("Year"==var_name | "Country_Id"==var_name | "OpponentTeam_Id"==var_name)){
        if(BinningParam=="20Perc" | BinningParam=="3Levels"){
          ctt <- ctt + 1
          
          # Reading the rule files for binning the new data set using the same rule used for modelling
          rulesInputDirectoryPath <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/2 - Model_Creation/MC_OutPut_Data/"
          rulesDirectoryonDate <- paste(rulesInputDirectoryPath,"MC-",teamName,"/",sep = "")
          Binning_Bin_Rules <- paste(rulesDirectoryonDate,BinningParam,"_Bin_Rules/",sep = "")
          RuleFileName <- paste(Binning_Bin_Rules,var_name,".csv",sep = "")
          
          if(!file.exists(RuleFileName)){
            
            print(paste("Rule File Doesnt exists for Variable : ",var_name,sep = ""))
            stop()
            
          } else{
            print("------------")
            df_rule <- read.csv(file = RuleFileName,as.is=TRUE,header = TRUE)
            #print(head(df_rule,2))
            #print(paste("Binning Param : ",BinningParam," :::: Variable Name : ",var_name))
            query <- paste("select t2.",var_name,",(select t1.level from df_rule as t1 where t2.",var_name," BETWEEN t1.GreaterThan AND t1.LessThan) as Level  from df_Num as t2",sep="")
            
            df_Bin <- sqldf(query)
            if(is.na(df_Bin$Level)){
              print("ITS NULL")
              query <- paste("select t2.",var_name,",(select t1.level from df_rule as t1 where round(t2.",var_name,",1) BETWEEN t1.GreaterThan AND t1.LessThan) as Level  from df_Num as t2",sep="")
              df_Bin <- sqldf(query)
            }
            df_Bin<- df_Bin$Level
            print(head(df_Bin,2))
            
            df_Num_res <- cbind(df_Num_res,df_Bin)
            dummy_var_name <- paste("dummy_",var_name,sep = "")
            names(df_Num_res)[which(colnames(df_Num_res)=="df_Bin")] <- dummy_var_name
            
            
          }
          
        } else{
          print("Binning Parameter Not Correct")
          stop()
        }
        
      }
      
    }
    
    #Taking out only binned variables
    dummy_var_list <- grep("dummy_", names(df_Num_res), value = TRUE)
    df_Num_res <- df_Num_res[,which(colnames(df_Num_res) %in% c(dummy_var_list,"Country_Id","OpponentTeam_Id"))]
    print(length(names(df_Num_res)))
    #print(head(df_Num_res,2))
    

    
    # Combining Both Categorical DF and the Num DF with Binned values
    df_result <- cbind(df_Cat, df_Num_res)
    
    
    
    outputFileName <- paste("NM-4-NewMatch_",BinningParam,"_",teamName,".csv",sep = "")
    outputFileNameWithPath <- paste(teamNameDirectoryonDate,outputFileName,sep = "")
    write.csv(df_result,file = outputFileNameWithPath,row.names = FALSE)
    
    
  }
  
}