setwd("D:/Study_Material/DataAnalytics/Cricket/ODIs/2 - Model_Creation/MC_Code/")
options(warn=-1)

library(reshape2)
library(dplyr)
library(sqldf)
library(ggplot2)
library(car)
library(caret)
library("e1071")


CreateModelForTeam_Automated <- function(teamName,onDate,BinningCriteria="No-Binning"){
  
  inputDirectoryPath <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/2 - Model_Creation/MC_OutPut_Data/"
  teamNameDirectoryonDate <- paste(inputDirectoryPath,"MC-",teamName,"/",sep = "")
  
  fileName <- paste("MC-4-",BinningCriteria,"-AllMatchesDataFor_",teamName,"_",onDate,"_Binned",".csv",sep = "")
  inputFileName <- paste(teamNameDirectoryonDate,fileName,sep = "")
  
  if(!file.exists(inputFileName)){
    
    print("File Doesnt exists")
    stop()
    
  } else{
    # Reading the Data from modeling
    df <- read.csv(file = inputFileName,as.is=TRUE,header = TRUE)
    
    
    
    # Dropping Few Variables for now, may use in future
    #drops <- c("Date","Toss","Decision")
    #df <- df[ , !(names(df) %in% drops)]
    
    # Getting Only the Numeric column for modeling
    nums <- sapply(df, is.numeric)
    df <- df[,nums]
    
    descrCor <-  cor(df)
    highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .5)
    print("--------------------")
    print(highCorr)
    print("--------------------")
    
    # Target variable
    win_var <- paste(gsub(" ", "", teamName, fixed = TRUE),"_Win",sep = "")
    print(win_var)
    
    # Get the Target variable index
    win_var_index <- which( colnames(df)==win_var )
    print(win_var_index)
    
    

    set.seed(100)
    index <- createDataPartition(df[,win_var_index], p=0.6, list=FALSE, times=1)
    
    Train_df <- df[ index,]
    Test_df  <- df[-index,]
    
    
    # Get the Target variable index
    win_var_index_Train_df <- which( colnames(Train_df)==win_var )
    print(win_var_index_Train_df)
    
    
    # Creatign formula for teamName Win as target variable
    fmla  <- as.formula(paste(win_var," ~ .",sep = ''))
    
    
    ### VIF Check 1
    print("---VIF CHECK 1 STARTS")
    check1 <- 0
    while(TRUE) {
      check1 <- check1 + 1
      print(paste("VIF CHeck 1 Iteration : ",check1,sep = ""))
      # Creating first Model with all the varibles
      mod <- glm(fmla,data = Train_df,family = "binomial")
      print(summary(mod))
      
      # Identifying the linearly dependent variables
      ld.vars <- attributes(alias(mod)$Complete)$dimnames[[1]]
      
      
      if(length(ld.vars)==0){
        print("Check 1 - ld.vars is 0")
        vif_res_df <- as.data.frame(vif(mod))
        print(vif_res_df)
        
        if (length(colnames(vif_res_df))==1){
          colnames(vif_res_df) <- c("VIF")
        }else {
          colnames(vif_res_df) <- c("GVIF","Df","VIF")        
        }
        
        vif_max_val1 <- (vif_res_df[which.max(vif_res_df$VIF),"VIF"])
        row_name1 <- rownames(vif_res_df)[vif_res_df$VIF == vif_max_val1]
        
        print(vif_max_val1)
        print(row_name1)
        
        if(vif_max_val1>5){
          rem_var <- row_name1
          Train_df <- Train_df[ , !(names(Train_df) %in% rem_var)]
        } else {
          break
        }
        
      } else {
        print("Check 1 else ::::")
        print(ld.vars)
        Train_df <- Train_df[ , !(names(Train_df) %in% ld.vars)]
      }
      
    }
    
   
    
  }
  
}