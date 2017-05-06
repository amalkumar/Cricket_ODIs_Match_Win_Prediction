setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/2 - Model_Creation/MC_Code/")
options(warn=-1)


library(reshape2)
#library(plyr)
#library(dplyr)
library(sqldf)
library(ggplot2)
library(car)
library(caret)
library("e1071")


CreateModelForTeam_Automated <- function(teamName,onDate,BinningCriteria="No-Binning"){
  
  inputDirectoryPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/2 - Model_Creation/MC_Output/"
  teamNameDirectoryonDate <- paste(inputDirectoryPath,"MC-",teamName,"/",sep = "")
  
  fileName <- paste("MC-4-",BinningCriteria,"-AllMatchesDataFor_",teamName,"_",onDate,"_Selected_Variables",".csv",sep = "")
  inputFileName <- paste(teamNameDirectoryonDate,fileName,sep = "")
  
  if(!file.exists(inputFileName)){
    
    print("File Doesnt exists")
    stop()
    
  } else{
    # Reading the Data from modeling
    df <- read.csv(file = inputFileName,as.is=TRUE,header = TRUE)
    
    # Getting Only the Numeric column for modeling
    nums <- sapply(df, is.numeric)
    df <- df[,nums]
    
    # Dropping Few Extra Numerical Variables , not required
    #drops <- c("Src_Match_No","Year")
    #df <- df[ , !(names(df) %in% drops)]
    
    print("------------------")
    print(head(df,2))
    print("------------------")

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

    
    outputFileNameTrain <- paste("MC-5-",BinningCriteria,"-AllMatchesDataFor_",teamName,"_",onDate,"_Train_Data",".csv",sep = "")
    outputFileNameWithPathTrain <- paste(teamNameDirectoryonDate,outputFileNameTrain,sep = "")
    write.csv(Train_df,file = outputFileNameWithPathTrain,row.names = FALSE)
    
    outputFileNameTest <- paste("MC-5-",BinningCriteria,"-AllMatchesDataFor_",teamName,"_",onDate,"_Test_Data",".csv",sep = "")
    outputFileNameWithPathTest <- paste(teamNameDirectoryonDate,outputFileNameTest,sep = "")
    write.csv(Test_df,file = outputFileNameWithPathTest,row.names = FALSE)
    
    

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
    
    print(paste("Before STepWISE "," Total Traing rows ",nrow(Train_df)," ::: Total COlumns",length(names(Train_df))))
   
    
    
    ### Stepwise with VIF ####
    stepWiseIter <- 0
    while(TRUE){
      stepWiseIter <- stepWiseIter + 1
      print(paste("STEPWISE ITERATION :  ",stepWiseIter))
      # using stepwise regression to choose the variables and their sequences
      stepWiseModel <- step(mod,direction = "both")
      
      # Storing step output in data frame
      stepWiseModel_df <- stepWiseModel$model
      
      # Idenitfy Significant variables
      SW_Sig_Var <- colnames(stepWiseModel_df)
      
      # Updating the train data frame
      Train_df <- Train_df[,which(names(Train_df) %in% SW_Sig_Var)]
      
      # Get the Target variable index
      win_var_index_Train_df <- which( colnames(Train_df)==win_var )
      print(win_var_index_Train_df)
      
      # updating model
      mod <- glm(fmla,data = Train_df,family = "binomial")
      
      # Aliased Variables
      ld.vars.Iterate <- attributes(alias(mod)$Complete)$dimnames[[1]]
      
      if(length(ld.vars.Iterate)==0){
        print("-------NO ALIASED LINEARLY DEPENDENT VARIBALE  AFTER STEPWISE")
        print("Checking and removing variables with VIF more than  5")
        
        
        vif_res_df_iter <- as.data.frame(vif(mod))
        print("----Before rename column")
        print(vif_res_df_iter)
        if (length(colnames(vif_res_df_iter))==1){
          colnames(vif_res_df_iter) <- c("VIF")
        }else {
          colnames(vif_res_df_iter) <- c("GVIF","Df","VIF")        
        }
        print("----After rename column")
        print(vif_res_df_iter)
        
        print("-----ROW Names")
        print(rownames(vif_res_df_iter))
        
        print("-----")
        vif_max_val <- (vif_res_df_iter[which.max(vif_res_df_iter$VIF),"VIF"])
        row_name <- rownames(vif_res_df_iter)[vif_res_df_iter$VIF == vif_max_val]
        
        #print(rownames(vif_res_df_iter[which.max(vif_res_df_iter$VIF),]))
        print(row_name)
        print(vif_max_val)
        
        if(vif_max_val>5){
          rem_var <- row_name
          Train_df <- Train_df[ , !(names(Train_df) %in% rem_var)]
          # updating model
          mod <- glm(fmla,data = Train_df,family = "binomial")
        }else {
          
          # Check Significant variables
          modIterateMatrix <- summary(mod)$coefficients
          #print("-----MODEL MATRIX---")
          #print(modIterateMatrix)
          model_var_len <- length(modIterateMatrix[,1])-1
          #print(paste("Model variables length : ",model_var_len,sep = ""))
          
          mod_sig_variable <- character(0)
          
          print("-----MODEL MATRIX DATA FRAME----")
          modIterateDF <- (as.data.frame(modIterateMatrix))
          colnames(modIterateDF) <- c("Estimate","StdError","zVal","pVal")
          modIterateDF <- modIterateDF[rownames(modIterateDF) != "(Intercept)", ]
          print(modIterateDF)
          
          print(rownames(modIterateDF[which.max(modIterateDF$pVal),]))
          print((modIterateDF[which.max(modIterateDF$pVal),"pVal"]))
          
          if(modIterateDF[which.max(modIterateDF$pVal),"pVal"]>0.1){
            rem_var <- rownames(modIterateDF[which.max(modIterateDF$pVal),])
            Train_df <- Train_df[ , !(names(Train_df) %in% rem_var)]
            # updating model
            mod <- glm(fmla,data = Train_df,family = "binomial")
          } else {
            break
          }
          
        }
        
      }else{
        print("-------ALIASED LINEARLY DEPENDENT VARIBALE  AFTER STEPWISE")
        print(ld.vars.Iterate)
        Train_df <- Train_df[ , !(names(Train_df) %in% ld.vars.Iterate)]
        # updating model
        mod <- glm(fmla,data = Train_df,family = "binomial")
        
      }      
      
      
    }
    
    
    
    
    print("-----SUMMARY OF FINAL MODEL-----")
    print(summary(mod))
    print("-----SUMMARY OF FINAL MODEL-----")
    
    print("-----VIF STATS OF FINAL MODEL-----")
    print(vif(mod))
    print("-----VIF STATS OF FINAL MODEL-----")
    
    
    
    print("---------------------")
    outputMatrixDFFileName <- paste("MC-5-",BinningCriteria,"-AllMatchesDataFor_",teamName,"_",onDate,"_Final_Variables",".csv",sep = "")
    outputMatrixDFFileNameWithPath <- paste(teamNameDirectoryonDate,outputMatrixDFFileName,sep = "")
    write.csv(modIterateDF,file = outputMatrixDFFileNameWithPath)
    print("---------------------")
    
    
    #model_name <- paste("MC-5-",BinningCriteria,"-",teamName,"_",onDate,"_final_model.rda",sep = "")
    model_name <- paste("MC-5-",BinningCriteria,"-",teamName,"_final_model.rda",sep = "")
    outputFileNameWithPath <- paste(teamNameDirectoryonDate,model_name,sep = "")
    save(mod, file = outputFileNameWithPath)
    print("Model Saved")
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  }
  
}