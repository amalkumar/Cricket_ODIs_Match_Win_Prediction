setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/2 - Model_Creation/MC_Code/")

options(warn=-1)

library(reshape2)
library(dplyr)
library(sqldf)
library(ggplot2)
library(car)
library(caret)
library("e1071")
library(ROCR)

ModelEvaluationMatixGenerator <- function(teamName,onDate){
  cuttOff_range <-  seq(0.45, 0.75, 0.01)
  
  confusionMatrix_Evaluation <- data_frame()
  
  confusionMatrix_Evaluation <- data.frame(Model_Name=character(0),DataSet=character(0),Fold=integer(0),CutOff_Probability=numeric(0),AIC=numeric(0),NULL_Dev=numeric(0),Residual_Dev=numeric(0),AUC=numeric(0),FPR=numeric(0),TPR=numeric(0),Accuracy=numeric(0),Kappa=numeric(0))
  
  BinningCriteriaList <- c("No-Binning","20Perc","3Levels")
  for (BinningCriteria in BinningCriteriaList){
    inputDirectoryPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/2 - Model_Creation/MC_Output/"
    teamNameDirectoryonDate <- paste(inputDirectoryPath,"MC-",teamName,"/",sep = "")
    
    
    model_name <- paste("MC-5-",BinningCriteria,"-",teamName,"_",onDate,"_final_model.rda",sep = "")
    inputModelFileNameWithPath <- paste(teamNameDirectoryonDate,model_name,sep = "")
    
    
    if(!file.exists(inputModelFileNameWithPath)){
      
      print("Model File Doesnt exists")
      stop()
      
    } else{
      
      
      
      DatafileName <- paste("MC-4-",BinningCriteria,"-AllMatchesDataFor_",teamName,"_",onDate,"_Binned",".csv",sep = "")
      DatainputFileName <- paste(teamNameDirectoryonDate,DatafileName,sep = "")
      
      if(!file.exists(DatainputFileName)){
        
        print("Data File Doesnt exists")
        stop()
        
      } else{
        # Loading the model
        load(inputModelFileNameWithPath)
        
        # Reading the Data from modeling
        df <- read.csv(file = DatainputFileName,as.is=TRUE,header = TRUE)
        
        #Randomly shuffle the data
        df<-df[sample(nrow(df)),]
        
        #Create 5 equally size folds
        folds <- cut(seq(1,nrow(df)),breaks=5,labels=FALSE)
        
        confusionMatrix_Evaluation_Folds <- data_frame()
        
        confusionMatrix_Evaluation_Folds <- data.frame(DataSet=character(0),CutOff_Probability=numeric(0),AIC=numeric(0),NULL_Dev=numeric(0),Residual_Dev=numeric(0),AUC=numeric(0),FPR=numeric(0),TPR=numeric(0),Total_Actual_Negative=integer(0),Total_Actual_Positive=integer(0),True_Negative=integer(0),False_Negative=integer(0),True_Positive=integer(0),False_Positive=integer(0),Accuracy=numeric(0),Kappa=numeric(0),AccuracyLower=numeric(0),AccuracyUpper=numeric(0),AccuracyNull=numeric(0),AccuracyPValue=numeric(0),McnemarPValue=numeric(0),Sensitivity=numeric(0),Specificity=numeric(0),Pos.Pred.Value=numeric(0),Neg.Pred.Value=numeric(0),Prevalence=numeric(0),Detection.Rate=numeric(0),Detection.Prevalence=numeric(0),Balanced.Accuracy=numeric(0),Fold=integer(0),Model_Name=character(0))
        
        #Perform 10 fold cross validation
        for(j in 1:5){
          
          #Segement your data by fold using the which() function 
          testIndexes <- which(folds==j,arr.ind=TRUE)
          testData <- df[testIndexes, ]
          trainData <- df[-testIndexes, ]
          
          # Target variable
          win_var <- paste(gsub(" ", "", teamName, fixed = TRUE),"_Win",sep = "")
          print(win_var)
          
          # Get the Target variable index
          win_var_index_train <- which( colnames(trainData)==win_var )
          print(win_var_index_train)
          
          win_var_index_test <- which( colnames(testData)==win_var )
          print(win_var_index_test)
          
          
          #Use the test and train data partitions however you desire...
          print("-------------------")
          print(paste("Train Data Row Count :",nrow(trainData)))
          print(paste("Test Data Row Count :",nrow(testData)))
          
          confusionMatrix_Evaluation_Train_DF <- data_frame()
          
          confusionMatrix_Evaluation_Train_DF <- data.frame(DataSet=character(0),CutOff_Probability=numeric(0),AIC=numeric(0),NULL_Dev=numeric(0),Residual_Dev=numeric(0),AUC=numeric(0),FPR=numeric(0),TPR=numeric(0),Total_Actual_Negative=integer(0),Total_Actual_Positive=integer(0),True_Negative=integer(0),False_Negative=integer(0),True_Positive=integer(0),False_Positive=integer(0),Accuracy=numeric(0),Kappa=numeric(0),AccuracyLower=numeric(0),AccuracyUpper=numeric(0),AccuracyNull=numeric(0),AccuracyPValue=numeric(0),McnemarPValue=numeric(0),Sensitivity=numeric(0),Specificity=numeric(0),Pos.Pred.Value=numeric(0),Neg.Pred.Value=numeric(0),Prevalence=numeric(0),Detection.Rate=numeric(0),Detection.Prevalence=numeric(0),Balanced.Accuracy=numeric(0))
          
          for (i in cuttOff_range){
            aic_train <- summary(mod)$aic
            null_dev_train <- summary(mod)$null.deviance
            residual_dev_train <- summary(mod)$deviance
            
            pred <- predict(mod,type="response",newdata = trainData)
            pred_class <- ifelse(pred>=i,1,0)
            
            confMatrix <- confusionMatrix(pred_class,trainData[,win_var_index_train], positive = "1")
            
            # Getting Table values in data frame
            confMatrix_Table_df <-as.data.frame.matrix(confMatrix$table) 
            colnames(confMatrix_Table_df) <- c("Actual_Negative","Actual_Positive")
            rownames(confMatrix_Table_df) <- c("Predicted_Negative","Predicted_Positive")
            
            Total_Actual_Negative <- confMatrix_Table_df["Predicted_Negative",]$Actual_Negative + confMatrix_Table_df["Predicted_Positive",]$Actual_Negative
            Total_Actual_Positive <- confMatrix_Table_df["Predicted_Negative",]$Actual_Positive + confMatrix_Table_df["Predicted_Positive",]$Actual_Positive
            
            True_Negative <- confMatrix_Table_df["Predicted_Negative",]$Actual_Negative
            False_Negative <- confMatrix_Table_df["Predicted_Negative",]$Actual_Positive
            
            True_Positive <- confMatrix_Table_df["Predicted_Positive",]$Actual_Positive
            False_Positive <- confMatrix_Table_df["Predicted_Positive",]$Actual_Negative
            
            table_df <- data.frame(Total_Actual_Negative,Total_Actual_Positive,True_Negative,False_Negative,True_Positive,False_Positive)
            
            # Calc Area under the ROC curve
            ROCRpred <- prediction(pred_class, trainData[,win_var_index_train])
            ROCRperf <- performance(ROCRpred, 'tpr','fpr')
            auc <- performance(ROCRpred, measure = "auc")
            auc <- auc@y.values[[1]]
            
            # Calc the TPR and FPR
            cutoffs <- data.frame(cut=ROCRperf@alpha.values[[1]], fpr=ROCRperf@x.values[[1]], tpr=ROCRperf@y.values[[1]])
            fpr <- cutoffs[cutoffs$cut==1,]$fpr
            tpr <- cutoffs[cutoffs$cut==1,]$tpr
            
            
            # Getting Overall Performance values in data frame
            overall_df <- as.data.frame(as.list(confMatrix$overall))
            byClass_df <- as.data.frame(as.list(confMatrix$byClass))
            confMatrix_Eval_df <- cbind(overall_df,byClass_df)
            
            # Binding Table_df and Eval df    
            all_combined_df <- cbind(table_df,confMatrix_Eval_df)
            
            
            # Adding Cut-Off Probability
            confMatrix_Eval_df <- cbind(data.frame("DataSet" = "Train","CutOff_Probability" = i,"AIC" = aic_train,"NULL_Dev" = null_dev_train,"Residual_Dev"= residual_dev_train,"AUC" = auc,"FPR"=fpr,"TPR"=tpr),all_combined_df)
            
            # Creating final Evaualtion Data fram
            confusionMatrix_Evaluation_Train_DF <- rbind(confusionMatrix_Evaluation_Train_DF,confMatrix_Eval_df)
            
          }
          
          confusionMatrix_Evaluation_Test_DF <- data_frame()
          
          confusionMatrix_Evaluation_Test_DF <- data.frame(DataSet=character(0),CutOff_Probability=numeric(0),AIC=numeric(0),NULL_Dev=numeric(0),Residual_Dev=numeric(0),AUC=numeric(0),FPR=numeric(0),TPR=numeric(0),Total_Actual_Negative=integer(0),Total_Actual_Positive=integer(0),True_Negative=integer(0),False_Negative=integer(0),True_Positive=integer(0),False_Positive=integer(0),Accuracy=numeric(0),Kappa=numeric(0),AccuracyLower=numeric(0),AccuracyUpper=numeric(0),AccuracyNull=numeric(0),AccuracyPValue=numeric(0),McnemarPValue=numeric(0),Sensitivity=numeric(0),Specificity=numeric(0),Pos.Pred.Value=numeric(0),Neg.Pred.Value=numeric(0),Prevalence=numeric(0),Detection.Rate=numeric(0),Detection.Prevalence=numeric(0),Balanced.Accuracy=numeric(0))
          
          for (i in cuttOff_range){
            aic_test <- summary(mod)$aic
            null_dev_test <- summary(mod)$null.deviance
            residual_dev_test <- summary(mod)$deviance
            
            pred <- predict(mod,type="response",newdata = testData)
            pred_class <- ifelse(pred>=i,1,0)
            confMatrix <- confusionMatrix(pred_class,testData[,win_var_index_test], positive = "1")
            
            # Getting Table values in data frame
            confMatrix_Table_df <-as.data.frame.matrix(confMatrix$table) 
            colnames(confMatrix_Table_df) <- c("Actual_Negative","Actual_Positive")
            rownames(confMatrix_Table_df) <- c("Predicted_Negative","Predicted_Positive")
            
            Total_Actual_Negative <- confMatrix_Table_df["Predicted_Negative",]$Actual_Negative + confMatrix_Table_df["Predicted_Positive",]$Actual_Negative
            Total_Actual_Positive <- confMatrix_Table_df["Predicted_Negative",]$Actual_Positive + confMatrix_Table_df["Predicted_Positive",]$Actual_Positive
            
            True_Negative <- confMatrix_Table_df["Predicted_Negative",]$Actual_Negative
            False_Negative <- confMatrix_Table_df["Predicted_Negative",]$Actual_Positive
            
            True_Positive <- confMatrix_Table_df["Predicted_Positive",]$Actual_Positive
            False_Positive <- confMatrix_Table_df["Predicted_Positive",]$Actual_Negative
            
            table_df <- data.frame(Total_Actual_Negative,Total_Actual_Positive,True_Negative,False_Negative,True_Positive,False_Positive)
            
            # Calc Area under the ROC curve
            ROCRpred <- prediction(pred_class, testData[,win_var_index_test])
            ROCRperf <- performance(ROCRpred, 'tpr','fpr')
            auc <- performance(ROCRpred, measure = "auc")
            auc <- auc@y.values[[1]]
            
            # Calc the TPR and FPR
            cutoffs <- data.frame(cut=ROCRperf@alpha.values[[1]], fpr=ROCRperf@x.values[[1]], tpr=ROCRperf@y.values[[1]])
            fpr <- cutoffs[cutoffs$cut==1,]$fpr
            tpr <- cutoffs[cutoffs$cut==1,]$tpr
            
            
            # Getting Overall Performance values in data frame
            overall_df <- as.data.frame(as.list(confMatrix$overall))
            byClass_df <- as.data.frame(as.list(confMatrix$byClass))
            confMatrix_Eval_df <- cbind(overall_df,byClass_df)
            
            # Binding Table_df and Eval df    
            all_combined_df <- cbind(table_df,confMatrix_Eval_df)
            
            
            # Adding Cut-Off Probability
            confMatrix_Eval_df <- cbind(data.frame("DataSet" = "Test","CutOff_Probability" = i,"AIC"=aic_test,"NULL_Dev" = null_dev_test,"Residual_Dev"= residual_dev_test,"AUC"=auc,"FPR"=fpr,"TPR"=tpr),all_combined_df)
            
            # Creating final Evaualtion Data fram
            confusionMatrix_Evaluation_Test_DF <- rbind(confusionMatrix_Evaluation_Test_DF,confMatrix_Eval_df)
            
            
          }
          
          
          
          
          confusionMatrix_Evaluation_Folds <- rbind(confusionMatrix_Evaluation_Folds,data.frame(head(confusionMatrix_Evaluation_Train_DF[which(confusionMatrix_Evaluation_Train_DF$AUC == max(confusionMatrix_Evaluation_Train_DF$AUC)), ],1),Fold=j,Model_Name=BinningCriteria))
          confusionMatrix_Evaluation_Folds <- rbind(confusionMatrix_Evaluation_Folds,data.frame(head(confusionMatrix_Evaluation_Test_DF[which(confusionMatrix_Evaluation_Test_DF$AUC == max(confusionMatrix_Evaluation_Test_DF$AUC)), ],1),Fold=j,Model_Name=BinningCriteria))
          
          
        }
        print(paste("----------Train/Test-------",sep = ""))
        confusionMatrix_Evaluation_Folds <- confusionMatrix_Evaluation_Folds[,c("Model_Name","DataSet" ,"Fold",	"CutOff_Probability",	"AIC",	"NULL_Dev",	"Residual_Dev",	"AUC",	"FPR",	"TPR",	"Accuracy",	"Kappa")]
        confusionMatrix_Evaluation_Folds <- confusionMatrix_Evaluation_Folds[order(confusionMatrix_Evaluation_Folds$DataSet),]
        print("-------------------------------------------------")
        
        
      }
    }
    
    confusionMatrix_Evaluation <- rbind(confusionMatrix_Evaluation,confusionMatrix_Evaluation_Folds)
    
  }
  outputFileName <- paste("MC-6-ModelEvaluationMatrix_",teamName,"_",onDate,".csv",sep = "")
  outputFileNameWithPath <- paste(teamNameDirectoryonDate,outputFileName,sep = "")
  write.csv(confusionMatrix_Evaluation,file = outputFileNameWithPath,row.names = FALSE)
  
}