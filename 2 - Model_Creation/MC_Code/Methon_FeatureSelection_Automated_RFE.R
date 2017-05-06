setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/2 - Model_Creation/MC_Code/")
options(warn=-1)

FeatureSelection_Automated_RFE <- function(train_data,target_variable){
  library(mlbench)
  library(caret)
  
  #print(paste("Target Variable : ",target_variable))
  #print(head(train_data[,!colnames(train_data)==target_variable],2))
  #print(head(train_data[,colnames(train_data)==target_variable],2))
  
  # define the control using a random forest selection function
  control <- rfeControl(functions=rfFuncs, method="cv", number=10)
  
  # run the RFE algorithm
  results <- rfe(train_data[,!colnames(train_data)==target_variable], train_data[,colnames(train_data)==target_variable], sizes=c(1:8), rfeControl=control)
  
  # important variables
  impVariables <- results$optVariables
  
  detach(package:mlbench)
  detach(package:caret)
  
  return(impVariables)
  
  
}