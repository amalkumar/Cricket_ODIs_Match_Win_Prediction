setwd("D:/Study_Material/DataAnalytics/Cricket/ODIs/3 - New_Match_Prediction/NM_Code/")


options(warn=-1)

library(reshape2)
library(dplyr)
library(sqldf)

Copy_Reference_Files <- function(cur_folder="D:/Study_Material/DataAnalytics/Cricket/ODIs/1 - Data_Creation/DC_Output/",new_folder="D:/Study_Material/DataAnalytics/Cricket/ODIs/3 - New_Match_Prediction/NM_Reference_Data/"){
  
  print(paste("Source Directory : ", cur_folder))
  print(paste("Target Directory : ", new_folder))
  
  # list the files in source directory
  listOfFileCurFolder <- list.files(cur_folder,full.names = TRUE)
  print(listOfFileCurFolder)
  
  # copy the files to the new folder
  file.copy(listOfFileCurFolder, new_folder)
  
}














