setwd("D:/Study_Material/DataAnalytics/Cricket/ODIs/2 - Model_Creation/MC_Code/")


options(warn=-1)

library(reshape2)
library(dplyr)
library(sqldf)

DonDate <- "2017-03-05"
teamName <- "England"




source("1 - Method_CreateAllMatchesDataForTeamOnDate.R")
CreateAllMatchesDataForTeamOnDate(teamName,DonDate)

source("2 - Method_CreateAggregatedDataForTeamOnDate.R")
CreateAggregatedDataForTeamOnDate(teamName,DonDate)

# Place the csv file with relevant variables before executing this
source("3 - Method_RemoveCorrelatedVarsAndCombineVarsForTeamOnDate.R")
RemoveCorrelatedVarsAndCombinedVars(teamName,DonDate)


source("4a - Method_No_Binning_AutomatedForTeam.R")
No_Binning_AutomatedForTeam(teamName,DonDate)

source("4b - Method_DataBinning_Logical_AutomatedForTeam.R")
DataBinning_Logical_AutomatedForTeam(teamName,DonDate,"3Levels")
DataBinning_Logical_AutomatedForTeam(teamName,DonDate,"20Perc")

source("5 - Methon_CreateModelForTeam_Automated.R")
CreateModelForTeam_Automated(teamName,DonDate,"No-Binning")
CreateModelForTeam_Automated(teamName,DonDate,"3Levels")
CreateModelForTeam_Automated(teamName,DonDate,"20Perc")

source("6 - Method_ModelEvaluationMatixGenerator.R")
ModelEvaluationMatixGenerator(teamName,DonDate)


