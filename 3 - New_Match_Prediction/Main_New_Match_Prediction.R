setwd("D:/Study_Material/DataAnalytics/Cricket/ODIs/3 - New_Match_Prediction/NM_Code/")


options(warn=-1)

library(reshape2)
library(dplyr)
library(sqldf)



DonDate <- "2017-03-05"
team1 <- "West Indies"
team2 <- "England"


source("0 - Method_CopyReferenceFiles.R")
Copy_Reference_Files()

source("1 - Method_CreateNewMatchData.R")
Create_NewMatch_Data(team1,DonDate)
Create_NewMatch_Data(team2,DonDate)

source("2 -  Method_CreateAggregatedDataForTeam.R")
CreateAggregatedDataForTeamOnDate(team1,DonDate)
CreateAggregatedDataForTeamOnDate(team2,DonDate)

source("3 - Method_RemoveCorrelatedVarsAndCombineVarsForTeamOnDate.R")
RemoveCorrelatedVarsAndCombinedVars(team1,DonDate)
RemoveCorrelatedVarsAndCombinedVars(team2,DonDate)

source("4a - Method_No-Binning_Automated.R")
No_Binning_AutomatedForTeam(team1,DonDate)
No_Binning_AutomatedForTeam(team2,DonDate)

source("4b - Method_DataBinning_Logical_Automated.R")
DataBinning_Logical_AutomatedForTeam(team1,DonDate,"20Perc")
DataBinning_Logical_AutomatedForTeam(team1,DonDate,"3Levels")

DataBinning_Logical_AutomatedForTeam(team2,DonDate,"20Perc")
DataBinning_Logical_AutomatedForTeam(team2,DonDate,"3Levels")

source("5 - Method_PredictOutcome_Porbablity_ForTeamOnDate.R")
PredictOutcome_Probability_ForTeamOnDate(team1,DonDate,"No-Binning")
PredictOutcome_Probability_ForTeamOnDate(team2,DonDate,"No-Binning")

PredictOutcome_Probability_ForTeamOnDate(team1,DonDate,"20Perc")
PredictOutcome_Probability_ForTeamOnDate(team2,DonDate,"20Perc")

PredictOutcome_Probability_ForTeamOnDate(team1,DonDate,"3Levels")
PredictOutcome_Probability_ForTeamOnDate(team2,DonDate,"3Levels")











