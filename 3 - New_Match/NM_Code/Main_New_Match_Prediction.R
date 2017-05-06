setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/3 - New_Match/NM_Code/")


options(warn=-1)

newMatchDir="D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/3 - New_Match/"
newMatchFile="New_Match.csv"

source("1 - Method_CreateAggregatedDataForTeamOnDate.R")
CreateAggregatedDataForTeamOnDate(newMatchDir,newMatchFile)

source("2a - Method_No_Binning_AutomatedForTeam.R")
No_Binning_AutomatedForTeam(newMatchDir,newMatchFile)

source("2b - Method_DataBinning_Logical_AutomatedForTeam.R")
DataBinning_Logical_AutomatedForTeam(newMatchDir,newMatchFile,"3Levels")
DataBinning_Logical_AutomatedForTeam(newMatchDir,newMatchFile,"20Perc")

source("3 - Method_PredictOutcome_Porbablity_ForTeamOnDate.R")
PredictOutcome_Probability_ForTeamOnDate(newMatchDir,newMatchFile,"No-Binning")
PredictOutcome_Probability_ForTeamOnDate(newMatchDir,newMatchFile,"3Levels")
PredictOutcome_Probability_ForTeamOnDate(newMatchDir,newMatchFile,"20Perc")
