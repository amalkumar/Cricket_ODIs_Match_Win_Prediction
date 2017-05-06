setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/2 - Model_Creation/MC_Code/")


options(warn=-1)


start_Date <- "2002-01-01"
DonDate <- "2017-03-05"
teamName <- "Australia"


#start.time <- Sys.time()
#print(paste("1 - Start Time : ",start.time))
#source("1 - Method_CreateAllMatchesDataForTeamOnDate.R")
#CreateAllMatchesDataForTeamOnDate(teamName,DonDate,start_Date)
#print(paste("1 - End Time : ",Sys.time()))
#print(paste("1 - Total Time : ",Sys.time() - start.time))


#start.time <- Sys.time()
#print(paste("2 - Start Time : ",start.time))
#source("2 - Method_CreateAggregatedDataForTeamOnDate.R")
#CreateAggregatedDataForTeamOnDate(teamName,DonDate)
# took 1 hour 45 mins to complete
#print(paste("2 - End Time : ",Sys.time()))
#print(paste("2 - Total Time : ",Sys.time() - start.time))




# Place the csv file with relevant variables before executing this
#source("3 - Method_RemoveCorrelatedVarsAndCombineVarsForTeamOnDate.R")
#RemoveCorrelatedVarsAndCombinedVars(teamName,DonDate)

#source("4a - Method_No_Binning_AutomatedForTeam.R")
#No_Binning_AutomatedForTeam(teamName,DonDate)

#source("4b - Method_DataBinning_Logical_AutomatedForTeam.R")
#DataBinning_Logical_AutomatedForTeam(teamName,DonDate,"3Levels")
#DataBinning_Logical_AutomatedForTeam(teamName,DonDate,"20Perc")


#source("4c - Methon_FeatureSelection_Automated_RFE.R")
#GetImportantVariablesForTeamAutomated(teamName,DonDate,BinningCriteria="No-Binning")
#GetImportantVariablesForTeamAutomated(teamName,DonDate,BinningCriteria="3Levels")
#GetImportantVariablesForTeamAutomated(teamName,DonDate,BinningCriteria="20Perc")



source("5 - Methon_CreateModelForTeam_Automated.R")
CreateModelForTeam_Automated(teamName,DonDate,"No-Binning")
#CreateModelForTeam_Automated(teamName,DonDate,"3Levels")
#CreateModelForTeam_Automated(teamName,DonDate,"20Perc")


#source("6 - Method_ModelEvaluationMatixGenerator.R")
#ModelEvaluationMatixGenerator(teamName,DonDate)



