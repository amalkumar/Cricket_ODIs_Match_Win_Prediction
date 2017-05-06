setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/0 - ScrapWebData/SW_Code/Full_Load/")

outputPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/0 - ScrapWebData/Full_Load/"
commonRefPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Common_Reference_Data/"

ODIMatchFileWithPath <- paste(outputPath,"All_ODI_Matches.csv",sep = "")
df_Other_ODI <- read.csv(file = ODIMatchFileWithPath,as.is=TRUE,header = TRUE)

WCodiMatchFileWithPath <- paste(outputPath,"All_WC_ODI_Matches.csv",sep = "")
df_WC_ODI <- read.csv(file = WCodiMatchFileWithPath,as.is=TRUE,header = TRUE)

df <- rbind(df_Other_ODI,df_WC_ODI)
df$Date <- as.Date(df$Date,"%d/%m/%Y")

df <- df[order(df$Date),]
df$Match_Seq <- 1:nrow(df) 

df <- df[,c("Match_Seq","Date","Team1","Team2","Ground","Winner","Win_Style","ScoreCard")]


outputFileWithPath <- paste(outputPath,"All_ODIs.csv",sep = "")
write.csv(df,file = outputFileWithPath,row.names = FALSE)

