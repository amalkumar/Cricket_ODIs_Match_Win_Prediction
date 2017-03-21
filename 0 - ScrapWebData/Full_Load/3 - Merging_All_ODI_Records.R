setwd("D:/Study_Material/DataAnalytics/R_WebScrapping/Code/")

df_Other_ODI <- read.csv(file = "D:/Study_Material/DataAnalytics/R_WebScrapping/Data/All_ODI_Matches.csv",as.is=TRUE,header = TRUE)
df_WC_ODI <- read.csv(file = "D:/Study_Material/DataAnalytics/R_WebScrapping/Data/All_WC_ODI_Matches.csv",as.is=TRUE,header = TRUE)

df <- rbind(df_Other_ODI,df_WC_ODI)
df$Date <- as.Date(df$Date,"%d/%m/%Y")

df <- df[order(df$Date),]
df$Match_Seq <- 1:nrow(df) 

df <- df[,c("Match_Seq","Date","Team1","Team2","Ground","Winner","Win_Style","ScoreCard")]

write.csv(df,file = "D:/Study_Material/DataAnalytics/R_WebScrapping/Data/All_ODIs.csv",row.names = FALSE)