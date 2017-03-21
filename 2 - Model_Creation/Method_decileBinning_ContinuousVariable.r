library(dplyr)
library(sqldf)

setwd("D:/Study_Material/DataAnalytics/Cricket/ODIs/2 - Model_Creation/MC_Code/")


decileBinning_ContinuousVariable <- function(decile,dataFrame,varName,targetVar){
  
  #print(decile)
  #print(varName)
  #print(targetVar)
  ##print(dataFrame)
  
  
  dat1 <- dataFrame %>% mutate(dec=ntile(dataFrame[,varName],n=decile)) 
  #print(dat1)
  dat2 <- dat1 %>% count(dat1[,targetVar],dec) 
  names(dat2)[1] <- 'Event'
  dat3 <- subset(dat2,Event==1)
  
  dat11 <- dat1 %>% count(dec)
  dat111 <- dat11 %>% unname()
  dat3$Decile_Records <- unclass(dat111)[[2]]
  
  dat3$Event_Rate <- dat3$n/dat3$Decile_Records
  
  ssdfmin <- sqldf(paste("select dec, min(",varName,") from dat1 group by dec"))
  dat3$GreaterThan <- unclass(ssdfmin)[[2]]
  

  ssdfmax <- sqldf(paste("select dec, max(",varName,") from dat1 group by dec"))
  dat3$LessThan <- unclass(ssdfmax)[[2]]
  
  dat3$varname <- rep(varName,nrow(dat3))

  
  return(dat3)
  
  
}

