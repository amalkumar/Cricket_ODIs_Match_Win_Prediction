setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/0 - ScrapWebData/SW_Code/Full_Load/")

library(rvest)
library(dplyr)
library(XML)
library(tidyr)

outputPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/0 - ScrapWebData/Full_Load/"

# Reading ODI page url
howstat <- read_html("http://howstat.com/cricket/Statistics/Matches/MatchListMenu.asp#odis")

# all the links from ODI page
all_links <- howstat %>% html_nodes(".TextBlack8 .LinkOff") %>% html_attr('href')

# Worldcup matches will b processed seperately cz of difference in SUmmary Page Format
# WorldCUp Matches URLs
WC_ODI_Links_All_Years <- character(0)
for (lnk in all_links){
  if(grepl("World%20Cup", lnk)){
    WC_ODI_Links_All_Years <- c(WC_ODI_Links_All_Years,lnk)
  }
}


# ODI matches URL without WorldCUp matches
ODI_Links_All_Years <- character(0)
for (lnk in all_links){
  if(grepl("MatchList_ODI", lnk) & !(lnk %in% WC_ODI_Links_All_Years)){
    ODI_Links_All_Years <- c(ODI_Links_All_Years,paste("http://howstat.com/cricket/Statistics/Matches/",lnk,sep = ""))
  }
}


Summary_DF <- data.frame()
Summary_DF <- data.frame("Â"=character(0),"Date"=character(0),"Countries"=character(0),"Ground"=character(0),"Result"=character(0),"ScoreCard"=character(0))
scorecardLinks <- as.character()
for(ODILink in ODI_Links_All_Years){
  #print(ODILink)
  Summary_DF <- rbind(Summary_DF,(readHTMLTable(ODILink))[[8]])
  scorecard <- paste("http://howstat.com/cricket/Statistics/Matches/",read_html(ODILink) %>% html_nodes("td:nth-child(6) .LinkNormal") %>% html_attr('href'),sep = "")
  #print(scorecard)
  scorecardLinks <- c(scorecardLinks,scorecard)
  #print("----------")
}

# removing Unneccesary column
Summary_DF <- Summary_DF[,-1]

print(nrow(Summary_DF))
print(length(scorecardLinks))

Summary_DF$NewCol <- scorecardLinks
colnames(Summary_DF) <- c("Date","Countries","Ground","Result","Extra","ScoreCard")

# Removing scorecard column, only keeping scorecard URL
ScoreCard_index <- colnames(Summary_DF)=="Extra"
Summary_DF <- Summary_DF[,!ScoreCard_index]

# Spliting COuntries into Team1 and Team2
Summary_DF <- Summary_DF %>% separate(Countries,c("Team1","Team2"),sep = " v. ")

# Spliting Result into Winner and How
Summary_DF <- Summary_DF %>% separate(Result,c("Winner","Win_Style"),sep = " won ")

#print(Summary_DF)

outputFileWithPath <- paste(outputPath,"All_ODI_Matches.csv",sep = "")

write.csv(Summary_DF,file = outputFileWithPath,row.names = FALSE)



