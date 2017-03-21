setwd("D:/Study_Material/DataAnalytics/R_WebScrapping/Code/")

library(rvest)
library(dplyr)
library(XML)
library(tidyr)

# Reading ODI page url
howstat <- read_html("http://howstat.com/cricket/Statistics/Matches/MatchListMenu.asp#odis")

# all the links from ODI page
all_links <- howstat %>% html_nodes(".TextBlack8 .LinkOff") %>% html_attr('href')

# Worldcup matches will b processed seperately cz of difference in SUmmary Page Format
# WorldCUp Matches URLs
WC_ODI_Links_All_Years <- character(0)
for (lnk in all_links){
  if(grepl("World%20Cup", lnk)){
    WC_ODI_Links_All_Years <- c(WC_ODI_Links_All_Years,paste("http://howstat.com/cricket/Statistics/Matches/",lnk,sep = ""))
  }
}


Summary_DF <- data.frame()
Summary_DF <- data.frame("Â"=character(0),"Series"=character(0),"Match"=character(0),"Date"=character(0),"Ground"=character(0),"Result"=character(0),"ScoreCard"=character(0))
scorecardLinks <- as.character()
for(ODILink in WC_ODI_Links_All_Years){
  Summary_DF <- rbind(Summary_DF,(readHTMLTable(ODILink))[[8]])
  scorecards <- paste("http://howstat.com/cricket/Statistics/Matches/",read_html(ODILink) %>% html_nodes("td~ td+ td .LinkNormal") %>% html_attr('href'),sep = "")
  
  match_score_card <- as.character()
  for (sc in scorecards)
  {
    if(grepl("MatchScorecard_ODI", sc)){
      match_score_card <- c(match_score_card,sc)
    }
    
  }
  
  scorecardLinks <- c(scorecardLinks,match_score_card)
  #break
}
print("------------------------")

# removing Unneccesary column
Summary_DF <- Summary_DF[,-1]

print(nrow(Summary_DF))
print(length(scorecardLinks))

Summary_DF$NewCol <- scorecardLinks
colnames(Summary_DF) <- c("Series","Match","Date","Ground","Result","Extra","ScoreCard")

# Removing scorecard column, only keeping scorecard URL
ScoreCard_index <- colnames(Summary_DF)=="Extra"
Summary_DF <- Summary_DF[,!ScoreCard_index]

# Spliting COuntries into Extra and Countries
Summary_DF <- Summary_DF %>% separate(Series,c("Extra","Countries"),sep = "[(]")

# Removing scorecard column, only keeping scorecard URL
ScoreCard_index <- colnames(Summary_DF)=="Extra"
Summary_DF <- Summary_DF[,!ScoreCard_index]

# Spliting COuntries into Countries and Extra
Summary_DF <- Summary_DF %>% separate(Countries,c("Countries","Extra"),sep = "[)]")

# Removing scorecard column, only keeping scorecard URL
ScoreCard_index <- colnames(Summary_DF)=="Extra"
Summary_DF <- Summary_DF[,!ScoreCard_index]

# Spliting COuntries into Team1 and Team2
Summary_DF <- Summary_DF %>% separate(Countries,c("Team1","Team2"),sep = " v. ")

# Spliting Result into Winner and How
Summary_DF <- Summary_DF %>% separate(Result,c("Winner","Win_Style"),sep = " won ")

#print(head(Summary_DF,2))

#print(Summary_DF)



df_teamName <- read.csv(file = "Abb_to_TeamName.csv",as.is=TRUE,header = TRUE)

# Updating Team1
Summary_DF <- merge(Summary_DF,df_teamName,by.x = "Team1",by.y = "Abb")
# droppin the old Team1 column
Team1_index <- colnames(Summary_DF)=="Team1"
Summary_DF <- Summary_DF[,!Team1_index]
# Renaming TeamName column to Team1
names(Summary_DF)[names(Summary_DF) == 'TeamName'] <- 'Team1'

# Updating Team2
Summary_DF <- merge(Summary_DF,df_teamName,by.x = "Team2",by.y = "Abb")
# droppin the old Team2 column
Team2_index <- colnames(Summary_DF)=="Team2"
Summary_DF <- Summary_DF[,!Team2_index]
# Renaming TeamName column to Team2
names(Summary_DF)[names(Summary_DF) == 'TeamName'] <- 'Team2'




Summary_DF <- Summary_DF[,c("Date","Team1","Team2","Ground","Winner","Win_Style","ScoreCard")]
Summary_DF <- data.frame(Summary_DF %>% group_by(Date,Team1,Team2,Ground,Winner,Win_Style,ScoreCard) %>% summarise(cnt=n()))
Summary_DF <- Summary_DF[,c("Date","Team1","Team2","Ground","Winner","Win_Style","ScoreCard")]

#print(Summary_DF)

write.csv(Summary_DF,file = "All_WC_ODI_Matches.csv",row.names = FALSE)