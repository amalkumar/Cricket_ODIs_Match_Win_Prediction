setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/0 - ScrapWebData/SW_Code/Full_Load/")

library(rvest)
library(dplyr)
library(XML)
library(tidyr)

outputPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/0 - ScrapWebData/Full_Load/"
commonRefPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Common_Reference_Data/"


ground_DF <- data.frame()
ground_DF <- data.frame("Ground_Name"=character(0),"City"=character(0),"Country"=character(0),"First_ODI_Match"=character(0),"Last_ODI_Match"=character(0),"Matches_Played"=character(0),"Matches_Won_by_Home_Side"=character(0),"Matches_Won_by_Touring_Side"=character(0),"Matches_Won_by_Neutral_Side"=character(0),"Matches_Tied"=character(0),"Matches_with_No_Result"=character(0),"Highest_Individual_Innings"=character(0),"Best_Bowling"=character(0),"Highest_Team_Innings"=character(0),"Lowest_Team_Innings"=character(0),"Highest_Run_Chase_Achieved"=character(0),"Average_Runs_per_Wicket"=character(0),"Average_Runs_per_Over"=character(0))


for (i in 211:400){
  
  tryCatch(
    
    {
      valChar <- as.character(i)
      
      if(nchar(valChar)==1){
        valChar <- paste("00",valChar,sep = "")
      } else if(nchar(valChar)==2){
        valChar <- paste("0",valChar,sep = "")
      } else{
        valChar <- valChar
      }
      
      url <- paste("http://www.howstat.com/cricket/Statistics/Grounds/GroundStats_ODI.asp?GroundCode=",valChar,sep = "")
      print(url)
      
      # Reading ODI page url
      howstat_Grounds <- read_html(url)
      
      # get the ground name
      ground_name <- howstat_Grounds %>% html_nodes(".TextGreenBold12")
      ground_name <- as.character(ground_name)
      Ground_Name <- gsub("^.*>(*.*)<.*$", "\\1", ground_name)
      
      # get all the ground detail fields
      #ground_Fields <- howstat_Grounds %>% html_nodes(".TextBlackBold8")
      
      # get all the ground detail fields value
      ground_Fields_Value <- howstat_Grounds %>% html_nodes(".TextBlackBold8+ td")
      
      City						            <- gsub("^.*\\t(*.*)\\r.*$", "\\1", as.character(ground_Fields_Value[1]))
      Country						          <- gsub("^.*\\t(*.*)\\r.*$", "\\1", as.character(ground_Fields_Value[2]))
      First_ODI_Match				      <- gsub("^.*\\t(*.*)\\r.*$", "\\1", as.character(ground_Fields_Value[3]))
      Last_ODI_Match				      <- gsub("^.*\\t(*.*)\\r.*$", "\\1", as.character(ground_Fields_Value[4]))
      Matches_Played				      <- gsub("^.*\\t(*.*)\\r.*$", "\\1", as.character(ground_Fields_Value[5]))
      Matches_Won_by_Home_Side	  <- gsub("^.*\\t(*.*)\\r.*$", "\\1", as.character(ground_Fields_Value[6]))
      Matches_Won_by_Touring_Side	<- gsub("^.*\\t(*.*)\\r.*$", "\\1", as.character(ground_Fields_Value[7]))
      Matches_Won_by_Neutral_Side	<- gsub("^.*\\t(*.*)\\r.*$", "\\1", as.character(ground_Fields_Value[8]))
      Matches_Tied				        <- gsub("^.*\\t(*.*)\\r.*$", "\\1", as.character(ground_Fields_Value[9]))
      Matches_with_No_Result		  <- gsub("^.*\\t(*.*)\\r.*$", "\\1", as.character(ground_Fields_Value[10]))
      Highest_Individual_Innings	<- gsub("^.*\\t(*.*)\\r.*$", "\\1", as.character(ground_Fields_Value[11]))
      Best_Bowling				        <- gsub("^.*\\t(*.*)\\r.*$", "\\1", as.character(ground_Fields_Value[12]))
      Highest_Team_Innings		    <- gsub("^.*\\t(*.*)\\r.*$", "\\1", as.character(ground_Fields_Value[13]))
      Lowest_Team_Innings			    <- gsub("^.*\\t(*.*)\\r.*$", "\\1", as.character(ground_Fields_Value[14]))
      Highest_Run_Chase_Achieved	<- gsub("^.*\\t(*.*)\\r.*$", "\\1", as.character(ground_Fields_Value[15]))
      Average_Runs_per_Wicket		  <- gsub("^.*\\t(*.*)\\r.*$", "\\1", as.character(ground_Fields_Value[16]))
      Average_Runs_per_Over		    <- gsub("^.*\\t(*.*)\\r.*$", "\\1", as.character(ground_Fields_Value[17]))
      
      
      
      ground_DF <- rbind(ground_DF,data.frame(Ground_Name,City,Country,First_ODI_Match,Last_ODI_Match,Matches_Played,Matches_Won_by_Home_Side,Matches_Won_by_Touring_Side,Matches_Won_by_Neutral_Side,Matches_Tied,Matches_with_No_Result,Highest_Individual_Innings,Best_Bowling,Highest_Team_Innings,Lowest_Team_Innings,Highest_Run_Chase_Achieved,Average_Runs_per_Wicket,Average_Runs_per_Over))
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    
    )
  
}


#print(ground_DF)
outputFileWithPath <- paste(outputPath,"All_Grounds_Data_1.csv",sep = "")
write.csv(ground_DF,file = outputFileWithPath,row.names = FALSE)

