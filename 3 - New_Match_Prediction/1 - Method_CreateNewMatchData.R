setwd("D:/Study_Material/DataAnalytics/Cricket/ODIs/3 - New_Match_Prediction/NM_Code/")


options(warn=-1)

library(reshape2)
library(dplyr)
library(sqldf)
library(lubridate)

Create_NewMatch_Data <- function(teamName,onDate) {
  
  # Creating Team and Date Specific Directory
  inputDirectoryPath <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/3 - New_Match_Prediction/NM_Reference_Data/"
  inputTeamNameDirectoryonDate <- paste(inputDirectoryPath,"MC-",teamName,"/",sep = "")
  newMatchInputFile <- paste(inputTeamNameDirectoryonDate,"1 - NM_NewMatch_",teamName,".csv",sep = "")
  #print(newMatchInputFile)
  
  if(!file.exists(newMatchInputFile)){
    
    print("New Match Input File Doesnt exists")
    stop()
    
  } else{
    
    inputFileName <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/3 - New_Match_Prediction/NM_Reference_Data/DC-2-TeamWinningRecord_UpdatedCountry.csv"
    
    if(!file.exists(inputFileName)){
      
      print("Team Winning Record Reference Input File Doesnt exists")
      stop()
      
    } else{
      
      ####### Creating New Match Record for the Team ##########
      
      # reading reference data file i.e. All team winning record with venue country
      df_All_Team_Data <- read.csv(file = inputFileName,as.is=TRUE,header = TRUE)
      max_match_seq <- as.integer(max(df_All_Team_Data$Match_Number))
      new_match_seq <- max_match_seq + 1
      
      
      # reading new match file
      df_newMatch <- read.csv(file = newMatchInputFile,as.is=TRUE,header = TRUE)
      
      # adding Match Number to the new match
      df_result <- cbind("Match_Number" = new_match_seq,df_newMatch)
      #print(df_result)
      
      
      # Creating Team and Date Specific Directory
      outputDirectoryPath <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/3 - New_Match_Prediction/NM_OutPut_Data/"
      teamNameDirectoryonDate <- paste(outputDirectoryPath,"MC-",teamName,"/",sep = "")
      dir.create(path = teamNameDirectoryonDate,showWarnings = FALSE)
      
      
      
      fileName <- paste("NM-1-NewMatchRecord_",teamName,".csv",sep = "")
      outputFileName <- paste(teamNameDirectoryonDate,fileName,sep = "")
      write.csv(df_result,file = outputFileName,row.names = FALSE)
      
      ####### Upating Team Winning Records with New Match Bowling Record for the Team ##########
      
      refInputFileNameTeamWinningRecord <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/3 - New_Match_Prediction/NM_Reference_Data/DC-1-TeamWinningRecord.csv"
      
      if(!file.exists(refInputFileNameTeamWinningRecord)){
        
        print("Reference Team Winning Record File Doesnt exists")
        stop()
        
      } else{
        
        df_win_record <- read.csv(file = refInputFileNameTeamWinningRecord,as.is=TRUE,header = TRUE)
        
        fileName <- paste("DC-1-TeamWinningRecord_",teamName,".csv",sep = "")
        outputFileName <- paste(teamNameDirectoryonDate,fileName,sep = "")
        write.csv(df_win_record,file = outputFileName,row.names = FALSE)
        
        
      }
      
      ####### Upating Team Winning Records With Country with New Match Bowling Record for the Team ##########
      
      refInputFileNameTeamWinningRecordCountry <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/3 - New_Match_Prediction/NM_Reference_Data/DC-2-TeamWinningRecord_UpdatedCountry.csv"
      
      if(!file.exists(refInputFileNameTeamWinningRecordCountry)){
        
        print("Reference Team Winning Record with Country File Doesnt exists")
        stop()
        
      } else{
        df_win_record_country <- read.csv(file = refInputFileNameTeamWinningRecordCountry,as.is=TRUE,header = TRUE)
        
        fileName <- paste("DC-1-TeamWinningRecord_UpdatedCountry_",teamName,".csv",sep = "")
        outputFileName <- paste(teamNameDirectoryonDate,fileName,sep = "")
        write.csv(df_win_record_country,file = outputFileName,row.names = FALSE)
        
      }
      
      
      ####### Upating Bowling Records with New Match Bowling Record for the Team ##########
      
      refInputFileNameBowling <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/3 - New_Match_Prediction/NM_Reference_Data/DC-4-bowling_record.csv"
      
      if(!file.exists(refInputFileNameBowling)){
        
        print("Reference Bowling File Doesnt exists")
        stop()
        
      } else{
        # new match Bowling file name
        newMatchBowlingInputFile <- paste(inputTeamNameDirectoryonDate,"2 - NM_Bowling_",teamName,".csv",sep = "")
        
        if(!file.exists(newMatchBowlingInputFile)){
          
          print("Reference Bowling File Doesnt exists")
          stop()
          
        } else{
          
          # reading reference bowling data file 
          df_ref_bowling <- read.csv(file = refInputFileNameBowling,as.is=TRUE,header = TRUE)
          #print(head(df_ref_bowling,2))
          
          max_match_seq <- as.integer(max(df_ref_bowling$Match_Number))
          new_match_seq <- max_match_seq + 1
          
          # reading new match bowling data file 
          df_newMatch_Bowling <- read.csv(file = newMatchBowlingInputFile,as.is=TRUE,header = TRUE)
          
          # adding Match Number/Season/Team Name to the new match bowling records
          season <- year(as.Date(max(df_newMatch_Bowling$Match_Date)))
          bowling_team <- teamName
          
          df_newMatch_Bowling <- data.frame(cbind(Match_Number=new_match_seq,Season=season,bowling_team=bowling_team,df_newMatch_Bowling))
          df_newMatch_Bowling <- df_newMatch_Bowling[,c("Match_Number","Season","Match_Date","bowling_team","bowler")]
          
          
          # updating new match bowling records with Bowling reference records
          df_merged_bowling <- bind_rows(df_ref_bowling,df_newMatch_Bowling)
          
          # saving the updated file
          fileName <- paste("DC-1-BowlingRecords_",teamName,".csv",sep = "")
          outputFileName <- paste(teamNameDirectoryonDate,fileName,sep = "")
          write.csv(df_merged_bowling,file = outputFileName,row.names = FALSE)
          
        }
        
      }
      
      
      
      
      
      
      ####### Upating Batting Records with New Match Batting Record for the Team ##########
      
      refInputFileNameBatting <- "D:/Study_Material/DataAnalytics/Cricket/ODIs/3 - New_Match_Prediction/NM_Reference_Data/DC-3-batting_record.csv"
      #print(refInputFileNameBatting)
      if(!file.exists(refInputFileNameBatting)){
        #print("ERROR")
        print("Reference Batting File Doesnt exists")
        stop()
        
      } else{
        # new match Batting file name
        newMatchBattingInputFile <- paste(inputTeamNameDirectoryonDate,"3 - NM_Batting_",teamName,".csv",sep = "")
        #print(newMatchBattingInputFile)
        if(!file.exists(newMatchBattingInputFile)){
          
          print("Reference Batting File Doesnt exists")
          stop()
          
        } else{
          
          # reading reference batting data file 
          df_ref_batting <- read.csv(file = refInputFileNameBatting,as.is=TRUE,header = TRUE)
          #print(head(df_ref_batting,2))
          
          max_match_seq <- as.integer(max(df_ref_batting$Match_Number))
          new_match_seq <- max_match_seq + 1
          
          # reading new match batting data file 
          df_newMatch_Batting <- read.csv(file = newMatchBattingInputFile,as.is=TRUE,header = TRUE)
          
          
          # adding Match Number/Season/Team Name to the new match bowling records
          season <- year(as.Date(max(df_newMatch_Batting$Match_Date)))
          batting_team <- teamName
          
          
          df_newMatch_Batting <- data.frame(cbind(Match_Number=new_match_seq,Season=season,batting_team=batting_team,df_newMatch_Batting))
          #print(head(df_newMatch_Batting,2))
          
          
          df_newMatch_Batting <- df_newMatch_Batting[,c("Match_Number","Season","Match_Date","batting_team","batsman")]
          
          
          # updating new match batting records with batting reference records
          df_merged_batting <- bind_rows(df_ref_batting,df_newMatch_Batting)
          
          # saving the updated file
          fileName <- paste("DC-1-BattingRecords_",teamName,".csv",sep = "")
          outputFileName <- paste(teamNameDirectoryonDate,fileName,sep = "")
          write.csv(df_merged_batting,file = outputFileName,row.names = FALSE)
          
        }
        
      }
      
      
      
      
      
      
      
    }
    
  }
  
}