setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/3 - New_Match/NM_Code/")
options(warn=-1)

library(dplyr)

DataBinning_Logical_AutomatedForTeam <- function(newMatchDir="D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/3 - New_Match/",newMatchFile="New_Match.csv",BinningParam="3Levels"){
  
  
  # creating the file name with path
  newMatchFileWithPath <- paste(newMatchDir,newMatchFile,sep = "")
  
  if(!file.exists(newMatchFileWithPath)){
    
    print("File Doesnt exists")
    stop()
    
  } else{
    print(newMatchFileWithPath)
    
    # Reading the new match file for prediction
    new_df <- read.csv(file = newMatchFileWithPath,as.is=TRUE,header = TRUE)
    new_df$Date <- as.Date(new_df$Date)
    
    #print(new_df)
    
    count <- 0
    for (i in 1:nrow(new_df)) {
      count <- count + 1
      
      print(paste("Processing New Match file record : ",count))
      
      # Getting the team names
      team1 <- new_df[i,"Team1"]
      team2 <- new_df[i,"Team2"]
      Match_Date <- new_df[i,"Date"]
      countryName <- new_df[i,"Country_Name"]
      
      print(paste("Team 1 : ",team1," :::: Team 2 : ",team2,sep = ""))
      
      # treating each team as main team and other as opponent
      teams <- c(team1,team2)
      for (teamName in teams){
        
        opponentTeam <- ifelse(team1==teamName,team2,team1)
        print(paste("Team is : ",teamName," and Opponent is : ",opponentTeam))
        
        
        teamNameInputDir <- paste(newMatchDir,"MC-",teamName,"/",sep = "")
        inputFileName <- paste("MC-2-No-Binning-AllMatchesDataFor_",teamName,"_Binned",".csv",sep = "")
        inputFileNameWithPath <- paste(teamNameInputDir,inputFileName,sep = "")
        
        if(!file.exists(inputFileNameWithPath)){
          
          print("File Doesnt exists")
          stop()
          
        } else{
          #print(inputFileNameWithPath)
          
          # Reading data
          df_Num <- read.csv(file = inputFileNameWithPath,as.is=TRUE,header = TRUE)
          #print(df_Num)
          
          # Getting index of every Numeric column 
          index <- col(df_Num)
          index <- index[1,]
          
          
          ctt <- 0
          df_Num_res <- df_Num
          for (i in 1:length(names(df_Num))){
            #print("Step - 1")
            var_name <- colnames(df_Num)[index[i]]
            #print("Step - 2")
            if(!("Country_Id"==var_name | "OpponentTeam_Id"==var_name)){
              if(BinningParam=="20Perc" | BinningParam=="3Levels"){
                
                #print("Step - 3")
                ctt <- ctt + 1
                
                # Reading the rule files for binning the new data set using the same rule used for modelling
                rulesInputDirectoryPath <- "D:/Study_Material/DataAnalytics/GIT/ODIs/Output_Data/2 - Model_Creation/MC_Output/"
                rulesDirectoryonDate <- paste(rulesInputDirectoryPath,"MC-",teamName,"/",sep = "")
                Binning_Bin_Rules <- paste(rulesDirectoryonDate,BinningParam,"_Bin_Rules/",sep = "")
                RuleFileName <- paste(Binning_Bin_Rules,var_name,".csv",sep = "")
                #print("Step - 4")
                if(!file.exists(RuleFileName)){
                  
                  print(paste("Rule File Doesnt exists for Variable : ",var_name,sep = ""))
                  stop()
                  
                } else{
                  #print("------------")
                  df_rule <- read.csv(file = RuleFileName,as.is=TRUE,header = TRUE)
                  #print(head(df_rule,2))
                  #print(paste("Binning Param : ",BinningParam," :::: Variable Name : ",var_name))
                  query <- paste("select t2.",var_name,",(select t1.level from df_rule as t1 where t2.",var_name," BETWEEN t1.GreaterThan AND t1.LessThan) as Level  from df_Num as t2",sep="")
                  
                  df_Bin <- sqldf(query)
                  if(is.na(df_Bin$Level)){
                    print("ITS NULL")
                    query <- paste("select t2.",var_name,",(select t1.level from df_rule as t1 where round(t2.",var_name,",1) BETWEEN t1.GreaterThan AND t1.LessThan) as Level  from df_Num as t2",sep="")
                    df_Bin <- sqldf(query)
                  }
                  df_Bin<- df_Bin$Level
                  #print(head(df_Bin,2))
                  
                  df_Num_res <- cbind(df_Num_res,df_Bin)
                  dummy_var_name <- paste("dummy_",var_name,sep = "")
                  names(df_Num_res)[which(colnames(df_Num_res)=="df_Bin")] <- dummy_var_name
                  
                  
                }
                
              } else{
                print("Binning Parameter Not Correct")
                stop()
              }
              
            }
            
          }
          
          #Taking out only binned variables
          dummy_var_list <- grep("dummy_", names(df_Num_res), value = TRUE)
          df_Num_res <- df_Num_res[,which(colnames(df_Num_res) %in% c(dummy_var_list,"Country_Id","OpponentTeam_Id"))]
          #print(length(names(df_Num_res)))
          
          
          #print("===============================")
          #print(df_Num_res)
          #print("===============================")
          
          teamNameOutPutDir <- paste(newMatchDir,"MC-",teamName,"/",sep = "")
          outputFileName <- paste("MC-2-",BinningParam,"-AllMatchesDataFor_",teamName,"_Binned",".csv",sep = "")
          outputFileNameWithPath <- paste(teamNameOutPutDir,outputFileName,sep = "")
          write.csv(df_Num_res,file = outputFileNameWithPath,row.names = FALSE)
          

        }
        
      }
      
    }
    
  }
  
}