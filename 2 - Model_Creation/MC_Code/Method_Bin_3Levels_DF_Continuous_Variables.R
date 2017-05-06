setwd("D:/Study_Material/DataAnalytics/GIT/ODIs/Cricket_ODIs_Match_Win_Prediction/2 - Model_Creation/MC_Code/")

source("Method_decileBinning_ContinuousVariable.r")




Bin_DF_3Levels_Continuous_Variables <- function(df,var_name,target_var,teamNameDirectoryonDate){
  
  print(teamNameDirectoryonDate)
  Levels_Bin_Rules <- paste(teamNameDirectoryonDate,"3Levels_Bin_Rules/",sep = "")
  dir.create(path = Levels_Bin_Rules,showWarnings = FALSE)
  dec <- 10
  
  while(TRUE){
    print("-----Debug 0")
    print(dec)
    print(var_name)
    print("###########################################################")
    dat <- decileBinning_ContinuousVariable(dec,df,var_name,target_var)
    print("###########################################################")
    print("-----Debug 1")
    dat<-dat[order(dat$Event_Rate),]
    print("-----Debug 2")
    #print(dat)
    #print("-----------------------------")
    #print(dat[!(is.na(dat$GreaterThan)),])
    dat <- dat[!(is.na(dat$GreaterThan)),]
    dat <- dat[!(is.na(dat$LessThan)),]
    
    equal_levels <- sum(dat$GreaterThan == dat$LessThan)
    print(equal_levels)
    print("-----Debug 3")
    print(equal_levels)
    if(equal_levels>0){
      dec <- dec - 1
    } else{
      break
    }
    print("-----Debug 4")
    
  }
  
  dat$dec_Seq <- 1:nrow(dat)
  print("-----Debug 5")
  print(dat)
  
  
  total_levels <- nrow(dat)
  
  l1 <- floor(total_levels/3)
  total_levels <- total_levels - l1
  l2 <- floor(total_levels/2)
  l3 <- total_levels <- total_levels - l2
  l1_seq_vec <- dat$dec_Seq[dat$dec_Seq<=l1]    
  l2_seq_vec <- dat$dec_Seq[dat$dec_Seq>l1 & dat$dec_Seq<=l2+l1]  
  l3_seq_vec <- dat$dec_Seq[dat$dec_Seq>l1+l2 & dat$dec_Seq<=l1+l2+l3] 
  
  dat$level <- ifelse(dat$dec_Seq %in% l1_seq_vec,1,ifelse(dat$dec_Seq %in% l2_seq_vec,2,3))
  
  fileName <- paste(var_name,".csv",sep = "")
  outputFileName <- paste(Levels_Bin_Rules,fileName,sep = "")
  write.csv(dat,file = outputFileName,row.names = FALSE)
  
  query <- paste("select t2.",var_name,",(select t1.level from dat as t1 where t2.",var_name," BETWEEN t1.GreaterThan AND t1.LessThan) as Level  from df as t2",sep="")
  
  df_Bin <- sqldf(query)
  df_Bin<- df_Bin$Level
  
  res <- cbind(df,df_Bin)
  dummy_var_name <- paste("dummy_",var_name,sep = "")
  names(res)[which(colnames(res)=="df_Bin")] <- dummy_var_name
  
  
  return(res)
  
  
  
}


#dat <- decileBinning_ContinuousVariable(dec,df,"NewZealand_Winning_Ratio_At_Venue","NewZealand_Win")