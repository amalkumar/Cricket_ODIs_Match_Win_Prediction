setwd("D:/Study_Material/DataAnalytics/Cricket/ODIs/2 - Model_Creation/MC_Code/")

source("Method_decileBinning_ContinuousVariable.r")




Bin_DF_20Perc_Continuous_Variables <- function(df,var_name,target_var,teamNameDirectoryonDate){
  
  Perc_Bin_Rules <- paste(teamNameDirectoryonDate,"20Perc_Bin_Rules/",sep = "")
  dir.create(path = Perc_Bin_Rules,showWarnings = FALSE)
  
  dec <- 10
  total_record <- nrow(df)
  LevelPerc <- floor(0.2*total_record)
  
  while(TRUE){
    #print(dec)
    dat <- decileBinning_ContinuousVariable(dec,df,var_name,target_var)
    #print("-----Debug 0")
    dat<-dat[order(dat$Event_Rate),]
    #print("-----Debug 1")
    equal_levels <- sum(dat$GreaterThan == dat$LessThan)
    #print("-----Debug 2")
    #print(equal_levels)
    if(equal_levels>0){
      dec <- dec - 1
    } else{
      break
    }
    #print("-----Debug 3")
  
  
  }
  
  dat$dec_Seq <- 1:nrow(dat)
  #print("-----Debug 4")
  
  rec_count_EqualOrGreaterThan70 <- dat %>% filter(Event_Rate>=0.7) %>% summarise(recs=sum(Decile_Records))
  dec_seq_EqualOrGreaterThan70 <- (dat %>% filter(Event_Rate>=0.7))$dec_Seq
  
  rec_count_EqualOrGreaterThan60 <- dat %>% filter(Event_Rate>=0.6) %>% summarise(recs=sum(Decile_Records))
  dec_seq_EqualOrGreaterThan60 <- (dat %>% filter(Event_Rate>=0.6))$dec_Seq
  
  rec_count_EqualOrGreaterThan50 <- dat %>% filter(Event_Rate>=0.5) %>% summarise(recs=sum(Decile_Records))
  dec_seq_EqualOrGreaterThan50 <- (dat %>% filter(Event_Rate>=0.5))$dec_Seq
  #print("-----Debug 5")
  
  
  rec_count_LessThan70 <- dat %>% filter(Event_Rate<0.7) %>% summarise(recs=sum(Decile_Records))
  dec_seq_LessThan70 <- (dat %>% filter(Event_Rate<0.7))$dec_Seq
  
  rec_count_LessThan60 <- dat %>% filter(Event_Rate<0.6) %>% summarise(recs=sum(Decile_Records))
  dec_seq_LessThan60 <- (dat %>% filter(Event_Rate<0.6))$dec_Seq
  
  rec_count_LessThan50 <- dat %>% filter(Event_Rate<0.5) %>% summarise(recs=sum(Decile_Records))
  dec_seq_LessThan50 <- (dat %>% filter(Event_Rate<0.5))$dec_Seq
  #print("-----Debug 6")
  
  
  rec_count_Between50_LessThan70 <- dat %>% filter(Event_Rate>=0.5,Event_Rate<0.7) %>% summarise(recs=sum(Decile_Records))
  dec_seq_Between50_LessThan70 <- (dat %>% filter(Event_Rate>=0.5,Event_Rate<0.7))$dec_Seq
  
  rec_count_Between50_LessThan60 <- dat %>% filter(Event_Rate>=0.5,Event_Rate<0.6) %>% summarise(recs=sum(Decile_Records))
  dec_seq_Between50_LessThan60 <- (dat %>% filter(Event_Rate>=0.5,Event_Rate<0.6))$dec_Seq
  
  rec_count_Between60_LessThan70 <- dat %>% filter(Event_Rate>=0.6,Event_Rate<0.7) %>% summarise(recs=sum(Decile_Records))
  dec_seq_Between60_LessThan70 <- (dat %>% filter(Event_Rate>=0.6,Event_Rate<0.7))$dec_Seq
  #print("-----Debug 7")
  
  
  l3_dec_seq <- -1
  l2_dec_seq <- -1
  l1_dec_seq <- -1
  l0_dec_seq <- -1
  exception_level <- -1
  #print("-----Debug 8")
  if(rec_count_EqualOrGreaterThan70>=LevelPerc){
    l3_dec_seq <- dec_seq_EqualOrGreaterThan70
    
    if (rec_count_Between60_LessThan70>=LevelPerc){
      l2_dec_seq <- dec_seq_Between60_LessThan70
      
      if(rec_count_Between50_LessThan60>=LevelPerc){
        l1_dec_seq <- dec_seq_Between50_LessThan60
        l0_dec_seq <- dec_seq_LessThan50
        
      } else {
        l1_dec_seq <- dec_seq_LessThan60
        
      }
    } else {
      if (rec_count_Between50_LessThan70>=LevelPerc){
        l2_dec_seq <- dec_seq_Between50_LessThan70
        l1_dec_seq <- dec_seq_LessThan50
        
      } else {
        l2_dec_seq <- dec_seq_LessThan70
        
      }
      
    }
  } else {
    if (rec_count_EqualOrGreaterThan60>=LevelPerc){
      l2_dec_seq <- dec_seq_EqualOrGreaterThan60
      
      if(rec_count_Between50_LessThan60>=LevelPerc){
        l1_dec_seq <- dec_seq_Between50_LessThan60
        l0_dec_seq <- dec_seq_LessThan50
        
      }else {
        l1_dec_seq <- dec_seq_LessThan60
        
      }
      
    }else {
      if(rec_count_EqualOrGreaterThan50>=LevelPerc){
        l2_dec_seq <- dec_seq_EqualOrGreaterThan50
        l1_dec_seq <- dec_seq_LessThan50
        
      }else {
        exception_level <- 0
      }
      
    }
    
    
  } 
  #print("-----Debug 9")
  
  dat$level <- ifelse(dat$dec_Seq %in% l3_dec_seq,3,ifelse(dat$dec_Seq %in% l2_dec_seq,2,ifelse(dat$dec_Seq %in% l1_dec_seq,1,ifelse(dat$dec_Seq %in% l0_dec_seq,0,-1))))

  fileName <- paste(var_name,".csv",sep = "")
  outputFileName <- paste(Perc_Bin_Rules,fileName,sep = "")
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