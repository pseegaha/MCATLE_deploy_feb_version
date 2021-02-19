
######################/////
###### Library  ######/////
library(stringr)
library(tidyverse)



###### Ram clean  ######/////
rm(list=ls())

############################/////
###### Main Function  ######/////
 #sample(1:10, 1)
format(Sys.time(), "%S")
a<-format(Sys.time(), "%H:%M:%S")
a
paste0(substr(format(Sys.time(), "%H:%M:%S"), 4, 5),"_",substr(format(Sys.time(), "%H:%M:%S"), 7, 8))



euid_func<- 
  function(data){
  
  data_db<-data
  
  euid_db<-
    data_db%>%
    #mutate(new0= str_split(uid, fixed("-")))%>%
    mutate(id1 = str_extract(uid, ".+?(?=-)"))%>%
    mutate(id2 = str_sub(uid, 25, 30))%>%
    mutate(journey_st2=ifelse(Journey_Start>=0,paste0("D",Journey_Start),paste0("M",-1*Journey_Start)))%>%
    mutate(journey_ed2=ifelse(Journey_End>=0,paste0("D",Journey_End),paste0("M",-1*Journey_End)))%>%
    mutate(journey_range2=paste0(journey_st2,"_",journey_ed2))%>%
    mutate(market_channel2=ifelse(Marketing_channel=="Landing Page",as.character("LP"),as.character(Marketing_channel)))%>%
    mutate(geo2=ifelse(NROW(Geo)>1,"M.G",Geo))%>%
    mutate(culture2=ifelse(NROW(Culture)>1,"M.C",Culture))%>%
    mutate(E_UID = paste0(BU_Info,"_",Event_Type,"_",market_channel2,"_",Geo,"_",Culture,"_",journey_range2,"_",id1,id2))%>%
    select(uid,E_UID)
  
  euid_db2<-
  data_db%>%
    left_join(euid_db, "uid")
  
  euid_db2
}



###################################/////
###### initial value example ######/////
# Geo<-c("NA","EMEA")
# Culture<-c("en-us","en-ca")
# NROW(Geo)
# NROW(Culture)

#setwd("C:/Users/wma1/OneDrive - McAfee/Documents/DS Projects/TnL Automated EcoSystem/Data Table Sample/Test data/")


db_sample<-read.csv("/Users/pruthvi/Documents/GitHub/MCATLE_002/fucntion_testing/db_sample.csv")
db_sample
euid_func(db_sample)





