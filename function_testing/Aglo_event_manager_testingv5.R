######################/////
###### Library  ######/////
library(tidyverse)



###### Ram clean  ######/////
rm(list=ls())

############################/////
###### Main Function  ######/////
performance_manager_funcv2<-function(
  Data_raw,
  Campaign_id,
  Day_st,
  Day_ed,
  Journey_st,
  Journey_ed,
  Var,
  Geo_P,
  Culture_P,
  Affiliate_P,
  Package_P
){
  
  result_v1<-
    Data_raw%>%
    filter(E_UID %in% Campaign_id )%>%
    filter(between(Date, Day_st, Day_ed))%>%
    filter(between(Customer_Journey,Journey_st,Journey_ed))%>%
    filter(Variation %in% Variant)%>%
    filter(Geo%in% Geo_P )%>%
    filter(Culture %in% Culture_P)%>%
    filter(Affiliate_Id %in% Affiliate_P)%>%
    filter(Package_Id %in% Package_P)%>%
    group_by(Variation)%>%
    summarise(
      Ngm_Traffic=sum(Ngm_Traffic),
      ngm_order=sum(Ngm_Conversion),
      non_ngm_order=sum(Non_Ngm_Conversion),
      tot_order=sum(Ngm_Conversion)+sum(Non_Ngm_Conversion),
      
      tot_cr=round((sum(Ngm_Conversion)+sum(Non_Ngm_Conversion))/
                     sum(Ngm_Traffic),5),
      ngm_cr=round(sum(Ngm_Conversion)/sum(Ngm_Traffic),5),
      non_ngm_cr=round(sum(Non_Ngm_Conversion)/sum(Ngm_Traffic),5)
    )%>%
    data.frame()
  
  
  confidence_v1<-
    prop.test( c(result_v1$tot_order[1],
                result_v1$tot_order[2]), 
               c(result_v1$Ngm_Traffic[1],
                 result_v1$Ngm_Traffic[2]), p = NULL, alternative = "two.sided",
              correct = TRUE)
  
  HS_confidencev1<-rbind("-",
                         paste0(round(1-confidence_v1$p.value,2)*100,"%")
                         )%>%data.frame()
  colnames(HS_confidencev1)<-"Test_confidence_main"
  
  confidence_v2<-
    prop.test( c(result_v1$ngm_order[1],
                 result_v1$ngm_order[2]), 
               c(result_v1$Ngm_Traffic[1],
                 result_v1$Ngm_Traffic[2]), p = NULL, alternative = "two.sided",
               correct = TRUE)
  
  HS_confidencev2<-rbind("-",
                         paste0(round(1-confidence_v2$p.value,2)*100,"%")
                         )%>%data.frame()
  colnames(HS_confidencev2)<-"Test_confidence_secondary"
  
  confidence_v3<-
    prop.test( c(result_v1$non_ngm_order[1],
                 result_v1$non_ngm_order[2]), 
               c(result_v1$Ngm_Traffic[1],
                 result_v1$Ngm_Traffic[2]), p = NULL, alternative = "two.sided",
               correct = TRUE)
  
  HS_confidencev3<-rbind("-",
                         paste0(round(1-confidence_v3$p.value,2)*100,"%")
                         )%>%data.frame()
  colnames(HS_confidencev3)<-"Test_confidence_tertiary"

  
  days_of_campaign<-
    count(Data_raw%>%
            filter(E_UID %in% Campaign_id )%>%
            distinct(Date))
  
  num_of_variation<-
    count(Data_raw%>%
            filter(E_UID %in% Campaign_id )%>%
            distinct(Variation))
  
  initial_sample<-
    Data_raw%>%
    filter(E_UID %in% Campaign_id )%>%
    filter(between(Customer_Journey,Journey_st,Journey_ed))%>%
    filter(Variation %in% Variant)%>%
    filter(Geo%in% Geo_P )%>%
    filter(Culture %in% Culture_P)%>%
    filter(Affiliate_Id %in% Affiliate_P)%>%
    filter(Package_Id %in% Package_P)%>%
    summarise(Ngm_Traffic=sum(Ngm_Traffic))
  
  initial_daily<-
    initial_sample/days_of_campaign
  
  
  main_cr_1<-result_v1$tot_cr[1]
  main_cr_2<-result_v1$tot_cr[2]
  
  main_cr_power<-power.prop.test(p1 = main_cr_1 , p2 = main_cr_2,power=.8,sig.level = 0.1)
  
  total_days_req<-round(main_cr_power$n/initial_daily,0)+1
  
  previous_days_ext<-round(2*(days_of_campaign*0.8)/num_of_variation,0)-1
  
  further_days_req <-total_days_req-previous_days_ext
  
  HS_samplepowerv3<-rbind("-",paste0(further_days_req,"days to go"))%>%data.frame()
  colnames(HS_samplepowerv3)<-"Num_days_togo"
  
  result_outputv1<-cbind(result_v1,HS_confidencev1,HS_samplepowerv3,HS_confidencev2,HS_confidencev3)%>%
    mutate(tot_cr=paste0(tot_cr*100,"%"))%>%
    mutate(ngm_cr=paste0(ngm_cr*100,"%"))%>%
    mutate(non_ngm_cr=paste0(non_ngm_cr*100,"%"))%>%
    select(Variation,
           tot_cr,
           Test_confidence_main,
           Num_days_togo,
           ngm_cr,
           Test_confidence_secondary,
           non_ngm_cr,
           Test_confidence_tertiary)%>%
    data.frame()
    
  result_outputv1
}


###################################/////
###### initial value example ######/////
#setwd("C:/Users/wma1/OneDrive - McAfee/Documents/DS Projects/TnL Automated EcoSystem/Data Table Sample/Test data/")

data_test1<-read.csv("/Users/pruthvi/Documents/GitHub/MCATLE_002/fucntion_testing/NGM Test _600_603_581_542_536_537v6.csv")%>%
  mutate(Date = as.Date(as.factor(Date),"%m/%d/%Y"))%>%
  mutate(Geo=as.character(Geo))%>%
  mutate(Geo= ifelse(is.na(Geo), "NA",Geo))%>%data.frame()
  #rename(geo=mcafee_geo)%>%
  #rename(culture=allocation_culture)%>%
  #rename(country=allocation_country)%>%
  #rename(Affiliate_P_id = allocation_Affiliate_P_id)%>%
  #rename(Package_Id=allocation_Package_Id)


Data_raw<-data_test1
Campaign_id <-c("ATP_EN-US_At-Risk_Test_Ph2")
Day_st <- as.Date("2021-01-15")
Day_ed <- as.Date("2021-01-25")
Journey_st <- as.integer(0)
Journey_ed <- as.integer(30)
Variant <- c("a","b")
Geo_P <- c("NA","EMEA")
Culture_P <- c("en-us","en-gb")
Affiliate_P <- c("105","714")
Package_P <- c("431")



df<-performance_manager_funcv2(
  Data_raw,
  Campaign_id,
  Day_st,
  Day_ed,
  Journey_st,
  Journey_ed,
  Variant,
  Geo_P,
  Culture_P,
  Affiliate_P,
  Package_P
)

datatable(df)

