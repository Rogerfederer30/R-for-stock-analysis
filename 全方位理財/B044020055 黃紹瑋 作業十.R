rm(list=ls())

library(tidyquant)
library(dplyr)

rf<-0.015/250

setwd("C:/Users/USER/Desktop/全方位理財")
Sp_elect<-read.table(file="electstocks.txt",header = T,stringsAsFactors = F,sep="\t")
colnames(Sp_elect)<-c("code","company","Ind","date","Open","High","Low","Close","Vol","Value")

Sp_elect1<-cbind(Sp_elect[,1:10],year=floor(Sp_elect$date/10000))

Sp_elect_x<-Sp_elect1 %>% group_by(code) %>%  filter(year!=2018) %>% arrange(code,date) %>%
  mutate(lag_Close=lag(Close),
         ret=log(Close/lag_Close),
         date=as.Date(as.character(date), "%Y%m%d"))
         

Sp_elect_x$ret[which(is.na(Sp_elect_x$ret))]<-0

sharpeRatio  <-  Sp_elect_x %>%  ungroup()  %>% group_by(code,year) %>% filter(n()>2) %>%
  
  tq_performance(Ra = ret,
                 Rb = NULL,
                 performance_fun = SharpeRatio,
                 Rf = rf)

colnames(sharpeRatio) <- c("code","year","ES","SD","VAR") 


Sp_elect2 <- Sp_elect_x %>% group_by(code,year) %>% 
  filter(row_number()==min(row_number())|row_number()==max(row_number()))%>% 
  mutate(out_date=lead(date),
         lead_Close=lead(Close))
         
Sp_elect3 <- left_join(Sp_elect2,sharpeRatio,by=c("code"="code","year"="year")) %>% 
  filter(row_number()==min(row_number())) %>% ungroup() %>% group_by(code) %>% 
                                              mutate(ES=lag(ES),
                                                     SD=lag(SD),
                                                     VAR=lag(VAR))  %>% ungroup()
 
Final_Portifolio<-NULL
for(x in 2013:2017){
   
     Portifolio<-Sp_elect3  %>% filter(year==x)  %>% arrange(-(ES)) %>% head(20) %>%
       mutate(ret1=(lead_Close-Open)/Open) 
     Portifolio1<-Portifolio %>% mutate(year=x,
                                        ES_avg_ret=sum(ret1)/20) %>% select(year,ES_avg_ret) %>% slice(1)
     
     
     
     Portifolio<-Sp_elect3  %>% filter(year==x)  %>% arrange(-(SD)) %>% head(20) %>%
       mutate(ret1=(lead_Close-Open)/Open) 
     Portifolio2<-Portifolio %>% mutate(year=x,
                                        SD_avg_ret=sum(ret1)/20) %>% select(SD_avg_ret) %>% slice(1)
     
     
     Portifolio<-Sp_elect3  %>% filter(year==x)  %>% arrange(-(VAR)) %>% head(20) %>%
       mutate(ret1=(lead_Close-Open)/Open) 
     Portifolio3<-Portifolio %>% mutate(year=x,
                                        VAR_avg_ret=sum(ret1)/20) %>% select(VAR_avg_ret) %>% slice(1)
     
     Total_Portifolio<-cbind(Portifolio1,Portifolio2,Portifolio3)
     Final_Portifolio<-bind_rows(Final_Portifolio,Total_Portifolio)
     
  }
