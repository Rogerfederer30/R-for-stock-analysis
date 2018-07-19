rm(list=ls())
library(quantmod)
library(dplyr)
setwd("c:/Users/USER/Desktop")
sp_elect<-read.table(file="food.txt",header=T,stringsAsFactors = F,sep="\t")
colnames(sp_elect)<-c("code","company","date","open","High","Low","Close","vol","value")
#2
sp_elect1<- sp_elect %>% group_by(code) %>% filter(date==20180425) %>%
  arrange(-Close) 
sp_elect2<-sp_elect1[1:10,] %>% select(code)

#3
sp_elect3<- sp_elect %>% group_by(code)  %>% filter(n()>60) %>% arrange(code) %>%
  mutate(MA5=SMA(Close,5),
         MA10=SMA(Close,10),
         MA20=SMA(Close,20),
         MA60=SMA(Close,60),
         bull_con=as.numeric((MA5>MA10)&(MA10>MA20)&(MA20>MA60))) %>%
filter((bull_con==1)&(date==20180425)) %>% select(code)

#4
sp_elect4<- sp_elect %>% group_by(code)  %>% filter(row_number()>=(n()-246)) %>% arrange(code) %>%
  mutate(logRet=log(Close/lag(Close,1))) %>%          # ­pºâlog³ø¹S²v
  summarise(cumRet=exp(sum(logRet, na.rm=T))-1) %>%
  arrange(-cumRet)
sp_elect5<- sp_elect4[1:10,] 

#5
sp_elect6<- sp_elect %>% group_by(code)  %>% filter(row_number()>=(n()-239)) %>% arrange(code) %>%
  mutate(logret=log(Close/lag(Close,1))) %>%
  summarise(Annual_Ret=mean(logret,na.rm=T)*240,
            Annual_std=sd(logret,na.rm=T)*sqrt(240),
            Sharpe_ratio=Annual_Ret/Annual_std) %>% 
  arrange(-Sharpe_ratio) %>% select(code) 
sp_elect8 <-sp_elect6 [1:10,]
  
  

       

  