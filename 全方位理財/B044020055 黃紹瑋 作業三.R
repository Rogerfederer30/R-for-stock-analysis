rm(list=ls())
library(quantmod)
library(dplyr)
setwd("c:/Users/USER/Desktop")
sp_elect<-read.table(file="electstocks.txt",header=T,stringsAsFactors = F,sep="\t")
colnames(sp_elect)<-c("code","company","Ind","date","open","High","Low","Close","vol","value")

sp_elect<-sp_elect %>% group_by(code) %>% filter(n()>240) %>% 
  arrange(code,date) %>%
  mutate(MA5=SMA(Close,n=5),MA20=SMA(Close,n=20),lag_close=lag(Close),
                            lag_MA5=lag(MA5),lag_MA20=lag(MA20),
                            lag_2=lag(Close,2),lag_open=lag(open),lag_open2=lag(open,2),
                            is_longred=as.numeric((Close>MA20)&(MA20>MA5)&(MA5>open)
                                                                 &(lag_MA20>lag_MA5)&(lag_MA5>lag_open)
                                                                 &(open<lag_open)&(Close>lag_open)&(open>lag_close)&
                                                                  ((Close-open)/lag_close)>0.02)&((lag_close-lag_open)/lag_2)<(-0.02)) %>%
  
    filter(is_longred==1) %>%
  select(code,date)

                                                               
                                                                  
                                                                  