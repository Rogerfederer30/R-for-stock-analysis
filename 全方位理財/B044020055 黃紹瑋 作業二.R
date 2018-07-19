rm(list=ls())
library(dplyr)
library(quantmod)
library(Hmisc)
setwd("C:/Users/USER/Desktop")
OTC<-read.table(file = "OTC.txt", header=T, stringsAsFactors = F,sep = "\t")
colnames(OTC)<-c("code","company","Ind","date","open","High","Low","Close","vol","value","capital")

#ANSWER FOR QUESTION 2

OTC1<-OTC %>% group_by(code) %>% filter(n()>240) %>%
  arrange(code,date) %>%
  mutate(MA5=SMA(Close,n=5),MA10=SMA(Close,n=10),MA20=SMA(Close,n=20),
         MA60=SMA(Close,n=60),MA120=SMA(Close,n=120),MA240=SMA(Close,n=240)
         )
HLC<-OTC1[,6:8]
OTC1$K_VALUE=stoch(HLC)[,1]
OTC1$D_VALUE=stoch(HLC)[,2]
OTC1<-OTC1 %>% group_by() %>%
  group_by(code) %>% 
  filter(n()>240) %>%
  arrange(code,date) %>%
  mutate(DIF=MACD(Close)[,1],
  MACD=MACD(Close)[,2],
  OSC=DIF-MACD)

#ANSWER FOR QUESTION 3
todaysdate<-max(OTC1$date)
OTC_isbull<-OTC1 %>% filter(date==todaysdate) %>% 
  mutate(bull_con=as.numeric((MA5>=MA10)&(MA10>=MA20)&(MA20>=MA60)&(MA60>=MA120)&(MA120>=MA240))) %>%
  filter(bull_con==T) %>%
  select(code)
#ANSWER FOR QUESTION 4
OTC_isbull1<-OTC1 %>% filter(date==todaysdate) %>% 
  mutate(bull_con=as.numeric((MA5>=MA10)&(MA10>=MA20)&(MA20>=MA60)&(MA60>=MA120)&(MA120>=MA240)),
         OSC_OVER0=as.numeric(OSC>0),
         is_k_lager_d=as.numeric(K_VALUE>D_VALUE),
         K_LESSTHAN20=as.numeric(K_VALUE<0.2)) %>%
  filter((bull_con==T),(bull_con+OSC_OVER0+is_k_lager_d+K_LESSTHAN20>=3)) %>%
  select(code,bull_con,OSC_OVER0,is_k_lager_d,K_LESSTHAN20)

#ANSWER FOR QUESTION 5-1
OTCisbull3<-OTC1 %>%
  mutate(groupnum=as.numeric(cut2(capital,g=5)),bull_con=as.numeric((MA5>=MA10)&
                                                                  (MA10>=MA20)&
                                                                  (MA20>=MA60)&
                                                                  (MA60>=MA120)&
                                                                  (MA120>=MA240))) %>%
 filter(n()>240,date==20180329) %>%
  group_by(groupnum,Ind) %>%
  summarise(countbull=sum(bull_con==1,na.rm = T),
            totalnum=n(),
            rate=100*mean(bull_con,na.rm = T))
#ANSWER FOR QUESTION 5-2
options(digit=3)
OTCisbull4<-OTC1 %>% 
  filter(row_number()>(n()-251)) %>%
  filter(n()>200) %>%
  mutate(groupnum=as.numeric(cut2(capital,g=5)),
         log_ret=log(Close/lag(Close))) %>%
  group_by(code,Ind,groupnum) %>%
   summarise(avg_ret=252*mean(log_ret,na.rm = T),
            std=sqrt(252)*sd(log_ret,na.rm=T),
            Sharpe_Ratio=avg_ret/std) %>% 

  group_by(Ind,groupnum) %>%
  summarise(avg_avg_ret=mean(avg_ret,na.rm=T),
            avg_std=mean(std,na.rm=T),
            avg_Sharpe_Ratio=avg_avg_ret/avg_std,
            Count_num=n())


#ANSWER FOR QUESTION 5-3 此假設都為連續分布
options(digit=3)
OTCisbull5<-OTC1 %>% 
  filter(row_number()>(n()-251)) %>%
  filter(n()>200) %>%
  mutate(groupnum=as.numeric(cut2(capital,g=5)),
         log_ret=log(Close/lag(Close))) %>%
  group_by(code,Ind,groupnum) %>%
  summarise(annual_ret=252*mean(log_ret,na.rm = T),
            annualsd=sqrt(252)*sd(log_ret,na.rm=T),
            annualSharpe_Ratio=annual_ret/annualsd) %>% 
  
  group_by(Ind,groupnum) %>%
  summarise(avg_ret=mean(annual_ret,na.rm=T),
            avg_std=mean(annualsd,na.rm=T),
            avg_Sharpe_Ratio=avg_ret/avg_std,
            Count_num=n()) %>%
  arrange(-avg_Sharpe_Ratio) 
OTCisbull5<-OTCisbull5[1,]

#ANSWER FOR QUESTION 5-4
options(digit=3)
OTCisbull6<-OTC1 %>% 
  filter(row_number()>(n()-251)) %>%
  filter(n()>200) %>%
  mutate(groupnum=as.numeric(cut2(capital,g=5)),
         log_ret=log(Close/lag(Close)),
         Cum_ret=exp(sum(log_ret,na.rm = T))-1) %>%
  group_by(Ind,groupnum) %>%
summarise(avg_Cum_ret=mean(Cum_ret)*sqrt(252))
