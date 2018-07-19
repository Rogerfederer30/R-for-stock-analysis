rm(list=ls());gc()                                                                                    #清除變數及記憶體
library(quantmod)                                                                                     #套件讀取
library(dplyr)
source("MACD FUNCTION.R")

tc_buy<-0.001425
tc_sell<-0.004425

setwd("C:/Users/USER/Desktop/全方位理財")                                                              #路徑設定
Sp_elect<-read.table(file="electstocks.txt",header = T,stringsAsFactors = F,sep="\t")                      #檔案選取
colnames(Sp_elect)<-c("Code","company","Ind","date","Open","High","Low","Close","Volume","Value")     #欄位命名


#answer for Q1 

fin_sp1<- Sp_elect %>% group_by(Code) %>% arrange(Code,date) %>% filter(Code=="2330") %>%
  mutate(
         EMA12=MACDfunc(Close)$EMA12LIST,
         EMA26=MACDfunc(Close)$EMA26LIST,
         DIF=MACDfunc(Close)$DIFLIST,
         MACD=MACDfunc(Close)$MACDLIST,
         OSC=MACDfunc(Close)$OSCLIST
         )
        

target_sp<-fin_sp1 %>%
  select(Code, company,date, MACD, OSC)




#answer for Q2 #TYPE 1
Sp_elect1 <- Sp_elect %>% group_by(Code) %>% filter(n()>300) %>%              #取出長度夠長之資料(大於300天)
  mutate(
         MA5=SMA(Close,5),
         MA10=SMA(Close,10),
         MA20=SMA(Close,20),
         lag_Close=lag(Close,1),                                              #前1~2日開盤價、收盤價
         lag2_Close=lag(Close,2),                                           
         lag_Open=lag(Open,1),
         lag2_Open=lag(Open,2),
         RedK=ifelse(Open<Close,1,0),                                         #當日紅K判斷
         BlackK=ifelse(lag_Open>lag_Close,1,0),                               #當日黑K判斷
         ret=(Close-Open)/lag_Close,                                          #計算當日報酬
         lag_ret=(lag_Close-lag_Open)/lag2_Close,                             #計算昨日報酬
         CP=runPercentRank(ret,n=240),                                        #將報酬依照歷史資料進行百分位排序(基準為前(n-1)筆資料)                      
         long_RK=ifelse((CP>0.8)&(ret>=0.01),1,0),                            #長紅K判斷(條件八)
         #----------------型態:長紅吞噬------------------------
         Type_longRK=ifelse(
             (lag_ret<(-0.01))& 
             (Open>MA5)&
             (MA5>MA10)&
             (MA5>MA20)&
             (Close>MA5)&
             (lag_Open>MA5)&
             (lag_Close>MA5)&
             (Close>Open)&                                     #條件四
             (lag_Close<lag_Open)&                             #條件五
             (Close>lag_Open)&                                 #條件六
             (lag_Close>Open)&                                 #條件七
             (long_RK==1),1,0),                                #條件八
         Buy_date=lead(date,1),                                              #紀錄隔日進場日期
         Sell_date5=lead(date,5),
         Sell_date10=lead(date,10),                                                                      #紀錄持有十日後出場日期
         Buy_Price=lead(Open,1),
         Sell_Price5=lead(Close,5), 
         Sell_Price10=lead(Close,10),
         Ret5=(Sell_Price5*(1-tc_sell)-Buy_Price*(1+tc_buy))/(Buy_Price*(1+tc_buy)),
         Ret10=(Sell_Price10*(1-tc_sell)-Buy_Price*(1+tc_buy))/(Buy_Price*(1+tc_buy))) %>%                         #計算持有十日後報酬率
  filter(Type_longRK==1) %>% group_by()    


Sp_elect2<-Sp_elect1 %>% summarise(avg_ret5=mean(Ret5,na.rm = T),
                                   win_prob5=mean(Ret5>0,na.rm = T),
                                   avg_ret10=mean(Ret10,na.rm = T),
                                   win_prob10=mean(Ret10>0,na.rm = T))


#TYPE 2

Sp_elect3 <- Sp_elect %>% group_by(Code) %>% filter(n()>300) %>%              #取出長度夠長之資料(大於300天)
  mutate(
    MA5=SMA(Close,5),
    MA10=SMA(Close,10),
    lag_Close=lag(Close,1),                                              #前1~2日開盤價、收盤價
    lag2_Close=lag(Close,2),                                           
    lag_Open=lag(Open,1),
    lag2_Open=lag(Open,2),
    RedK=ifelse(Open<Close,1,0),                                         #當日紅K判斷
    BlackK=ifelse(lag_Open>lag_Close,1,0),                               #當日黑K判斷
    ret=(Close-Open)/lag_Close,                                          #計算當日報酬
    lag_ret=(lag_Close-lag_Open)/lag2_Close,                             #計算昨日報酬
    CP=runPercentRank(ret,n=240),                                        #將報酬依照歷史資料進行百分位排序(基準為前(n-1)筆資料)                      
    JumpRK=ifelse((CP>0.75)&(ret>=0.01),1,0),                            #長紅K判斷(條件八)
    
    Type_JumpRK=ifelse(
        (lag_ret<(-0.01))&
        (Open>MA5)&
        (MA5>MA10)&
        (Close>MA5)&
        (lag_Open>MA5)&
        (lag_Close>MA5)&
        (Close>Open)&                                     #條件四
        (lag_Close<lag_Open)&                             #條件五
        (Close>lag_Open)&
        (Close>lag_Close)&
        (lag_Close<Open)&
        (lag_Open<Open)&
        (JumpRK==1),1,0),                                #條件八
    Buy_date=lead(date,1),                                              #紀錄隔日進場日期
    Sell_date=lead(date,5),
    Sell_date=lead(date,10),                                                                      #紀錄持有十日後出場日期
    Buy_Price=lead(Open,1),
    Sell_Price5=lead(Close,5), 
    Sell_Price10=lead(Close,10),                                          #計算持有十日後出場收盤價
    Ret5=(Sell_Price5*(1-tc_sell)-Buy_Price*(1+tc_buy))/(Buy_Price*(1+tc_buy)),
    Ret10=(Sell_Price10*(1-tc_sell)-Buy_Price*(1+tc_buy))/(Buy_Price*(1+tc_buy))) %>%                         #計算持有十日後報酬率
  filter(Type_JumpRK==1) %>% group_by()    


Sp_elect4<-Sp_elect3 %>% summarise(avg_ret5=mean(Ret5,na.rm = T),
                                   win_prob5=mean(Ret5>0,na.rm = T),
                                   avg_ret10=mean(Ret10,na.rm = T),
                                   win_prob10=mean(Ret10>0,na.rm = T))
