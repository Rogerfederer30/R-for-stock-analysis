rm(list=ls())
library(dplyr)
library(quantmod)
setwd("D:/")
stockprice<-read.table(file ="finstock.txt",header = T,stringsAsFactors =F,sep = "\t")
colnames(stockprice)<-c("code","company","date","open","high","low","close","volumn","trading_value")

#第一題 找出每年公司收盤價大於60MA的比例
SMA_60<-stockprice%>% arrange(code,date)%>%
  group_by(code)%>%
  filter(n()>=60)%>%
  mutate(year=as.numeric(substring(date,1,4)),
         month=as.numeric(substring(date,5,6)),MA60=SMA(close,60),is_longposition=as.numeric(close>MA60))%>%
  group_by()%>%group_by(code,company,year)%>%
  summarise(totalNums=n(),isone=sum(is_longposition==TRUE,na.rm = T),rate=isone/totalNums)
  


#第二題 找出 10MA>20MA
SMA_10<-stockprice%>% arrange(code,date)%>%
  group_by(code)%>%
  filter(n()>=60)%>%
  mutate(MA10=SMA(close,10),MA20=SMA(close,20))%>%
  filter(MA10>MA20,date==20180321)%>%
  select(code)


