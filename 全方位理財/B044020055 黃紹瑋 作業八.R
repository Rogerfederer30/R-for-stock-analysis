rm(list=ls())
library(dplyr)
library(quantmod)
setwd("C:/Users/user/Desktop/全方位理財/") 

load("sample.RData") #讀完會有FinancialReport及StockPrice兩個dataframe

#################################################### 投資事前準備 ####################################################

#--------------------------------------initial parameters-------------------------------
start_year<-2012
end_year<-2015
tran_buy<-0.001425
tran_sell<-0.001425+0.003
Buy_num<-5
weight_port<-rep(1,5)/5
Fund_Value = Initial_Fund_Value <-10000000
#--------------------------------------initial parameters-------------------------------

#新增欄位
FinancialReport<-cbind(FinancialReport[,1:2], 
                       year=floor(FinancialReport$date/100), 
                       FinancialReport[,c(3:4)])
#重新命名欄位
colnames(FinancialReport) <- c("code","month","year","season","roe")

# 整理財報公佈日時間
# 財報公告日(2013以前) 3/31,4/30,8/31,10/31
# 財報公告日(2013以後) 3/31,5/15,8/14,11/14

# 根據start_year及end_year的條件篩選財報整裡方式

if ((start_year<=2012)&(end_year<=2012)){
  Date<-rep(start_year:end_year,each=4)*10000+c(331,430,831,1031)
  Year<-rep(start_year:end_year,each=4)
  Year<-c(start_year-1,Year)
  Year<-Year[1:(length(Year)-1)]
  Season<-rep(c(1,2,3,4),length(start_year:end_year))
  Season<-c(4,Season)
  Season<-Season[1:(length(Season)-1)]
  FinancialReportDate<-cbind(Date,Year,Season)
} else if ((start_year>2012)&(end_year>2012)){
  Date<-rep(start_year:end_year,each=4)*10000+c(331,515,814,1114)
  Year<-rep(start_year:end_year,each=4)
  Year<-c(start_year-1,Year)
  Year<-Year[1:(length(Year)-1)]
  Season<-rep(c(1,2,3,4),length(start_year:end_year))
  Season<-c(4,Season)
  Season<-Season[1:(length(Season)-1)]
  FinancialReportDate<-cbind(Date,Year,Season)
  
} else {
  # 財報公告日(2013以前) 3/31,4/30,8/31,10/31
  Date<-rep(start_year:2012,each=4)*10000+c(331,430,831,1031)
  Year<-rep(start_year:2012,each=4)
  Year<-c(start_year-1,Year)
  Year<-Year[1:(length(Year)-1)]
  Season<-rep(c(1,2,3,4),length(start_year:2012))
  Season<-c(4,Season)
  Season<-Season[1:(length(Season)-1)]
  FinancialReportDate1<-cbind(Date,Year,Season)
  
  # 財報公告日(2013以後) 3/31,5/15,8/14,11/14
  Date<-rep(2013:end_year,each=4)*10000+c(331,515,814,1114)
  Year<-rep(2013:end_year,each=4)
  Year<-c(2013-1,Year)
  Year<-Year[1:(length(Year)-1)]
  Season<-rep(c(1,2,3,4),length(2013:end_year))
  Season<-c(4,Season)
  Season<-Season[1:(length(Season)-1)]
  FinancialReportDate2<-cbind(Date,Year,Season)
  # 合併
  FinancialReportDate<-rbind(FinancialReportDate1,FinancialReportDate2)
}



#################################################### 投資事前準備 ####################################################

#################################################### 正式開始投資 ####################################################


StockPrice<-StockPrice %>% group_by(code) %>% mutate(MA5=SMA(close,n=5),
                                               MA10=SMA(close,n=10),
                                               condition=ifelse((MA5>MA10),1,0)
                                                     ) %>% group_by() 
#取出交易日清單
TradeDate<-unique(StockPrice$date)

#建立投組交易表
Profoilo_Table<-data.frame(in_time=NA,out_time=NA,Fund_Value=Fund_Value,Ret=0,CumRet=0)

#建立空表
Trade_Table_list<-NULL

#格式轉換
FinancialReportDate<-as_tibble(FinancialReportDate)
FinancialReport$code<-as.numeric(as.character(FinancialReport$code))
FinancialReport$roe<-as.numeric(as.character(FinancialReport$roe))
FinancialReport<-as_tibble(FinancialReport)




## 選股
## 選股流程：1.找公佈日日有股價、roe的股票，2.roe由大到小排序

for (i in 2:nrow(FinancialReportDate)){
  
  cat(sprintf("目前正在回測%d資料，進度：%d / %d \n",FinancialReportDate$Date[i],i-1,nrow(FinancialReportDate)-1))
  # 找公佈日有股價、roe的股票
  
  Date<-TradeDate[min(which(TradeDate>=FinancialReportDate$Date[i-1]))] # 季報實際公佈交易日
  
  
  stock<-StockPrice %>% filter(date==Date)         # 找出公佈日時，有交易的股票
  stock$year <- FinancialReportDate$Year[i-1]        # 財報年標籤
  stock$season <- FinancialReportDate$Season[i-1]    # 財報季標籤
 
  #合併股價資料與財報資料
  target_data<-left_join(stock,FinancialReport,by=c("code"="code","year"="year","season"="season"))
  
  #取出依roe排序之股票
  target_code<-target_data %>% filter((trade_volume>2000)&(condition==1)) %>% arrange(desc(roe)) %>% slice(1:Buy_num) %>% pull(code)
  
  # 找出季報實際公佈日的INDEX
  Rep_index<-min(which(TradeDate>=FinancialReportDate$Date[i-1])) 
  
  # 找出實際進場日，財報發布之下一交易日進場
  if(Date>FinancialReportDate$Date[i-1]){
    Buy_date <- TradeDate[Rep_index]
  }
  if(Date==FinancialReportDate$Date[i-1]){
    Buy_date <- TradeDate[Rep_index+1]
  }
  
  # 找出實際出場日，下一財報發布前
  Sell_date<-TradeDate[max(which(TradeDate<=FinancialReportDate$Date[i]))]
  
  
  # 交易紀錄
  Buy_Position<-StockPrice %>% 
    filter(date==Buy_date,code %in% target_code) %>%
    mutate(Buy_Price=open)
  
  Sell_Position<-StockPrice %>% 
    filter(date==Sell_date,code %in% target_code) %>%
    mutate(Sell_Price=close,Sell_date=date) %>% select(code,Sell_date,Sell_Price)
  
  Trade_Table<-left_join(Buy_Position,Sell_Position,by=c("code"="code"))
  
 
  #計算報酬
  Ret<-(Trade_Table$Sell_Price*(1-tran_sell)-Trade_Table$Buy_Price*(1+tran_buy))/(Trade_Table$Buy_Price*(1+tran_buy))
  
  #合併回圈交易紀錄
  Trade_Table_list<-bind_rows(Trade_Table_list,Trade_Table)
  
  #投組報酬
  Profoilo_Ret<-sum(weight_port*Ret)
  
  #投組價值
  Fund_Value<-Fund_Value*(1+Profoilo_Ret) 
  
  #累積報酬
  cumRet=round((Fund_Value/Initial_Fund_Value-1),2)
  
  #合併投組交易表
  Profoilo_Table<-bind_rows(Profoilo_Table,tibble(in_time=Buy_date,
                                                  out_time=Sell_date,
                                                  Fund_Value=Fund_Value,
                                                  Ret=Profoilo_Ret,
                                                  CumRet=cumRet))
}

#################################################### 正式開始投資 ####################################################

#################################################### 投資績效表現 ####################################################
## 畫累積報酬圖
plot(c(1:nrow(Profoilo_Table)),Profoilo_Table$CumRet,type = "l",xlab="time",ylab="return")
#################################################### 投資績效表現 ####################################################