#################################################### 事前資料準備 ####################################################
## 資料匯入
# FinancialReport<-read.csv("C:/Users/NKFUST_Quant/Desktop/FinancialReport.csv") # 財報資料
# colnames(FinancialReport) = c("code","date","ROE")
# StockPrice<-read.csv("C:/Users/NKFUST_Quant/Desktop/stockprice.csv")
# colnames(StockPrice) = c("code","date","open","high","low","close","trade_volume")
## 整理財報表格，增加年度欄
rm(list=ls())
setwd("D:/F_Analysis/")
load("sample.RData")

#--------------------------------------initial parameters-------------------------------
start_year<-2012
end_year<-2015
tran_buy<-0.001425
tran_sell<-0.001425+0.003
Delay_buy<-1
Buy_num<-20
weight_port<-rep(1,20)/20
Fund_Value<-10000000
#--------------------------------------initial parameters-------------------------------


FinancialReport<-cbind(FinancialReport[,1:2], 
                       year=floor(FinancialReport[,2]/100), 
                       FinancialReport[,c(3:4)])
#################################################### 事前資料準備 ####################################################
#################################################### 投資事前準備 ####################################################
## 整理財報公佈日時間

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
  Date<-rep(start_year:2012,each=4)*10000+c(331,430,831,1031)
  Year<-rep(start_year:2012,each=4)
  Year<-c(start_year-1,Year)
  Year<-Year[1:(length(Year)-1)]
  Season<-rep(c(1,2,3,4),length(start_year:2012))
  Season<-c(4,Season)
  Season<-Season[1:(length(Season)-1)]
  FinancialReportDate1<-cbind(Date,Year,Season)
  
  Date<-rep(2013:end_year,each=4)*10000+c(331,515,814,1114)
  Year<-rep(2013:end_year,each=4)
  Year<-c(2013-1,Year)
  Year<-Year[1:(length(Year)-1)]
  Season<-rep(c(1,2,3,4),length(2013:end_year))
  Season<-c(4,Season)
  Season<-Season[1:(length(Season)-1)]
  FinancialReportDate2<-cbind(Date,Year,Season)
  FinancialReportDate<-rbind(FinancialReportDate1,FinancialReportDate2)
}

TradeDate<-unique(StockPrice[,2])
# 
# FinancialReportDate<-data.frame()
# for (y in 2010:2015){
#   if (y<2013){ 
#     # 紀錄IFRS前的財報公佈日
#     FinancialReportDate<-rbind(FinancialReportDate,cbind(rbind(y*10000+rbind(0430,0831,1031),(y+1)*10000+0331),
#                                                          t(t(rep(y,4))),rbind(1,2,3,4)))
#   }else{
#     # 紀錄IFRS後的財報公佈日
#     FinancialReportDate<-rbind(FinancialReportDate,cbind(rbind(y*10000+rbind(0515,0814,1114),(y+1)*10000+0331),t(t(rep(y,4))),rbind(1,2,3,4)))
#   }
# }
# colnames(FinancialReportDate)<-c("date","year","season")
# # 刪除20151231後的公布日
# FinancialReportDate<-FinancialReportDate[-which(FinancialReportDate$date>20151231),]
# ## 整理實際市場交易日
# TradeDate<-data.frame(date=unique(StockPrice[,2]))
#################################################### 投資事前準備 ####################################################
#################################################### 正式開始投資 ####################################################

Profoilo_Table<-data.frame(Time=NA,Fund_Value=Fund_Value,Ret=0)
Trade_Table_list<-NULL
FinancialReportDate<-as.tibble(FinancialReportDate)

FinancialReport$code<-as.numeric(as.character(FinancialReport$code))
FinancialReport$ROE<-as.numeric(as.character(FinancialReport$ROE))
FinancialReport<-as.tibble(FinancialReport)
for (i in 2:nrow(FinancialReportDate)){
  ## 選股
  ## 選股流程：1.找公佈日日有股價、ROE的股票，2.ROE由大到小排序，挑選前5檔的股票投資。
  cat(sprintf("目前正在回測%d資料，進度：%d / %d \n",FinancialReportDate$Date[i],i-1,nrow(FinancialReportDate)-1))
  # 找公佈日有股價、ROE的股票
  Date<-TradeDate[min(which(TradeDate>=FinancialReportDate$Date[i-1]))] # 找出季報實際公佈日
  #stock<-StockPrice[StockPrice[,2]==Date,] # 找出公佈日時，有交易的股票
  stock<-StockPrice %>% filter(date==Date) # 找出公佈日時，有交易的股票

  colnames(FinancialReportDate)


  target_data<-left_join(stock,FinancialReportDate,by=c("date"="Date"))
  target_data<-left_join(target_data,FinancialReport,by=c("code"="code","Year"="year","Season"="season"))

  target_code<-target_data %>% arrange(desc(ROE)) %>% slice(1:Buy_num) %>% pull(code)
  
  Rep_index<-min(which(TradeDate>=FinancialReportDate$Date[i-1])) # 找出季報實際公佈日的INDEX
  
  Buy_date<-TradeDate[Rep_index+Delay_buy]
  Sell_date<-TradeDate[min(which(TradeDate>=FinancialReportDate$Date[i]))]
  
  Buy_Position<-StockPrice %>% 
    filter(date==Buy_date,code %in% target_code) %>%
    mutate(Buy_Price=open)
  
  Sell_Position<-StockPrice %>% 
    filter(date==Sell_date,code %in% target_code) %>%
    mutate(Sell_Price=close,Sell_date=date) %>% select(code,Sell_date,Sell_Price)
  
  
  Trade_Table<-left_join(Buy_Position,Sell_Position,by=c("code"="code"))
  Trade_Table$Sell_Price[which(is.na( Trade_Table$Sell_Price))]<-0
  
  Ret<-(Trade_Table$Sell_Price*(1-tran_sell)-Trade_Table$Buy_Price*(1+tran_buy))/(Trade_Table$Buy_Price*(1+tran_buy))
  
  Trade_Table_list<-bind_rows(Trade_Table_list,Trade_Table)
  
  Profoilo_Ret<-sum(weight_port*Ret)
  Fund_Value<-Fund_Value*(1+Profoilo_Ret) 
  
  Profoilo_Table<-bind_rows(Profoilo_Table,tibble(Time=Sell_date,Fund_Value=Fund_Value,Ret=Profoilo_Ret))
}
  #   
#   
#   stocklist<-FinancialReport[which(FinancialReport$year==FinancialReportDate[i-1,2] & FinancialReport$season==FinancialReportDate[i-1,3] & FinancialReport$code%in%stock$code),]
#   # 投資ROE前5檔的股票
#   stocklist$ROE<-as.numeric(as.character(stocklist$ROE))
#   stocklist<-stocklist[order(stocklist$ROE,decreasing=TRUE),]
#   stocklist<-stocklist[1:5,]
#   stocklist<-as.data.frame(stocklist)
#   ## 紀錄投資狀況
#   # 紀錄買賣日期
#   Date<-cbind(buy_date=TradeDate$date[min(which(TradeDate$date>=FinancialReportDate[i-1,1]))+1],
#               sell_date=TradeDate$date[min(which(TradeDate$date>=FinancialReportDate[i,1]))+1])
#   # 記錄個股投資損益
#   InvestStock<-data.frame()
#   for (ix in 1:nrow(stocklist)){
#     # 記錄個股買賣價格
#     InvestStock<-rbind(InvestStock,  cbind(buy_price=StockPrice$close[which(StockPrice$code==as.character(stocklist[ix,1]) & StockPrice$date==Date[1,1])],
#                                            sell_price=StockPrice$close[which(StockPrice$code==as.character(stocklist[ix,1]) & StockPrice$date==Date[1,2])])) 
#   }
#   # 記錄個股報酬
#   InvestStock<-cbind(stocklist[,c(1:4)],InvestStock,return=InvestStock$sell_price/InvestStock$buy_price-1) 
#   ProfoiloStock<-rbind(ProfoiloStock,InvestStock)
#   ProfoiloReturn<-rbind(ProfoiloReturn,cbind(start=FinancialReportDate[i-1,1],end=FinancialReportDate[i,1],return=mean(InvestStock$return,na.rm = TRUE)-0.004))
# }
# ProfoiloReturn<-cbind(ProfoiloReturn,CumReturn=cumprod(ProfoiloReturn$return+1)-1)
#################################################### 正式開始投資 ####################################################
#################################################### 投資績效表現 ####################################################
## 畫累積報酬圖
plot(c(1:nrow(ProfoiloReturn)),ProfoiloReturn$CumReturn,type = "l",xlab="time",ylab="return")
## 評估績效
Performance<-rbind(YearReturn=(ProfoiloReturn$CumReturn[nrow(ProfoiloReturn)]+1)^(4/nrow(ProfoiloReturn))-1,
                   SharpRatio=mean(ProfoiloReturn$return)/sd(ProfoiloReturn$return))
#################################################### 投資績效表現 ####################################################