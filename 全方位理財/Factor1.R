#################################################### 事前資料準備 ####################################################
## 資料匯入
# FinancialReport<-read.csv("C:/Users/NKFUST_Quant/Desktop/FinancialReport.csv") # 財報資料
# colnames(FinancialReport) = c("code","date","ROE")
# StockPrice<-read.csv("C:/Users/NKFUST_Quant/Desktop/stockprice.csv")
# colnames(StockPrice) = c("code","date","open","high","low","close","trade_volume")
## 整理財報表格，增加年度欄
load("/Volumes/HD 500G/Work/Quantitative Web/R/sample/sample.RData")
FinancialReport<-cbind(FinancialReport[,1:2], year=trunc(FinancialReport[,2]/100), FinancialReport[,c(3:4)])
#################################################### 事前資料準備 ####################################################
#################################################### 投資事前準備 ####################################################
## 整理財報公佈日時間
FinancialReportDate<-data.frame()
for (y in 2010:2015){
  if (y<2013){ 
    # 紀錄IFRS前的財報公佈日
    FinancialReportDate<-rbind(FinancialReportDate,cbind(rbind(y*10000+rbind(0430,0831,1031),(y+1)*10000+0331),
                                                         t(t(rep(y,4))),rbind(1,2,3,4)))
  }else{
    # 紀錄IFRS後的財報公佈日
    FinancialReportDate<-rbind(FinancialReportDate,cbind(rbind(y*10000+rbind(0515,0814,1114),(y+1)*10000+0331),t(t(rep(y,4))),rbind(1,2,3,4)))
  }
}
colnames(FinancialReportDate)<-c("date","year","season")
# 刪除20151231後的公布日
FinancialReportDate<-FinancialReportDate[-which(FinancialReportDate$date>20151231),]
## 整理實際市場交易日
TradeDate<-data.frame(date=unique(StockPrice[,2]))
#################################################### 投資事前準備 ####################################################
#################################################### 正式開始投資 ####################################################
ProfoiloStock<-data.frame()
ProfoiloReturn<-data.frame()
for (i in 2:nrow(FinancialReportDate)){
  ## 選股
  ## 選股流程：1.找公佈日日有股價、ROE的股票，2.ROE由大到小排序，挑選前5檔的股票投資。
  cat(sprintf("目前正在回測%d資料，進度：%d / %d \n",FinancialReportDate[i,1],i-1,nrow(FinancialReportDate)-1))
  # 找公佈日有股價、ROE的股票
  Date<-TradeDate[min(which(TradeDate[,1]>=FinancialReportDate[i-1,1])),1] # 找出季報實際公佈日
  stock<-StockPrice[StockPrice[,2]==Date,] # 找出公佈日時，有交易的股票
  stocklist<-FinancialReport[which(FinancialReport$year==FinancialReportDate[i-1,2] & FinancialReport$season==FinancialReportDate[i-1,3] & FinancialReport$code%in%stock$code),]
  # 投資ROE前5檔的股票
  stocklist$ROE<-as.numeric(as.character(stocklist$ROE))
  stocklist<-stocklist[order(stocklist$ROE,decreasing=TRUE),]
  stocklist<-stocklist[1:5,]
  stocklist<-as.data.frame(stocklist)
  ## 紀錄投資狀況
  # 紀錄買賣日期
  Date<-cbind(buy_date=TradeDate$date[min(which(TradeDate$date>=FinancialReportDate[i-1,1]))+1],
              sell_date=TradeDate$date[min(which(TradeDate$date>=FinancialReportDate[i,1]))+1])
  # 記錄個股投資損益
  InvestStock<-data.frame()
  for (ix in 1:nrow(stocklist)){
    # 記錄個股買賣價格
    InvestStock<-rbind(InvestStock,  cbind(buy_price=StockPrice$close[which(StockPrice$code==as.character(stocklist[ix,1]) & StockPrice$date==Date[1,1])],
                                           sell_price=StockPrice$close[which(StockPrice$code==as.character(stocklist[ix,1]) & StockPrice$date==Date[1,2])])) 
  }
  # 記錄個股報酬
  InvestStock<-cbind(stocklist[,c(1:4)],InvestStock,return=InvestStock$sell_price/InvestStock$buy_price-1) 
  ProfoiloStock<-rbind(ProfoiloStock,InvestStock)
  ProfoiloReturn<-rbind(ProfoiloReturn,cbind(start=FinancialReportDate[i-1,1],end=FinancialReportDate[i,1],return=mean(InvestStock$return,na.rm = TRUE)-0.004))
}
ProfoiloReturn<-cbind(ProfoiloReturn,CumReturn=cumprod(ProfoiloReturn$return+1)-1)
#################################################### 正式開始投資 ####################################################
#################################################### 投資績效表現 ####################################################
## 畫累積報酬圖
plot(c(1:nrow(ProfoiloReturn)),ProfoiloReturn$CumReturn,type = "l",xlab="time",ylab="return")
## 評估績效
Performance<-rbind(YearReturn=(ProfoiloReturn$CumReturn[nrow(ProfoiloReturn)]+1)^(4/nrow(ProfoiloReturn))-1,
                   SharpRatio=mean(ProfoiloReturn$return)/sd(ProfoiloReturn$return))
#################################################### 投資績效表現 ####################################################