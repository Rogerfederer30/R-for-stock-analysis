rm(list=ls());gc()                                                                                    #清除變數及記憶體
library(quantmod)                                                                                     #套件讀取
library(dplyr)
library(TTR)
library(ggplot2)
library(ggpubr)
library(tidyquant)

#--------------------------------------initial parameters-------------------------------


Buy_TC<-0.001425*0.3
Sell_TC<-0.001425*0.3+0.003
Stop_loss<-0.07

#--------------------------------------initial parameters-------------------------------

setwd("C:/Users/USER/Desktop/全方位理財")                                                              #路徑設定
TSE_OTC<-read.table(file="上市上櫃普通股.txt",header = T,stringsAsFactors = F,sep="\t")
TSE_OTC_MONEY<-read.table(file="外資投信買賣超.txt",header = T,stringsAsFactors = F,sep="\t")
#檔案選取
colnames(TSE_OTC)<-c("code","company","par_value","date","Open","High","Low","Close","Volume","shares_outstanding","MKV")     #欄位命名
colnames(TSE_OTC_MONEY)<-c("code","company","Ind","capital","date","foreign","invest_fund")     #欄位命名

TSE_OTC1<-left_join(TSE_OTC,TSE_OTC_MONEY,by=c("code"="code","company"="company","date"="date"))



TSE_OTC2 <- TSE_OTC1  %>% filter((n()>300)&(date==20100104)) %>% #取出長度夠長之資料(大於300天且20100104市值前300的公司) 
  arrange(-MKV) %>% slice(1:300) 
TSE_OTC2 <- semi_join(TSE_OTC1,TSE_OTC2,by=c("code"="code"))
Code_list<-unique(TSE_OTC2$code)  #取出股票代碼清單

Total_table<-NULL
for(x in c(1:length(Code_list))){
  
  #顯示回測進度
  cat("code Now:",Code_list[x],"code num",x,"/",length(Code_list),'\n')
  #目前回測號碼
  Code_here<-Code_list[x]
  
  TSE_OTC2<-TSE_OTC2  %>% arrange(code,date) %>%  mutate(new_high=NA)
  
  #取出目前回測股票
  Close_list<- TSE_OTC2 %>% filter(code==Code_here)
  for(i in c(300:length(Close_list$Close)-1)){
    if(Close_list$Close[i+1]>max(Close_list$High[(i-299):i])) {
      Close_list$new_high[i+1]<-1
    }
    
    if (Close_list$Close[i+1]<max(Close_list$High[(i-299):i]))
    {
      Close_list$new_high[i+1]<-0
    }
    
  }
  
  Total_table<-bind_rows(Total_table,Close_list)
  
}#for(x in c(1:length(Code_list)))
#-------------------------------strategy------------------------------#

strategy<-function(n=n,x=x,Data=Total_table,Stop_loss=Stop_loss){ #默認參數設定  
  
  TSE_OTC3 <- Total_table  %>% select(code,company,par_value,date,Open,High,Low,Close,Volume,shares_outstanding,MKV,foreign,invest_fund,new_high)%>% 
    filter(n()>300) %>% group_by(code) %>%
    mutate(
      Year=floor(date/10000),
      date=as.Date(as.character(date),"%Y%m%d"), 
      股本=par_value*shares_outstanding*1000,
      MA5=SMA(Close,5),
      MA10=SMA(Close,10),
      MAn=SMA(Close,58),
      Open=as.numeric(Open),
      lag_Close=lag(Close,1), 
      lag_2Close=lag(Close,2),
      lag_Low=lag(Low,1),
      lag_Open=lag(Open,1),
      lag_High=lag(High,1),
      lag_invest_fund=lag(invest_fund,1),
      
      volume_35=SMA(Volume,35),#前1~2日開盤價、收盤價
      in_condition=ifelse((Close>MA5)
                          ,1,0),      
      Buy_date=lead(date,1),
      Buy_Price=lead(Open,1), 
      #計算持有十日後出場收盤價
      投本比=(round(invest_fund/(股本/100000000)/100.0,digit=2)),
      外本比=(round(foreign/(股本/100000000)/100.0,digit=2)),
      out_condition1=ifelse((Close<MAn),1,0),
      out_condition2=ifelse((High>Open)&
                              (High>lag_High)&
                              (Open>Close)&
                              (((High-Open)/High)>n)&
                              ((Volume/volume_35)>x),1,0)
      
    ) 
  
  
  
  
  TSE_OTC4 <- TSE_OTC3 %>% 
    filter((投本比>0)&(in_condition==1)&(new_high==1)) %>% group_by(date) %>%
    arrange(date,(-投本比)) %>% ungroup() 
  Datelist<-unique(TSE_OTC4$date)
  
  
  Buy_Trade_Table<-NULL
  for(i_date in c(1:(length(Datelist)-1)))
    
  {
    #顯示回測進度
    cat("Date Now:",Datelist[i_date],"Date num",i_date,"/",length(Datelist)-1,'\n')
    Date_here<-Datelist[i_date]
    
    Buylist <- TSE_OTC4 %>% filter((date==Datelist[i_date]))  %>% slice(1) 
    #Totallist<-bind_rows(Totallist,Buylist)
    Buy_code <- Buylist %>% pull(code) 
    TSE_code <- TSE_OTC3 %>% 
      filter((code==Buy_code)&(date>Datelist[i_date]))
    #TSE_code$jump[which(is.na(TSE_code$jump))]<-0
    
    #起始部位設定 
    Long_Poit<-0
    
    for(ix in c(1:length(Buylist$code)))
    {
      #第一天投本比最高進場日
      #Buy_start_ind<-min(which(Buylist$jump==1)) 
      #進場條件
      if ((Long_Poit==0)&(Buylist$code[ix]!=1))  #目前做多部位為0且黃金交叉發生
      {
        Long_Poit<-1                                              #做多部位變1
        In_price<-Buylist$Buy_Price[ix]                        #進場價格
        In_date<-Buylist$Buy_date[ix] 
        In_year<-Buylist$Year[ix]#進場日期  
        Stop_Loss_Price<-In_price*(1-Stop_loss)
        # Stop_Gain_Price<-In_price*(1+Stop_gain)
        Out_price<-NULL                                           #建立空欄
      }
      
      TSE_code1 <- TSE_code %>% filter(code==Buylist$code[ix])
      for (i in 1:nrow(TSE_code1)){
        #出場條件
        if ((Long_Poit==1)&                                     #目前做多部位為1 且 以下四個條件任一發生                        
            ((TSE_code1$out_condition1[i]==1)|
             (TSE_code1$Close[i]<Stop_Loss_Price)|
             #(TSE_code1$Close[i]>Stop_Gain_Price)|
             #(TSE_code1$out_condition3[i]==1)|
             (TSE_code1$out_condition2[i]==1)|#死亡交叉發生
             (i==nrow(TSE_code1))))                      #最後交易日發生
        {
          Long_Poit<-0                                               #做多部位變0
          Out_price<-TSE_code1$Close[i]                         #出場價格
          Out_date<-TSE_code1$date[i]
          Out_year<-TSE_code1$Year[i]#進場日期                                        #進場時
          Hold_days<-Out_date-In_date 
          
          
        } 
        
        if ((TSE_code1$out_condition1[i]==1)|
            (TSE_code1$Close[i]<Stop_Loss_Price)|
            #(TSE_code1$Close[i]>Stop_Gain_Price)|
            #(TSE_code1$out_condition3[i]==1)|
            (TSE_code1$out_condition2[i]==1)|#死亡交叉發生
            (i==nrow(TSE_code1)))
          break
      } # for (i_ts in Buy_start_ind:nrow(Stock_price)) 
      
      #紀錄出場原因
      if ((TSE_code1$Close[i]<Stop_Loss_Price)){Out_State="停損"}
      #if ((TSE_code1$Close[i]>Stop_Gain_Price)) {Out_State="停利"}
      if (TSE_code1$out_condition2[i]==1){Out_State="高檔爆量"}
      if (TSE_code1$out_condition1[i]==1){Out_State="破MA"}
      #if (TSE_code1$out_condition3[i]==1){Out_State="當天暴跌"}
      if (i==nrow(TSE_code1)) {Out_State="Happy_End"}
      
      #紀錄交易資訊
      Ret<-(Out_price*(1-Sell_TC)-In_price*(1+Buy_TC))/(In_price*(1+Buy_TC))
      Buy_Trade_Table<-bind_rows(Buy_Trade_Table,
                                 tibble(position="Buy",
                                        code=Buylist$code[ix],
                                        In_Year=In_year,
                                        In_date=In_date,
                                        Out_Year=Out_year,
                                        Out_date=Out_date,
                                        In_price=In_price,
                                        Out_price=Out_price,
                                        Ret=Ret,
                                        Hold_days=Hold_days,
                                        Out_State=Out_State))
      
    }# for (i in 1:nrow(TSE_code1))
    
    
  } #for(i_date in c(1:(length(Datelist)-1)))
  
 
  return(Buy_Trade_Table)
}  #strategy



#-------------------------------尋找最適nMA天數and n----------------------------------


#建立空表
MA_table<-NULL
for (n in c(0.022)) #長MA範圍設定
{
  for(x in c(2))
  {#顯示回測進度
    cat("n:",n, "x:",x, '\n' )
    
    #MA_strategy測試
    output<-strategy(n=n,
                     x=x,
                     Data=Total_table,
                     Stop_loss = Stop_loss
    )
    
    #併表紀錄
    MA_table<-bind_rows(MA_table,output %>%  summarise(Avg_ret=mean(round(Ret,digit=2),na.rm = T),
                                                       Prob_Win=mean(Ret>0,na.rm = T),
                                                       Trade_num=n(),
                                                       Avg_Hold_days=mean(as.numeric(Hold_days)),
                                                       Annual_Ret=252*Avg_ret/Avg_Hold_days,
                                                       minRet=min(Ret,na.rm = T),
                                                       maxRet=max(Ret,na.rm = T),
                                                       GL_ratio=abs(mean(Ret[Ret>0],na.rm = T)/mean(Ret[Ret<0],na.rm = T))) %>% 
                          mutate(n=n,x=x)) 
    print(MA_table)
  }
}



MA_table_max<-MA_table %>% arrange(desc(Annual_Ret)) %>% slice(1) #報酬最高的

#輸入最佳MA參數
output<-strategy(
  n=MA_table_max$n[1],  
  x=MA_table_max$x[1],
  
  Data=Total_table,
  Stop_loss=Stop_loss
)

print(n)

output<-output %>% arrange(In_date) 

#-------------------------------尋找最適nMA天數and n----------------------------------
ggplot(output,aes(Ret,color="cut"))+geom_histogram(bins=75)

#-------------------------------績效結果----------------------------------
Annual_result <- output %>% #新增年欄位
  group_by(In_Year) %>% 
  summarise(Avg_ret=mean(Ret),
            Prob_Win=mean(Ret>0),
            Trade_num=n(),Avg_Hold_days=mean(as.numeric(Hold_days)),
            Annual_Ret=252*Avg_ret/Avg_Hold_days,
            minRet=min(Ret),maxRet=max(Ret),
            GL_ratio=abs(mean(Ret[Ret>0])/mean(Ret[Ret<0])))
#-------------------------------績效結果----------------------------------

# 設定要繪製的交易明細樣本位置
plotSample <- 293                                  # 只要修改此值即可繪製不同的交易(1到nrow(Sp_elect1))

# 繪製交易的股票代碼
plotCode <- output$code[plotSample]                              #樣本股要代碼
inDate <- output$In_date[plotSample]                            #樣本進場日期
outDate <- output$Out_date[plotSample]                          #樣本出場日期

# 整理該股票的股價資料
stockData <- TSE_OTC3 %>% filter((code==plotCode))
stockData <- stockData[which(stockData$code==plotCode),]                          # 取出樣本公司
stockData <- stockData[,c("date","Open","High","Low","Close","Volume")] %>%     # 取出繪圖所需資料(開,高,收,低,成交量)
  mutate(MA5=SMA(Close,5),                                                      # 利用新資料重新計算MA
         MA10=SMA(Close,10),
         MA20=SMA(Close,20),
         MA58=SMA(Close,58),
         MA120=SMA(Close,120),
         MA240=SMA(Close,240))
# 繪圖起始日
matchSite <- which(stockData$date==inDate)-20                                               # 取進場前35天
plotStartDate <- stockData$date[ifelse(matchSite<1, 1, matchSite)]                         # 此處用ifelse避免資料超出邊界

# 繪圖結束日
matchSite <- which(stockData$date==outDate)+20                                            # 取出場後35天
plotEndDate <- stockData$date[ifelse(matchSite>nrow(stockData), nrow(stockData), matchSite)] # 此處用ifelse避免資料超出邊界

# 要繪製的股價資料範圍
plotData <- stockData[which((stockData$date>=plotStartDate)&(stockData$date<=plotEndDate)),] 

# 加入進場位置資訊
plotData$inSite <- rep(NA, nrow(plotData))                                                     #先新增空欄位
plotData$inSite[which(plotData$date==inDate)] <- plotData$Open[which(plotData$date==inDate)]      #找出進場開盤價

# 加入出場位置資訊
plotData$outSite <- rep(NA, nrow(plotData))                                                       #先新增空欄位
plotData$outSite[which(plotData$date==outDate)] <- plotData$Close[which(plotData$date==outDate)]  #找出進場收盤價

# 將plotData資料由data.frame格式轉為xts，符合繪圖資料格式要求
plotdate <-plotData$date   #轉換日期格式
plotData$MA5[which(is.na(plotData$MA5))]<-0
plotData$MA10[which(is.na(plotData$MA10))]<-0
plotData$MA20[which(is.na(plotData$MA20))]<-0
plotData$MA58[which(is.na(plotData$MA58))]<-0
plotData$MA120[which(is.na(plotData$MA120))]<-0
plotData$MA240[which(is.na(plotData$MA240))]<-0
plotData$inSite[which(is.na(plotData$inSite))]<-0
plotData$outSite[which(is.na(plotData$outSite))]<-0

plotData <- xts(plotData[,-1], order.by= plotdate )                #原日期欄位刪除，row名稱轉為日期


# 設定K棒顏色
myTheme <- chart_theme()                # theme設定 
myTheme$col$dn.col <- c("chartreuse3")  # 跌K棒顏色
myTheme$col$up.col <- c("firebrick3")   # 漲K棒顏色
#先繪出技術線圖底稿
plot(chart_Series(plotData[,c("Open","High","Low","Close","Volume")],name=paste0(plotCode," 技術分析圖形"),theme=myTheme))
add_Vo()                                # 加入成交量圖形

# type=畫圖類型，l(line)為畫線，p(point)為畫點
# col=顏色設定
# pch=繪製點的圖形
# cex=圖形大小
# lwd=線的粗度

# 加入5日移動平均線
add_TA(plotData$MA5, on=1, type="l", col="blue", lwd=1.5)
# 加入10日移動平均線
add_TA(plotData$MA10, on=1, type="l", col="pink", lwd=1.5)
# 加入20日移動平均線
add_TA(plotData$MA20, on=1, type="l", col="orange", lwd=1.5)
# 加入60日移動平均線
add_TA(plotData$MA58, on=1, type="l", col="green", lwd=1.5)
# 標註進場位置
add_TA(plotData$inSite, on=1, type="p", col="red", pch=2, cex=5, lwd=3)
# 標註出場位置
add_TA(plotData$outSite, on=1, type="p", col="green", pch=6, cex=4, lwd=3)



stockData <- c("^TWII", "2317.TW", "2330.TW", "2412.TW") %>%
  tq_get(get = "stock.price", from = "2011-01-01", to = "2011-12-31")
stockData %>%
  filter(symbol == "^TWII") %>%
  ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close),
                   color_up = "firebrick3", color_down = "chartreuse3", 
                   fill_up  = "firebrick3", fill_down  = "chartreuse3") +
  geom_ma(ma_fun = SMA, n = 5, color = "blue", linetype = 7,size = 1) +
  geom_ma(ma_fun = SMA, n = 10, color = "orange", linetype = 7, size = 1) + 
  geom_ma(ma_fun = SMA, n = 20, color = "green", linetype = 7, size = 1) +
  labs(title = "加權指數 Candlestick Chart", y = "Closing Price", x = "") + 
  theme_tq() +
  scale_x_date(expand = c(0, 0))

source('performance.R')

performance(output$Ret)





