rm(list=ls());gc()                                                                                    #清除變數及記憶體
library(quantmod)                                                                                     #套件讀取
library(dplyr)
library(ggplot2)
library(ggpubr)
setwd("C:/Users/USER/Desktop")                                                              #路徑設定
Sp_elect<-read.table(file="electstocks.txt",header = T,stringsAsFactors = F,sep="\t")                      #檔案選取
colnames(Sp_elect)<-c("code","company","Ind","date","Open","High","Low","Close","Volume","Value")     #欄位命名


#answer for q1 
Sp_elect1 <- Sp_elect %>% group_by(code) %>% filter(n()>300) %>%              #取出長度夠長之資料(大於300天)
  mutate(MA5=SMA(Close,5),                                                    #計算MA5~240
         MA10=SMA(Close,10),
         MA20=SMA(Close,20),
         MA60=SMA(Close,60),
         MA120=SMA(Close,120),
         MA240=SMA(Close,240),
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
           (lag_ret<(-0.01))&                                #昨日報酬為報酬小於-1%
             (MA60>MA5)&                                       #條件一
             (MA60>MA10)&                                      #條件二
             (MA60>MA20)&                                      #條件三
             (Close>Open)&                                     #條件四
             (lag_Close<lag_Open)&                             #條件五
             (Close>lag_Open)&                                 #條件六
             (lag_Close>Open)&                                 #條件七
             (long_RK==1),1,0),                                #條件八
         Buy_date=lead(date,1),                                              #紀錄隔日進場日期
         Sell_date=lead(date,10),                                            #紀錄持有十日後出場日期
         Buy_Price=lead(Open,1),                                             #計算隔日進場開盤價
         Sell_Price=lead(Close,10),                                          #計算持有十日後出場收盤價
         Ret10=(Sell_Price-Buy_Price)/Buy_Price) %>%                         #計算持有十日後報酬率
  filter(Type_longRK==1) %>% group_by()                                       #篩出滿足長紅吞噬條件之公司  

Sp_elect2<-Sp_elect1 %>% summarise(avg_ret=mean(Ret10,na.rm = T),
                                   win_prob=mean(Ret10>0,na.rm = T),
                                   count_num=n(),
                                   minret10=min(Ret10,na.rm = T),
                                   maxret10=max(Ret10,na.rm = T)
                                   )



# 設定要繪製的交易明細樣本位置
plotSample <-  500                                    # 只要修改此值即可繪製不同的交易(1到nrow(Sp_elect1))

# 繪製交易的股票代碼
plotCode <- Sp_elect1$code[plotSample]                              #樣本股要代碼
inDate <- Sp_elect1$Buy_date[plotSample]                            #樣本進場日期
outDate <- Sp_elect1$Sell_date[plotSample]                          #樣本出場日期

# 整理該股票的股價資料
stockData <- Sp_elect[which(Sp_elect$code==plotCode),]                          # 取出樣本公司
stockData <- stockData[,c("date","Open","High","Low","Close","Volume")] %>%     # 取出繪圖所需資料(開,高,收,低,成交量)
  mutate(MA5=SMA(Close,5),                                                      # 利用新資料重新計算MA
         MA10=SMA(Close,10),
         MA20=SMA(Close,20),
         MA60=SMA(Close,60),
         MA120=SMA(Close,120),
         MA240=SMA(Close,240))
# 繪圖起始日
matchSite <- which(stockData$date==inDate)-35                                                # 取進場前35天
plotStartDate <- stockData$date[ifelse(matchSite<1, 1, matchSite)]                         # 此處用ifelse避免資料超出邊界

# 繪圖結束日
matchSite <- which(stockData$date==outDate)+35                                               # 取出場後35天
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
plotdate <- as.Date(as.character(plotData$date),format="%Y%m%d")   #轉換日期格式
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
add_TA(plotData$MA60, on=1, type="l", col="green", lwd=1.5)
# 標註進場位置
add_TA(plotData$inSite, on=1, type="p", col="red", pch=2, cex=5, lwd=3)
# 標註出場位置
add_TA(plotData$outSite, on=1, type="p", col="green", pch=6, cex=5, lwd=3)

  
  
  
 #ANSWER FOR Q2 
  Sp_elect3<-Sp_elect %>% group_by(code) %>% filter(n()>300) %>%
  mutate(MA5=SMA(Close,5),
         MA10=SMA(Close,10),
         type=ifelse((Close>MA5)&(MA5>MA10),1,0),
         buy_date=lead(date,1),
         sell_date5=lead(date,5),
         sell_date10=lead(date,10),
         sell_date20=lead(date,20),
         buy_price=lead(Open,1),
         sell_price5=lead(Open,5),
         sell_price10=lead(Open,10),
         sell_price20=lead(Open,20),
         ret5=(sell_price5-buy_price)/buy_price,
         ret10=(sell_price10-buy_price)/buy_price,
         ret20=(sell_price20-buy_price)/buy_price
        ) %>% filter(type==1) %>% group_by() 
Sp_elect4<-Sp_elect3 %>% summarise(avgret5=mean(ret5,na.rm = T),
                                   prob_win5=mean(ret5>0,na.rm = T),
                                   count_num5=n(),
                                   avgret10=mean(ret10,na.rm = T),
                                   prob_win10=mean(ret10>0,na.rm = T),
                                   count_num10=n(),
                                   avgret20=mean(ret20,na.rm = T),
                                   prob_win20=mean(ret20>0,na.rm = T),
                                   count_num20=n() 
                                   )


                                
                                  
                                   
                                   
                                   
          
