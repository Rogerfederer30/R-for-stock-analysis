args = commandArgs(trailingOnly=TRUE)

source("RealTimeFunction.R")

date<-args[1]
Date<-paste0(substr(date,1,4),"-",substr(date,5,6),"-",substr(date,7,8))

#設定初始倉位，若為0則為無在倉部位
index <- 0

#設定停損停利點
stopLoss <- 10
takeProfit <- 10

#設定進出場時機點
OrderPrice <- 0
CoverPrice <- 0
OrderTime <- strptime('08:46:00.00','%H:%M:%OS')
CoverTime <- strptime('09:48:00.00','%H:%M:%OS')

#進場條件判斷
while (index==0){
 MatchInfo <- GetMatchData(date)
 MatchPrice <- as.numeric(MatchInfo[[1]][2])
 MatchTime <- strptime(MatchInfo[[1]][1],"%H:%M:%OS")

 if(MatchTime >= OrderTime){
  OrderPrice <- MatchPrice
  index <- 1
  print(paste("Order Buy Success Price:",OrderPrice))
  x<-OrderMKT('TXFK7','B',1)
  print(x)
  break
 }
}

#出場條件判斷
while (index!=0){
 MatchInfo <- GetMatchData(date)
 MatchPrice <- as.numeric(MatchInfo[[1]][2])
 MatchTime <- strptime(MatchInfo[[1]][1],"%H:%M:%OS")

 if(MatchPrice > OrderPrice + takeProfit | MatchPrice + stopLoss < OrderPrice | MatchTime >= CoverTime ){
  CoverPrice <- MatchPrice
  index <- 0
  print(paste("Cover Buy Success Price:",CoverPrice,"Profit:",CoverPrice-OrderPrice))
  x<-OrderMKT('TXFK7','S',1)
  print(x)

  break
 }
}

