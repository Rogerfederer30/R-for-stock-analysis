

MACDfunc<-function(Close, n=12 ,m=26, k=34, p=35){
  
  MA<-matrix(0,nrow = length(Close),1)
  MA[1:n-1,1]<-NA
  MA12<-MA
  MA12[n]<-sum(Close[1:n],na.rm = T)/n
  MA[1:m-1,1]<-NA
  MA26<-MA
  MA26[m]<-sum(Close[1:m],na.rm = T)/m
  EMA<-matrix(0,nrow = length(Close),1)
  DIF<-matrix(0, nrow = length(Close),1)
  for (ix in c(n:length(Close))) {
    MA12[ix+1]<- (MA12[ix]*12-Close[ix-11]+Close[ix+1])/12
    
  }
  
  for (iy in c(m:length(Close))) {
    MA26[iy+1]<- (MA26[iy]*26-Close[iy-25]+Close[iy+1])/26
  }
  EMA[1:n-1,1]<-NA
  EMA12<-EMA
  EMA[1:m-1,1]<-NA
  EMA26<-EMA
  
  for (x in c(n:length(Close))) {
    EMA12[x]<-(MA12[x]*(x-1)+Close[x]*2)/(x+1)
  }
  for (y in c(m:length(Close))) {
    EMA26[y]<-(MA26[y]*(y-1)+Close[y]*2)/(y+1)
  }
  DIF[1:m-1,1]<-NA
  for (j in c(m:length(Close))) {
    DIF[j]<- EMA12[j]-EMA26[j]
  }
  
  MACD<-matrix(0, nrow = length(Close),1)
  MACD[1:k-1,1]<-NA
  MACD[p-1] = sum(DIF[m:p-1],na.rm = T)/9
  OSC<-matrix(0, nrow = length(Close),1)
  OSC[1:k-1,1]<-NA
  for (l in c(p:length(Close))) {
    MACD[l]<- (MACD[l-1]*(9-1)+DIF[l]*2)/(9+1)
    
  }
  for (o in c(k:length(Close))) {
    OSC[o]<-DIF[o]-MACD[o]
  }
  list(  MALIST=MA12, EMA12LIST=EMA12, EMA26LIST=EMA26, DIFLIST=DIF, MACDLIST=MACD, OSCLIST=OSC)
}