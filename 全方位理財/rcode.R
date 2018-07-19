x_annual_mean<-0.03
x_annual_sd<-0.05
M<-10000
Ret_daily<-x_annual_mean/252+rnorm(M)*x_annual_sd/sqrt(252)

quantile95<-quantile(Ret_daily,prob=0.05)#¥ª§À
Ret_mean<-mean(Ret_daily)

VaR_daily95<-Ret_mean-quantile95




x<-rnorm(100000)
n<-length(x)-sum(is.na(x))
output<-sqrt(  ( sum(x^2,na.rm=T)-   n*( sum(x,na.rm=T)/n )^2 )/(n-1)   )
#sqrt(  ( sum(x^2,na.rm=T)-   n*( mean(x,na.rm=T) )^2)/(n-1)   )
output

x<-c(1,2,3,3232,123,32,NA,NA)

sd(x,na.rm=T)
sd_x(x)