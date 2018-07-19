
fin_sp1 <-fin_sp %>% select(code,company, date ,close) %<%
  mutate(year=as.numeric(substring(date,1,4)),
         month=as.numeric(substring(date,5,6)),
         day=as.numeric(substring(date,7,8)),
         week=isoweek(ymd(date)),
         w_day=wdat(ymd(date),week_start=getOption("lubridate.week.start",1)),
         lag_week=lag(week),
         new_week-ifelse(lag_week!=week,1,0),
         true_week=(cumsum(new_week)+1))
fin_sp1$new_week[which(is.na(fin_sp1$new_week))]<-0
fin_sp1<- fin_sp1 %>% mutate(true_week=(cumsum(new_week)+1))

fin_sp2 <- fin_sp1%>% group_by(code,company,true_week) %>%
  mutate(w_high=max(close),
         w_low=min(close),
         w_close=close[row_number=n()],
         w_open=close[1]) %>%
  filter(w_day==max(w_day)) #找出每周最後一天


 #source"" 把函數另外放在別的檔案 讀取
#寫函數
KD_Ind<-function(Close,k=9) {
  k_min<-runMin(close,k)
  k_max<-runMax(close,k)
  
  RSV_t<-(close-k_min)/(k_max-k_min)
  k<-matrix(0,nrow = length(close),1)
  d<-matrix(0,nrow = length(close),1)
  
  k[1:k-1,1]<-0.5
  d[1:k-1],1<-0.5
  
  for(ix in c(k:length(close)))
  {
    k[ix]<-k[ix-1]*2/3+RSV_t[ix]/3
    d[ix]<-d[ix-1]*2/3+k[ix]/3
  }
  return(cbind(k,d))
  
  #list(klist=k,dlist=d)
  
}



target_sp<-fin_sp %>% group_by(code) %>% filter(code=="2801") 

is_buy_post<-0 

for(ix in c(2:nrow(target_sp)-1))
{
  if(is_buy_post==0){#判斷買入
    if((target_sp$w_k[ix-1]<target_sp$w_d[ix-1])&(target_sp$w_k[ix]<target_sp$w_d[ix])){
      is_buy_post<-1
      buy_price<-target_sp$w_open[ix+1]
      sell_price<-NULL
    }
    
    
  }
}

for(ix in c(2:nrow(target_sp)-1))
{
  if(is_buy_post==0){#判斷賣出
    if((target_sp$w_k[ix-1]>target_sp$w_d[ix-1])&(target_sp$w_k[ix]<target_sp$w_d[ix])){
      is_buy_post<0
      sell_price<-target_sp$w_close[ix]
      p_l<-(sell_price*(1-tc_sell)-buy_price*(1+tc_buy))
      
    }
    
    
  }
}