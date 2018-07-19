#每月平均價格
sp_ret<-stockprice %>% group_by(code)%>%  arrange(code,date)%>%
  filter(n()>60)#計算分群的個數%>%
mutate(year=as.numeric(substring(date,1,4)),month=as.numeric(substring(date,5,6)),ret=log(close/lag(close)),
            MA10=SMA(close,10),MA20=SMA(close,20))
# lag? group %>%
  group_by()%>% group_by(code,year,month)%>%
  summarise(Avg_Close_price=mean(close,na.rm=T),Annual_std=sd(ret,na.rm=T)*sqrt%>%
              summarise((Cum_ret=exp(sum(ret,na.rm = T))-1))%>%
              
            Sharpe_ratio=Annual_Ret/Annual_std)%>% #arrange(-Annual_Ret)
#每個月的平均累積報酬率
  group_by(month)%>%
  summarise(Avg_CUM_ret=mean(Cum_ret))