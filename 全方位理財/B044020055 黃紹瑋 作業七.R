#2018/04/19/上課內容修改
rm(list=ls());gc()                                                                                    #清除變數及記憶體
library(quantmod)                                                                                     #套件讀取
library(dplyr)
library(ggplot2)
library(ggpubr)



setwd("C:/Users/user/Desktop/全方位理財/")                                                          #路徑設定

#--------------------initial paramerter-------------------------------
Buy_TC<-0.001425*0.3
Sell_TC<-0.001425*0.3+0.003

#--------------------initial paramerter-------------------------------



Sp_elect<-read.table(file="electstocks.txt",header = T,stringsAsFactors = F,sep="\t")                      #檔案選取
colnames(Sp_elect)<-c("code","company","Ind","date","Open","High","Low","Close","Volume","Value")     #欄位命名


#------------------------------------------------------------------------
MA_strategy<-function(SMA_day=SMA_day,LMA_day=LMA_day,Data=Sp_elect,test_num=NULL,stop_loss_rate=0.1,stop_gain_rate=0.1) {
  #SMA_day<-5
  #LMA_day<-10
  Sp_elect1 <- Sp_elect %>% group_by(code) %>% filter(n()>300) %>%              #取出長度夠長之資料(大於300天)
    mutate(SMA=SMA(Close,SMA_day),                                                    #計算MA5~240
           LMA=SMA(Close,LMA_day),
           Lag_SMA=lag(SMA),
           Lag_LMA=lag(LMA),
           Gold_Cross=ifelse((Lag_SMA<Lag_LMA)&(LMA<SMA),1,0),
           Dead_Cross=ifelse((Lag_SMA>Lag_LMA)&(LMA>SMA),1,0)
    )
  
  Code_list<-unique(Sp_elect1$code)
  Long_Trade_Table<-NULL
  Short_Trade_Table<-NULL
  if (is.null(test_num)) {test_num<-length(Code_list)}
  #for (i_code in 1:length(Code_list) ) #for
  for (i_code in 1:test_num ) #for
  {
    cat("Code Now:",Code_list[i_code],"Code num",i_code,"/",length(Code_list),'\n')
    Long_Poit<-0
    
    Code_here<-Code_list[i_code]
    
    Stock_price<-Sp_elect1 %>% filter(code==Code_here) %>%　arrange(date)
#---------------------------------------------------------------------------------------long position    
    if (sum(which(Stock_price$Gold_Cross==1))>0)
    {
      startlong_ind<-min(which(Stock_price$Gold_Cross==1))
      
      
      for (i_ts in startlong_ind:nrow(Stock_price))
      {
        if ((Long_Poit==0)&(Stock_price$Gold_Cross[i_ts]==1)) # no Buy position
        {
          Long_Poit<-1
          In_price<-Stock_price$Close[i_ts]
          In_date<-Stock_price$date[i_ts]
          Stop_Loss_Price<-In_price*(1-stop_loss_rate)
          Stop_Gain_Price<-In_price*(1+stop_gain_rate) 
          Out_price<-NULL
          In_date_ind<-i_ts                                  #count the initial buying date
        } 
          else if ((Long_Poit==1)& 
                   ((Stock_price$Close[i_ts]<Stop_Loss_Price)|
                    (Stock_price$Close[i_ts]>Stop_Gain_Price)|
                    (Stock_price$Dead_Cross[i_ts]==1)|
                    (i_ts==nrow(Stock_price)))   ) {
          # Clear Position
          Long_Poit<-0
          Out_price<-Stock_price$Close[i_ts]
          Out_date<-Stock_price$date[i_ts]
          Hold_days<-i_ts-In_date_ind+1
          if ((Stock_price$Close[i_ts]<Stop_Loss_Price)) {State="Stop_loss"}
          if ((Stock_price$Close[i_ts]>Stop_Gain_Price)) {State="Stop_gain"}
          if (Stock_price$Dead_Cross[i_ts]==1) {State="Dead_Cross"}
          if (i_ts==nrow(Stock_price)) {State="Last"}
          
          
          Long_Ret<-(Out_price*(1-Sell_TC)-In_price*(1+Buy_TC))/(In_price*(1+Buy_TC))
          Long_Trade_Table<-bind_rows(Long_Trade_Table,
                                      tibble(Position="Buy",
                                             code=Code_here,
                                             In_date=In_date,
                                             Out_date=Out_date,
                                             In_price=In_price,
                                             Out_price=Out_price,
                                             Ret=Long_Ret,
                                             Hold_days=Hold_days,
                                             State=State))
        }
        
      }
      
    }
#-------------------------------------------------------------------------------------- Short position    
    Short_Poit<-0
    if (sum(which(Stock_price$Dead_Cross==1))>0)
    {
      startshort_ind<-min(which(Stock_price$Dead_Cross==1))
      
      
      for (i_ts in startshort_ind:nrow(Stock_price))
      {
        if ((Short_Poit==0)&(Stock_price$Dead_Cross[i_ts]==1)) # Short position
        {
          Short_Poit<-1
          In_price<-Stock_price$Close[i_ts]
          In_date<-Stock_price$date[i_ts]
          Stop_Loss_Price<-In_price*(1+stop_loss_rate)
          Stop_Gain_Price<-In_price*(1-stop_gain_rate)
          Out_price<-NULL
          In_date_ind<-i_ts
        } 
        else if ((Short_Poit==1)& 
                   ((Stock_price$Close[i_ts]<Stop_Gain_Price)|
                    (Stock_price$Close[i_ts]>Stop_Loss_Price)|
                    (Stock_price$Gold_Cross[i_ts]==1)|
                    (i_ts==nrow(Stock_price)))) 
          {
          # Clear Position
          Short_Poit<-0
          Out_price<-Stock_price$Close[i_ts]
          Out_date<-Stock_price$date[i_ts]
          Hold_days<-i_ts-In_date_ind+1
          if ((Stock_price$Close[i_ts]>Stop_Loss_Price)) {State="Stop_loss"}
          if ((Stock_price$Close[i_ts]<Stop_Gain_Price)) {State="Stop_gain"}
          if (Stock_price$Gold_Cross[i_ts]==1) {State="Gold_Cross"}
          if (i_ts==nrow(Stock_price)) {State="Last"}
          
          
          Short_Ret<-(In_price*(1-Sell_TC)-Out_price*(1+Buy_TC))/(In_price*(1-Sell_TC))
          Short_Trade_Table<-bind_rows(Short_Trade_Table,
                                       tibble(Position="Short",
                                       code=Code_here,
                                       In_date=In_date,
                                       Out_date=Out_date,
                                       In_price=In_price,
                                       Out_price=Out_price,
                                       Ret=Short_Ret,
                                       Hold_days=Hold_days,
                                       State=State))
        }
        
      }
      
    }  #if (sum(which(Stock_price$Gold_Cross==1))>0)
    Trade_Table <- rbind(Long_Trade_Table,Short_Trade_Table)  %>% arrange (code,In_date)              # bind the two tables toghether
  }  # for (i_code in 1:length(Code_list) ) #for
  
  
  return(Trade_Table)
}




    

#------------------------------------------------------------------------

MA_table<-NULL
for (SMA_days in c(5,10,20))                                         #set for the MA
{
  for (LMA_days in c(60,120,240))
  {
    cat("LMA:",LMA_days,"SMA",SMA_days,"\n")
    
    output<-MA_strategy(SMA_day=SMA_days,LMA_day=LMA_days,Data=Sp_elect,test_num=20,stop_loss_rate=0.1,stop_gain_rate = 0.1)
    MA_table<-bind_rows(MA_table,
                        output %>% summarise(Avg_ret=mean(Ret),
                                             Prob_Win=mean(Ret>0), 
                                             Trade_num=n(),
                                             Avg_Hold_days=mean(Hold_days),
                                             Annual_Ret=252*Avg_ret/Avg_Hold_days,
                                             minRet=min(Ret),maxRet=max(Ret),
                                             Std_Ret=sd(Ret)) %>% 
                          mutate(SMA_day=SMA_days,LMA_day=LMA_days)
    )
    print(MA_table)
    
  }
}

MA_table<-MA_table %>% arrange(desc(Annual_Ret)) %>% slice(1)                               # take the highly-performance MA in the first row



output<-MA_strategy(SMA_day=MA_table$SMA_day[1],LMA_day=MA_table$LMA_day[1],
                    Data=Sp_elect,test_num=NULL,stop_loss_rate=0.1,stop_gain_rate = 0.1)

year_profit<-output %>% 
  mutate(Year=floor(In_date/10000)) %>% 
  group_by(Year) %>%
  summarise(Avg_ret=mean(Ret),
            Prob_Win=mean(Ret>0), Trade_num=n(),
            Avg_Hold_days=mean(Hold_days),
            Annual_Ret=252*Avg_ret/Avg_Hold_days,
            minRet=min(Ret),maxRet=max(Ret),
            GL_ratio=abs(mean(Ret[Ret>0])/mean(Ret[Ret<0]))) 


