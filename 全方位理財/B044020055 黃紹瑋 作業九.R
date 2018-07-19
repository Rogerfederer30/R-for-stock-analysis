# 1. 最近年度股東權益報酬率＞平均值(市場及產業) 
# 2. 五年平均股東權益報酬率＞15%
# 3. 最近年度毛利率＞產業平均值 
# 4. (7年內市值增加值/7年內保留盈餘增加值)>1 
# 5. (最近年度自由現金流量/6年前(7年內)自由現金流量)-1 > = 1 
# 6. 市值/7年內自由現金流量加總值<1

rm(list=ls())
library(quantmod)
library(dplyr)
library(tidyverse)

#-------------------------initial parameter--------------------------
ROE_min_Rate<-15
ROE_min_Rate_cond1<-10
Margin_min_Rate_cond1<-10
#-------------------------initial parameter--------------------------


setwd("C:/Users/USER/Desktop/全方位理財/")
Fin_Report<-read.table(file="Fin_Report.txt",header = T,stringsAsFactors = F,sep="\t")
colnames(Fin_Report)<-c("code","company","TSE_Ind","Report_date","Quarter","Margin","ROE","FCF","Mkt_Value","Retent1","Retent2","Retent3")

#轉數值
Fin_Report<-Fin_Report %>% 
  mutate(
    code=as.numeric(code),
    TSE_Ind=as.numeric(TSE_Ind),
    Report_date=as.numeric(Report_date),
    Margin=as.numeric(Margin),
    ROE=as.numeric(ROE),
    FCF=as.numeric(FCF),
    Mkt_Value=as.numeric(Mkt_Value),
    Retent1=as.numeric(Retent1),
    Retent2=as.numeric(Retent2),
    Retent3=as.numeric(Retent3))

# 找出code、TSE_Ind有缺值的row並移除
loc_code_NA1<-which(is.na(Fin_Report$code))
loc_code_NA2<-which(is.na(Fin_Report$TSE_Ind))
Fin_Report<-Fin_Report[-c(loc_code_NA1,loc_code_NA2),]

codelist<-unique(Fin_Report$code) #Fin_Report代碼清單

#照codee分群
Fin_Report2<-Fin_Report %>% group_by(code)

#---------------------補缺值---------------

#創空表
Stock_Data<-NULL

for (ix in 1:length(codelist)) {
  if ((ix %%100)==0){cat("Code:",codelist[ix],"Progrss:",ix,"/",length(codelist),"\n")}
  
  stock_here<-Fin_Report2 %>% filter(code==codelist[ix])
  
  for (i_col in c(6:ncol(Fin_Report2))) {
    if (is.na(stock_here[1,i_col])==TRUE) {
      stock_here[1,i_col]<-(10^(-10)) #先針對每隻股票的第一筆有缺值的補值(否則na.locf無法根據以前的數值連續補值)
    }
    stock_here[,i_col]<-na.locf(stock_here[,i_col], fromLast = F) #根據以前的數值連續補值
  }
  Stock_Data<-bind_rows(Stock_Data,stock_here) #合併
}

Fin_Report2<-Stock_Data %>% mutate(Year=floor(Report_date/100))
min_Year<-min(Fin_Report2$Year) #1995
max_Year<-max(Fin_Report2$Year) #2017

#---------------------補缺值----------------


result <- NULL
for (Pick_year in (min_Year+6):max_Year ) { #至少需要7年資料
  print(Pick_year)
  
  #---------------------------------Cond4------------------------------------
  #(7年內市值增加值/7年內保留盈餘增加值)>1 
  Fin_Report_target_cond4<-Fin_Report2 %>% filter(Year>=Pick_year-6,Year<=Pick_year) %>%
    select(code,Year,Mkt_Value,Retent1,Retent2,Retent3) %>% 
    filter(n()==7) %>% group_by(code) %>% 
    mutate(First_Mkt=Mkt_Value[1]) %>% filter(First_Mkt!=(10^(-10))) %>% #把剛補10^(-10)的濾掉
    mutate(Mkt_Val_Add=Mkt_Value[7]-First_Mkt,#7年內市值增加值
           First_Ret1=Retent1[1],
           First_Ret2=Retent2[1],
           First_Ret3=Retent3[1]) %>%
    filter(First_Ret1!=(10^(-10)),
           First_Ret2!=(10^(-10)),
           First_Ret3!=(10^(-10))) %>%
    mutate(Retent_Add=(Retent1[7]-Retent1[1])+(Retent2[7]-Retent2[1])+(Retent3[7]-Retent3[1]))%>% #7年內保留盈餘增加值
    filter(Retent_Add>0,Mkt_Val_Add>0,Mkt_Val_Add/Retent_Add>1) #計算7年內市值增加值/7年內保留盈餘增加值>1
  Code_Pull_Cond4<-unique(Fin_Report_target_cond4$code)
  #---------------------------------Cond4
  #---------------------------------Cond3
  Fin_Report_target_cond3<-Fin_Report2 %>% filter(Year==Pick_year) %>% 
    select(code,Year,TSE_Ind,Margin) %>% 
    filter(Margin!=(10^(-10))) %>% group_by(TSE_Ind) %>%
    mutate(avg_Margin=mean(Margin)) %>% 
    filter(Margin>pmax(Margin_min_Rate_cond1,avg_Margin))  #Margin要大於產業平均或10
  Code_Pull_Cond3<-unique(Fin_Report_target_cond3$code) 
  
  #---------------------------------Cond3
  #---------------------------------Cond2------------------------------------
  #五年平均股東權益報酬率＞15%
  Fin_Report_target_cond2<-Fin_Report2 %>% filter(Year>=Pick_year-4,Year<=Pick_year) %>%
    select(code,Year,ROE) %>%
    filter(n()==5) %>% group_by(code) %>% 
    mutate(First_ROE=ROE[1]) %>% filter(First_ROE!=(10^(-10))) %>% #把剛補10^(-10)的濾掉
    mutate(Avg_ROE=mean(ROE)) %>% filter(Avg_ROE>=ROE_min_Rate)
  Code_Pull_Cond2<-unique(Fin_Report_target_cond2$code)
  
  #---------------------------------Cond2
  
  
  
  #---------------------------------Cond1------------------------------------
  #最近年度股東權益報酬率＞平均值(市場及產業) 
  #先算產業平均值
  Fin_Report_target_cond1<-Fin_Report2 %>% filter(Year==Pick_year) %>% 
    select(code,Year,TSE_Ind,ROE) %>% 
    filter(ROE!=(10^(-10))) %>% group_by(TSE_Ind) %>%
    mutate(avg_ROE=mean(ROE)) %>% 
    filter(ROE>pmax(ROE_min_Rate_cond1,avg_ROE))  #ROE要大於產業平均或10
  Code_Pull_Cond1<-unique(Fin_Report_target_cond1$code) 
  #---------------------------------Cond1
  
  #---------------------------------Cond5
  #(最近年度自由現金流量/七年內自由現金流量-1)>=1
  Fin_Report_target_cond5<-Fin_Report2 %>% filter(Year>=Pick_year-6,Year<=Pick_year) %>%
    select(code,Year,FCF) %>% 
    filter(n()==7) %>% group_by(code) %>% 
    mutate(First_FCF=FCF[1]) %>% filter(First_FCF!=(10^(-10))) %>% #把剛補10^(-10)的濾掉
    mutate(ratio=(FCF[7]/First_FCF)-1 #7年內市值增加值
          ) %>%
    filter(ratio>=1)
    
  Code_Pull_Cond5<-unique(Fin_Report_target_cond5$code)
  #---------------------------------Cond5
  
  #---------------------------------Cond6
  #目前市值/七年內自由現金流量加總值<0
  Fin_Report_target_cond6<-Fin_Report2 %>% filter(Year>=Pick_year-6,Year<=Pick_year) %>%
    select(code,Year,FCF,Mkt_Value) %>% 
    filter(n()==7) %>% group_by(code) %>% 
   filter((FCF[1]!=(10^(-10)))&(Mkt_Value[1]!=(10^(-10)))) %>% #把剛補10^(-10)的濾掉
    mutate(total_FCF=sum(FCF,na.rm = T),
           Mkt_div_FCF=Mkt_Value[7]/total_FCF      
           ) %>%
    filter(Mkt_div_FCF<0)
  
  Code_Pull_Cond6<-unique(Fin_Report_target_cond6$code)
  #---------------------------------Cond6
  
  #---------------------------------result------------------------------------
  total_code_list <- data.frame(year=Pick_year,
                                code=c(Code_Pull_Cond1,
                                       Code_Pull_Cond2,
                                       Code_Pull_Cond3,
                                       Code_Pull_Cond4,
                                       Code_Pull_Cond5,
                                       Code_Pull_Cond6))
  
  total_code_list <- total_code_list %>% group_by(year,code) %>%
    summarise(count=n()) %>% filter(count>=4) #篩選符合n個條件
  
  result <- rbind(result,total_code_list)
  
}

result


