#檔案位置
DataPath <- "D:/data/"
#tail執行檔位置
tailPath <- "D:/data/"

#取得成交資訊
GetMatchData <- function(date)
{

 data <- system(paste0(tailPath,'tail.exe -n1 ',DataPath,"/",date,"_MicroPlayI020.txt"),intern=TRUE)
 mdata <- strsplit(data,",")
 return(mdata)

}

#取得委託資訊
GetOrderData <- function(date)
{

 data <- system(paste0(tailPath,'tail.exe -n1 ',DataPath,date,"_MicroPlayI030.txt"),intern=TRUE)
 mdata <- strsplit(data,",")
 return(mdata)

}

#取得上下五檔價資訊
GetUpDn5Data <- function(date)
{

 data <- system(paste0(tailPath,'tail.exe -n1 ',DataPath,date,"_MicroPlayI080.txt"),intern=TRUE)
 mdata <- strsplit(data,",")
 return(mdata)

}

#設定下單程式位置
ExecPath <- "C:/OrderCMD/"

#市價委託單
OrderMKT<-function(Product,BorS,Qty){
 OrderNo<-system2(paste0(ExecPath,'Test_Order.exe') ,args=paste(Product,BorS,'0',Qty,'MKT','IOC','1'),stdout = TRUE)
 #Match<-system2(paste0(ExecPath,'GetAccount.exe'),args=paste(OrderNo),stdout = TRUE)
 return(OrderNo)
}

#限價委託單
OrderLMT<-function(Product,BorS,Price,Qty){
 OrderNo<-system2(paste0(ExecPath,'Test_Order.exe')  ,args=paste(Product,BorS,Price,Qty,'LMT','ROD','0'),stdout = TRUE)
 return(OrderNo)
}


CancelOrder<-function(No){
 OrderNo<-system2(paste0(ExecPath,'Test_Order.exe')  ,args=paste("Delete",No),stdout = TRUE)
 return(OrderNo)
}


