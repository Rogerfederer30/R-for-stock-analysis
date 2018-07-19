library(quantmod)
#-------------------------------------------------------------
KD_Ind<-function(Close,k=9) {
  k_min<-runMin(Close,k)
  k_max<-runMax(Close,k)
  RSV_t<-(Close-k_min)/(k_max-k_min)
  Kx<-matrix(0,nrow=length(Close),1)
  Dx<-matrix(0,nrow=length(Close),1)
  Kx[1:(k-1),1]<-0.5
  Dx[1:(k-1),1]<-0.5
  for (ix in c(k:length(Close)) )
  {
    Kx[ix]<-Kx[ix-1]*2/3+RSV_t[ix]/3
    Dx[ix]<-Dx[ix-1]*2/3+Kx[ix]/3
  }
  return(cbind(Kx,Dx))
}

#-------------------------------------------------------------

KD_Ind_HLC<-function(HLC,k=9) {
  k_min<-runMin(HLC[,2],k)
  k_max<-runMax(HLC[,1],k)
  Close<-HLC[,3]
  RSV_t<-(Close-k_min)/(k_max-k_min)
  Kx<-matrix(0,nrow=length(Close),1)
  Dx<-matrix(0,nrow=length(Close),1)
  Kx[1:(k-1),1]<-0.5
  Dx[1:(k-1),1]<-0.5
  for (ix in c(k:length(Close)) )
  {
    Kx[ix]<-Kx[ix-1]*2/3+RSV_t[ix]/3
    Dx[ix]<-Dx[ix-1]*2/3+Kx[ix]/3
  }
  # return(cbind(Kx,Dx))
  list(Klist=Kx,Dlist=Dx)
}
