rm(list = ls())
  Z<-c()
  for(i in 1:10^6){
  y1<-rexp(1,rate = 1)
  y2<-rexp(1,rate = 1)
  while(y2-((y1-1)^2)/2<=0){
    y1<-rexp(1,rate = 1)
    y2<-rexp(1,rate = 1)
  }
  y<-y2-((y1-1)^2)/2
  u<-runif(1)
  if(u>1/2){
    z<- -y1 
  
  }else{
    z<- y1
  }
  
Z[i]<-z}
  mean(Z)
  var(Z)
  