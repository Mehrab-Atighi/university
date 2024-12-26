rm(list=ls())
n=100
q=8
M<-c()
e<-10^-q
for(i in 1:n){
  baze<-c(0,1)
  a<-baze[1];b<-baze[2]
  f<-function(c){
    f=c-(1/2)*cos(c)
  }
  c<-runif(1,a,b)
  
  m=1
  while((abs(b-a))>=(e)){
    c<-runif(1,a,b)
    if(f(c)*f(a)<0){b<-c
    }else{
      a<-c}
    m<-m+1}
  M[i]<-m}

print(paste("the last value c in our function is :",f(c)))
print(paste("the mean of M is :",mean(M)))


e<-10^-q
r=0
baze<-c(0,1)
a<-baze[1];b<-baze[2]
c<-(a+b)/2
while((abs(b-a))>=(e)){
  c<-(a+b)/2
  if(f(c)*f(a)<0){b<-c
  }else{
    a<-c}
  r<-r+1}
print(r) 

