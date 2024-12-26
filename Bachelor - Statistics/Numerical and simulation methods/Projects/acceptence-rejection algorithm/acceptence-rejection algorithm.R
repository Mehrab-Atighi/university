rm(list = ls())
par(mfrow=c(1,1))
Niha<-c(rep(0 ,40000))
N.m<-matrix(Niha ,  nrow = 4 , byrow = 10000)
c<-c(4,3,2.5,2.073)
for (k in 1:4){for(i in 1:10000){
  
N<-0
a<-c[k]
n<-1
u<-runif(n)
y<-runif(n)
g<-function(y){g=1}
f<-function(y){f=pbeta(y,4,3)}
while(f(y)/a*g(y)<u)
{
  N<-N+1
  u<-runif(n)
  y<-runif(n)
N.m[k,i]<-N
}}
  }
z<-apply(N.m , 1 ,mean)
x<-apply(N.m , 1 ,sd)

plot(c , z,type="p" , col="red" ,xlab= "a value" , ylab = "N.bar" ,xlim = c(1,5) , ylim = c(-10,17) )
for(i in 1:4){
  segments(c[i],z[i]-x[i],x1=c[i] , y1 = z[i]+x[i] , col = "black")}
ks.test(N.m[1,],"pnorm" , mean=z[1], sd=x[1])


N<-0
a<-2.073
n<-10000
u<-runif(n)
y<-runif(n)
g<-function(y){g=1}
f<-function(y){f=pbeta(y,4,3)}
for(i in 1:10000){
if(f(y[i])/a*g(y[i])>=u[i]){
  N<-N+1
  
}
}
print(N)
par(mfrow=c(2,2))
for(i in 1:4){
  hist(N.m[i,])}



