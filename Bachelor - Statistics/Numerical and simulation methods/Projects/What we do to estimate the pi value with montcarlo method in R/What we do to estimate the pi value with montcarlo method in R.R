#Issue:
#A) Approximate the Pi number with the Montcarlo method and display the graphs of this method.
#B) Repeat this approximation 200 times and obtain a graph of the averages of these approximations at points for different values of N, and then plot its standard deviation as a line on either side of the same point next to each mean.

#solve:
memory.limit(size = 9999999999)
#we do this to estimate the pi value with montcarlo method:
rm(list = ls())
x<-runif(2^20,0,1)
y<-runif(2^20,0,1)
d<-(x^2+y^2<1)
plot(x,y,col=d+1 , pch=10)
#The ratio of the number of points within a quadrilateral to the total points is equal to:
q<-c(1:(2^20*200))
A<-matrix(q, nrow = 200)
for (j in 1:200) {
  a<-0
  b<-1
  N<-20
  l<-0
  num<-2^N
  x<-runif(num,a,b)
  y<-runif(num,a,b)
  k<-c()  
  for(i in 1:num){
    if(x[i]^2+y[i]^2<1){
      l<-l+1
      k[i]<-l}
    else
    {
      k[i]<-l
    }
    A[j,i]<-4*(k[i]/i)
  }
}
print("the pi value estiamte is:")
(4*(k[i]/num))
Mean<-c()
a<-c()
a<-apply(A, 2,mean)
for (i in 1:N){
  Mean[i]<-a[2^i]
}
sigma<-c()
b<-c()
b<-apply(A, 2,sd)
for (i in 1:N){
  sigma[i]<-b[2^i]
}

n<-c(1:N)
pi<-c()

for(j in 1:4){
  par(new= "TRUE")
  for (i in 1:N) {
  pi[i]<-A[j,2^i]}
  plot(n,pi , col= 85*j , type = "o" , ylab = "the pi estimate value" , xlab = "2^N" ,ylim =c(0,6), new= "True")}
abline(h=3.141593  , col= "150")
plot(Mean ,type="p" , col="red" ,xlab= "2^N" , ylab = "Mean of pi value estimates"   )
for(i in 1:N){
  segments(n[i],Mean[i]-sigma[i],x1=n[i] , y1 = Mean[i]+sigma[i] , col = "85")}
abline(h=3.141593 , col="black")

