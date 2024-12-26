rm(list = ls())
memory.limit(size = 9999999999999)
n=10
N=10^n
#the Box Muller alg:
x<-c()
y<-c()
i=1
end_time.Box.muller<-c()
for(j in 1:n){
  end_time.Box.muller[j]<-system.time(while(i!=10^j){
    U1<-runif(1)
    U2<-runif(1)
    x[i]<-sqrt(-2*log(U1))*cos(2*pi*U2)
    y[i]<-sqrt(-2*log(U1))*sin(2*pi*U2)
    i=i+1
  })[3]
}
end_time.Box.muller
#pollar alg:
n=9
N=10^n
end_time.pollar<-c()
i=1
for(w in 1:n){end_time.pollar[w]<-system.time(while(i!=10^w){
     s=2
    while (s>1) {
      U3<-runif(1)
      U4<-runif(1)
      V1<-(2*U3)-1
      V2<-(2*U4)-1
      s<-V1^2+V2^2
    }
    x[i]<-sqrt((-2*log(s))/s)*V1
    y[i]<-sqrt((-2*log(s))/s)*V2
    i<-i+1}
    )[3]}
end_time.pollar

data.frame(end_time.pollar,end_time.Box.muller)

plot(1:9,end_time.Box.muller, ylab = "time(sec)" , xlab = "20^n" ,type = "b" , col="red" , xlim = c(1,10))
points(end_time.pollar,type = "b" ,col="blue")

   

