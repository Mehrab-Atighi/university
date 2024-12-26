pnorm(-20)
N=10^6
u<-runif(N,min = 0 , max = 1/20)
fx<-function(u){exp(20/(-2*u^2))/u^2 * sqrt(2*pi)}
E<-c()
for(i in 1:N){
  E[i]<-fx(u[i])
}
print(1/20*mean(E))

