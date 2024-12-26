rm(list=ls())
memory.limit(size = 999999999)
n=c(100,50000,100,50000)
Lower.band<-vector(length = 4)
  Upper.band<-vector(length = 4)
  L<-c()
  z=abs(c(qnorm(0.05),qnorm(0.05),qnorm(0.01),qnorm(0.01)))

  for(w in 1:4){ 
  theta.hat<-vector(length = n[w])
  N=10^4
  for(j in 1:n[w]){
    g<-vector(length = N)
    for(i in 1:N){
      u<-runif(1,0,10)
      g[i]<-exp(u^2)
    }
    theta.hat[j]<-10*mean(g)}
  sigma2<-sd(theta.hat)
  theta.hat.mean<-mean(theta.hat)
  Lower.band[w]<-theta.hat.mean-z[w]*(sigma2/sqrt(n[w]))
  Upper.band[w]<-theta.hat.mean+z[w]*(sigma2/sqrt(n[w]))
  L[w]<-Upper.band[w]-Lower.band[w]}
  for(w in 1 :4){
  print(paste("the confidence interval is:","(",Lower.band,Upper.band,")","with alpha=",1-pnorm(z[w]),"repeat=",n[w]))
print(paste("the long of our confidence is:",L[w]))}
