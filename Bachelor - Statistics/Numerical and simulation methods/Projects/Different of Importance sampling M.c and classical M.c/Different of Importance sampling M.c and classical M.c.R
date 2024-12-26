#a)
H<-function(x){exp(-(x^2)/2)*(exp(-1/2*(x-3)^2)/sqrt(2*pi)+exp(-(x^2)/2)*exp(-1/2*(x-6)^2)/sqrt(2*pi))}
integrate(H ,-Inf,Inf)
a<-integrate(H ,-Inf,Inf)$val
#b)
g<-function(x){exp(-(x^2)/2)}
N=10^3
H1<-rnorm(N,mean = 3,sd=1)
H2<-rnorm(N,mean = 6,sd=1)
E<-(cumsum(g(H1))/1:N)+(cumsum(g(H2))/1:N)
plot(E , type = "l",col="Blue4",xlab="N" , ylab = "E[h(x)]")
abline(h=a , col="850")
E[N]
a
abs(a-E[N])
sd(E)
#c)
N=10^3
G1<-function(x){exp(-(x^2)/2)*exp(-1/2*(x-3)^2)/(sqrt(2*pi))}
G2<-function(x){exp(-(x^2)/2)*exp(-1/2*(x-6)^2)/(sqrt(2*pi))}
G<-integrate(G1,-8,-1)$val+integrate(G2,-8,-1)$val
u<-runif(N,min=-8,max=-1)
g1<-function(x){exp(-(x^2)/2)*exp(-1/2*(x-3)^2)/(7*sqrt(2*pi))}
g2<-function(x){exp(-(x^2)/2)*exp(-1/2*(x-6)^2)/(7*sqrt(2*pi))}
R<-7*((cumsum(g1(u))/1:N)+(cumsum(g2(u))/1:N))
plot(R, type = "l" , ylim =c(min(R),G), xlab="N" , ylab = "E[h(u)f(u)/f(u)]")
abline(h=G ,col="Blue")
R[N]
G
abs(G-R[N])
sd(R)

#d)
N=10^3
F1<-function(x){exp(-(x^2)/2)*exp(-1/2*(x-3)^2)/(sqrt(2*pi))}
F2<-function(x){exp(-(x^2)/2)*exp(-1/2*(x-6)^2)/(sqrt(2*pi))}
F<-integrate(F1,0,Inf)$val+integrate(F2,0,Inf)$val
q<-rexp(N,rate =1)
f1<-function(x){(1/exp(-x/2))*exp(-(x^2)/2)*exp(-1/2*(x-3)^2)/(sqrt(2*pi))}
f2<-function(x){(1/exp(-x/2))*exp(-(x^2)/2)*exp(-1/2*(x-6)^2)/(sqrt(2*pi))}
W<-((cumsum(f1(q))/1:N)+(cumsum(f2(q))/1:N))
plot(W, type = "l" , ylim =c(min(W),F))
abline(h=F,col="Blue")
W[N]
F
abs(F-W[N])
sd(W)
