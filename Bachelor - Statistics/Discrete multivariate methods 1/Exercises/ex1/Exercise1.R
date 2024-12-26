data<-matrix(c(30,12,50,11) ,nrow=2, byrow = TRUE )
colnames(data)<-c("adame ebtela(0)" , "ebtela(1)")
rownames(data)<-c("tazrigh nakarde(0)" , "tazrigh karde(1)")
data

L<-function(alpha , beta){
  (1 / (1 + exp(alpha))) ^ data[1,1] *
    (exp(alpha) / (1 + exp(alpha))) ^ data[1,2] *
    (1 / ( 1+ exp(alpha + beta)))^ data[2,1] *
    (exp(alpha + beta) /( 1 + exp(alpha + beta))) ^ data[2,2]
}

l<-function(alpha,beta){
  log(L(alpha,beta))
}
Negative.ll<-function(par){
  alpha=par[1];beta=par[2]
  -l(alpha,beta)
}
alpha=seq(-6 , -2 , length=100)
beta=seq(5,10 , length=100)
ll<-outer(alpha , beta , l) 
persp(alpha , beta , ll , theta = 30 , phi =30)

(ml<-optim(par = c(1,1) ,Negative.ll ))
ml.alpha=ml$par[1]
ml.beta=ml$par[2]

logit<-function(x){
  ml.alpha + (ml.beta * x)
}
expit<-function(x){
  exp(x) / (1+exp(x))
}

P<-function(x){
expit(logit(x))
  }


##
expit(ml.alpha)
12/42
P(0)



expit(ml.alpha + ml.beta)
11/61
P(1)

ml.beta
log((11/50) /(12/30))
P(0)

