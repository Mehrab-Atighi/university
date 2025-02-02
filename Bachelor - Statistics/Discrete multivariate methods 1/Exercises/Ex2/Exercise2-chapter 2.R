
  

rm(list=ls())
#soal6:
I=#number of Rows
data<-matrix(c(N10,N11,N20,N21,...,NI0,NI1) ,nrow=I, byrow = TRUE )
colnames(data)<-c("y=0" , "y=1")
rownames(data)<-c("x=1" , "x=2", ... , "x=I")
data
betaI=0
L<-function(alpha , c(beta1,beta2 , ... , betaI)){
  (1 / (1 + exp(alpha-beta1))) ^ data[1,1] *
    (exp(alpha+beta1) / (1 + exp(alpha+beta1))) ^ data[1,2] *
    (1 / ( 1+ exp(alpha + beta2)))^ data[2,1] *
    (exp(alpha + beta2) /( 1 + exp(alpha + beta2))) ^ data[2,2]*
    ... *
    (1 / ( 1+ exp(alpha + betaI)))^ data[I,1] *
    (exp(alpha + betaI) /( 1 + exp(alpha + betaI))) ^ data[I,2]
}

l<-function(alpha,c(beta1,beta2 , ... , betaI)){
  log(L(alpha,beta))
}
Negative.ll<-function(par){
  alpha=par[1];beta1=par[2];...;betaI-1 =par[I-1]
  -l(alpha,c(beta1,beta2 , ... , betaI))
}

(ml<-optim(par = c(1,1) ,Negative.ll ))
ml.alpha=ml$par[1]
ml.beta1=ml$par[2]
...
ml.betaI-1=ml$par[I-1]


#soal8:
y=c(rep(1 ,90 ) ,rep(0 , 40 ))
x= c(rep(0 , 25) ,rep(0.5 ,16) , rep(1 , 20) , rep(1.5 , 29) ,
     rep(0,16) , rep(0.5 , 11) , rep(1 ,4) , rep(1.5 , 9))

table(x,y)
fit1<-glm(y~x,family = binomial(link = "logit") )
summary(fit1)
residuals.glm(fit1 , type = "pearson")
fitted.values(fit1)
predict.glm(fit1 , type = "response")


seq<-data.frame(x=seq(min(x),max(x),len=50))
seq$y=predict(fit1,newdata= seq,type="response")
plot(x=c(0 , 1.5),y=c(0,1),type="n" ,col= c(1:50),pch=19,cex=1.1,
     ylab="probabilty of asib didan" , xlab="Solfoz Level")
lines(y~x, data = seq,col=6,lwd= 2)

residuals(fit1)
rstudent(fit1)
rstandard(fit1)
anova(fit1)
fit1$null.deviance
fit1$deviance
qqnorm(residuals(fit1))

