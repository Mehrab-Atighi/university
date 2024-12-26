library(CircStats)
#example 1.8:
t<-c(711.5,1051,6303.9,1883.6,6054.3,6853.7,7201.9,279.8,2311.1,7.5,5296.6,848.2,9068.5,10609.7,592.1,1657.2,5637.9,2951.2,1425.5,121.5)
sample.quantiles.exp<-sort(t)
i<-1:length(t)
E<--log(1-(i/(length(t)+1)))
theoretical.quantiles.exp=E
fit<-lm(sort(t)~E)
plot(E,sort(t),col="Orange",xlab = "Ei",ylab="ti")
abline(fit,col="Brown")
fit$coefficients
lamba.hat=1/fit$coefficients[2];theta.hat=fit$coefficients[1]
print(paste("theta.hat is equal to ",theta.hat,
            "the lambda.hat is equal to",lamba.hat))
qqnorm(t,log.p = TRUE)
qqline(t)

pp.plot(t,ref.line = TRUE, ,col="Orange")

#example 2.8:
library("fitdistrplus","qualityTools")
x<-(c(5.77,5.03,4.5,4.1,3.3,3.0,2.5,1.76,1.54,1.33,1.32,1,0.88,0.5,0.22))
i<-1:length(x)
W<-log(-log(1-(i/(length(x)+1))))
plot(W,log(sort(x)),col="Orange",xlab="Wi",ylab="Ln(ti)")
fit1<-lm(log(sort(x))~W)
abline(fit1,col="Brown")
fit1$coefficients   
lamba=exp(-fit1$coefficients[1]);Betha.hat=1/fit1$coefficients[1]
print(paste("betha.hat is equal to ",Betha.hat,
            "the lambda is equal to",lamba))
qqnorm(x)
qqline(x)


#example 3.8:
t<-c(17.88,28.92,33.00,41.52,41.12,45.60,48.40,51.84,51.95,54.12,55.56,67,80,68.64,68.64,84.12,93.12,98.64,105.12,127.92,128.04,173.40)
i<-1:length(t)
Zi<-qnorm(i/(length(t)+1))
plot(log(sort(t)),Zi,xlab = "Ln(ti)",ylab= "Zi",col="Orange")
fit<-lm(Zi~log(sort(t)))
abline(fit,col="brown")
fit$coefficients
mu.hat<-fit$coefficients[1];sigma.hat<-fit$coefficients[2]
print(paste("the mu.hat is equal to",mu.hat,
            "the sigma.hat is equal to",sigma.hat))
qqnorm(t)
qqline(t)


