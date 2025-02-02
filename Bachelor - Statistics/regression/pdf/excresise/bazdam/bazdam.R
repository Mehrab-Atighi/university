rm(list = ls())
bazdam.v<-c(1.6,1.0,1.4,2.6,1.2,1.5,1.6,2.3,2.1,0.7)
life.v<-c(2.2,1.5,1.6,3.4,2.0,1.9,2.2,3.3,2.4,0.9)
complete.v<-c(2.5,3.2,5.0,4.4,4.4,3.3,3.2,3.3,3.7,3.6)
bazdam.v.fit<-lm(bazdam.v~life.v+complete.v)
summary(bazdam.v.fit)
confint(bazdam.v.fit)
newd<-data.frame(life.v=3,complete.v=9.3)
(pred.w.clim<-predict(bazdam.v.fit , interval="confidence"))
(pred.w.clim<-predict(bazdam.v.fit ,newdata = newd, interval="confidence"))
(pred.w.clim<-predict(bazdam.v.fit , interval="prediction"))
newd<-data.frame(life.v=6.3,complete.v=5.2)
(pred.w.clim<-predict(bazdam.v.fit ,newdata =c(6.3,5.2) ,interval="prediction"))
anova(bazdam.v.fit)
life.v.fit1<-lm(bazdam.v~life.v)
complete.v.fit1<-lm(bazdam.v~complete.v)
anova(life.v.fit1)
anova(complete.v.fit1)
par(mfrow=c(1,2))
plot(complete.v,bazdam.v)
plot(life.v,bazdam.v)

lm.fit1<-lm(bazdam.v~life.v)
anova(lm.fit1)
lm.fit2<-lm(bazdam.v~complete.v)
anova(lm.fit2)
lm.fit<-update(lm.fit2,.~.+life.v)
anova(lm.fit2,lm.fit)

z<-life.v+complete.v
Reduce.fit<-lm(bazdam.v~z)
anova(Reduce.fit,bazdam.v.fit)


summary(bazdam.v.fit)
yhat<-bazdam.v.fit$fitted.values
(e<-bazdam.v.fit$residuals)
(t<-rstudent(bazdam.v.fit))
(s<-rstandard(bazdam.v.fit))
qqnorm(e)
par(mfrow=c(1,2))
plot(yhat,e)
abline(h=0,col="green")
plot(yhat,t)
abline(h=0,col="green")
plot(life.v,e)
abline(h=0,col="REd")
plot(life.v,t)
abline(h=0,col="red")
plot(complete.v,e)
abline(h=0,col="Blue")
plot(complete.v,t)
abline(h=0,col="blue")


par(mfrow=c(3,2))
plot(life.v,e)
abline(h=0,col="green")
plot(complete.v,e)
abline(h=0,col="green")
plot(life.v,s)
abline(h=0,col="green")
plot(complete.v,s)
abline(h=0,col="green")
plot(life.v,t)
abline(h=0,col="green")
plot(complete.v,t)
abline(h=0,col="green")


