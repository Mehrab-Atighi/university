rm(list = ls())
farvardin<-c(840000,935000,2215000,665000,1038000,1046000,2193000,817000,678000,651000,6090000,643000,783000,660000,640000,643000,3610000,775000,770000,4170000,690000,1045000,2235000,880000,855000,1455000,835000,2600000,775000,3530000,820000)
ordibehesht<-c(1775000,795000,760000,3795000,855000,1655000,3130000,765000,1645000,1120000,765000,1610000,1185000,1080000,895000,775000,780000,740000,820000,720000,890000,755000,795000,780000,785000,845000,800000,1280000,1055000,1010000,740000)
khordad<-c(625000,895000,655000,780000,960000,655000,685000,640000,820000,795000,790000,750000,695000,620000,630000,600000,790000,690000,640000,698000,760000,984000,589000,790000,1000000,877800,946000,1230000,868000,2400000,650000)
tir<-c(736000,706000,1473000,681000,1486000,699800,1033000,2393000,1276000,1238000,907000,603000,874000,872000,665000,648000,654000,639000,632000,788000,1104000,1306000,666000,620000,769000,1182000,608000,1253000,661000,692000,678000)
mordad<-c(1845000,719000,791000,748000,683000,1680000,461000,603000,733000,820000,1065000,888000,903000,641000,692000,716000,1213000,681000,657000,606000,631000,749000,948000,916000,678000,623000,318000,944000,2601000,893000,1240000)
shahrivar<-c(650000,897000,869000,764000,991000,679800,632000,870000,760000,750000,600000,840000,869500,875000,950000,980000,640000,610000,1046000,1010000,643000,967000,730000,750000,810000,2219000,681000,660000,720000,763000,695000)

x2<-all.time.m<-c(12,10,11,9,10,9)

farvardin.city.s.m=22
farvardin.outcity.s.m=9
ordibehesht.city.s.m=24
ordibehesht.outcity.s.m=7
khordad.city.s.m=26
khordad.outcity.s.m=6
tir.city.s.m=24
tir.outcity.s.m=7
mordad.city.s.m=26
mordad.outcity.s.m=5
shahrivar.city.s.m=30
shahrivar.outcity.s.m=1
x3<-all.city.s.m<-c(farvardin.city.s.m,ordibehesht.city.s.m,khordad.city.s.m,tir.city.s.m,mordad.city.s.m,shahrivar.city.s.m)
x4<-all.outcity.s.m<-c(farvardin.outcity.s.m,ordibehesht.outcity.s.m,khordad.outcity.s.m,tir.outcity.s.m,mordad.outcity.s.m,shahrivar.outcity.s.m)



income.f.driver<-sum(farvardin)
income.o.driver<-sum(ordibehesht)
income.kh.driver<-sum(khordad)
income.t.driver<-sum(tir)
income.m.driver<-sum(mordad)
income.sh.driver<-sum(shahrivar)
x1<-income.all.driver<-c(income.f.driver,income.o.driver,income.kh.driver,income.t.driver,income.m.driver,income.sh.driver)

income.farvardin<-sum(farvardin)*0.15
income.ordibehesht<-sum(ordibehesht)*0.15
income.khordad<-sum(khordad)*0.15
income.tir<-sum(tir)*0.15
income.mordad<-sum(mordad)*0.15
income.shahrivar<-sum(shahrivar)*0.15
y<-income.agant<-c(income.farvardin,income.ordibehesht,income.khordad,income.tir,income.mordad,income.shahrivar)

data<-data.frame(y,x1,x2,x3,x4)
View(data)
y=x1
x1=x2
x2=x3
x3=x4



#start
fit<-lm(y~x1+x2+x3)
summary(fit)
(pred.w.clim<-predict(fit , interval="confidence"))
newd=data.frame(x1=11,x2=20,x3=3)
(pred.w.clim<-predict(fit ,newdata = newd, interval="confidence"))
(pred.w.clim<-predict(fit , interval="prediction"))
newd=data.frame(x1=8,x2=12,x3=7)
(pred.w.clim<-predict(fit ,newdata = newd,interval="prediction"))
anova(fit)
fit1<-lm(y~x1)
anova(fit1)
summary(fit1)
fit2<-lm(y~x2)
summary(fit2)
anova(fit2)
fit3<-lm(y~x3)
summary(fit3)
anova(fit3)

fit4<-update(fit1,.~.+x2)
anova(fit1,fit4,test="F")

z1=x1
z2=x2*x3
Reduce.model<-lm(y~z1+z2)
anova(fit,Reduce.model)

summary(fit)




confint(fit)
e<-fit$residuals
print(e)
s<-rstandard(fit)
print(s)
t<-rstudent(fit)
print(t)
par(mfrow=c(1,1))
qqnorm(e)
y.hat<-fit$fitted.values
par(mfrow=c(1,3))
plot(y.hat,s)
abline(h=0 , col="blue")
plot(y.hat,t)
abline(h=0 , col="red")
plot(y.hat,e)
abline(h=0,col="yellow")

par(mfrow=c(1,2))
plot(income.agant~income.all.driver)
fit1<-lm(income.agant~income.all.driver)
abline(fit1 , col="red")
summary(fit1)
anova(fit1)
pred.fit1.clim<-predict(fit, interval = "confidence")
pred.fit1.plim<-predict(fit, interval = "prediction")
matplot(income.all.driver,cbind(pred.fit1.plim,pred.fit1.clim ),type= "l", ylab= "prediction y")
cor(income.agant,income.all.driver)
model.reg<-lm(income.agant~income.all.driver)
model.aov<-aov(income.agant~factor(income.all.driver))
anova(model.reg,model.aov)
#the pure error value is:
anova(model.reg,model.aov)[2,2]
#the pure error df is :
anova(model.reg,model.aov)[2,1]
#the lack of fit value is:
anova(model.reg,model.aov)[2,4]
#the lack of fit df is:
anova(model.reg,model.aov)[2,3]



par(mfrow=c(1,2))
plot(income.agant~all.time.m)
fit2<-lm(income.agant~all.time.m)
abline(fit2,col="Blue")
summary(fit2)
anova(fit2)
pred.fit2.clim<-predict(fit2, interval = "confidence")
pred.fit2.plim<-predict(fit2, interval = "prediction")
matplot(all.time.m,cbind(pred.fit2.plim,pred.fit2.clim ),type= "l", ylab= "prediction y")
cor(income.agant,all.time.m)
model.reg<-lm(income.agant~all.time.m)
model.aov<-aov(income.agant~factor(all.time.m))
anova(model.reg,model.aov)
#the pure error value is:
anova(model.reg,model.aov)[2,2]
#the pure error df is :
anova(model.reg,model.aov)[2,1]
#the lack of fit value is:
anova(model.reg,model.aov)[2,4]
#the lack of fit df is:
anova(model.reg,model.aov)[2,3]


par(mfrow=c(1,2))
plot(income.agant~all.city.s.m)
fit3<-lm(income.agant~all.city.s.m)
abline(fit3,col="black")
summary(fit3)
anova(fit3)
pred.fit3.clim<-predict(fit3, interval = "confidence")
pred.fit3.plim<-predict(fit3, interval = "prediction")
matplot(all.city.s.m,cbind(pred.fit3.plim,pred.fit3.clim ),type= "l", ylab= "prediction y")
cor(income.agant,all.city.s.m)
model.reg<-lm(income.agant~all.city.s.m)
model.aov<-aov(income.agant~factor(all.city.s.m))
anova(model.reg,model.aov)
#the pure error value is:
anova(model.reg,model.aov)[2,2]
#the pure error df is :
anova(model.reg,model.aov)[2,1]
#the lack of fit value is:
anova(model.reg,model.aov)[2,4]
#the lack of fit df is:
anova(model.reg,model.aov)[2,3]



par(mfrow=c(1,2))
plot(income.agant~all.outcity.s.m)
fit4<-lm(income.agant~all.outcity.s.m)
abline(fit4,col="green")
summary(fit4)
anova(fit4)
pred.fit4.clim<-predict(fit4 ,interval = "confidence")
pred.fit4.plim<-predict(fit4, interval = "prediction")
matplot(all.outcity.s.m,cbind(pred.fit4.plim,pred.fit4.clim ),type= "l", ylab= "prediction y")
cor(income.agant,all.outcity.s.m)
model.reg<-lm(income.agant~all.outcity.s.m)
model.aov<-aov(income.agant~factor(all.outcity.s.m))
anova(model.reg,model.aov)
#the pure error value is:
anova(model.reg,model.aov)[2,2]
#the pure error df is :
anova(model.reg,model.aov)[2,1]
#the lack of fit value is:
anova(model.reg,model.aov)[2,4]
#the lack of fit df is:
anova(model.reg,model.aov)[2,3]

#x is the point we want to predict.
x<-data.frame(income.all.driver=900000,all.time.m=7,all.city.s.m=5,all.outcity.s.m=26)
prediction<-predict(fit,newdata = x ,interval = "confidence")
prediction


x<-data.frame(income.all.driver=900000,all.time.m=7,all.city.s.m=5,all.outcity.s.m=26)
prediction2<-predict(fit,newdata = x ,interval = "prediction")
prediction2
