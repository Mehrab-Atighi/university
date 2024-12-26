#Solve the first exercise of the third session of the linear regression lesson
#Methadone is often prescribed to treat addiction and chronic pain. Krantz et al. Examined the association between methadone dose and QTC interval (QTC) correction for 17 individuals with advanced torsade de pointes. The higher the QTC, the higher the risk of heart death. One question to consider is:"Can the amount of QTC be predicted from the methadone dose?"
#A- Draw a distribution diagram. 
#B- Execute and interpret the regression model.
#C- Predict and interpret the QTC value for a person with a methadone dose of 200.
Methadone_Dose<-c(1000,550,97,90,85,126,300,110,65,650,600,660,270,680,540,680,330)
#the methadone_Dose unit is (mg/day)
QTc<-c(600,625,560,585,590,500,700,570,540,785,765,611,600,625,650,635,522)
#the QTc unit is (mm/sec)
data.frame(Methadone_Dose,QTc)
  

#solve:
#A)the scatter plot is:
par(mfrow=c(1,1))
plot(Methadone_Dose,QTc)

#B)
Methadone_QTc_realationships.fit1<-lm(QTc~Methadone_Dose)
plot(QTc~Methadone_Dose)
abline(Methadone_QTc_realationships.fit1,col="red")

summary(Methadone_QTc_realationships.fit1)

#C)we should predict our Regression line equation.So first we have to get the slope and width from the origin of the line
Methadone_QTc_realationships.fit1
f<-function(x)#x is the point we want to predict.
{print((coef(Methadone_QTc_realationships.fit1)[1])+coef(Methadone_QTc_realationships.fit1)[2]*x)}
#coef(Methadone_QTc_realationships.fit1)[1] is the width of the origin of the desir ed line. 
#coef(Methadone_QTc_realationships.fit1)[2] is the slope of the desired line.
f(x)#is the our predict for 200mg/day methadone Dose 
#We can see a direct and ascending relationship between our two variables 
#if we want to have the ANOVA table for this question we should do this :
anova(Methadone_QTc_realationships.fit1)
#then we can see the answer
#To obtain the desired confidence interval, we do the following:
confint(Methadone_QTc_realationships.fit1)
#This means that with 95% confidence our regression line slope is in the range (0.1391315 and 3.666251).
#This means that with 95% confidence from the origin of our regression line is in the range (-1862.5041111 and 324.285546).
new<-data.frame(Methadone_Dose=c(650 , 115))
predict(Methadone_QTc_realationships.fit1, newdata= new, interval= "confidence")

pred.w.clim<-predict(Methadone_QTc_realationships.fit1,interval = "confidence")
pred.w.plim<-predict(Methadone_QTc_realationships.fit1,newdata = new,interval = "prediction")
matplot(Methadone_Dose,cbind(pred.w.plim,pred.w.clim ),type= "l", ylab= "prediction y")
cor(Methadone_Dose,QTc)
cor.test(Methadone_Dose,QTc , method = "pearson")

e<-Methadone_QTc_realationships.fit1$residuals
print(e)
s<-rstandard(Methadone_QTc_realationships.fit1)
print(s)
t<-rstudent(Methadone_QTc_realationships.fit1)
print(t)
y.hat<-Methadone_QTc_realationships.fit1$fitted.values
qqnorm(e)

par(mfrow=c(3,2))
plot(y.hat,s)
abline(h=0 , col="blue")
plot(Methadone_Dose,s)
abline(h=0 , col="green")
plot(y.hat,t)
abline(h=0 , col="red")
plot(Methadone_Dose,t)
abline(h=0, col=45)
plot(y.hat,e)
abline(h=0,col="yellow")
plot(Methadone_Dose,e)
abline(h=0 , col="black")


model.reg<-lm(QTc~Methadone_Dose)
model.aov<-aov(QTc~factor(Methadone_Dose))
anova(model.reg,model.aov)
#the pure error value is:
anova(model.reg,model.aov)[2,2]
#the pure error df is :
anova(model.reg,model.aov)[2,1]
#the lack of fit value is:
anova(model.reg,model.aov)[2,4]
#the lack of fit df is:
anova(model.reg,model.aov)[2,3]
#for chek the answers we should do this:
if(anova(model.reg,model.aov)[2,2]+anova(model.reg,model.aov)[2,4]==anova(model.reg,model.aov)[1,2])
{print("its true")}


QTc.prim<-QTc
Methadone_Dose.prim<-Methadone_Dose
for (i in 1:length(Methadone_Dose.prim)) {
  if(s[i]>2){
    Methadone_Dose.prim<-c(Methadone_Dose.prim[-i])
    QTc.prim<-QTc.prim[-i]
  }
  
}
for (i in 1:length(Methadone_Dose.prim)) {
  if(s[i]<= -2){
    Methadone_Dose.prim<-c(Methadone_Dose.prim[-i])
    QTc.prim<-QTc.prim[-i]
  }
}

print(Methadone_Dose.prim)
print(QTc.prim)
print(length(Methadone_Dose.prim))
print(length(QTc.prim))



print(QTc.prim)
print(Methadone_Dose.prim)
plot(Methadone_Dose.prim,QTc.prim)
Methadone_QTc_realationships.fit2<-lm(Methadone_Dose.prim~QTc.prim)
plot(QTc.prim~Methadone_Dose.prim)
abline(Methadone_QTc_realationships.fit2,col="red")
#baraye moghayese plot ha darim:
par(mfrow=c(2,1))
plot(QTc~Methadone_Dose)
abline(Methadone_QTc_realationships.fit1,col="red")
plot(Methadone_Dose.prim~QTc.prim)
abline(Methadone_QTc_realationships.fit2,col="blue")
#baraye dashtn e,s,t jadid darim :
e.prim<-Methadone_QTc_realationships.fit2$residuals
print(e.prim)
s.prim<-rstandard(Methadone_QTc_realationships.fit2)
print(s.prim)
t.prim<-rstudent(Methadone_QTc_realationships.fit2)
print(t.prim)
y.hat.prim<-Methadone_QTc_realationships.fit2$fitted.values

par(mfrow=c(1,2))
plot(y.hat.prim,s.prim)
abline(h=0 , col="blue")
plot(Methadone_Dose.prim,s.prim)
abline(h=0 , col="green")
plot(y.hat.prim,t.prim)
abline(h=0 , col="red")
plot(Methadone_Dose.prim,t.prim)
abline(h=0, col=45)
plot(y.hat.prim,e.prim)
abline(h=0,col="yellow")
plot(Methadone_Dose.prim,e.prim)
abline(h=0 , col="black")

#baraye moghayese plot e ha :
par(mfrow=c(1,2))

plot(y.hat.prim,s.prim)
abline(h=0 , col="blue")
plot(y.hat,s)
abline(h=0 , col="blue")

plot(Methadone_Dose.prim,e.prim)
abline(h=0 , col="green")
plot(Methadone_Dose,e)
abline(h=0 , col="green")


plot(y.hat.prim,t.prim)
abline(h=0 , col="red")
plot(y.hat,t)
abline(h=0 , col="red")

plot(Methadone_Dose.prim,t.prim)
abline(h=0, col=45)
plot(Methadone_Dose,t)
abline(h=0, col=45)

plot(y.hat.prim,e.prim)
abline(h=0,col="yellow")
plot(y.hat,e)
abline(h=0,col="yellow")

plot(Methadone_Dose.prim,e.prim)
abline(h=0 , col="black")
plot(Methadone_Dose,e)
abline(h=0 , col="black")

#baraye baresy lack of fit and pure error darim:
`
model.reg.prim<-lm(QTc.prim~Methadone_Dose.prim)
model.aov.prim<-aov(QTc.prim~factor(Methadone_Dose.prim))
anova(model.reg.prim,model.aov.prim)
#the pure error value is:
anova(model.reg.prim,model.aov.prim)[2,2]
#the pure error df is :
anova(model.reg.prim,model.aov.prim)[2,1]
#the lack of fit value is:
anova(model.reg.prim,model.aov.prim)[2,4]
#the lack of fit df is:
anova(model.reg.prim,model.aov.prim)[2,3]
#for chek the answers we should do this:
if(anova(model.reg.prim,model.aov.prim)[2,2]+anova(model.reg.prim,model.aov.prim)[2,4]==anova(model.reg.prim,model.aov.prim)[1,2])
{print("its true")}


anova(model.reg.prim)
#the pure error value is:
anova(model.reg.prim)[2,2]
#the pure error df is :
anova(model.reg.prim)[2,1]
#the lack of fit value is:
anova(model.reg.prim)[2,4]
#the lack of fit df is:
anova(model.reg.prim)[2,3]
#for chek the answers we should do this:
if(anova(model.reg.prim)[2,2]+anova(model.reg.prim)[2,4]==anova(model.reg.prim)[1,2])
{print("its true")}
