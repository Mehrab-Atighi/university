#exercise two:
response<-c(73,68,74,71,67,73,67,75,72,70,75,68,78,73,68,73,71,75,75,69)
treatment1<-factor(rep(c(1:4),each=5))
treatment2<-factor(rep(c(1:5),4))
#now we want to see our data
g<-data.frame(response,treatment1,treatment2)
head(g)
#Now we want to oneway analize variance:

fit<-lm(response~treatment1+treatment2)
anova<-aov(fit , data =g)
summary(anova)

#for LSD test we have:
#install.packages("agricolae")
#library("agricolae")

lsd<-LSD.test(fit,"treatment1",alpha=0.05)
lsd

#tukey test:
TukeyHSD(anova)

#duncan test:
duncan<-duncan.test(fit,"treatment1",alpha=0.05)
duncan

#for ploting the residuals we have:
x<-fit$fitted.values
y1<-residuals(fit)
y2<-rstandard(fit)
y3<-rstudent(fit)
par(mfrow=c(1,3))
plot(x,y1 , xlab = "predictions" , ylab = "residuals" , col="Orange" , type="p")
plot(x,y1 , xlab = "predictions" , ylab = "standard residuals" , col="Blue" , type="p")
plot(x,y1 , xlab = "predictions" , ylab = "students residuals" , col="Black" , type="p")


#kruskal walis test:
kruskal.test(response~treatment1 , data = g)
kruskal.test(response~treatment2 , data = g)

