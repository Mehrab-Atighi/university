# exercise one:
percent<-c(rep(c(0,4,6,8,10)))
values<-c(7,7,15,11,9,
          12,17,12,18,18,
          14,18,18,19,19,
          19,25,22,19,23,
          7,10,11,15,11)
data<-matrix(values,ncol = 5 , byrow = FALSE )
colnames(data)=c(1:5)
rownames(data)=percent
#Now we can see our data :
data
#Now we want to oneway analize variance:
response<-values
treatments=factor(rep(percent,each=5))
head(g<-data.frame(response,treatments),10)
fit<-lm(response~treatments)
summary(fit)
anova(fit)

#for LSD test we have:
#install.packages("agricolae")
#library("agricolae")
lsd<-LSD.test(fit,"treatments",alpha = 0.05)
lsd

#tukey test:
TukeyHSD(aov(fit))

#Duncan test:
duncan<-duncan.test(fit,"treatments",alpha = 0.05)
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
kruskal.test(response~treatments , data = g)
#so we can say the means arre not equal

#contrasts: 
#for example we want to test that 
#mu1 + 2mu2+mu3-3mu4-mu5= 0
C1<-factor(rep(c(1,2,1,-3,-1) , each = 5 ))
g[,3]<-C1;colnames(g)=c("response" , "treatments" , "C1 (contrast)")
g

#now for test it we have:
contrast<-lm(response~C1 , data=g)
anova(contrast)
