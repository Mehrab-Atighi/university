
#section 17:
#1)
#a)
#install.packages("heplots")
library(heplots)
data(RootStock)
attach(RootStock)
head(RootStock)
treatments<-rep(c("A","B","C","D","E","F"),each = 8)
response<-c(RootStock$girth4,RootStock$ext4,RootStock$girth15,RootStock$weight15)
C1<-c(rep(c(2,-1,-1,-1,-1,2),each = 8))
C2<-c(rep(c(1,0,0,0,0,-1),each = 8))
Data<-data.frame(response,treatments,C1,C2)
head(Data)
contrast<-lm(response~ C1+C2 ,data = Data)
anova(contrast)
  


#2)
#a)

A1y1<-c(7.80,7.10,7.89,7.82,9,8.43,7.65,7.7,7.28,8.96,7.75,7.8,7.6,7,7.82,7.8)
A1y2<-c(90.7,88.9,85.9,88.8,82.5,92.4,82.4,87.4,79.6,95.1,90.2,88,94.1,86.6,85.9,88.8)
A2y1<-c(7.12,7.06,7.45,7.45,8.19,8.25,7.45,7.45,7.15,7.15,7.70,7.45,7.06,7.04,7.52,7.70)
A2y2<-c(85.1,89.0,75.9,77.9,66.0,74.5,83.1,86.4,81.2,72.0,79.9,71.9,81.2,79.9,86.4,76.4)

B<-factor(rep(1:4 ,2, each =4))
A<-factor(rep(1:2 , each = 16 ))
y1<-c(A1y1,A2y1)
y2<-c(A1y2,A2y2)
response<-cbind(y1,y2)
Data<-data.frame(B,A,y1,y2)
View(Data)

result<-manova(response~B*A)
summary(result)



