#example 1:
#install.packages("calibrate")
library(calibrate)
data("heads")
data<-heads[,1:2]
tail(data,4)
apply(data ,2,mean)
r=cor(data)
s=cov(data)
eigen(s)
eigen(r)
pc<-princomp(data , scores = T , cor=FALSE)
summary(pc)
pc$loadings
plot(pc$scores[,1],pc$scores[,2] ,col="blue")
#plot(pc$scores ,col="blue")
abline(h=0 , v=0 , col="orange")



