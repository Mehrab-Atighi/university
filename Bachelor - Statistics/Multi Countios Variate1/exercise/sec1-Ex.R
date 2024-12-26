library(mAr)
bumpus<-as.data.frame(sparrows)
View(bumpus)

live<-bumpus[1:21,]
dead<-bumpus[21:49,]

a<-as.matrix(cov(live))
b<-as.matrix(cov(dead))
View(a)
View(b)

View(apply(bumpus[1:21,],2,mean))
View(apply(bumpus[22:49,],2,mean))
