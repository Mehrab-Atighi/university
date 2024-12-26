library(mAr)
data("sparrows")
bumpus<-as.data.frame(sparrows)
View(bumpus)

library(ICSNP)
#H0 : mu_live = mu_dead
#H1 : mu_live != mu_dead 

HotellingsT2(bumpus[1:21,],bumpus[22:42,])

#H0 : mu_live = mu_dead
#H1 : mu_live != mu_dead 
#for x1(length):
t.test(x=bumpus[1:21,1],mu = colMeans(bumpus[22:49,])[1],var.equal = TRUE)

#H0 : mu_live = mu_dead
#H1 : mu_live != mu_dead 
#for x2(extend):
t.test(x=bumpus[1:21,2],mu = colMeans(bumpus[22:49,])[2])

#H0 : mu_live = mu_dead
#H1 : mu_live != mu_dead 
#for x3(Head):
t.test(x=bumpus[1:21,3],mu = colMeans(bumpus[22:49,])[3])

#H0 : mu_live = mu_dead
#H1 : mu_live != mu_dead 
#for x4(Humerus):
t.test(x=bumpus[1:21,4],mu = colMeans(bumpus[22:49,])[4])

#H0 : mu_live = mu_dead
#H1 : mu_live != mu_dead 
#for x5(Sternum):
t.test(x=bumpus[1:21,5],mu = colMeans(bumpus[22:49,])[5])







