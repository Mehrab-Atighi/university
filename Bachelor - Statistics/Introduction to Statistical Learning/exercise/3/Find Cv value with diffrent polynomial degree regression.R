library(ISLR)
library(tibble)
data(Auto)

# Leave-one-out cross-validation :
dt<-data.frame(Auto$mpg , Auto$horsepower)
head(dt)
all.MSE.table<-tibble(number.of.leave.out.data = rep(c(1:nrow(dt)),10) , degree = rep(c(1:10),each=nrow(dt)) ,MSE =c(1:3920))
# i show the degree of the polynomial regression function
# j show the number of data that we will leave out (MSE_j) for each i
test<-data.frame(dt$Auto.horsepower ,  dt$Auto.mpg)
count=1
for(i in 1:10){
  for(j in 1:nrow(dt)){
    train<-data.frame(train1=dt$Auto.mpg[-j] , train2=dt$Auto.horsepower[-j])
    fit<-glm( train1 ~poly(train2,degree= i),data = train)
    new= data.frame(train2 = test$dt.Auto.horsepower[j])
    all.MSE.table$MSE[count]<-(test$dt.Auto.mpg[j]-predict(fit ,newdata = new ,type="response"))^2
    count=count+1
    }
}
View(all.MSE.table)
Cv=c(1:10)
Cv.table<-tibble(Degree =1:10 ,Cv)

for(i in 1:10){
Cv.table$Cv[i]=mean(all.MSE.table$MSE[which(all.MSE.table$degree==i)])
}
View(Cv.table)
