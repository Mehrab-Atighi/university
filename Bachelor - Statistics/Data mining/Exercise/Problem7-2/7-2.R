
rm(list=ls())

Data<-read.csv("F:/lessons/Bachelor/Data mining/Data/UniversalBank.csv")
View(Data)
head(Data,4)
dim(Data)
data=Data[,-c(1,5)]
data$Education_1=c(1:nrow(data))
data$Education_2=c(1:nrow(data))
data$Education_3=c(1:nrow(data))
for(i in 1:nrow(data)){
  if(data$Education[i]==1){
    data$Education_1[i] = 1 
  }else(data$Education_1[i] = 0)
  if(data$Education[i]==2){
    data$Education_2[i] = 1 
  }else(data$Education_2[i] = 0)
  if(data$Education[i]==3){
    data$Education_3[i] = 1 
  }else(data$Education_3[i] = 0)
}
data$Education=c()

head(data)
dim(data)
summary(data)

set.seed(5)
n=sample(c(0,1) , nrow(data) ,prob = c(0.6 , 0.4) ,replace = TRUE)
train_data=data[which(n==0),]
test_data=data[which(n==1),]


train_normal<- train_data
test_normal<-test_data
data_normal<-data
new.df=data.frame(Age = 40, Experience = 10 , Income = 84,
                  Family = 2 , CCAvg = 2, Education_1= 0 ,Education_2= 1,
                  Education_3=0 , Mortgage = 0 , Securities.Account = 0 ,
                  CD.Account = 0 , Online = 1 , CreditCard =1)

#install.packages("caret")
library(caret)

norm.values<-preProcess(train_data[,-7] , method = c("center" , "scale"))
train_normal<-predict(norm.values , train_data[,-7])
test_normal<-predict(norm.values , test_data[,-7])
data_normal<-predict(norm.values , data[,-7])
new.norm.df <-predict(norm.values, new.df)


#install.packages("FNN")
library(FNN)
nn<-knn(train = train_normal , test = new.df,
        cl =train_data[,7] ,k=1)
nn
row.names(train_data)[attr(nn, 'nn.index')]




#b)
#Now we want to select K value:
#we should work with all of data
library(caret);library(e1071)

accuracy_data=data.frame(k = seq(1,nrow(train_normal),1) , accuracy=rep(0,nrow(train_normal)))
system.time(
  for(i in 1:nrow(train_normal)){
    knn.predict<-FNN::knn(train =train_normal[,-7] ,test = test_normal[,-7] ,
                          cl = train_data[,7] ,k=i)
    accuracy_data[i , 2] = confusionMatrix(data = knn.predict[1:nrow(test_data)],
                                           factor(test_data[,7]) ,)$overall[1]
  })
#head(accuracy_data)
best = which.max(accuracy_data$accuracy)


#c

knn.predict<-FNN::knn(train =train_normal[,-7] ,test = test_normal[,-7] ,
                      cl = train_data[,7] ,k=best)
confusionMatrix(data = knn.predict[1:nrow(test_data)],
                factor(test_data[,7]) )$overall[1]

#d)
new.df=data.frame(Age = 40, Experience = 10 , Income = 84,
                  Family = 2 , CCAvg = 2, Education_1= 0 ,Education_2= 1,
                  Education_3=0 , Mortgage = 0 , Securities.Account = 0 ,
                  CD.Account = 0 , Online = 1 , CreditCard =1)

nn<-knn(train = train_normal , test = new.df,
        cl =train_data[,7] ,k=best)
r=row.names(train_data)[attr(nn, 'nn.index')]
table(train_data[r,7])


#e)
n=sample(c(0,1,2) , nrow(data) ,prob = c(0.5 , 0.3 ,0.2) ,replace = TRUE)
train_data_2=data[which(n==0),]
valid_data=data[which(n==1),]
test_data_2=data[which(n==2),]

norm.values<-preProcess(train_data_2[,-7] , method = c("center" , "scale"))
train_normal_2<-predict(norm.values , train_data_2[,-7])
test_normal_2<-predict(norm.values , test_data_2[,-7])
valid_normal<-predict(norm.values , valid_data[,-7])
data_normal_2<-predict(norm.values , data[,-7])

#for test data:
knn.predict<-FNN::knn(train = train_normal_2[,-7] ,test = test_normal_2[,-7] ,
                      cl = train_data_2[,7] ,k=best)
confusionMatrix(data = knn.predict[1:nrow(test_data_2)],
                factor(test_data_2[,7]) )$overall[1]
#for valid data:
knn.predict<-FNN::knn(train =train_normal_2[,-7] ,test = test_normal_2[,-7] ,
                      cl = train_data_2[,7] ,k=best)
confusionMatrix(data = knn.predict[1:nrow(valid_normal)],
                factor(valid_data[,7]) )$overall[1]

