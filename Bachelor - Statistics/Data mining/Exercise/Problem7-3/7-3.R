rm(list=ls())

Data<-read.csv("F:/lessons/Data mining/Data/BostonHousing.csv")
data=Data[,-13]
#view(data)
head(data)
dim(data)

set.seed(5)
n = sample(c(0,1),nrow(data) ,prob = c(0.6,0.4)  ,replace = TRUE)
train_data=data[which(n==0),]
test_data=data[which(n==1),]



#a)
train_normal<- train_data
test_normal<-test_data
data_normal<-data


library(caret)

norm.values<-preProcess(train_data[,-13] , method = c("center" ,"scale"))
train_normal<-predict(norm.values , train_data[,-13])
test_normal<-predict(norm.values , test_data[,-13])
data_normal<-predict(norm.values , data[,-13])



library(class)

accuracy_data=data.frame(k = seq(1,5,1) , accuracy=rep(0,5))
system.time(
  for(i in 1:5){
    knn.predict<-class::knn(train =train_normal[,-13] ,test = test_normal[,-13] ,
                            cl = train_data[,13] ,k=i)
    accuracy_data[i , 2] = confusionMatrix(data = knn.predict[1:nrow(test_normal)],
                                           factor(test_data[,13]) ,)$overall[1]
  })
accuracy_data
(best = which.max(accuracy_data[,2]))
#b)
new.df<-data.frame(CRIM=0.2 , ZN = 0 , INDUS = 7 ,
                   CHAS = 0 , NOX = 0.538 , RM = 6 ,
                   AGE = 62 , DIS = 4.7 , RAD = 4 ,
                   TAX  =307 , PTRATIO = 21 , LSTAT = 10)

new.norm.df <-predict(norm.values, new.df)
class::knn(train_normal[,-13] ,test = new.df , cl = train_data[,13] , k= best)


#c)

knn.2<-class::knn(train_normal[,-13] ,test = train_normal[,-13] , cl = train_data[,13] , k= best)
confusionMatrix(data = knn.2[1:nrow(train_normal)],
                factor(train_data[,13]) ,)$overall[1]

#d)
#e)
