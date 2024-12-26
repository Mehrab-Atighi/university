################ packeges##################
library(ggplot2)
library(caTools)
library(corrplot)
library(dplyr)
library(GGally)
library(ggpubr)
library(class)
library(gmodels)
library(tidyverse)
library(dplyr)
library(car)
library(MLmetrics)
library(rpart)
library(rpart.plot) 
library(randomForest)
library(varImp)
library(gbm)
library(caret)
library(corrr)
library(DT)
library(e1071)
library(kernlab)

################## visualization ##############

#Load dataset and DATA EXPLORATION
wdbc=read.csv("data.csv",header=TRUE)
head(wdbc,3)
glimpse(wdbc)

#structure of the dataset
str(wdbc)

#dimension of data set
dim(wdbc)

#summary of data set
summary(wdbc)
##remove na's
wdbc=wdbc[-33]
summary(wdbc)
wdbc %>% count(diagnosis)
wdbc %>% count(diagnosis)%>%group_by(diagnosis) %>%
  summarize(perc_dx = round((n / 569)* 100, 2))

################### pie chart #################
diagnosis.table <- table(wdbc$diagnosis)
colors <- terrain.colors(2) 
# Create a pie chart 
diagnosis.prop.table <- prop.table(diagnosis.table)*100
diagnosis.prop.df <- as.data.frame(diagnosis.prop.table)
pielabels <- sprintf("%s - %3.1f%s", diagnosis.prop.df[,1], diagnosis.prop.table, "%")
pie(diagnosis.prop.table,
    labels=pielabels,  
    clockwise=TRUE,
    col=colors,
    border="gainsboro",
    radius=0.8,
    cex=0.8, 
    main="frequency of cancer diagnosis")
legend(1, .4, legend=diagnosis.prop.df[,1], cex = 0.7, fill = colors)
# calculate collinearity
c <- cor(wdbc[,3:31])
corrplot(c, order = "hclust", tl.cex = 0.7)
#########
ggpairs(wdbc[,c(3:12)])
#radius
A=ggplot(data=wdbc,aes(x=diagnosis,y=radius_mean))+geom_boxplot()+ggtitle("radius")


#area
B=ggplot(data=wdbc,aes(x=diagnosis,y=area_mean))+geom_boxplot()+ggtitle("area")

#concavity 
C=ggplot(data=wdbc,aes(x=diagnosis,y=concavity_mean))+geom_boxplot()+ggtitle("concavity")

ggarrange(A, B, C + rremove("x.text"), 
          ncol = 3, nrow = 1)
ggplot(wdbc,aes(x=diagnosis,fill=texture_mean))+
  geom_bar()+ggtitle("women affected in benign and malingnant stage")


sel_data=wdbc[wdbc$radius_mean>10&
                wdbc$radius_mean<15&
                wdbc$compactness_mean>0.1,]
ggplot(sel_data,aes(x=diagnosis,y=radius_mean,fill=diagnosis))+geom_col()+
  ggtitle("womens affected in higher levels based on mean")

ggplot(wdbc,aes(x=texture_mean,fill=as.factor(diagnosis)))+
  geom_density(alpha=0.4)+
  ggtitle(" texture mean  for benign vs malignant")

ggplot(wdbc,aes(x=as.factor(diagnosis),y=perimeter_mean))+
  geom_violin()+
  ggtitle(" perimeter mean  for benign vs malignant")

data1=wdbc%>%filter(concavity_mean>0.2)
ggplot(data1,aes(x=concavity_mean,y=diagnosis,size=perimeter_se))+
  geom_point()+
  ggtitle("concavity mean  for benign vs malignant")

#########
A=ggplot(wdbc,aes(x=concavity_mean,fill=diagnosis))+
  geom_histogram(binwidth=10)+
  ggtitle(" concavity mean  for benign vs malignant")

B=ggplot(wdbc, aes(x = texture_se)) +
  geom_histogram(binwidth=10) +
  facet_wrap(~ diagnosis)+
  ggtitle(" texture se  for benign vs malignant")

C=ggplot(wdbc, aes(x = perimeter_mean)) +
  geom_histogram(binwidth=10) +
  facet_wrap(~ diagnosis)+
  ggtitle(" perimeter mean  for benign vs malignant")

ggarrange(A, B, C + rremove("x.text"), 
          ncol = 1, nrow = 3)

################## KNN-model #######################
wdbc <- select(wdbc,-id)
dim(wdbc)
table(wdbc$diagnosis)

round(prop.table(table(wdbc$diagnosis)) * 100, digits = 1)

#nomalized data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

new_wdbc <- as.data.frame(lapply(select(wdbc,-diagnosis), normalize))

summary(select(new_wdbc,radius_mean,smoothness_mean))

############## tuning test and train data ##############
wdbc_train <- new_wdbc[1:429,]
wdbc_test <- new_wdbc[430:569,]

#labels
wdbc_train_labels <- wdbc[1:429, 1]
wdbc_test_labels  <- wdbc[430:569, 1]

library(class)
wdbc_test_pred <- knn(train = wdbc_train, test = wdbc_test, cl = wdbc_train_labels, k = 20)
cm =CrossTable(x = wdbc_test_labels , y = wdbc_test_pred, prop.chisq = FALSE)
cm

(105+ 33)/ 140

################## decision tree ####################

best_decision_tree <- rpart(as.factor(wdbc_train_labels)~., data = wdbc_train,
                            control = rpart.control(minsplit = 11,
                                                    maxdepth = 10))
rpart.plot(x = best_decision_tree, box.palette="RdBu", shadow.col="gray", nn=TRUE, yesno = 2)

##################### random forest ####################
best_random_forest <- randomForest(as.factor(wdbc_train_labels)~ ., data = wdbc_train,
                                   nodesize = 9,
                                   sampsize = 329,
                                   mtry = 7,
                                   ntree = 210)
best_random_forest

# Identify the most significant independent variables
varImpPlot(best_random_forest)

################## support vector machine ####################

wdbc1=read.csv("data.csv",header=TRUE)
wdbc1=wdbc1[,-33]
wdbc1=wdbc1[,-1]
wdbc_train1 <- wdbc1[1:429,]
wdbc_test1 <- wdbc1[430:569,]
###### linear kernel ############
cost_range <-c(0.001, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 5)
tune.out <- tune(svm, as.factor(diagnosis)~., data = wdbc_train1, kernel = "linear",
                 ranges = list(cost=cost_range))

bestmod_linear <- tune.out$best.model
summary(bestmod_linear)

####### confusion matrix for linear ##########
predictions_train <- predict(bestmod_linear)
confusionMatrix(predictions_train, as.factor(wdbc_train1$diagnosis))
############# accuracy for test data ##################
predictions_test1 <- predict(bestmod_linear, newdata = wdbc_test1)
confusionMatrix(predictions_test1, as.factor(wdbc_test1$diagnosis))

############ Polynomial kernel ################
tune.out2 <- tune(svm,  as.factor(diagnosis)~., data = wdbc_train1, kernel = "polynomial",
                  ranges = list(cost = cost_range))

bestmod_polynomial <- tune.out2$best.model
summary(bestmod_polynomial)

######## confusion matrix for Polynomial ##########
predictions_train2 <- predict(bestmod_polynomial)
confusionMatrix(predictions_train2, as.factor(wdbc_train1$diagnosis))
############# accuracy for test data ##################
predictions_test2 <- predict(bestmod_polynomial, newdata = wdbc_test1)
confusionMatrix(predictions_test2, as.factor(wdbc_test1$diagnosis))
################ Radial kernel #####################
gamma_range = c(0.5,1,2,3,4)

tune.out23 <- tune(svm, as.factor(diagnosis) ~., data=wdbc_train1 , kernel = "radial",
                   ranges = list(cost = cost_range,
                                 gamma = gamma_range))
bestmod_radial <- tune.out23$best.model
summary(bestmod_radial)


######## confusion matrix for radial ##########

predictions_train3 <- predict(bestmod_radial)
confusionMatrix(predictions_train3, as.factor(wdbc_train1$diagnosis))

############# accuracy for test data ##################
predictions_test <- predict(bestmod_radial, newdata =wdbc_test1)
confusionMatrix(predictions_test, as.factor(wdbc_test1$diagnosis))




