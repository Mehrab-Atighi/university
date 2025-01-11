#Read Datasets
# dfB = read.csv("Data/ss15pusb.csv")
Predictors = c("PWGTP" , "AGEP" , "SEX" , "MAR" , "DIS",
               "COW" , "DDRS" , "DEAR" , "DEYE" , "DOUT",
               "DPHY" , "DREM" , "HINS1" , "HINS2" ,"HINS3" ,
               "HINS4" , "JWMNP" , "JWTR" , "MIL" , "SCHG" , 
               "NATIVITY" , "PRIVCOV" , "PUBCOV" , "RAC1P")

newdfB = dfB[ , Predictors]
#newdfB = na.omit(newdfB)
newdfB$DIS = newdfB$DIS - 1
categorical_columns = c(3,4,6:16, 18:length(Predictors) )
response_index = 3
for(i in categorical_columns){
  newdfB[,i] = as.factor(newdfB[,i])

}

library(fastDummies)
newdfB = data.frame(newdfB[,Predictors[c(1,2,5, 17)] ], dummy_cols(newdfB[,Predictors[categorical_columns]] , remove_selected_columns = TRUE))
newdfB$DIS = as.factor(newdfB$DIS)
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(newdfB), replace = T,
                 prob = c(0.6,0.4))

train <- newdfB[sample, ]
test <- newdfB[!sample, ]




ml1 = glm(DIS ~ . , data = train , family = binomial())

summary(ml1)

# pp1 = predict.glm(ml1 ,newdata = test[,-response_index],type = "response")
# pp1 = ifelse(pp1 >= 0.5 , 1 , 0)
# pp1 = as.factor(pp1)
# library(caret)
# 
# confusionMatrix(data = pp1 , reference = factor(test[,response_index]) , mode = "everything")


library(leaps)
forward = regsubsets(DIS ~ . , data = train ,nvmax = 20 , method = "forward")
backward = regsubsets(DIS ~ . , data = train ,nvmax = 20, method = "backward")
(result_forward = summary(forward))
(result_backward = summary(backward))

library(ggplot2)
library(tidyverse)

par(mfrow=c(2,3))
plot(1:length(result_forward$cp) ,result_forward$cp , xlab="number of predictors" , ylab="Cp" , type = "b" ,main = "Forward Cp")
points(which.min(result_forward$cp) , y = min(result_forward$cp) ,col="red" , cex=2)


plot(1:length(result_forward$bic) ,y = result_forward$bic , xlab="number of predictors" , ylab="BIC" , type = "b" , main = "Forward BIC")
points(which.min(result_forward$bic) , min(result_forward$bic) ,col="red" , cex=2)


plot(1:length(result_forward$adjr2) ,result_forward$adjr2 , xlab="number of predictors" , ylab="AdjR2" , type = "b" , main = "Forward AdjR2")
points(which.max(result_forward$adjr2) , max(result_forward$adjr2) ,col="red" , cex=2)

##second row ( backward)

plot(1:length(result_forward$cp) ,result_forward$cp , xlab="number of predictors" , ylab="Cp" , type = "b" ,main = "Backward Cp" )
points(which.min(result_forward$cp) , y = min(result_forward$cp) ,col="red" , cex=2)


plot(1:length(result_forward$bic) ,y = result_forward$bic , xlab="number of predictors" , ylab="BIC" , type = "b", main = "Backward BIC")
points(which.min(result_forward$bic) , min(result_forward$bic) ,col="red" , cex=2)


plot(1:length(result_forward$adjr2) ,result_forward$adjr2 , xlab="number of predictors" , ylab="AdjR2" , type = "b"  , main = "Backward AdjR2")
points(which.max(result_forward$adjr2) , max(result_forward$adjr2) ,col="red" , cex=2)

