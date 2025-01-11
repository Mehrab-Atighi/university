

#Start Main Work:
dfA = read.csv("Data/ss15pusa.csv")
dfB = read.csv("Data/ss15pusb.csv")
finaldf = rbind(dfA , dfB)
save(finaldf , file ="finaldf.RData")
Predictors = c(  "AGEP" , "SEX" , "MAR" , "PINCP",
               "COW" ,"MIL"  , "SCHG" , "DIS" )

newfinaldf = finaldf[ , Predictors]
#newfinaldf = na.omit(newfinaldf)
newfinaldf$DIS = newfinaldf$DIS - 1
categorical_columns = c(2,3,5,6,7)
for(i in categorical_columns){
  newfinaldf[,i] = as.factor(newfinaldf[,i])
  
}

library(fastDummies)
newfinaldf = data.frame(newfinaldf[,Predictors[c(8,1,4)] ], dummy_cols(newfinaldf[,Predictors[categorical_columns]] , remove_selected_columns = TRUE))
response_index = 1
newfinaldf$DIS = as.factor(newfinaldf$DIS)
newfinaldf$DIS = relevel(newfinaldf$DIS , ref =1)
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(newfinaldf), replace = T,
                 prob = c(0.6,0.4))

train <- newfinaldf[sample, ]
test <- newfinaldf[!sample, ]




ml1 = glm(DIS ~ . , data = train , family = binomial())
save(ml1 , file = "MultinomialLogisticRegressionModel.RData")
summary(ml1)

pp1 = predict.glm(ml1 ,newdata = test[,-response_index],type = "response")
pp1 = ifelse(pp1 >= 0.5 , 1 , 0)
pp1 = as.factor(pp1)
library(caret)

confusionMatrix(data = pp1 , reference = factor(test[,response_index]) , mode = "everything")

#new prediction probabilities :

new_data = data.frame(AGEP  =85 ,  PINCP =10000 ,  SEX_1 =0 ,  SEX_2  =1 , MAR_1 =0 ,  MAR_2  =0 , MAR_3 =0 ,
                      MAR_4 =0 ,  MAR_5  =1 ,COW_1 =0 ,  COW_2 =0 , COW_3 =0 ,  COW_4  =0 , COW_5 =0 ,  COW_6  =0 , COW_7 =0 ,
                      COW_8 =0 ,  COW_9 =0 ,  COW_NA =1 , MIL_1  =0, MIL_2  =0 , MIL_3 =0 ,  MIL_4  =0 ,MIL_NA =1 , SCHG_1 =0 ,
                      SCHG_2 =0 , SCHG_3  =0 , SCHG_4 =0 , SCHG_5 =0 ,SCHG_6 =0 , SCHG_7 =0 , SCHG_8 =0 , SCHG_9 =0 , SCHG_10 =0 ,
                      SCHG_11=0 , SCHG_12 =0 , SCHG_13 =0 , SCHG_14 =0 , SCHG_15 =0 ,SCHG_16 =0 , SCHG_NA=1)
predict.glm(ml1 , newdata = new_data , type = "response")

