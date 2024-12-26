############################################## Loading libraries ################################################################

library(ggplot2)
library(kernlab)
library(caret)
library(caTools)
library(gridExtra)
library(imagefx)
library(imager)
library(OpenImageR)
library("EBImage")
library(magick)
library(nnet)

################################################ Multinomial Logistic Regression  ################################################

##read and check data set

Data=load("F:/lessons/Bachelor/Gosaste/Hoda/gosate-project/HodaDigits.RData")


dim(Pictures)
dim(Pictures.test)
length(Labels)
length(Labels.test)

df <- data.frame(Labels = Labels, Pictures = Pictures)

row <- 12
image(matrix(Pictures[row, ], 32)[, 32:1], 
      col = grey(seq(1,0,length=256)),
      main = paste0("Label = ", Labels[row]))

#--------------------------------------------- Logistic model  ----------------------------------------------#


#m1 <- multinom(Labels ~ . - 1, MaxNWts = 11000, data = df)
#save(m1, file =  "train model .RData" )
load( "F:/lessons/Bachelor/Gosaste/Hoda/gosate-project/train model .RData")
beta.m1 <- coef(m1)
dim(beta.m1)

###################### Ploting Coefficient ######################################################################################

row = 3

image(matrix(beta.m1[row, ], 32)[, 32:1], 
      col = grey(seq(1,0,length=256)),
      main = paste0("Beta", row))

######################## check prediction ###########################

row <- c(12889,1999,8999)
image(matrix(Pictures.test[row[3], ], 32)[,32:1], 
      col = grey(seq(1,0,length=256)),
      main = paste0("Test Label = ", Labels.test[row[3]]))

data1 <- data.frame(t(Pictures.test[row,]))
names(data1) = names(df[,-1])

predict(m1, newdata = data1)
round(predict(m1, newdata = data1, type = "p"), 2)


######################## Ploting prediction and accuracy ####################

##Preprocessing data for visualisation with ggplot2 and Accuracy calculation

df.test <- data.frame(Pictures.test)
names(df.test) = names(df[,-1])

Predict.test <- predict(m1, newdata = df.test)
(CM <- table(Predict.test, Labels.test))
(Accuracy <- sum(diag(CM)) / nrow(Pictures.test) * 100)
table(Labels.test)

CM.percent <- scale(CM, center = F, scale = colSums(CM)) * 100
round(CM.percent)

CM.long <- reshape2::melt(CM.percent)
CM.long$Labels.test <- factor(CM.long$Labels.test)
CM.long$Predict.test <- factor(CM.long$Predict.test)


ggplot(CM.long, aes(x = Predict.test, y = Labels.test, label = round(value))) + 
  geom_point(aes(color = Labels.test, size = value), alpha = 0.6) +
  geom_text(size = 2) + scale_size(range = c(0.2, 18.5))

######################################### Testing with real pictures ########################################################################

## Loading pictures 

n=70
data = matrix(rep(0,n*1024),nrow= n )
for(i in 0:69){
  im <- readImage(paste0("C:/Users/atusa/Desktop/real-test-pic/real-test-pic/",i,".jpg"))
  m = sort(dim(im))[2]
  im <- image_read(paste0("C:/Users/atusa/Desktop/real-test-pic/real-test-pic/",i,".jpg"))
  im = image_convert(im , type = 'Bilevel')
  im = image_crop(im, geometry_area(m, m), repage = FALSE)
  im = image_resize(im, geometry_size_pixels(32, 32, preserve_aspect = FALSE))
  image_write(im, path = paste0("C:/Users/atusa/Desktop/real-test-pic/real-test-pic/",100*i,".jpg"),
              format = "jpg" , quality = 100)
  im <- readImage(paste0("C:/Users/atusa/Desktop/real-test-pic/real-test-pic/",100*i,".jpg"))
  data[i+1,]= abs(1-round(im))
}


Data=load("C:/Users/atusa/Downloads/ex/ex/HodaDigits.RData")

df <- data.frame(Labels = Labels, Pictures = Pictures)


## modeling with real pictures and result

row=3
image(matrix(data[row,], 32)[, 32:1], 
      col = grey(c(1,0)),
      main = paste0("Label =", row-1 ))

data_test <- data.frame(data)
names(data_test) = names(df[,-1])
round(predict(m1, newdata = data_test[row,], type = "p"), 2) 



pp = predict(m1, newdata = data_test)
l = rep(c(0:9),n/10)
(CM <- table(pp, l))
(Accuracy <- sum(diag(CM)) / n * 100)
table(l)


CM.percent <- scale(CM, center = F, scale = colSums(CM)) * 100
round(CM.percent)

CM.long <- reshape2::melt(CM.percent)
CM.long$Labels.test <- factor(CM.long$l)
CM.long$Predict.test <- factor(CM.long$pp)



ggplot(CM.long, aes(x = pp, y = l, label = round(value))) + 
  geom_point(aes(color = l, size = value), alpha = 0.6) +
  geom_text(size = 2) + scale_size(range = c(0.2, 18.5))


#--------------------------------------------- SVM model ----------------------------------------------#

## Convert label variable into factor

Labels <- factor(Labels)
summary(Labels)

Labels.test <- factor(Labels.test)
summary(Labels.test)

## Linear kernel using default parameters

#model1_linear <- ksvm(Labels ~ ., data = Pictures, scaled = FALSE, kernel = "vanilladot", C = 1)
#print(model1_linear)

load("C:/Users/atusa/Downloads/model1_linear.RData")

eval1_linear <- predict(model1_linear, newdata = Pictures.test, type = "response")
confusionMatrix(eval1_linear, Labels.test) 


## Linear kernel using stricter C

#model2_linear <- ksvm(Labels ~ ., data = train, scaled = FALSE, kernel = "vanilladot", C = 10)
#print(model2_linear) 

load("C:/Users/atusa/Downloads/model2_linear.RData")

eval2_linear <- predict(model2_linear, newdata = test, type = "response")
confusionMatrix(eval2_linear, test$label) 

## Linear kernel using stricter C

#model2_linear <- ksvm(label ~ ., data = train, scaled = FALSE, kernel = "vanilladot", C = 10)
#print(model2_linear) 

#eval2_linear <- predict(model2_linear, newdata = test, type = "response")
#confusionMatrix(eval2_linear, test$label) 


## Radial kernel using default parameters

#model1_rbf <- ksvm(label ~ ., data = train, scaled = FALSE, kernel = "rbfdot", C = 1, kpar = "automatic")
#print(model1_rbf) 

load("C:/Users/atusa/Downloads/model1_rbf.RData")
eval1_rbf <- predict(model1_rbf, newdata = test, type = "response")
confusionMatrix(eval1_rbf, test$label)

########################################### Ploting SVM ############################################################

## linear kernel
a=confusionMatrix(eval1_linear, Labels.test) 
(Accuracy <- sum(diag(a$table)) / nrow(Pictures.test) * 100)
table(Labels.test)

CM.percent <- a$table * 100
round(CM.percent)

CM.long <- reshape2::melt(CM.percent)
CM.long$Labels.test <- factor(CM.long$Reference)
CM.long$Predict.test <- factor(CM.long$Prediction )



ggplot(CM.long, aes(x = Predict.test, y = Labels.test, label = round(value))) + 
  geom_point(aes(color = Labels.test, size = value), alpha = 0.6) +
  geom_text(size = 2) + scale_size(range = c(0.2, 18.5))

## rbf kernel

c=confusionMatrix(eval1_rbf, Labels.test) 
(Accuracy <- sum(diag(c$table)) / nrow(Pictures.test) * 100)
table(Labels.test)

CM.percent <- c$table * 100
round(CM.percent)

CM.long <- reshape2::melt(CM.percent)
CM.long$Labels.test <- factor(CM.long$Reference)
CM.long$Predict.test <- factor(CM.long$Prediction )



ggplot(CM.long, aes(x = Predict.test, y = Labels.test, label = round(value))) + 
  geom_point(aes(color = Labels.test, size = value), alpha = 0.6) +
  geom_text(size = 2) + scale_size(range = c(0.2, 18.5))



