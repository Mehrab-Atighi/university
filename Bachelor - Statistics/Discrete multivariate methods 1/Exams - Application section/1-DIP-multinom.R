## OCR English Digits
##
## Author: Mehrab Atighi
## Arak University, Iran
##
## Date: 2021-12-11
##
## Email: mehrab.atighi@gmail.com


load("mnist.RData")


dim(Pictures)
length(Labels)

Pictures.test <- Pictures[-(1:6000), ]
Labels.test <- Labels[-(1:6000)]

Pictures <- Pictures[1:6000, ]
Labels <- Labels[1:6000]


df <- data.frame(Labels = Labels, Pictures)

row <- 5
image(matrix(Pictures[row, ], 28)[, 28:1], 
      col = grey(seq(1,0,length=256)),
      main = paste0("Label = ", Labels[row]))


View(head(Pictures))

Labels <- factor(Labels)
table(Labels)



library(nnet)

m1 <- multinom(Labels ~ . - 1, MaxNWts = 10000, data = df)
# save(m1, file = "TrainedModel-v1.RData")
beta.m1 <- coef(m1)
dim(beta.m1)

row = 3
image(matrix(beta.m1[row, ], 28)[, 28:1], 
      col = grey(seq(1,0,length=256)),
      main = paste0("Beta", row))




row <- 5
image(matrix(Pictures.test[row, ], 28)[,28:1], 
      col = grey(seq(1,0,length=256)),
      main = paste0("Test Label = ", Labels.test[row]))

data1 <- data.frame(t(Pictures.test[row,]))
predict(m1, newdata = data1)
round(predict(m1, newdata = data1, type = "p"), 2)


df.test <- data.frame(Pictures.test)
Predict.test <- predict(m1, newdata = df.test)
(CM <- table(Predict.test, Labels.test))
(Accuracy <- sum(diag(CM)) / nrow(Pictures.test) * 100)
table(Labels.test)

CM.percent <- scale(CM, center = F, scale = colSums(CM)) * 100
round(CM.percent)

CM.long <- reshape2::melt(CM.percent)
CM.long$Labels.test <- factor(CM.long$Labels.test)
CM.long$Predict.test <- factor(CM.long$Predict.test)

library(ggplot2)

ggplot(CM.long, aes(x = Predict.test, y = Labels.test, label = round(value))) + 
  geom_point(aes(color = Labels.test, size = value), alpha = 0.6) +
  geom_text(size = 2) + scale_size(range = c(0.2, 18.5))
