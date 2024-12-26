library(MASS)
data = read.table("F:/lessons/Multi countios Variate2/data/apple-data.txt",header = TRUE)
#View(data)
model = qda(data$group~. , data = data)
model
pp = predict(model)
(t=table(pp$class,data$group))
(accuracy = sum(diag(t)) / length(data$group))
(miss_err_classification_rate = 1 - accuracy)