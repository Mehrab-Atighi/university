library(dplyr)
library(readxl)
library(ggplot2)
library(DBI)
library(tidyverse)
library(data.table)
library(caret)       # For machine learning
library(tensorflow)  # For TensorFlow backend
library(keras)       # For neural networks
library(doParallel)
# cl <- makePSOCKcluster(4)  # تعداد هسته‌های پردازنده (مثلاً 4)
# registerDoParallel(cl)
load("FinalDf.RData")
FinalDf = as.data.frame(FinalDf[1:5000,])
for(i in 42:44){
  FinalDf[,i] = ifelse(is.na(FinalDf[,i]) , 0  , FinalDf[,i])
}

FinalDf$TotalLoss = FinalDf$LifeLoss_Value +
  FinalDf$PassengerLoss_Value +
  FinalDf$FinanceLoss_Value

#remove some columns for na values and other loss types.

# Check for missing values in each column
missing_values <- colSums(is.na(FinalDf))

FinalDf = FinalDf[,-c(42:44)]
FinalDf = FinalDf[,-c(which(colnames(FinalDf) %in% names(which(missing_values>0 ))))]


split_data <- function(data, train_percentage) {
  # Ensure the train_percentage is between 0 and 1
  if (train_percentage < 0 || train_percentage > 1) {
    stop("train_percentage must be between 0 and 1")
  }
  
  # Calculate the number of rows for the training set
  n <- nrow(data)
  n_train <- floor(train_percentage * n)
  
  # Randomly sample the indices for the training set
  train_indices <- sample(1:n, n_train)
  
  # Create the training and test sets
  train_set <- data[train_indices, ]
  test_set <- data[-train_indices, ]
  
  # Return the training and test sets as a list
  return(list(train = train_set, test = test_set))
}
set.seed(123)
# Assuming finalDf is your dataset and you want 80% for training
result <- split_data(FinalDf, train_percentage = 0.8)
train_set <- result$train
test_set <- result$test

######
dim(train_set)
head(train_set)

# Separate features (X) and target (y) for training and testing sets
X_train <- train_set[, -c(18:dim(train_set)[2])]  # All columns except the last one
y_train <- train_set[, c(dim(train_set)[2])]   # Last column (TotalLoss)

X_test <- test_set[, -c(18:dim(test_set)[2])]    # All columns except the last one
y_test <- test_set[, c(dim(test_set)[2])]     # Last column (TotalLoss)
# Scale/normalize the features
preprocess_params <- preProcess(X_train, method = c("center", "scale"))
X_train_scaled <- predict(preprocess_params, X_train)
X_test_scaled <- predict(preprocess_params, X_test)


X_train_scaled = cbind(X_train_scaled, train_set[,18:32])
X_test_scaled = cbind(X_test_scaled, test_set[,18:32])

train_control <- trainControl(
  method = "cv",  # Cross-validation
  number = 5,     # 5-fold CV
  savePredictions = "final",
  allowParallel = TRUE,
  verboseIter = TRUE  # Show progress updates
  
)

# Define a list of models to train
models <- c(
   "lm",          # Linear Regression
   #"glm",         # Logistic Regression
   "glmnet",      # Ridge/Lasso Regression
   "rpart",       # Decision Trees
   "rf",          # Random Forests
   "gbm",         # Gradient Boosting Machines
   "xgbTree",     # XGBoost (Gradient Boosting)
  "svmRadial",   # Support Vector Machines (Radial Kernel)
  "knn",         # k-Nearest Neighbors
  "pls",         # Principal Component Regression
  #"lda",         # Linear Discriminant Analysis
  #"qda",         # Quadratic Discriminant Analysis
  #"naive_bayes", # Naive Bayes
  "nnet"         # Neural Networks
)

# Train and evaluate all models
results <- list()
test_errors <- data.frame(Model = character(), RMSE = numeric(), R2 = numeric(), MAE = numeric(), stringsAsFactors = FALSE)

for (model in models) {
  set.seed(123)  # For reproducibility
  print(paste("Training model:", model))
  
  # Train the model
  fit <- caret::train(
    x = X_train_scaled,  # Features
    y = y_train,         # Target variable
    method = model,      # Model type
    trControl = train_control
  )
  
  # Store the results
  results[[model]] <- fit
  
  # Predict on the test set
  predictions <- predict(fit, newdata = X_test_scaled)
  
  # Calculate test error metrics
  if (is.factor(y_test)) {  # Classification
    cm <- confusionMatrix(predictions, y_test)
    accuracy <- cm$overall["Accuracy"]
    kappa <- cm$overall["Kappa"]
    test_errors <- rbind(test_errors, data.frame(Model = model, Accuracy = accuracy, Kappa = kappa))
  } else {  # Regression
    rmse <- sqrt(mean((as.numeric(predictions) - as.numeric(y_test))^2))
    r2 <- cor(as.numeric(predictions), as.numeric(y_test))^2
    mae <- mean(abs(as.numeric(predictions) - as.numeric(y_test)))
    test_errors <- rbind(test_errors, data.frame(Model = model, RMSE = rmse, R2 = r2, MAE = mae))
  }
}

# Stop parallel processing
# stopCluster(cl)

# Print test errors
print(test_errors)

# Compare model performance (cross-validation results)
comparison <- resamples(results)
summary(comparison)

# Visualize model performance
dotplot(comparison)

