# Extract the 'loss' column
library(fitdistrplus)
data("danishuni")
loss_data <- danishuni$Loss

# Sort the loss data
sorted_loss <- sort(loss_data)

# Generate theoretical quantiles for an exponential distribution
lambda1 <- 1/mean(loss_data)#quantile(loss_data , probs = 0.975) # Estimate rate parameter (lambda) from the data
lambda2 <- 1/quantile(loss_data , probs = 0.975) # Estimate rate parameter (lambda) from the data

n <- length(sorted_loss)
exp_quantiles1 <- qexp(ppoints(n), rate = lambda1)  # Theoretical quantiles
exp_quantiles2 <- qexp(ppoints(n), rate = lambda2)  # Theoretical quantiles

# Create the QQ-plot
par(mfrow = c(1,2))
qqplot(y = exp_quantiles1, x = sorted_loss, 
       main = "QQ-Plot of Loss Data vs Exponential Distribution with lambda = 1/mean",
       xlab = "Theoretical Quantiles (Exponential)",
       ylab = "Empirical Quantiles (Loss)",
       pch = 16, col = "blue")
# Add a 45-degree reference line
abline(0, 1, col = "red", lty = 2)
#add second plot 
qqplot(y = exp_quantiles2, x = sorted_loss, 
       main = "QQ-Plot of Loss Data vs Exponential Distribution with lambda = 1/percentile(0.975)",
       xlab = "Theoretical Quantiles (Exponential)",
       ylab = "Empirical Quantiles (Loss)",
       pch = 16, col = "blue")
# Add a 45-degree reference line
abline(0, 1, col = "red", lty = 2)

