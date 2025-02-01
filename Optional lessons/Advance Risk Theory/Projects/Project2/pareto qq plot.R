# Load the dataset
library(fitdistrplus)
data("danishuni")
loss_data <- danishuni$Loss

# Sort the loss data
sorted_loss <- sort(loss_data)
n <- length(sorted_loss)

# Estimate Pareto parameters
# We'll estimate the scale (sigma) and shape (alpha) using the method of moments
sigma <- min(loss_data)  # Scale parameter (minimum value of the data)
alpha <- 1 / (log(mean(loss_data / sigma)))  # Shape parameter

# Generate theoretical quantiles for the Pareto distribution
pp <- ppoints(n)  # Proportions for quantiles
pareto_quantiles <- sigma * (1 - pp)^(-1 / alpha)  # Theoretical quantiles

# Create the QQ-plot
par(mfrow = c(1,1))
qqplot(y = pareto_quantiles, x = sorted_loss, 
       main = "QQ-Plot of Loss Data vs Pareto Distribution",
       xlab = "Theoretical Quantiles (Pareto)",
       ylab = "Empirical Quantiles (Loss)",
       pch = 16, col = "blue")

# Add a 45-degree reference line
abline(0, 1, col = "red", lty = 2)
