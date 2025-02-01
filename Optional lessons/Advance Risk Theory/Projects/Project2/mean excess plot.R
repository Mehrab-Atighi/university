# Load the dataset
# Assuming your dataset is a data frame with columns 'date' and 'loss'
# Example of loading a dataset:
library(fitdistrplus)
data("danishuni")

# Extract the 'loss' column
loss_data <- danishuni$Loss

# Sort the loss data
n <- length(loss_data)
sorted_data <- sort(loss_data)

# Function to calculate the mean excess for a given k
mean_excess <- function(data, k) {
  threshold <- data[k]
  excesses <- data[(k+1):n] - threshold
  return(mean(excesses, na.rm = TRUE))
}

# Compute the mean excess values for each k
mean_excess_values <- sapply(1:(n-1), function(k) mean_excess(sorted_data, k))

# Prepare points for the plot
x_points <- sorted_data[1:(n-1)] # X_{k,n}
y_points <- mean_excess_values   # e_n(X_{k,n})

# Create the Mean Excess Plot
plot(x_points, y_points, type = "p", pch = 16, col = "blue",
     xlab = "Threshold (X_{k,n})",
     ylab = "Mean Excess e_n(X_{k,n})",
     main = "Mean Excess Plot" 
    #, xlim = c(0 , 70)
     )
abline(h = 0, col = "red", lty = 2)

