# Load Danish dataset
library(fitdistrplus)
data("danishuni")  # Assuming 'evir' package
loss_data <- danishuni$Loss          # Extract the loss column
n <- length(loss_data)               # Number of observations

# Values of p to consider
p_values <- seq(1,4.5 , 0.5)

# Preallocate storage for R_n(p)
R_n_matrix <- matrix(NA, nrow = n, ncol = length(p_values))
colnames(R_n_matrix) <- paste0("p=", p_values)

# Calculate R_n(p) for each value of p
for (j in seq_along(p_values)) {
  p <- p_values[j]
  abs_loss_p <- abs(loss_data)^p
  
  for (i in 1:n) {
    S_n <- sum(abs_loss_p[1:i])       # Sum up to the i-th element
    M_n <- max(abs_loss_p[1:i])       # Max up to the i-th element
    R_n_matrix[i, j] <- M_n / S_n     # Ratio
  }
}

# Plot R_n(p) against n for each p
par(mfrow = c((round(length(p_values)/2 , 0)), 2))  # One plot for each p
for (j in seq_along(p_values)) {
  plot(1:n, R_n_matrix[, j], type = "l", 
       main = paste("R_n(p) for p =", p_values[j]),
       xlab = "n", ylab = "R_n(p)", col = "blue", lwd = 2)
  abline(h = 0, col = "red", lty = 2) # Add reference line at 0
}
