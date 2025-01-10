####### 1 ######
# Cramér–Lundberg approximation function
cramer_lundberg <- function(u, theta, mu, M_prime_X_R, R) {
  # Calculate constant C
  C <- (theta * mu) / (M_prime_X_R - mu * (1 + theta))
  
  # Calculate the approximation of ruin probability
  psi_CL <- C * exp(-R * u)
  
  return(psi_CL)
}

# Example usage:
# Define parameters
u <- 100              # initial reserve
theta <- 0.3          # safety loading factor
mu <- 1               # mean claim amount
M_prime_X_R <- 1.5    # derivative of moment generating function at R
R <- 0.05             # adjustment coefficient

# Calculate ruin probability
psi_CL <- cramer_lundberg(u, theta, mu, M_prime_X_R, R)
print(psi_CL)


####### 2 ######
# Exponential approximation function
exponential_approx <- function(u, mu, theta, mu_2, mu_3) {
  # Calculate the exponent part
  exponent <- -1 - (2 * mu * theta * u - mu_2) / sqrt((mu_2)^2 + (4/3) * theta * mu * mu_3)
  
  # Calculate the ruin probability
  psi_E <- exp(exponent)
  
  return(psi_E)
}

# Example usage:
# Define parameters
u <- 100        # initial reserve
mu <- 1         # mean claim amount
theta <- 0.3    # safety loading factor
mu_2 <- 2       # second moment of the claim size
mu_3 <- 3       # third moment of the claim size

# Calculate ruin probability
psi_E <- exponential_approx(u, mu, theta, mu_2, mu_3)
print(psi_E)

####### 3 ######
# Lundberg approximation function
lundberg_approx <- function(u, mu, theta, mu_2, mu_3) {
  # Calculate the coefficient
  coefficient <- 1 + (theta * u - (mu_2) / (2 * mu)) * (4 * theta * mu^2 * mu_3) / (3 * (mu_2)^3)
  
  # Calculate the ruin probability
  psi_L <- coefficient * exp(-2 * mu * theta * u / mu_2)
  
  return(psi_L)
}

# Example usage:
# Define parameters
u <- 100        # initial reserve
mu <- 1         # mean claim amount
theta <- 0.3    # safety loading factor
mu_2 <- 2       # second moment of the claim size
mu_3 <- 3       # third moment of the claim size

# Calculate ruin probability
psi_L <- lundberg_approx(u, mu, theta, mu_2, mu_3)
print(psi_L)


####### 4 ######
# Beekman–Bowers approximation function
beekman_bowers_approx <- function(u, mu, theta, mu_2, mu_3) {
  # Calculate alpha and beta for the gamma distribution
  alpha <- 1 + ((4 * mu * mu_3 / (3 * (mu_2)^2) - 1) * theta) / (1 + theta)
  beta <- (2 * mu * theta) / (mu_2 + (4 * mu * mu_3 / (3 * mu_2 - mu_2)) * theta)
  
  # Calculate G(u) using the gamma cumulative distribution function
  G_u <- pgamma(u, shape = alpha, rate = 1 / beta)
  
  # Calculate the ruin probability
  psi_BB <- (1 / (1 + theta)) * (1 - G_u)
  
  return(psi_BB)
}

# Example usage:
# Define parameters
u <- 100        # initial reserve
mu <- 1         # mean claim amount
theta <- 0.3    # safety loading factor
mu_2 <- 2       # second moment of the claim size
mu_3 <- 3       # third moment of the claim size

# Calculate ruin probability
psi_BB <- beekman_bowers_approx(u, mu, theta, mu_2, mu_3)
print(psi_BB)


####### 5 ######
# Rényi approximation function
renyi_approx <- function(u, mu, theta, mu_2) {
  # Calculate the ruin probability
  psi_R <- (1 / (1 + theta)) * exp(-2 * mu * theta * u / (mu_2 * (1 + theta)))
  
  return(psi_R)
}

# Example usage:
# Define parameters
u <- 100        # initial reserve
mu <- 1         # mean claim amount
theta <- 0.3    # safety loading factor
mu_2 <- 2       # second moment of the claim size

# Calculate ruin probability
psi_R <- renyi_approx(u, mu, theta, mu_2)
print(psi_R)

####### 6 ######
# De Vylder approximation function
de_vylder_approx <- Vectorize(function(u, theta, mu, mu_2, mu_3) {
  # Calculate parameters for the De Vylder approximation
  beta_bar <- 3 * mu_2 / mu_3
  lambda_bar <- (9 * mu * (mu_2)^3) / (2 * (mu_3)^2)
  theta_bar <- (2 * mu * mu_3) / (3 * (mu_2)^2) * theta
  
  # Calculate the De Vylder approximation
  result <- (1 / (1 + theta_bar)) * exp(-theta_bar * beta_bar * u / (1 + theta_bar))
  
  return(result)
})

# Example usage and plotting
u_vals <- seq(0, 100, by = 1)
theta <- 0.3
mu <- 1
mu_2 <- 2
mu_3 <- 3

# Compute De Vylder approximation over the range of u values
de_vylder_values <- de_vylder_approx(u_vals, theta, mu, mu_2, mu_3)

# Plot
plot(u_vals, de_vylder_values, type = "l", col = "blue",
     main = "De Vylder Approximation",
     xlab = "Initial Reserve (u)", ylab = "Ruin Probability")

####### 7 ######
# library(pracma)
# # Define the 3-moment gamma De Vylder approximation function
# three_moment_gamma_de_vylder <- function(u, theta, mu, mu_2, mu_3) {
#   # Step 1: Compute parameters
#   lambda_bar <- (2 * mu * mu_2^2) / (mu*(mu_3 + mu * mu_2))
#   theta_bar <- theta * mu(mu_3 + (mu_2 * mu)) / 2 * (mu_2)^2
#   mu_bar <- mu
#   mu_bar_2 <- (mu * (mu_3 + mu_2*mu)) / (2 * mu_2)
#   
#   # Step 2: Additional derived constants
#   R <- theta / (1 + theta)
#   alpha_bar <- mu_bar_2 / (mu_bar_2 - mu_bar^2)
#   beta_bar <- mu_bar / (mu_bar_2 - mu_bar^2)
#   
#   # Step 3: Integral calculation for I term
#   I_integrand <- function(x) {
#     exp(-x * (1 + beta_bar) / alpha_bar) *
#       ((x + 1) * (cos(pi * x) - sin(pi * x) / (pi * (x^2))))
#   }
#   
#   # Use pracma::integral to evaluate the integral from 0 to Inf
#   I <- tryCatch({
#     integral(I_integrand, 0, Inf)
#   }, error = function(e) {
#     warning("Integral failed to converge.")
#     return(NA)
#   })
#   
#   # Step 4: Approximation formula calculation
#   psi_3MGDV <- function(u) {
#     term1 <- theta_bar * (1 - R / alpha_bar) * exp(-beta_bar * u / alpha_bar)
#     term2 <- (1 + (1 + theta_bar) * R - (1 + theta_bar) * (1 - R / alpha_bar))
#     term3 <- (alpha_bar * theta_bar * sin(pi * alpha_bar)) / pi
#     term1 / term2 + term3 * I
#   }
#   
#   # Return the approximation result for input u
#   psi_3MGDV(u)
# }
# 
# # Example usage:
# # Define parameters (theta, mu, mu_2, mu_3) - replace with actual values
# theta <- 0.3    # Safety loading
# mu <- 1         # First moment (mean of claims)
# mu_2 <- 2       # Second moment of claims
# mu_3 <- 3       # Third moment of claims
# 
# # Calculate approximation for a given u
# u <- 10         # Initial reserve level
# result <- three_moment_gamma_de_vylder(u, theta, mu, mu_2, mu_3)
# print(result)
####### 8 ######
# Heavy Traffic approximation function
heavy_traffic_approx <- function(u, theta, mu_2) {
  # Calculate the ruin probability using the Heavy Traffic approximation
  psi_HT <- exp(-2 * theta * u / mu_2)
  
  return(psi_HT)
}

# Example usage:
# Define parameters
u <- 100        # initial reserve
theta <- 0.3    # safety loading factor
mu_2 <- 2       # second moment of the claim size

# Calculate ruin probability
psi_HT <- heavy_traffic_approx(u, theta, mu_2)
print(psi_HT)

####### 9 ######
library(pracma)
# Define the Light Traffic Approximation function
light_traffic_approximation <- function(u, theta, mu, survival_function) {
  # Define the integrand for the survival function
  integrand <- function(x) {
    survival_function(x)
  }
  
  # Perform the integration from u to infinity
  integral_value <- tryCatch({
    integral(integrand, u, Inf)
  }, error = function(e) {
    warning("Integral failed to converge.")
    return(NA)
  })
  
  # Calculate the approximation
  psi_LT <- (1 / ((1 + theta) * mu)) * integral_value
  
  return(psi_LT)
}

# Example survival function (exponential distribution with mean mu)
# Adjust this survival function based on the actual claim size distribution
survival_function_example <- function(x) {
  exp(-x / mu)  # For exponential distribution with mean mu
}

# Define parameters
theta <- 0.3   # Safety loading
mu <- 1        # Mean claim size
u <- 10        # Initial reserve level

# Calculate Light Traffic Approximation
result <- light_traffic_approximation(u, theta, mu, survival_function_example)
print(result)
####### 10 ######
HLT_Function = function(u , theta , mu , survival_function_example , mu_2){
  result = theta / (1 + theta ) * (light_traffic_approximation(u, theta, mu, survival_function_example)) *
    (theta * mu / (1+theta) ) + (1/(1+theta)^2) *  heavy_traffic_approx(u, theta, mu_2)
return(result)
}
#example 
theta <- 0.3   # Safety loading
mu <- 1        # Mean claim size
u <- 10 
mu_2 <- 2       # second moment of the claim size
HLT_Function(u , theta , mu , survival_function_example , mu_2)
