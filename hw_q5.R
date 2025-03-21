# Load the data
load("dataex5.Rdata")

# Identify the rows where Y is observed (non-missing) and where Y is missing
observed_data <- dataex5[!is.na(dataex5$Y), ]
missing_data <- dataex5[is.na(dataex5$Y), ]

# Define the logistic regression log-likelihood function
log_likelihood <- function(beta, x_obs, y_obs, x_mis, y_mis) {
  # Logistic function
  p_obs <- exp(beta[1] + beta[2] * x_obs) / (1 + exp(beta[1] + beta[2] * x_obs))
  p_mis <- exp(beta[1] + beta[2] * x_mis) / (1 + exp(beta[1] + beta[2] * x_mis))
  
  # Log-likelihood for observed and imputed missing data
  logL_obs <- sum(y_obs * log(p_obs) + (1 - y_obs) * log(1 - p_obs))
  logL_mis <- sum(log(p_mis) * y_mis + log(1 - p_mis) * (1 - y_mis))
  
  return(-(logL_obs + logL_mis))  # Return the negative log-likelihood
}

# E-step: Impute missing Y values with their expected values based on current beta
impute_missing <- function(beta, x_mis) {
  # Logistic function for the expected value of Y for missing data
  p_mis <- exp(beta[1] + beta[2] * x_mis) / (1 + exp(beta[1] + beta[2] * x_mis))
  return(p_mis)  # Return the expected Y values for missing data
}

# Initial guesses for beta_0 and beta_1
beta_initial <- c(0, 0)

# Extract the x and y values for observed and missing data
x_obs <- observed_data$X
y_obs <- observed_data$Y
x_mis <- missing_data$X

# Set the number of iterations for the EM algorithm
max_iter <- 100
tolerance <- 1e-6  # Tolerance for convergence

# Run the EM algorithm
for (iter in 1:max_iter) {
  
  # Impute missing values with their expected values based on current beta
  y_mis <- impute_missing(beta_initial, x_mis)
  
  # E-step & M-step: Maximize the log-likelihood function using optim()
  result <- optim(par = beta_initial, fn = log_likelihood, x_obs = x_obs, y_obs = y_obs, x_mis = x_mis, y_mis = y_mis, method = "BFGS")
  
  # Update beta estimates
  beta_new <- result$par
  
  # Check for convergence (if the change in beta is small enough, stop)
  if (sum(abs(beta_new - beta_initial)) < tolerance) {
    cat("Convergence reached at iteration", iter, "\n")
    break
  }
  
  # Update beta for the next iteration
  beta_initial <- beta_new
}

# Final maximum likelihood estimates for beta_0 and beta_1
cat("Maximum Likelihood Estimates:\n")
cat("Beta_0:", beta_new[1], "\n")
cat("Beta_1:", beta_new[2], "\n")
