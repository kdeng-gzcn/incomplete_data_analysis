# Load the data from the file
load("dataex3.Rdata")

# Set the known value of sigma^2
sigma_sq <- 1.5^2

# Define the log-likelihood function for normal distribution with left-censored data
log_likelihood <- function(mu, x, r, sigma_sq) {
  phi <- function(x, mu, sigma_sq) {
    return(dnorm(x, mean = mu, sd = sqrt(sigma_sq)))
  }
  
  Phi <- function(x, mu, sigma_sq) {
    return(pnorm(x, mean = mu, sd = sqrt(sigma_sq)))
  }
  
  # Log-likelihood expression for left-censored data
  logL <- sum(r * log(phi(x, mu, sigma_sq)) + (1 - r) * log(Phi(x, mu, sigma_sq)))
  return(-logL)  # We negate because optim() performs minimization
}

# Set initial guess for mu
initial_mu <- mean(dataex3[, 1])

# Perform the optimization to maximize the log-likelihood
result <- optimize(f = log_likelihood, interval = c(-10, 10), x = dataex3[, 1], r = dataex3[, 2], sigma_sq = sigma_sq)

# Display the MLE of mu
cat("The MLE of mu is:", result$minimum, "\n")
