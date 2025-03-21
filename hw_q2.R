library(mice)
load("dataex2.Rdata")

# Assuming dataex2 is a 3D array with shape (100, 2, 100)
# Each slice dataex2[, , i] is the ith dataset, where i ranges from 1 to 100
set.seed(1)

# Initialize parameters
true_beta <- 3  # True value of beta1
num_datasets <- dim(dataex2)[3]  # Number of datasets (100 datasets)
coverage_stochastic <- numeric(num_datasets)

# Loop through each dataset in dataex2
for (i in 1:num_datasets) {
  # Extract the ith dataset (100 rows, 2 columns)
  data <- dataex2[, , i]
  
  # Perform stochastic regression imputation using mice (m=20 imputed datasets)
  imp <- mice(data, m=20, method="norm", seed=1)
  
  # Use with() to apply the regression model on each imputed dataset
  fit <- with(imp, lm(Y ~ X))  # Apply lm(Y ~ X) for each imputed dataset
  
  # Pool the results of the regression
  pooled_fit <- pool(fit)  # Pool the regression results
  
  # Extract the confidence intervals for beta1
  summary_pooled <- summary(pooled_fit, conf.int = TRUE)
  ci_lower <- summary_pooled[2, "2.5 %"]  # Lower bound of 95% CI for beta1
  ci_upper <- summary_pooled[2, "97.5 %"]  # Upper bound of 95% CI for beta1
  
  # Check if the true beta1 lies within the confidence interval
  coverage_stochastic[i] <- ifelse(true_beta >= ci_lower & true_beta <= ci_upper, 1, 0)
}

# Calculate the empirical coverage probability for stochastic regression imputation
empirical_coverage_stochastic <- mean(coverage_stochastic)
print(paste("Empirical coverage probability (Stochastic regression imputation):", empirical_coverage_stochastic))

library(boot)
coverage_bootstrap <- numeric(num_datasets)

# Loop through each dataset in dataex2
for (i in 1:num_datasets) {
  # Extract the ith dataset (100 rows, 2 columns)
  data <- dataex2[, , i]
  
  # Perform boostrap regression imputation using mice (m=20 imputed datasets)
  imp <- mice(data, m=20, method="norm.boot", seed=1)
  
  # Use with() to apply the regression model on each imputed dataset
  fit <- with(imp, lm(Y ~ X))  # Apply lm(Y ~ X) for each imputed dataset
  
  # Pool the results of the regression
  pooled_fit <- pool(fit)  # Pool the regression results
  
  # Extract the confidence intervals for beta1
  summary_pooled <- summary(pooled_fit, conf.int = TRUE)
  ci_lower <- summary_pooled[2, "2.5 %"]  # Lower bound of 95% CI for beta1
  ci_upper <- summary_pooled[2, "97.5 %"]  # Upper bound of 95% CI for beta1
  
  # Check if the true beta1 lies within the confidence interval
  coverage_bootstrap[i] <- ifelse(true_beta >= ci_lower & true_beta <= ci_upper, 1, 0)
}

# Calculate the empirical coverage probability for bootstrap-based imputation
empirical_coverage_bootstrap <- mean(coverage_bootstrap)
print(paste("Empirical coverage probability (Bootstrap-based imputation):", empirical_coverage_bootstrap))
