#### Workshop on How to Design Better Experiments — Script 09
# Author: Ravi Ranjan (ORCID: 0000-0001-8644-9316)
# Repo: https://github.com/mridulkthomas/experimental-design-limnoseries-workshop
# License: MIT (code); CC BY 4.0 (teaching materials)
####


# Design of Experiments: Linear Regression with Maximum Likelihood
# Teaching script for optimal experimental design concepts

# Install required libraries
# install.packages("ellipse")
# install.packages("rootSolve")

# Load required libraries
library(ellipse)
library(rootSolve)

# Set seed for reproducibility
set.seed(123)

# ===== MAXIMUM LIKELIHOOD FUNCTION =====
# Used for fitting a line later - details can be ignored
fit_linear_mle <- function(x, y) {
  # Define negative log-likelihood function
  neg_log_likelihood <- function(params) {
    beta_0 <- params[1]
    beta_1 <- params[2]
    sigma <- params[3]
    
    # Predicted values
    y_pred <- beta_0 + beta_1 * x
    
    # Negative log-likelihood for normal distribution
    -sum(dnorm(y, mean = y_pred, sd = sigma, log = TRUE))
  }
  
  # Starting values
  start_params <- c(2, 2, 2)  # beta_0, beta_1, sigma
  
  # Optimize to find maximum likelihood estimates
  mle_result <- optim(start_params, neg_log_likelihood, method = "BFGS", hessian = TRUE)
  
  # Extract results
  beta_0_hat <- mle_result$par[1]
  beta_1_hat <- mle_result$par[2]
  sigma_hat <- mle_result$par[3]
  
  return(list(
    beta_0 = beta_0_hat,
    beta_1 = beta_1_hat,
    sigma = sigma_hat
  ))
}

# =====  CONFIDENCE ELLIPSE FUNCTION =====
# Used to make ellipses later
theoretical_vcov <- function(x, sigma) {
  X <- cbind(1, x)  # Design matrix
  XtX_inv <- solve(t(X) %*% X)  # (X'X)^(-1)
  return(sigma^2 * XtX_inv)  # Theoretical covariance matrix
}

# ===== SIMULATION SETUP =====
# True model parameters
beta_0_true <- 3  # intercept
beta_1_true <- 4  # slope
sigma_true <- 4   # error standard deviation
n <- 10           # sample size

# ===== PLOT 1: UNIFORM DESIGN WITH SIMULATION =====
# Uniform design
x_uniform <- seq(1, 10, length.out = n)

# Calculate confidence  ellipse
vcov_uniform <- theoretical_vcov(x_uniform, sigma_true)
ellipse_uniform <- ellipse(vcov_uniform, centre = c(beta_0_true, beta_1_true), level = 0.95)

# Simulate 1000 experiments
n_sims <- 1000
estimates <- matrix(NA, n_sims, 2)

for(i in 1:n_sims) {
  y_sim <- beta_0_true + beta_1_true * x_uniform + rnorm(n, 0, sigma_true)
  fit_sim <- fit_linear_mle(x_uniform, y_sim)
  estimates[i,] <- c(fit_sim$beta_0, fit_sim$beta_1)
}

# Create plot - 1000 simulation fits and a confidence ellipse
plot(beta_0_true, beta_1_true, 
     xlim = range(ellipse_uniform[, 1]) + c(-1, 1),
     ylim = range(ellipse_uniform[, 2]) + c(-1, 1),
     xlab = "Intercept Parameter (beta_0)", 
     ylab = "Slope Parameter (beta_1)",
     main = "95% Confidence Ellipse",
     pch = 4, cex = 1.2, lwd = 2, col = "black")

# Add confidence ellipse
lines(ellipse_uniform[, 1], ellipse_uniform[, 2], col = "blue", lwd = 2)

# Add simulated estimates
points(estimates[,1], estimates[,2], pch = 16, cex = 0.4, col = "gray")

# Add legend
legend("bottomleft", 
       legend = c("True parameters", "Theoretical 95% ellipse", "100 simulated estimates"),
       col = c("black", "blue", "gray"),
       lwd = c(3, 2, NA),
       pch = c(4, NA, 16),
       cex = 0.7,
       bg = "white")

# ===== PLOT 2: THREE DESIGN COMPARISON =====
# We only show the first two designs in the workshop
# But feel free to tinker with the number and type of designs here
# Define four designs
x1 <- seq(1, 10, length.out = n)  # Equally spaced
x2 <- rep(c(1, 10), each = n/2)   # Optimal (endpoints)
x3 <- rnorm(n, mean = 5.5, sd = 0.5)  # Poor (clustered)

# Calculate theoretical covariance matrices
vcov1 <- theoretical_vcov(x1, sigma_true)
vcov2 <- theoretical_vcov(x2, sigma_true)
vcov3 <- theoretical_vcov(x3, sigma_true)


# Create theoretical ellipses
ellipse1 <- ellipse(vcov1, centre = c(beta_0_true, beta_1_true), level = 0.95)
ellipse2 <- ellipse(vcov2, centre = c(beta_0_true, beta_1_true), level = 0.95)
ellipse3 <- ellipse(vcov3, centre = c(beta_0_true, beta_1_true), level = 0.95)

# Determine plot limits
all_ellipses <- rbind(ellipse1, ellipse2, ellipse3)
xlim_range <- range(all_ellipses[, 1]) + c(-1, 1)
ylim_range <- range(all_ellipses[, 2]) + c(-1, 1)

# Create plot 2
plot(beta_0_true, beta_1_true, 
     xlim = xlim_range,
     ylim = ylim_range,
     xlab = "Intercept Parameter (beta_0)", 
     ylab = "Slope Parameter (beta_1)",
     main = "95% Confidence Ellipses",
     pch = 4, cex = 1.2, lwd = 2, col = "black")

# Add theoretical confidence ellipses
lines(ellipse1[, 1], ellipse1[, 2], col = "blue", lwd = 2)
lines(ellipse2[, 1], ellipse2[, 2], col = "red", lwd = 2)
lines(ellipse3[, 1], ellipse3[, 2], col = "green", lwd = 2)

# Add legend
legend("bottomleft", 
       legend = c("Equally spaced", "Optimal (extremes)", "Poor (clustered)", "True parameters"),
       col = c("blue", "red", "green", "black"),
       lwd = c(2, 2, 2, 2),
       pch = c(NA, NA, NA, 4),
       cex = 0.7,
       bg = "white")

# The poor design's ellipse is really big here, so feel free to remove it to see 
# how the other two compare

# Also try out a replicated regression design. How do you expect it to do?
