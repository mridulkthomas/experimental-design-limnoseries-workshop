#### Workshop on How to Design Better Experiments — Script 05
# Author: Mridul K. Thomas (ORCID: 0000-0002-5089-5610)
# Repo: https://github.com/mridulkthomas/experimental-design-limnoseries-workshop
# License: MIT (code); CC BY 4.0 (teaching materials)
####


##### Simulation-based precision analysis (linear regression)
# Assumptions: normal errors, homoscedasticity.

# Load libraries
library(ggplot2)
theme_set(theme_minimal(base_size = 12))

# Set common simulation parameters
intercept <- 3
slope <- 4
sigma <- 4       # the error standard deviation
x_min <- 1       # minimum X value
x_max <- 10      # maximum X value
n <- 10          # total sample size (pick an even number)

# Define experimental designs
uniform_design <- seq(x_min, x_max, length.out = n)
optimal_design <- c(rep(x_min, n/2), rep(x_max, n/2))

# Visualise experimental designs
ggplot(as.data.frame(uniform_design), aes(uniform_design)) + 
  geom_histogram(bins = 50) + 
  labs(title = 'Uniform design', x = 'X')

ggplot(as.data.frame(optimal_design), aes(optimal_design)) + 
  geom_histogram(bins = 50) + 
  labs(title = 'Optimal design', x = 'X')



### 1. Spread of estimates at a fixed sample size

# Set additional simulation parameters
set.seed(195)
nsims <- 2000

# Create empty vectors to save the parameter estimates
intercept_uniform <- slope_uniform <- intercept_optimal <- slope_optimal <- rep(NA, nsims)


# Repeat simulations many times
for(i in 1:nsims){
  
  # Track how many simulations are done
  print(i)
  
  # Generate data with defined intercept & slope and random error
  y_optimal <- intercept + slope * optimal_design + rnorm(n, 0, sigma)
  y_uniform <- intercept + slope * uniform_design + rnorm(n, 0, sigma)

  ## uniform design
  # Fit model
  mod_uniform <- lm(y_uniform ~ uniform_design)
  
  # Save parameters
  intercept_uniform[i]   <- coef(mod_uniform)[1]
  slope_uniform[i] <- coef(mod_uniform)[2]
  
  # optimal (extremes) design
  # Fit model
  mod_optimal <- lm(y_optimal ~ optimal_design)
  
  # Save parameters
  intercept_optimal[i]   <- coef(mod_optimal)[1]
  slope_optimal[i] <- coef(mod_optimal)[2]
}

# Create data frames with all estimates
df_intercept <- data.frame(
  estimate = c(intercept_uniform, intercept_optimal),
  design   = rep(c("uniform", "optimal"), each = nsims)
)

df_slope <- data.frame(
  estimate = c(slope_uniform, slope_optimal),
  design   = rep(c("uniform", "optimal"), each = nsims)
)


# Plot distributions of estimates
ggplot(df_intercept, aes(estimate, fill = design)) +
  geom_histogram(aes(fill = design), 
                 alpha = 0.5, 
                 binwidth = 0.3, 
                 position="identity") + 
  geom_vline(xintercept = intercept) +
  labs(title = paste("Intercept estimates (sample size in 1 simulation =", n, ")"),
       x = "Estimated intercept", y = "Count")


ggplot(df_slope, aes(estimate, fill = design)) +
  geom_histogram(aes(fill = design), 
                 alpha = 0.5, 
                 binwidth = 0.05, 
                 position="identity") + 
  geom_vline(xintercept = slope) +
  labs(title = paste("Slope estimates (sample size in 1 simulation =", n, ")"),
       x = "Estimated slope", y = "Count")


### 2. How the slope standard error changes with sample size

# Set additional simulation parameters
set.seed(195)
n_seq <- seq(8, 80, by = 8)    # use even numbers
nsims <- 500

# Create empty vectors to save the mean parameter estimates
mean_se_uniform <- mean_se_optimal <- rep(NA, length(n_seq))

# Repeat simulations many times, at different sample sizes
for (i in 1:length(n_seq)) {
  
  n <- n_seq[i]
  print(paste("n =", n))
  uniform_design <- seq(x_min, x_max, length.out = n)
  optimal_design <- c(rep(x_min, n/2), rep(x_max, n/2))
  
  # Create empty vectors to save the parameter estimates from individual simulations
  std_errors_uniform <- std_errors_optimal <- rep(NA, nsims)

  # For each sample size, simulate many times and calculate the precision (standard error)
  for(j in 1:nsims){
    # Generate data with defined intercept & slope and random error
    y_uniform <- intercept + slope * uniform_design + rnorm(n, 0, sigma)
    y_optimal <- intercept + slope * optimal_design + rnorm(n, 0, sigma)

    # Save standard error for each simulation
    std_errors_uniform[j] <- summary(lm(y_uniform ~ uniform_design))$coefficients["uniform_design", "Std. Error"]
    std_errors_optimal[j] <- summary(lm(y_optimal ~ optimal_design))$coefficients["optimal_design", "Std. Error"]
  }
  
  # Save mean standard errors at each sample size
  mean_se_uniform[i] <- mean(std_errors_uniform)
  mean_se_optimal[i] <- mean(std_errors_optimal)
}

# Create data frame with estimates
df_se <- data.frame(
  n = rep(n_seq, 2),
  mean_se = c(mean_se_uniform, mean_se_optimal),
  design = rep(c("uniform", "optimal"), each = length(n_seq))
)

# Plot mean SE of slope vs n for each design
ggplot(df_se, aes(n, mean_se, linetype = design)) +
  geom_line(linewidth = 1.2) +
  labs(title = "With a better design, you can get the same precision with a smaller experiment",
       x = "Sample size (n)", y = "Mean standard error of slope")

