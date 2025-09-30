#### Workshop on How to Design Better Experiments — Script 06
# Author: Mridul K. Thomas (ORCID: 0000-0002-5089-5610)
# Repo: https://github.com/mridulkthomas/experimental-design-limnoseries-workshop
# License: MIT (code); CC BY 4.0 (teaching materials)
####


##### Simulation-based precision analysis (nonlinear regression, Monod function)
# Assumptions: normal errors, homoscedasticity.

# Load libraries
library(ggplot2)
theme_set(theme_minimal(base_size = 12))
library(dplyr)

# Set seed for repeatability
set.seed(195)

# Set 'true' parameters
mu_max_true <- 2
ks_true     <- 6
sigma       <- 0.1   # error standard deviation

# Define experimental designs
uniform_design <- seq(0, 15, 1)
optimal_design <- rep(c(4.2857, 15), each = 8)   # equal replicates at low and high

# Visualise experimental designs
ggplot(as.data.frame(uniform_design), aes(uniform_design)) + 
  geom_histogram(bins = 68)+ 
  xlim(-1, 16) + 
  labs(title = 'Uniform design', x = 'X')

ggplot(as.data.frame(optimal_design), aes(optimal_design)) + 
  geom_histogram(bins = 50) + 
  xlim(-1, 16) + 
  labs(title = 'Optimal design', x = 'X')

# Number of simulations
nsims <- 5000

# Create empty vectors to save the parameter estimates
mu_max_uniform_design <- ks_uniform <- mu_max_optimal_design <- ks_extreme <- rep(NA, nsims)

# Simulation loop
for(i in 1:nsims){
  # simulate responses
  y_uniform <- (mu_max_true * uniform_design / (ks_true + uniform_design)) + 
    rnorm(length(uniform_design), 0, sigma)
  y_extreme <- (mu_max_true * optimal_design / (ks_true + optimal_design)) + 
    rnorm(length(optimal_design), 0, sigma)
  
  # fit models
  mod_uniform <- try(
    nls(y_uniform ~ mu_max * uniform_design / (ks + uniform_design),
        start = list(mu_max = max(y_uniform), ks = mean(uniform_design)),
        control = nls.control(maxiter = 1000))
  )
  
  if (!inherits(mod_uniform, "try-error")){
    mu_max_uniform_design[i] <- coef(mod_uniform)[["mu_max"]]
    ks_uniform[i]     <- coef(mod_uniform)[["ks"]]
  }
  
  mod_extreme <- try(
    nls(y_extreme ~ mu_max * optimal_design / (ks + optimal_design),
        start = list(mu_max = max(y_extreme), ks = mean(optimal_design)),
        control = nls.control(maxiter = 1000)))
  
  if (!inherits(mod_extreme, "try-error")){
    mu_max_optimal_design[i] <- coef(mod_extreme)[["mu_max"]]
    ks_extreme[i]     <- coef(mod_extreme)[["ks"]]
  }
  
  if (i %% 100 == 0) print(paste("sim", i, "of", nsims))
}

# Summaries (precision and coverage)
sd(mu_max_uniform_design, na.rm = TRUE)
sd(mu_max_optimal_design, na.rm = TRUE)
sd(ks_uniform, na.rm = TRUE)
sd(ks_extreme, na.rm = TRUE)
quantile(mu_max_uniform_design, probs = c(0.025, 0.975), na.rm = TRUE)
quantile(mu_max_optimal_design, probs = c(0.025, 0.975), na.rm = TRUE)
quantile(ks_uniform, probs = c(0.025, 0.975), na.rm = TRUE)
quantile(ks_extreme, probs = c(0.025, 0.975), na.rm = TRUE)

# Create data frame with all estimates
dat <- data.frame(parameter = c(rep("mu_max", 2*nsims),
                                rep("ks", 2*nsims)),
                  design = rep(c(rep("uniform", nsims),
                                 rep("extreme", nsims)), 2),
                  value = c(mu_max_uniform_design, mu_max_optimal_design,
                            ks_uniform, ks_extreme))

# Plot distributions of estimates
dat %>%
  filter(parameter == "mu_max") %>%
  ggplot(aes(value, fill = design)) +
  geom_histogram(alpha = 0.5, binwidth = 0.05, position = "identity") +
  geom_vline(xintercept = mu_max_true) +
  labs(title = "Sampling distributions of mu_max estimates",
       x = "Estimated mu_max", y = "Count")

dat %>%
  filter(parameter == "ks") %>%
  ggplot(aes(value, fill = design)) +
  geom_histogram(alpha = 0.5, binwidth = 0.2, position = "identity") +
  geom_vline(xintercept = ks_true) +
  labs(title = "Sampling distributions of Ks estimates",
       x = "Estimated Ks", y = "Count")
