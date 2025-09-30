#### Workshop on How to Design Better Experiments — Script 07
# Author: Mridul K. Thomas (ORCID: 0000-0002-5089-5610)
# Repo: https://github.com/mridulkthomas/experimental-design-limnoseries-workshop
# License: MIT (code); CC BY 4.0 (teaching materials)
####


##### EXERCISE: Using simulations, find how precision changes with design (nonlinear regression, Monod function)
# Assumptions: normal errors, homoscedasticity.

# Load libraries
library(ggplot2)
theme_set(theme_minimal(base_size = 12))
library(dplyr)

############ ONLY MODIFY THIS SECTION ############
### Define experimental designs here
# CONDITIONS: 
# 1. You must have 16 values for each design
# 2. You can repeat values and choose values with decimal points. 
# 3. But the minimum must be 0 and the maximum 15
# CODING: Enter 16 numbers inside c() with commas between e.g. c(1.5, 2.6, 3, 5.2)
my_design1 <- c()       # Enter your first design 
my_design2 <- c()       # Enter your second design 
##################################################

####### AFTER THIS, JUST RUN THE CODE

# Set seed for repeatability
set.seed(195)

# Set 'true' parameters
mu_max_true <- 2    # maximum growth rate
ks_true     <- 6    # half-saturation constant
sigma       <- 0.1  # error standard deviation


# Visualise experimental designs
ggplot(as.data.frame(my_design1), aes(my_design1)) + 
  geom_histogram(bins = 50)+ 
  xlim(-2, 17) + 
  labs(title = 'My design 1', x = 'X')

ggplot(as.data.frame(my_design2), aes(my_design2)) + 
  geom_histogram(bins = 50) + 
  xlim(-2, 17) + 
  labs(title = 'My design 2', x = 'X')

# Number of simulations
nsims <- 5000

# Create empty vectors to save the parameter estimates
mu_max_my_design1 <- ks_my_design1 <- mu_max_my_design2 <- ks_my_design2 <- rep(NA, nsims)

# Simulation loop
for(i in 1:nsims){
  # simulate responses
  y_my_design1 <- (mu_max_true * my_design1 / (ks_true + my_design1)) + 
    rnorm(length(my_design1), 0, sigma)
  y_my_design2 <- (mu_max_true * my_design2 / (ks_true + my_design2)) + 
    rnorm(length(my_design2), 0, sigma)
  
  # fit models
  mod_uniform <- try(
    nls(y_my_design1 ~ mu_max * my_design1 / (ks + my_design1),
        start = list(mu_max = max(y_my_design1), ks = mean(my_design1)),
        control = nls.control(maxiter = 1000))
  )
  
  if (!inherits(mod_uniform, "try-error")){
    mu_max_my_design1[i] <- coef(mod_uniform)[["mu_max"]]
    ks_my_design1[i]     <- coef(mod_uniform)[["ks"]]
  }
  
  mod_extreme <- try(
    nls(y_my_design2 ~ mu_max * my_design2 / (ks + my_design2),
        start = list(mu_max = max(y_my_design2), ks = mean(my_design2)),
        control = nls.control(maxiter = 1000)))
  
  if (!inherits(mod_extreme, "try-error")){
    mu_max_my_design2[i] <- coef(mod_extreme)[["mu_max"]]
    ks_my_design2[i]     <- coef(mod_extreme)[["ks"]]
  }
  
  if (i %% 100 == 0) print(paste("sim", i, "of", nsims))
}

## Examine summaries of parameter distributions
# sd() is the standard deviation
# Here it tells you the standard error, which is our measure of precision
# Smaller value is better
sd(mu_max_my_design1, na.rm = TRUE)
sd(mu_max_my_design2, na.rm = TRUE)
sd(ks_my_design1, na.rm = TRUE)
sd(ks_my_design2, na.rm = TRUE)

# Create data frame with all estimates
dat <- data.frame(parameter = c(rep("mu_max", 2*nsims),
                                rep("ks", 2*nsims)),
                  design = rep(c(rep("uniform", nsims),
                                 rep("extreme", nsims)), 2),
                  value = c(mu_max_my_design1, mu_max_my_design2,
                            ks_my_design1, ks_my_design2))

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

###### Which design is better? Why?