#### Workshop on How to Design Better Experiments — Script 08
# Author: Mridul K. Thomas (ORCID: 0000-0002-5089-5610)
# Repo: https://github.com/mridulkthomas/experimental-design-limnoseries-workshop
# License: MIT (code); CC BY 4.0 (teaching materials)
####


##### EXERCISE: Using simulations, find how precision changes with design (nonlinear regression, Eilers–Peeters function)
# Assumptions: normal errors, homoscedasticity.

# Load libraries
library(ggplot2)
theme_set(theme_minimal(base_size = 12))
library(dplyr)

############ ONLY MODIFY THIS SECTION ############
### Define experimental designs here
# CONDITIONS:
# 1. You must have 10 values for each design
# 2. You can repeat values and choose values with decimal points.
# 3. But the minimum must be 0 and the maximum 1000
# CODING: Enter 10 numbers inside c() with commas between e.g. c(1, 2, 3)
my_design1 <- c()       # Enter your first design 
my_design2 <- c()       # Enter your second design 
##################################################

####### AFTER THIS, JUST RUN THE CODE

# Set seed for repeatability
set.seed(195)

# Set 'true' parameters for the Eilers–Peeters function
mu_max_true <- 2     # maximum growth rate
alpha_true  <- 0.02   # initial slope at low light
i_opt_true  <- 300   # optimum irradiance
sigma       <- 0.15  # error standard deviation

# Define Eilers–Peeters function
eilers_peeters <- function(light, mu_max, alpha, i_opt){
  (mu_max * light) /
    (((mu_max / (alpha * i_opt^2)) * (light^2)) +
       ((1 - ((2 * mu_max)/(alpha * i_opt))) * light) +
       (mu_max / alpha))
}

# Visualise experimental designs
ggplot(as.data.frame(my_design1), aes(my_design1)) +
  geom_histogram(bins = 50) +
  xlim(-20, 1050) +
  labs(title = "My design 1", x = "Light (I)")

ggplot(as.data.frame(my_design2), aes(my_design2)) +
  geom_histogram(bins = 50) +
  xlim(-20, 1050) +
  labs(title = "My design 2", x = "Light (I)")

# Number of simulations
nsims <- 1000

# Create empty vectors to save parameter estimates
mu_max_d1 <- alpha_d1 <- i_opt_d1 <- rep(NA, nsims)
mu_max_d2 <- alpha_d2 <- i_opt_d2 <- rep(NA, nsims)

# Simulation loop
for(i in 1:nsims){
  # Simulate responses
  y1 <- eilers_peeters(my_design1, mu_max_true, alpha_true, i_opt_true) +
    rnorm(length(my_design1), 0, sigma)
  y2 <- eilers_peeters(my_design2, mu_max_true, alpha_true, i_opt_true) +
    rnorm(length(my_design2), 0, sigma)
  
  # Fit models
  mod1 <- try(
    nls(y1 ~ (mu_max * my_design1) /
          (((mu_max / (alpha * i_opt^2)) * (my_design1^2)) +
             ((1 - ((2 * mu_max)/(alpha * i_opt))) * my_design1) +
             (mu_max / alpha)),
        start = list(mu_max = max(y1),
                     alpha = 0.02,
                     i_opt = mean(my_design1)),
        control = nls.control(maxiter = 1000)),
    silent = TRUE
  )
  
  if (!inherits(mod1, "try-error")){
    co <- coef(mod1)
    mu_max_d1[i] <- co[["mu_max"]]
    alpha_d1[i]  <- co[["alpha"]]
    i_opt_d1[i]  <- co[["i_opt"]]
  }
  
  mod2 <- try(
    nls(y2 ~ (mu_max * my_design2) /
          (((mu_max / (alpha * i_opt^2)) * (my_design2^2)) +
             ((1 - ((2 * mu_max)/(alpha * i_opt))) * my_design2) +
             (mu_max / alpha)),
        start = list(mu_max = max(y2),
                     alpha = 0.02,
                     i_opt = mean(my_design2)),
        control = nls.control(maxiter = 1000)),
    silent = TRUE
  )
  
  if (!inherits(mod2, "try-error")){
    co <- coef(mod2)
    mu_max_d2[i] <- co[["mu_max"]]
    alpha_d2[i]  <- co[["alpha"]]
    i_opt_d2[i]  <- co[["i_opt"]]
  }
  
  if (i %% 100 == 0) print(paste("sim", i, "of", nsims))
}

## Examine summaries of parameter distributions
# sd() is the standard deviation
# Here it tells you the standard error, which is our measure of precision
# Smaller value is better
sd(mu_max_d1, na.rm = TRUE)
sd(mu_max_d2, na.rm = TRUE)
sd(alpha_d1,  na.rm = TRUE)
sd(alpha_d2,  na.rm = TRUE)
sd(i_opt_d1,  na.rm = TRUE)
sd(i_opt_d2,  na.rm = TRUE)

# Create data frame with all estimates
dat <- data.frame(
  parameter = c(rep("mu_max", 2*nsims),
                rep("alpha",  2*nsims),
                rep("i_opt",  2*nsims)),
  design = rep(c(rep("design1", nsims),
                 rep("design2", nsims)), 3),
  value = c(mu_max_d1, mu_max_d2,
            alpha_d1,  alpha_d2,
            i_opt_d1,  i_opt_d2)
)

# Plot distributions of estimates
dat %>%
  filter(parameter == "mu_max") %>%
  ggplot(aes(value, fill = design)) +
  geom_histogram(alpha = 0.5, binwidth = 0.01, position = "identity") +
  geom_vline(xintercept = mu_max_true) +
  labs(title = "Sampling distributions of mu_max estimates",
       x = "Estimated mu_max", y = "Count")

dat %>%
  filter(parameter == "alpha") %>%
  ggplot(aes(value, fill = design)) +
  geom_histogram(alpha = 0.5, binwidth = 0.001, position = "identity") +
  geom_vline(xintercept = alpha_true) +
  labs(title = "Sampling distributions of alpha estimates",
       x = "Estimated alpha", y = "Count")

dat %>%
  filter(parameter == "i_opt") %>%
  ggplot(aes(value, fill = design)) +
  geom_histogram(alpha = 0.5, binwidth = 4, position = "identity") +
  geom_vline(xintercept = i_opt_true) +
  labs(title = "Sampling distributions of i_opt estimates",
       x = "Estimated i_opt", y = "Count")

###### Which design is better? Why?
