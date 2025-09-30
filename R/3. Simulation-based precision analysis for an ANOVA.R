#### Workshop on How to Design Better Experiments — Script 03
# Author: Mridul K. Thomas (ORCID: 0000-0002-5089-5610)
# Repo: https://github.com/mridulkthomas/experimental-design-limnoseries-workshop
# License: MIT (code); CC BY 4.0 (teaching materials)
####


##### Simulation-based precision analysis (ANOVA with 2 groups)
# Assumptions: balanced groups, equal variances, two-sided alpha, normal errors.

# Load libraries
library(ggplot2)
theme_set(theme_minimal(base_size = 12))

# Set common simulation parameters
delta <- 0.5       # Difference between groups
sigma <- 1         # Within-group standard deviation 


### 1. Spread of estimates at a fixed sample size

# Set additional simulation parameters
set.seed(29)
n_per_group <- 30
nsims <- 2000
estimates <- rep(NA, nsims)

# Repeat simulations many times
for(i in 1:nsims){
  dat <- data.frame(group = rep(c(0,1), each = n_per_group))
  dat$y <- 10 + delta*dat$group + rnorm(2*n_per_group, 0, sigma)
  mod <- lm(y ~ group, data = dat)
  estimates[i] <- coef(mod)["group"]
}

# Plot
ggplot(data.frame(est = estimates), aes(est)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = delta, colour = "red") +
  labs(title = paste("Sampling distribution of estimates (n =", n_per_group, "per group)"),
       subtitle = ('Standard deviation of this distribution is the standard error'),
       x = "Estimated group effect (delta-hat)", y = "Count")



### 2. How the standard error changes with sample size

# Set additional simulation parameters
set.seed(29)
n_seq <- seq(10, 200, by = 10)
nsims <- 200
mean_se <- rep(NA, length(n_seq))

# Repeat simulations many times, at different sample sizes
for (j in seq_along(n_seq)) {
  n <- n_seq[j]
  print(paste("n per group =", n))
  ses <- rep(NA, nsims)
  for(i in 1:nsims){
    dat <- data.frame(group = rep(c(0,1), each = n))
    dat$y <- 10 + delta*dat$group + rnorm(2*n, 0, sigma)
    m <- lm(y ~ group, data = dat)
    ses[i] <- summary(m)$coefficients["group","Std. Error"]
  }
  mean_se[j] <- mean(ses)
}

# Plot
ggplot(data.frame(n = n_seq, mean_se = mean_se), aes(n, mean_se)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Precision increases fast at first, then slowly",
       x = "Sample size per group (n)", y = "Mean standard error of estimate") + 
  ylim(0, max(mean_se))


