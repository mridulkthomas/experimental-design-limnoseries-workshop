#### Workshop on How to Design Better Experiments — Script 04
# Author: Mridul K. Thomas (ORCID: 0000-0002-5089-5610)
# Repo: https://github.com/mridulkthomas/experimental-design-limnoseries-workshop
# License: MIT (code); CC BY 4.0 (teaching materials)
####


##### EXERCISE: Using simulations, find how precision changes with error (ANOVA with 2 groups)
# Assumptions: balanced groups, equal variances, two-sided alpha, normal errors.

# Load libraries
library(ggplot2)
library(dplyr)
theme_set(theme_minimal(base_size = 12))

############ ONLY MODIFY THIS SECTION ############
# Enter 4 values of sigma, the within-group standard deviation
# CODING: Enter 4 numbers inside c() with commas between e.g. c(0.1, 2, 5, 10)
sigma_levels <- c()

# OPTIONAL - can leave this alone or change the values
delta <- 0.5             # Difference between groups (smallest effect size of interest)
n_per_group <- 30        # Per-group sample size
##################################################

# Simulation settings
set.seed(29)
nsims <- 1000

# Run simulations for each sigma
results <- data.frame()
for (s in sigma_levels){
  ests <- rep(NA, nsims)
  ses  <- rep(NA, nsims)
  for (i in 1:nsims){
    dat <- data.frame(group = rep(c(0,1), each = n_per_group))
    dat$y <- 10 + delta*dat$group + rnorm(2*n_per_group, 0, s)
    mod <- lm(y ~ group, data = dat)
    ests[i] <- coef(mod)["group"]
    ses[i]  <- summary(mod)$coefficients["group","Std. Error"]
  }
  results <- rbind(results,
                   data.frame(sigma = s,
                              estimate = ests,
                              se = ses))
  print(paste("sigma =", s, "done"))
}

### 1. Spread of estimates for each sigma
ggplot(results, aes(estimate, fill = factor(sigma))) +
  geom_histogram(alpha = 0.4, bins = 50, position = "identity") +
  geom_vline(xintercept = delta, colour = "red") +
  labs(title = paste("Sampling distributions at n =", n_per_group, "per group"),
       subtitle = "Wider spread means lower precision",
       x = "Estimated group difference",
       y = "Count",
       fill = "Sigma")

### 2. Mean SE for each sigma
mean_se <- results %>%
  group_by(sigma) %>%
  summarise(mean_se = mean(se, na.rm = TRUE), .groups = "drop")

ggplot(mean_se, aes(sigma, mean_se)) +
  geom_point(size = 5) +
  geom_line(linewidth = 1.2) +
  labs(title = paste("Precision vs error SD at n =", n_per_group, "per group"),
       x = "Within-group SD (sigma)",
       y = "Mean standard error of estimate")
