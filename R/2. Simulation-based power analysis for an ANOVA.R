#### Workshop on How to Design Better Experiments — Script 02
# Author: Mridul K. Thomas (ORCID: 0000-0002-5089-5610)
# Repo: https://github.com/mridulkthomas/experimental-design-limnoseries-workshop
# License: MIT (code); CC BY 4.0 (teaching materials)
####


##### Simulation-based power analysis (ANOVA with 2 groups)
# Assumptions: balanced groups, equal variances, two-sided alpha, normal errors.

# Load libraries
library(ggplot2)
theme_set(theme_minimal(base_size = 12))

## Function to estimate power by simulation
power_sim_anova_2groups <- function(n_per_group, delta, sigma = 1, alpha = 0.05, nsims = 1000){
  # Estimated power = proportion of p-values < alpha.
  
  # Create empty vector to save p-values 
  pvals <- rep(NA, nsims)
  
  for(i in 1:nsims){
    ## progress ping (every 100 sims)
    # if (i %% 100 == 0) cat("sim", i, "of", nsims, "\n")
    
    dat <- data.frame(group = rep(c(0, 1), each = n_per_group))
    dat$y <- 10 + delta * dat$group + rnorm(2*n_per_group, 0, sd = sigma)
    
    mod <- lm(y ~ group, data = dat)
    
    pvals[i] <- summary(mod)$coefficients[2,4]
  }
  
  mean(pvals < alpha)
}

### 1. How power changes with n per group
# Assumptions: balanced groups, equal variances, two-sided alpha, normal errors.
set.seed(29)

## User inputs
# sigma is determined by your system and your measurement precision
# delta is determined by your question (Smallest Effect Size of Interest)
# n_per_group is usually your main experimental choice

alpha <- 0.05           # Probability of Type I error (false positive)
sigma <- 1              # Within-group SD (assumed equal)
delta <- 0.5            # Smallest effect size of interest (difference between group means)
n_seq <- seq(10, 200, by = 10)  # per-group sample sizes to evaluate
nsims <- 300           # Increase for smoother curves; loops will run longer

# Calculate power for all n (simple outer loop over n)
pow_n <- rep(NA, length(n_seq))
for (j in seq_along(n_seq)) {
  n <- n_seq[j]
  print(paste("n per group =", n))
  pow_n[j] <- power_sim_anova_2groups(n_per_group = n, delta = delta,
                                           sigma = sigma, alpha = alpha, nsims = nsims)
}

# Plot
df_n <- data.frame(n = n_seq, power = pow_n)

ggplot(df_n, aes(x = n, y = power)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = c(0.8, 0.9), linetype = "dashed", colour = "gray50") +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  xlim(0,200) + 
  labs(title = "SIMULATION: Power increases with sample size (ANOVA)",
       x = "Sample size per group (n)",
       y = "Power")




### 2. How power changes with error (sigma)
set.seed(29)

## User inputs
# sigma is determined by your system and your measurement precision
# delta is determined by your question (Smallest effect size of interest)
# n_per_group is usually your main experimental choice

alpha       <- 0.05        # Probability of Type I error (false positive)
n_per_group <- 20          # Per-group sample size
delta       <- 0.5         # Smallest effect size of interest (difference between group means)
sigma_seq   <- seq(0.1, 2, by = 0.1)  # Within-group SD values to evaluate
nsims       <- 300

# Calculate power for all sigma (simple outer loop over sigma)
pow_sigma <- rep(NA, length(sigma_seq))

for (j in seq_along(sigma_seq)) {
  s <- sigma_seq[j]
  print(paste("sigma =", s))
  pow_sigma[j] <- power_sim_anova_2groups(n_per_group = n_per_group, delta = delta,
                                               sigma = s, alpha = alpha, nsims = nsims)
}

# Plot
df_sigma <- data.frame(sigma = sigma_seq, power = pow_sigma)

ggplot(df_sigma, aes(x = sigma, y = power)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = c(0.8, 0.9), linetype = "dashed", colour = "gray50") +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  labs(title = "SIMULATION: Power decreases with increasing error (ANOVA)",
       x = "Within-group SD (sigma)",
       y = "Power")

