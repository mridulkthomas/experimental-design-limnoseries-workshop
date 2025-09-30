#### Workshop on How to Design Better Experiments — Script 01
# Author: Mridul K. Thomas (ORCID: 0000-0002-5089-5610)
# Repo: https://github.com/mridulkthomas/experimental-design-limnoseries-workshop
# License: MIT (code); CC BY 4.0 (teaching materials)
####


##### Classical power analysis (ANOVA with 2 groups)
# Assumptions: balanced groups, equal variances, two-sided alpha, normal errors.

# Load libraries
library(ggplot2)
theme_set(theme_minimal(base_size = 12))

## Function to calculate power analytically given inputs
power_anova_2groups <- function(n_per_group, delta, sigma = 1, alpha = 0.05){
  # between.var uses (k-1) denominator, where k = number of groups
  between_var <- (delta^2) / 2    # for k = 2
  power.anova.test(groups = 2,
                   n = n_per_group,
                   between.var = between_var,
                   within.var  = sigma^2,
                   sig.level   = alpha)$power
}


### 1. How power changes with n per group

## User inputs
# sigma is determined by your system and your measurement precision
# delta is determined by your question (Smallest Effect Size of Interest)
# n_per_group is usually your main experimental choice

alpha <- 0.05   # Probability of Type I error (false positive)
sigma <- 1      # Within-group SD (assumed equal)
delta <- 0.5    # Smallest effect size of interest (difference between group means)
n_seq <- 4:200  # Per-group sample sizes to evaluate

# Calculate power for all n
pow_n <- sapply(n_seq, power_anova_2groups,
                delta = delta, sigma = sigma, alpha = alpha)

# plot
df_analytical <- data.frame(n = n_seq, power = pow_n)

ggplot(df_analytical, aes(x = n, y = power)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = c(0.8, 0.9), linetype = "dashed", colour = "gray50") +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  labs(title = "ANALYTICAL: Power increases with sample size (ANOVA)",
       x = "Sample size per group (n)",
       y = "Power")




### 2. How power changes with error (sigma)

## User inputs
# sigma is determined by your system and your measurement precision
# delta is determined by your question (Smallest Effect Size of Interest)
# n_per_group is usually your main experimental choice

alpha       <- 0.05        # Probability of Type I error (false positive)
n_per_group <- 20          # Per-group sample size
delta       <- 0.5         # Smallest effect size of interest (difference between group means)
sigma_seq   <- seq(0.1, 2, by = 0.1)  # Within-group SD values to evaluate

# Calculate power for all sigma
pow_sigma <- sapply(sigma_seq, function(s)
  power_anova_2groups(n_per_group, delta, sigma = s, alpha = alpha)
)

# Plot
df_sigma_analytical <- data.frame(sigma = sigma_seq, power = pow_sigma)

ggplot(df_sigma_analytical, aes(x = sigma, y = power)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = c(0.8, 0.9), linetype = "dashed", colour = "gray50") +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  labs(title = "ANALYTICAL: Power decreases with increasing error (ANOVA)",
       x = "Within-group SD (sigma)",
       y = "Power")

