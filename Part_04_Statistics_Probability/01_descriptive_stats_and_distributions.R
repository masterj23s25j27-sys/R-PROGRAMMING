# ==============================================================================
# PART 04: PROBABILITY, STATISTICS & HYPOTHESIS TESTING
# Module 01: Descriptive Statistics & Probability Distributions (`d/p/q/r` Family)
# ==============================================================================
# R was designed by statisticians for statisticians. In this script, you will learn:
# 1. Measures of Central Tendency, Dispersion, Skewness, and Kurtosis
# 2. Covariance & Correlation Matrices (Pearson, Spearman Rank, Kendall Tau)
# 3. R's Unified Probability Distribution Convention (`d*`, `p*`, `q*`, `r*`):
#    - `d*` : Density / Probability Mass Function (PDF / PMF)
#    - `p*` : Cumulative Distribution Function (CDF: P(X <= x))
#    - `q*` : Quantile Function / Inverse CDF
#    - `r*` : Random Number Generation
# 4. Monte Carlo Simulation of the Central Limit Theorem (CLT)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. DESCRIPTIVE STATISTICS & ROBUST ESTIMATORS
# ------------------------------------------------------------------------------
set.seed(42)
# Simulate right-skewed income data (in $1,000s) with a few high-earner outliers
incomes <- c(rlnorm(200, meanlog = 4.2, sdlog = 0.45), 450, 620, 890)

desc_stats <- c(
  Mean          = mean(incomes),
  Trimmed_Mean  = mean(incomes, trim = 0.05), # Drops top & bottom 5% outliers
  Median        = median(incomes),
  SD            = sd(incomes),
  MAD_Robust_SD = mad(incomes),               # Median Absolute Deviation
  IQR           = IQR(incomes),
  Min           = min(incomes),
  Max           = max(incomes)
)
cat("--- Descriptive Statistics (Sensitive vs. Outlier-Robust) ---\n")
print(round(desc_stats, 2))

# Sample Quantiles / Percentiles
cat("\nDeciles (10%, 20%, ..., 90%):\n")
print(round(quantile(incomes, probs = seq(0.1, 0.9, by = 0.1)), 2))

# Custom Skewness & Excess Kurtosis functions
calc_skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  m3 <- mean((x - mean(x))^3)
  m2 <- mean((x - mean(x))^2)
  m3 / (m2^(3 / 2))
}

calc_kurtosis <- function(x) {
  x <- x[!is.na(x)]
  m4 <- mean((x - mean(x))^4)
  m2 <- mean((x - mean(x))^2)
  (m4 / (m2^2)) - 3 # Excess kurtosis (Normal distribution = 0)
}

cat(sprintf("Skewness: %.3f | Excess Kurtosis: %.3f\n",
            calc_skewness(incomes), calc_kurtosis(incomes)))

# ------------------------------------------------------------------------------
# 2. CORRELATION: PEARSON (Linear) vs SPEARMAN (Monotonic Rank)
# ------------------------------------------------------------------------------
# Spearman rank correlation is far more robust to non-linear monotonic
# relationships and extreme outliers!
x_var <- 1:30
y_monotonic <- exp(x_var / 5) # Strongly non-linear but strictly monotonic

cat(sprintf("Pearson r (linear):    %.4f\n", cor(x_var, y_monotonic, method = "pearson")))
cat(sprintf("Spearman rho (rank):   %.4f\n", cor(x_var, y_monotonic, method = "spearman")))
cat(sprintf("Kendall tau (concord): %.4f\n", cor(x_var, y_monotonic, method = "kendall")))

# ------------------------------------------------------------------------------
# 3. PROBABILITY DISTRIBUTIONS: THE `d / p / q / r` CONVENTION
# ------------------------------------------------------------------------------
# Supported out of the box:
# `norm` (Normal), `t` (Student-t), `chisq` (Chi-Square), `f` (F),
# `binom` (Binomial), `pois` (Poisson), `exp` (Exponential),
# `gamma` (Gamma), `beta` (Beta), `unif` (Uniform), `lnorm` (Log-Normal)

# Example A: Normal Distribution IQ ~ N(mean = 100, sd = 15)
cat("\n--- Normal Distribution N(100, 15^2) ---\n")
cat("PDF at x = 100 (`dnorm`):             ", round(dnorm(100, mean = 100, sd = 15), 4), "\n")
cat("P(IQ <= 130) (`pnorm`):               ", round(pnorm(130, mean = 100, sd = 15), 4), "\n")
cat("P(IQ > 130) (`lower.tail = FALSE`):   ", round(pnorm(130, mean = 100, sd = 15, lower.tail = FALSE), 4), "\n")
cat("99th Percentile IQ score (`qnorm`):   ", round(qnorm(0.99, mean = 100, sd = 15), 2), "\n")

# Example B: Binomial Distribution (n = 20 trials, p = 0.25 conversion rate)
# What is the probability of getting EXACTLY 7 conversions? At least 7?
cat("\n--- Binomial Distribution Binom(n = 20, p = 0.25) ---\n")
cat("P(X == 7) (`dbinom`): ", round(dbinom(7, size = 20, prob = 0.25), 4), "\n")
cat("P(X >= 7) (`pbinom`): ", round(pbinom(6, size = 20, prob = 0.25, lower.tail = FALSE), 4), "\n")

# Example C: Poisson Distribution (lambda = 4.5 server requests per second)
cat("\n--- Poisson Distribution Pois(lambda = 4.5) ---\n")
cat("P(X == 5 requests) (`dpois`): ", round(dpois(5, lambda = 4.5), 4), "\n")
cat("P(X > 8 requests)  (`ppois`): ", round(ppois(8, lambda = 4.5, lower.tail = FALSE), 4), "\n")

# ------------------------------------------------------------------------------
# 4. MONTE CARLO SIMULATION: VERIFYING THE CENTRAL LIMIT THEOREM (CLT)
# ------------------------------------------------------------------------------
# Even when the underlying population is heavily skewed (Exponential with rate = 0.5,
# true mean = 2, true sd = 2), the distribution of sample means converges to Normal!
set.seed(123)
n_sims      <- 5000
sample_size <- 40
rate_param  <- 0.5

sample_means <- replicate(n_sims, mean(rexp(sample_size, rate = rate_param)))

theoretical_mean <- 1 / rate_param
theoretical_se   <- (1 / rate_param) / sqrt(sample_size)

cat("\n--- Central Limit Theorem Monte Carlo Verification (5,000 runs) ---\n")
cat(sprintf("Theoretical Mean: %.4f | Empirical Mean of Means: %.4f\n",
            theoretical_mean, mean(sample_means)))
cat(sprintf("Theoretical SE:   %.4f | Empirical SD of Means:   %.4f\n",
            theoretical_se, sd(sample_means)))

