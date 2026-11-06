# ==============================================================================
# PART 04: PROBABILITY, STATISTICS & HYPOTHESIS TESTING
# Module 03: Regression Modeling, GLMs, Diagnostics, and Non-Parametric Bootstrap
# ==============================================================================
# In this script, you will master:
# 1. Ordinary Least Squares (OLS) Simple & Multiple Linear Regression (`lm`)
# 2. Interaction Terms (`*`, `:`), Polynomial Terms (`poly()`), and Formula Syntax
# 3. Model Selection & Diagnostics (Adjusted R^2, AIC, BIC, Residuals, Cook's Distance)
# 4. Generalized Linear Models (`glm`): Logistic Regression (Binomial) & Poisson Regression
# 5. Non-Parametric Bootstrap Confidence Intervals & Permutation Tests from Scratch
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. MULTIPLE LINEAR REGRESSION (`lm`) & INTERACTION TERMS
# ------------------------------------------------------------------------------
# Predict vehicle `mpg` from weight (`wt`), horsepower (`hp`), and their interaction
ols_additive    <- lm(mpg ~ wt + hp, data = mtcars)
ols_interaction <- lm(mpg ~ wt * hp, data = mtcars) # Equivalent to: mpg ~ wt + hp + wt:hp

cat("--- 1. Multiple Linear Regression with Interaction (`mpg ~ wt * hp`) ---\n")
print(summary(ols_interaction))

# Compare nested models using Partial F-Test (`anova`) and Information Criteria (`AIC`, `BIC`)
cat("\n--- Nested Model Comparison (Additive vs Interaction) ---\n")
print(anova(ols_additive, ols_interaction))
cat("AIC (Additive vs Interaction):", AIC(ols_additive), "vs", AIC(ols_interaction), "\n")
cat("BIC (Additive vs Interaction):", BIC(ols_additive), "vs", BIC(ols_interaction), "\n")

# 95% Confidence Intervals on Regression Coefficients
cat("\n95% Confidence Intervals for Model Parameters:\n")
print(round(confint(ols_interaction, level = 0.95), 4))

# Predicting on new unseen data with 95% Prediction Interval (`interval = "prediction"`)
new_cars <- data.frame(wt = c(2.5, 3.5), hp = c(110, 180))
pred_int <- predict(ols_interaction, newdata = new_cars, interval = "prediction", level = 0.95)
cat("\nPredictions with 95% Prediction Intervals:\n")
print(round(cbind(new_cars, pred_int), 2))

# ------------------------------------------------------------------------------
# 2. REGRESSION DIAGNOSTICS & INFLUENTIAL OBSERVATIONS (COOK'S DISTANCE)
# ------------------------------------------------------------------------------
cooks_d <- cooks.distance(ols_interaction)
influential_threshold <- 4 / nrow(mtcars)
influential_cars <- names(cooks_d[cooks_d > influential_threshold])
cat("\nHigh-influence vehicles (Cook's D > 4/n):", paste(influential_cars, collapse = ", "), "\n")

# Shapiro-Wilk test on model residuals
cat("Residual normality p-value:", round(shapiro.test(residuals(ols_interaction))$p.value, 4), "\n")

# ------------------------------------------------------------------------------
# 3. GENERALIZED LINEAR MODELS (`glm`): LOGISTIC & POISSON REGRESSION
# ------------------------------------------------------------------------------
# 3.1 Logistic Regression (`family = binomial(link = "logit")`)
# Predict probability of Manual Transmission (`am == 1`) from `wt` and `hp`
logit_fit <- glm(am ~ wt + hp, data = mtcars, family = binomial(link = "logit"))
cat("\n--- 3.1 Logistic Regression (`glm` Binomial Logit) ---\n")
print(summary(logit_fit))

# Odds Ratios = exp(beta)
odds_ratios <- exp( cbind(OR = coef(logit_fit), confint.default(logit_fit)) )
cat("\nOdds Ratios (exp(beta)) and 95% Wald CIs:\n")
print(round(odds_ratios, 4))

# Predicted probabilities (`type = "response"`)
pred_probs <- predict(logit_fit, type = "response")
pred_class <- ifelse(pred_probs >= 0.5, 1, 0)
accuracy   <- mean(pred_class == mtcars$am)
cat(sprintf("In-sample Classification Accuracy: %.2f%%\n", accuracy * 100))

# 3.2 Poisson Regression for Count Data (`family = poisson(link = "log")`)
# Using built-in `warpbreaks` dataset (number of breaks per loom)
pois_fit <- glm(breaks ~ wool * tension, data = warpbreaks, family = poisson(link = "log"))
cat("\n--- 3.2 Poisson Regression (`warpbreaks` Count Model) ---\n")
print(round(coef(summary(pois_fit)), 4))

# ------------------------------------------------------------------------------
# 4. NON-PARAMETRIC BOOTSTRAP & PERMUTATION TESTS
# ------------------------------------------------------------------------------
# What if we want a 95% Confidence Interval for the MEDIAN or R-squared without
# making any parametric distributional assumptions? Use Bootstrap resampling!
set.seed(2026)
B <- 2000
boot_medians <- replicate(B, {
  resample_idx <- sample(seq_along(mtcars$mpg), replace = TRUE)
  median(mtcars$mpg[resample_idx])
})

boot_ci <- quantile(boot_medians, probs = c(0.025, 0.975))
cat("\n--- 4. Non-Parametric Bootstrap (2,000 Resamples) ---\n")
cat(sprintf("Sample Median MPG: %.2f | 95%% Percentile Bootstrap CI: [%.2f, %.2f]\n",
            median(mtcars$mpg), boot_ci[1], boot_ci[2]))
