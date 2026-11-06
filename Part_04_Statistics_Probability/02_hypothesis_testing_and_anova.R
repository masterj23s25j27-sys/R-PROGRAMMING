# ==============================================================================
# PART 04: PROBABILITY, STATISTICS & HYPOTHESIS TESTING
# Module 02: Parametric & Non-Parametric Hypothesis Testing, ANOVA, and A/B Testing
# ==============================================================================
# In this script, you will master:
# 1. One-Sample, Two-Sample (Welch), and Paired Student's t-Tests (`t.test`)
# 2. Non-Parametric Alternatives (`wilcox.test`: Mann-Whitney U & Wilcoxon Signed-Rank)
# 3. Normality & Homogeneity of Variance Checks (`shapiro.test`, `var.test`, `bartlett.test`)
# 4. Categorical Independence & Proportions (`chisq.test`, `fisher.test`, `prop.test`)
# 5. One-Way & Two-Way ANOVA (`aov`), Tukey HSD Post-Hoc, and `kruskal.test`
# 6. Multiple Testing Correction (`p.adjust`: Bonferroni vs Benjamini-Hochberg FDR)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. STUDENT'S T-TESTS (One-Sample, Two-Sample Welch, and Paired)
# ------------------------------------------------------------------------------
set.seed(101)
# Simulate A/B test latency measurements (in milliseconds)
control_latency   <- rnorm(45, mean = 210, sd = 28)
treatment_latency <- rnorm(45, mean = 192, sd = 24)

# 1.1 Two-Sample Welch's t-test (Default in R: does NOT assume equal variance!)
ab_t_test <- t.test(treatment_latency, control_latency, alternative = "two.sided")
cat("--- 1.1 Welch Two-Sample t-Test (Treatment vs Control Latency) ---\n")
print(ab_t_test)

# Calculate Cohen's d Effect Size
pooled_sd <- sqrt(((length(control_latency) - 1) * var(control_latency) +
                   (length(treatment_latency) - 1) * var(treatment_latency)) /
                  (length(control_latency) + length(treatment_latency) - 2))
cohens_d <- (mean(treatment_latency) - mean(control_latency)) / pooled_sd
cat(sprintf("Cohen's d Effect Size: %.3f\n", cohens_d))

# 1.2 Paired t-test (Before vs After on the SAME subjects)
bp_before <- c(142, 138, 150, 145, 136, 148, 155, 140, 139, 144)
bp_after  <- c(134, 132, 141, 139, 130, 140, 144, 135, 133, 137)
paired_res <- t.test(bp_after, bp_before, paired = TRUE)
cat("\n--- 1.2 Paired t-Test (Blood Pressure After vs Before) ---\n")
cat(sprintf("Mean Change: %.2f mmHg | p-value: %.5f\n",
            paired_res$estimate, paired_res$p.value))

# ------------------------------------------------------------------------------
# 2. NON-PARAMETRIC TESTS (When Normality Fails or Outliers Dominate)
# ------------------------------------------------------------------------------
# Shapiro-Wilk Normality Test (H0: Data is normally distributed)
shapiro_res <- shapiro.test(control_latency)
cat(sprintf("\nShapiro-Wilk Normality Test p-value: %.4f\n", shapiro_res$p.value))

# Wilcoxon Rank-Sum Test (Mann-Whitney U) for two independent skewed samples
skewed_a <- rexp(40, rate = 0.1)
skewed_b <- rexp(40, rate = 0.05)
wilcox_res <- wilcox.test(skewed_a, skewed_b, conf.int = TRUE)
cat("--- 2. Wilcoxon Rank-Sum (Mann-Whitney U) Test ---\n")
cat(sprintf("Hodges-Lehmann Location Shift: %.2f | p-value: %.5f\n",
            wilcox_res$estimate, wilcox_res$p.value))

# ------------------------------------------------------------------------------
# 3. CATEGORICAL TESTS: CHI-SQUARE, FISHER'S EXACT, & PROPORTION A/B TEST
# ------------------------------------------------------------------------------
# 3.1 Two-Proportion Conversion Rate A/B Test (`prop.test`)
# Variant A: 140 conversions out of 2,000 visitors (7.0%)
# Variant B: 185 conversions out of 2,000 visitors (9.25%)
prop_ab <- prop.test(x = c(185, 140), n = c(2000, 2000), correct = FALSE)
cat("\n--- 3.1 Two-Proportion Conversion Rate A/B Test ---\n")
print(prop_ab)

# 3.2 Chi-Square Test of Independence (`chisq.test`)
contingency_tbl <- matrix(
  c(120, 80, 40,
    90,  110, 60),
  nrow = 2, byrow = TRUE,
  dimnames = list(Plan = c("Free", "Paid"), Device = c("Mobile", "Desktop", "Tablet"))
)
chisq_res <- chisq.test(contingency_tbl)
cat("--- 3.2 Chi-Square Test of Independence ---\n")
cat(sprintf("X-squared = %.3f, df = %d, p-value = %.5f\n",
            chisq_res$statistic, chisq_res$parameter, chisq_res$p.value))

# ------------------------------------------------------------------------------
# 4. ANALYSIS OF VARIANCE (ANOVA) & POST-HOC COMPARISONS (`aov` & `TukeyHSD`)
# ------------------------------------------------------------------------------
# Compare `PlantGrowth` dry weight across `ctrl`, `trt1`, and `trt2` groups
anova_fit <- aov(weight ~ group, data = PlantGrowth)
cat("\n--- 4. One-Way ANOVA Summary (`PlantGrowth`) ---\n")
print(summary(anova_fit))

cat("\n--- Tukey's Honestly Significant Difference (TukeyHSD) Post-Hoc ---\n")
print(TukeyHSD(anova_fit))

# Non-parametric equivalent to One-Way ANOVA: Kruskal-Wallis Test
kw_res <- kruskal.test(weight ~ group, data = PlantGrowth)
cat(sprintf("Kruskal-Wallis p-value: %.5f\n", kw_res$p.value))

# ------------------------------------------------------------------------------
# 5. MULTIPLE HYPOTHESIS TESTING CORRECTION (`p.adjust`)
# ------------------------------------------------------------------------------
raw_pvals <- c(0.001, 0.008, 0.021, 0.039, 0.048, 0.120, 0.450, 0.810)
pval_table <- data.frame(
  raw_p       = raw_pvals,
  bonferroni  = round(p.adjust(raw_pvals, method = "bonferroni"), 4), # Controls Family-Wise Error Rate (FWER)
  holm        = round(p.adjust(raw_pvals, method = "holm"), 4),       # Uniformly more powerful than Bonferroni
  bh_fdr      = round(p.adjust(raw_pvals, method = "BH"), 4)          # Controls False Discovery Rate (FDR)
)
cat("\n--- 5. Multiple Testing Adjusted P-Values ---\n")
print(pval_table)

