# ==============================================================================
# PART 06: MACHINE LEARNING, TIME SERIES & PRODUCTION CAPSTONE
# Module 03: End-to-End Production Data Science Pipeline
#            (Data Generation -> Cleaning -> Feature Engineering -> CV Model
#             Comparison -> Model Artifact Serialization -> Executive Report)
# ==============================================================================
# This capstone script demonstrates how an enterprise R pipeline is structured
# from raw data ingestion to model deployment and automated reporting.
# ==============================================================================

set.seed(2026)

# ------------------------------------------------------------------------------
# STEP 1: GENERATE REALISTIC SYNTHETIC FINTECH CREDIT RISK DATASET
# ------------------------------------------------------------------------------
simulate_credit_dataset <- function(n = 800) {
  age             <- round(runif(n, 21, 70))
  annual_income   <- round(rlnorm(n, meanlog = 11.1, sdlog = 0.55), 0)
  debt_to_income  <- round(rbeta(n, shape1 = 2, shape2 = 5) * 0.65, 3)
  credit_score    <- round(pmin(850, pmax(300, rnorm(n, mean = 685, sd = 65))))
  loan_amount     <- round(runif(n, 5000, 60000), -2)
  employment_tier <- sample(c("Salaried", "Self-Employed", "Contract"),
                            size = n, replace = TRUE, prob = c(0.65, 0.20, 0.15))

  # Inject 3% realistic missing values into `debt_to_income`
  dti_with_na <- debt_to_income
  dti_with_na[sample(seq_len(n), size = floor(0.03 * n))] <- NA_real_

  # True underlying default log-odds
  emp_effect <- ifelse(employment_tier == "Salaried", -0.35,
                ifelse(employment_tier == "Self-Employed", 0.25, 0.45))
  log_odds <- 2.2 -
    0.0085 * (credit_score - 600) +
    3.8 * debt_to_income +
    0.65 * (loan_amount / annual_income) +
    emp_effect

  prob_default <- 1 / (1 + exp(-log_odds))
  default_flag <- rbinom(n, size = 1, prob = prob_default)

  data.frame(
    applicant_id    = sprintf("APP-%04d", seq_len(n)),
    age             = age,
    annual_income   = annual_income,
    debt_to_income  = dti_with_na,
    credit_score    = credit_score,
    loan_amount     = loan_amount,
    employment_tier = factor(employment_tier, levels = c("Salaried", "Self-Employed", "Contract")),
    default_flag    = default_flag,
    stringsAsFactors = FALSE
  )
}

raw_credit <- simulate_credit_dataset(n = 800)
cat("--- STEP 1: Raw Dataset Ingested ---\n")
cat("Rows:", nrow(raw_credit), "| Missing DTI count:", sum(is.na(raw_credit$debt_to_income)),
    "| Base Default Rate:", round(mean(raw_credit$default_flag) * 100, 2), "%\n")

# ------------------------------------------------------------------------------
# STEP 2: LEAKAGE-FREE PREPROCESSING & FEATURE ENGINEERING PIPELINE
# ------------------------------------------------------------------------------
train_mask <- sample(seq_len(nrow(raw_credit)), size = floor(0.75 * nrow(raw_credit)))
train_raw  <- raw_credit[train_mask, ]
test_raw   <- raw_credit[-train_mask, ]

# Fit imputer & feature transformer on `train_raw` ONLY
fit_preprocessor <- function(df_train) {
  median_dti <- median(df_train$debt_to_income, na.rm = TRUE)
  list(median_dti = median_dti)
}

transform_features <- function(df, prep_state) {
  out <- df
  # 1. Impute missing `debt_to_income` using training median
  out$dti_imputed_flag <- as.integer(is.na(out$debt_to_income))
  out$debt_to_income[is.na(out$debt_to_income)] <- prep_state$median_dti

  # 2. Engineer domain ratios
  out$loan_to_income <- out$loan_amount / out$annual_income
  out$log_income     <- log1p(out$annual_income)
  out$credit_bucket  <- cut(
    out$credit_score,
    breaks = c(-Inf, 580, 670, 740, 800, Inf),
    labels = c("Poor", "Fair", "Good", "Very_Good", "Exceptional")
  )
  out
}

prep_state <- fit_preprocessor(train_raw)
train_clean <- transform_features(train_raw, prep_state)
test_clean  <- transform_features(test_raw,  prep_state)

# ------------------------------------------------------------------------------
# STEP 3: MODEL TRAINING & STRATIFIED COMPARISON
# ------------------------------------------------------------------------------
# Model 1: Baseline Logistic Regression (Credit Score + DTI only)
m1_baseline <- glm(
  default_flag ~ credit_score + debt_to_income,
  data = train_clean, family = binomial(link = "logit")
)

# Model 2: Full Feature-Engineered Logistic Regression with Interaction
m2_engineered <- glm(
  default_flag ~ credit_score * debt_to_income + loan_to_income + log_income + employment_tier,
  data = train_clean, family = binomial(link = "logit")
)

calc_roc_auc <- function(actual, prob) {
  r <- rank(prob)[actual == 1]
  n1 <- sum(actual == 1)
  n0 <- sum(actual == 0)
  (sum(r) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

calc_brier <- function(actual, prob) mean((actual - prob)^2)

p1_test <- predict(m1_baseline,   newdata = test_clean, type = "response")
p2_test <- predict(m2_engineered, newdata = test_clean, type = "response")

leaderboard <- data.frame(
  Model      = c("1. Baseline GLM", "2. Feature-Engineered GLM"),
  AIC_Train  = round(c(AIC(m1_baseline), AIC(m2_engineered)), 1),
  Test_AUC   = round(c(calc_roc_auc(test_clean$default_flag, p1_test),
                       calc_roc_auc(test_clean$default_flag, p2_test)), 4),
  Test_Brier = round(c(calc_brier(test_clean$default_flag, p1_test),
                       calc_brier(test_clean$default_flag, p2_test)), 4)
)
cat("\n--- STEP 3: Holdout Model Leaderboard ---\n")
print(leaderboard)

# ------------------------------------------------------------------------------
# STEP 4: RISK DECILE LIFT TABLE & ARTIFACT SERIALIZATION
# ------------------------------------------------------------------------------
test_clean$pred_pd <- p2_test
test_clean$risk_decile <- cut(
  test_clean$pred_pd,
  breaks = quantile(test_clean$pred_pd, probs = seq(0, 1, by = 0.2)),
  include.lowest = TRUE,
  labels = paste0("Tier_", 1:5, c(" (Lowest Risk)", "", "", "", " (Highest Risk)"))
)

risk_table <- aggregate(
  cbind(Observed_Default_Rate = default_flag, Mean_Predicted_PD = pred_pd) ~ risk_decile,
  data = test_clean,
  FUN = function(x) round(mean(x) * 100, 2)
)
cat("\n--- STEP 4: Calibration & Risk Tier Lift Table (%) ---\n")
print(risk_table)

# Save production model bundle (preprocessor + fitted model + metadata) to `.rds`
artifact_path <- file.path(tempdir(), "credit_risk_model_bundle_v1.rds")
model_bundle <- list(
  version      = "1.0.0",
  trained_at   = Sys.time(),
  prep_state   = prep_state,
  model        = m2_engineered,
  test_metrics = leaderboard[2, ]
)
saveRDS(model_bundle, file = artifact_path)
cat("\nSaved deployable production model bundle to:", artifact_path, "\n")

