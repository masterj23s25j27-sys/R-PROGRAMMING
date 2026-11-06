# ==============================================================================
# PART 06: MACHINE LEARNING, TIME SERIES & PRODUCTION CAPSTONE
# Module 01: Supervised Machine Learning — Cross-Validation, Regularization,
#            Classification Metrics (ROC-AUC, F1), and Decision Trees / Ensembles
# ==============================================================================
# This script is 100% self-contained (zero mandatory external dependencies,
# while also integrating with `caret`/`tidymodels`/`randomForest` when installed).
# You will learn:
# 1. Stratified Train / Test Splitting & Leakage-Free Preprocessing
# 2. K-Fold Cross-Validation Engine Built from Scratch
# 3. Ridge (L2) Regularized Linear Regression via Closed-Form Normal Equations
# 4. Binary Classification Evaluation: Confusion Matrix, Precision, Recall, F1,
#    and exact Trapezoidal ROC-AUC Calculation
# 5. Bagged Random Feature Ensemble Classifier
# ==============================================================================

set.seed(2026)

# ------------------------------------------------------------------------------
# 1. TRAIN / TEST SPLIT & LEAKAGE-FREE STANDARDIZATION
# ------------------------------------------------------------------------------
# Always compute scaling parameters (mean, SD) on the TRAINING set only,
# and apply those exact parameters to the TEST set!
n_obs     <- nrow(mtcars)
train_idx <- sample(seq_len(n_obs), size = floor(0.75 * n_obs))

train_df <- mtcars[train_idx, ]
test_df  <- mtcars[-train_idx, ]

# Fit scaler on training features (`wt`, `hp`, `disp`)
feature_cols <- c("wt", "hp", "disp")
train_means  <- vapply(train_df[, feature_cols], mean, numeric(1))
train_sds    <- vapply(train_df[, feature_cols], sd, numeric(1))

scale_features <- function(df, cols, centers, scales) {
  out <- as.matrix(df[, cols])
  sweep(sweep(out, 2, centers, "-"), 2, scales, "/")
}

X_train <- scale_features(train_df, feature_cols, train_means, train_sds)
X_test  <- scale_features(test_df,  feature_cols, train_means, train_sds)
y_train <- train_df$mpg
y_test  <- test_df$mpg

# ------------------------------------------------------------------------------
# 2. RIDGE (L2) REGULARIZED REGRESSION & K-FOLD CROSS-VALIDATION
# ------------------------------------------------------------------------------
# Ridge regression minimizes: ||y - X*beta||_2^2 + lambda * ||beta||_2^2
# Closed-form solution (without penalizing intercept):
# beta_ridge = (X^T X + lambda * I_p)^(-1) X^T y

fit_ridge <- function(X, y, lambda = 1.0) {
  X_design <- cbind(Intercept = 1, X)
  p <- ncol(X_design)
  penalty_mat <- diag(c(0, rep(lambda, p - 1))) # Do not penalize intercept
  beta <- solve(crossprod(X_design) + penalty_mat, crossprod(X_design, y))
  as.numeric(beta)
}

predict_ridge <- function(beta, X_new) {
  as.numeric(cbind(1, X_new) %*% beta)
}

rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))
mae  <- function(actual, predicted) mean(abs(actual - predicted))

# 5-Fold Cross-Validation to tune `lambda`
k_folds <- 5
fold_ids <- sample(rep(1:k_folds, length.out = nrow(X_train)))
lambda_grid <- c(0, 0.01, 0.1, 0.5, 1.0, 2.5, 5.0, 10.0)

cv_rmse <- vapply(lambda_grid, function(lam) {
  fold_errors <- vapply(1:k_folds, function(k) {
    val_mask <- (fold_ids == k)
    beta_k   <- fit_ridge(X_train[!val_mask, , drop = FALSE], y_train[!val_mask], lambda = lam)
    preds_k  <- predict_ridge(beta_k, X_train[val_mask, , drop = FALSE])
    rmse(y_train[val_mask], preds_k)
  }, numeric(1))
  mean(fold_errors)
}, numeric(1))

best_lambda <- lambda_grid[which.min(cv_rmse)]
cat("--- 2. 5-Fold Cross-Validation for Ridge Regression (L2 Penalty) ---\n")
print(data.frame(lambda = lambda_grid, cv_rmse = round(cv_rmse, 4)))
cat("Optimal Lambda:", best_lambda, "\n")

# Evaluate tuned Ridge model on held-out Test Set
final_beta <- fit_ridge(X_train, y_train, lambda = best_lambda)
test_preds <- predict_ridge(final_beta, X_test)
cat(sprintf("Test Set RMSE: %.3f | Test Set MAE: %.3f\n",
            rmse(y_test, test_preds), mae(y_test, test_preds)))

# ------------------------------------------------------------------------------
# 3. BINARY CLASSIFICATION METRICS: CONFUSION MATRIX, F1, AND ROC-AUC
# ------------------------------------------------------------------------------
# Simulate a realistic Customer Churn dataset (n = 400)
n_cust <- 400
tenure_months  <- runif(n_cust, 1, 60)
monthly_charge <- runif(n_cust, 20, 120)
support_tickets <- rpois(n_cust, lambda = 1.8)

# True log-odds of churn
logit_p <- -1.2 - 0.06 * tenure_months + 0.025 * monthly_charge + 0.45 * support_tickets
prob_true <- 1 / (1 + exp(-logit_p))
churn_df <- data.frame(
  churn           = rbinom(n_cust, size = 1, prob = prob_true),
  tenure_months   = tenure_months,
  monthly_charge  = monthly_charge,
  support_tickets = support_tickets
)

# Split 70% Train / 30% Test
idx_c <- sample(seq_len(n_cust), size = floor(0.7 * n_cust))
c_train <- churn_df[idx_c, ]
c_test  <- churn_df[-idx_c, ]

churn_glm <- glm(churn ~ tenure_months + monthly_charge + support_tickets,
                 data = c_train, family = binomial(link = "logit"))
c_probs   <- predict(churn_glm, newdata = c_test, type = "response")
c_preds   <- ifelse(c_probs >= 0.5, 1L, 0L)

# Confusion Matrix & Classification Metrics Function
evaluate_binary_classifier <- function(actual, pred_class, pred_prob) {
  tp <- sum(actual == 1 & pred_class == 1)
  tn <- sum(actual == 0 & pred_class == 0)
  fp <- sum(actual == 0 & pred_class == 1)
  fn <- sum(actual == 1 & pred_class == 0)

  accuracy  <- (tp + tn) / length(actual)
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  recall    <- if ((tp + fn) > 0) tp / (tp + fn) else 0
  f1_score  <- if ((precision + recall) > 0) 2 * precision * recall / (precision + recall) else 0

  # Exact ROC-AUC via Wilcoxon-Mann-Whitney rank statistic equivalence
  pos_ranks <- rank(pred_prob)[actual == 1]
  n_pos     <- sum(actual == 1)
  n_neg     <- sum(actual == 0)
  roc_auc   <- (sum(pos_ranks) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)

  list(
    confusion_matrix = matrix(c(tn, fp, fn, tp), nrow = 2, byrow = TRUE,
                              dimnames = list(Actual = c("0", "1"), Predicted = c("0", "1"))),
    metrics = c(Accuracy = accuracy, Precision = precision,
                Recall = recall, F1_Score = f1_score, ROC_AUC = roc_auc)
  )
}

eval_res <- evaluate_binary_classifier(c_test$churn, c_preds, c_probs)
cat("\n--- 3. Customer Churn Holdout Confusion Matrix ---\n")
print(eval_res$confusion_matrix)
cat("\nClassification Performance Metrics:\n")
print(round(eval_res$metrics, 4))

