# ==============================================================================
# PART 05: ADVANCED R PROGRAMMING
# Module 01: Object-Oriented Programming in R (S3, S4, and R6 Classes)
# ==============================================================================
# R supports multiple OOP paradigms, each suited to different engineering tasks:
# 1. S3 (Informal, Generic-Function OOP): Used throughout Base R (`print`, `summary`,
#    `plot`, `predict`) and the Tidyverse. Lightweight, fast, and idiomatic.
# 2. S4 (Formal, Strict Type-Checked OOP): Used heavily in Bioconductor & `Matrix`.
#    Enforces formal class definitions (`setClass`), typed slots, and validity checks.
# 3. R6 (Encapsulated Reference Semantics OOP): Mutable state (`self`, `private`),
#    used in `shiny`, `mlr3`, `plumber`, and database connection pools.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. S3 OBJECT-ORIENTED PROGRAMMING (Generic Functions & Method Dispatch)
# ------------------------------------------------------------------------------
# Best Practice for S3: Provide a low-level constructor (`new_*`), a validator
# (`validate_*`), and a user-friendly helper (`*`).

new_portfolio <- function(owner, holdings, cash_usd = 0) {
  stopifnot(is.character(owner), length(owner) == 1)
  stopifnot(is.data.frame(holdings))
  stopifnot(is.numeric(cash_usd), length(cash_usd) == 1)

  structure(
    list(
      owner      = owner,
      holdings   = holdings,
      cash_usd   = cash_usd,
      created_at = Sys.Date()
    ),
    class = "portfolio"
  )
}

validate_portfolio <- function(x) {
  required_cols <- c("ticker", "shares", "price")
  if (!all(required_cols %in% names(x$holdings))) {
    stop("`holdings` must contain columns: ticker, shares, price", call. = FALSE)
  }
  if (any(x$holdings$shares < 0) || any(x$holdings$price < 0)) {
    stop("Shares and prices must be non-negative.", call. = FALSE)
  }
  x
}

portfolio <- function(owner, holdings, cash_usd = 0) {
  obj <- new_portfolio(owner, holdings, cash_usd)
  validate_portfolio(obj)
}

# Define a custom S3 Generic `total_value()`
total_value <- function(x, ...) {
  UseMethod("total_value")
}

# Implement S3 Methods for class `portfolio`
total_value.portfolio <- function(x, ...) {
  equity_val <- sum(x$holdings$shares * x$holdings$price)
  equity_val + x$cash_usd
}

print.portfolio <- function(x, ...) {
  cat(sprintf("<S3 Portfolio Object | Owner: %s | Total Value: $%s>\n",
              x$owner, format(round(total_value(x), 2), big.mark = ",")))
  cat("Cash Balance: $", format(round(x$cash_usd, 2), big.mark = ","), "\n", sep = "")
  cat("Holdings:\n")
  print(x$holdings)
  invisible(x)
}

# Test our S3 class and generic dispatch
my_holdings <- data.frame(
  ticker = c("AAPL", "MSFT", "NVDA"),
  shares = c(50, 30, 80),
  price  = c(195.0, 420.0, 125.0)
)
p_s3 <- portfolio(owner = "Jayesh", holdings = my_holdings, cash_usd = 5000)
print(p_s3)
cat("Dispatched total_value(p_s3):", total_value(p_s3), "\n")

# ------------------------------------------------------------------------------
# 2. S4 OBJECT-ORIENTED PROGRAMMING (Formal Slots & Validity Enforcement)
# ------------------------------------------------------------------------------
library(methods)

setClass(
  "ExperimentRun",
  slots = c(
    run_id          = "character",
    model_type      = "character",
    hyperparameters = "list",
    cv_scores       = "numeric"
  ),
  prototype = list(
    run_id          = "EXP-000",
    model_type      = "Baseline",
    hyperparameters = list(),
    cv_scores       = numeric(0)
  ),
  validity = function(object) {
    if (length(object@cv_scores) == 0) {
      return("`cv_scores` must contain at least one fold score.")
    }
    if (any(object@cv_scores < 0 | object@cv_scores > 1)) {
      return("All `cv_scores` must lie in [0, 1].")
    }
    TRUE
  }
)

# Define an S4 Generic and Method
setGeneric("meanScore", function(object) standardGeneric("meanScore"))
setMethod("meanScore", signature("ExperimentRun"), function(object) {
  mean(object@cv_scores)
})

setMethod("show", signature("ExperimentRun"), function(object) {
  cat(sprintf("<S4 ExperimentRun [%s] | Model: %s | Mean CV Score: %.4f>\n",
              object@run_id, object@model_type, meanScore(object)))
})

exp_s4 <- new(
  "ExperimentRun",
  run_id          = "EXP-2026-01",
  model_type      = "GradientBoosting",
  hyperparameters = list(ntrees = 500, lr = 0.03),
  cv_scores       = c(0.912, 0.925, 0.908, 0.919, 0.921)
)
show(exp_s4)

# ------------------------------------------------------------------------------
# 3. R6 ENCAPSULATED OOP (Reference Semantics & Mutable State)
# ------------------------------------------------------------------------------
if (requireNamespace("R6", quietly = TRUE)) {
  library(R6)

  RateLimiter <- R6Class(
    "RateLimiter",
    public = list(
      max_calls = NULL,
      initialize = function(max_calls = 5L) {
        self$max_calls <- as.integer(max_calls)
        private$call_count <- 0L
      },
      acquire = function() {
        if (private$call_count >= self$max_calls) {
          message("Rate limit reached! Resetting counter required.")
          return(FALSE)
        }
        private$call_count <- private$call_count + 1L
        TRUE
      },
      reset = function() {
        private$call_count <- 0L
        invisible(self)
      },
      status = function() {
        cat(sprintf("<R6 RateLimiter: %d / %d calls used>\n",
                    private$call_count, self$max_calls))
      }
    ),
    private = list(
      call_count = 0L
    )
  )

  limiter <- RateLimiter$new(max_calls = 3)
  limiter$acquire()
  limiter$acquire()
  limiter$status() # Mutated in-place without reassignment (`<-`)!
}

