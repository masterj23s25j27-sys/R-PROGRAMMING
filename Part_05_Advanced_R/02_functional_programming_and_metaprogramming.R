# ==============================================================================
# PART 05: ADVANCED R PROGRAMMING
# Module 02: Functional Programming (`purrr` & Higher-Order Functions) and
#            Metaprogramming / Non-Standard Evaluation (NSE)
# ==============================================================================
# In this module, you will master:
# 1. Closures & Function Factories (Functions that manufacture and return functions)
# 2. Higher-Order Functions (`Reduce`, `Filter`, `Map`, Function Composition)
# 3. `purrr` Type-Stable Iteration (`map`, `map_dbl`, `map_chr`, `map2`, `pmap`, `safely`)
# 4. Environments & Lexical Scoping Rules
# 5. Metaprogramming: Expressions, `substitute()`, `eval()`, `bquote()`, and
#    Tidy Evaluation (`{{ }}` curly-curly embrace operator)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CLOSURES & FUNCTION FACTORIES
# ------------------------------------------------------------------------------
# A function factory is a function that creates and returns another function,
# capturing its enclosing environment (lexical scoping).

make_power_fn <- function(exponent) {
  force(exponent) # Avoid lazy evaluation surprises
  function(x) {
    x ^ exponent
  }
}

square <- make_power_fn(2)
cube   <- make_power_fn(3)
cat("square(9):", square(9), "| cube(4):", cube(4), "\n")

# Stateful Closure using the Super-Assignment operator (`<<-`)
make_ema_tracker <- function(alpha = 0.3) {
  current_ema <- NULL
  function(new_value) {
    if (is.null(current_ema)) {
      current_ema <<- new_value
    } else {
      current_ema <<- alpha * new_value + (1 - alpha) * current_ema
    }
    current_ema
  }
}

ema <- make_ema_tracker(alpha = 0.25)
stream <- c(100, 108, 104, 112, 120)
ema_values <- vapply(stream, ema, numeric(1))
cat("Streaming Exponential Moving Average:", round(ema_values, 2), "\n")

# ------------------------------------------------------------------------------
# 2. HIGHER-ORDER FUNCTIONS IN BASE R (`Reduce`, `Filter`, `Map`)
# ------------------------------------------------------------------------------
# Find the intersection of multiple sets using `Reduce()`
customer_cohorts <- list(
  jan = c("C1", "C2", "C3", "C4", "C5"),
  feb = c("C2", "C3", "C5", "C6"),
  mar = c("C1", "C2", "C3", "C5", "C7")
)
retained_all_months <- Reduce(intersect, customer_cohorts)
cat("Customers retained across ALL 3 months (`Reduce(intersect)`):", retained_all_months, "\n")

# Compute cumulative product or custom fold with `Reduce(..., accumulate = TRUE)`
running_balance <- Reduce(function(acc, r) acc * (1 + r),
                          c(0.05, -0.02, 0.08, 0.04),
                          init = 1000, accumulate = TRUE)
cat("Running portfolio balance:", round(running_balance, 2), "\n")

# ------------------------------------------------------------------------------
# 3. `purrr` TYPE-STABLE ITERATION & ERROR-SAFE WRAPPERS (`safely`, `possibly`)
# ------------------------------------------------------------------------------
if (requireNamespace("purrr", quietly = TRUE)) {
  library(purrr)

  # Split `mtcars` by cylinder count and fit a linear model to each subgroup
  by_cyl <- split(mtcars, mtcars$cyl)
  r_squared_by_cyl <- by_cyl |>
    map(\(df) lm(mpg ~ wt, data = df)) |>
    map(summary) |>
    map_dbl("r.squared")

  cat("\n--- R-squared by Cylinder Group via `purrr::map_dbl()` ---\n")
  print(round(r_squared_by_cyl, 4))

  # Resilient error handling with `possibly()` (never crashes a long batch pipeline!)
  safe_log <- possibly(log, otherwise = NA_real_)
  mixed_inputs <- list(10, 100, "corrupted_string", 1000)
  log_results <- map_dbl(mixed_inputs, safe_log)
  cat("Resilient batch output with `possibly()`:", log_results, "\n")
}

# ------------------------------------------------------------------------------
# 4. METAPROGRAMMING & NON-STANDARD EVALUATION (NSE)
# ------------------------------------------------------------------------------
# R has the rare ability to capture unevaluated code expressions (Abstract Syntax
# Trees) before evaluating them! This is how `subset()`, `with()`, `ggplot2`, and
# `dplyr` allow you to write bare column names like `mpg > 20` instead of `df$mpg > 20`.

# Build a custom `my_subset()` function using `substitute()` and `eval()`
my_subset <- function(df, condition) {
  cond_call <- substitute(condition) # Capture the raw expression without evaluating it
  # Evaluate the expression inside the `df` data mask, falling back to caller's environment
  keep_rows <- eval(cond_call, envir = df, enclos = parent.frame())
  df[keep_rows & !is.na(keep_rows), , drop = FALSE]
}

fast_efficient_cars <- my_subset(mtcars, mpg > 25 & hp > 90)
cat("\n--- Custom Non-Standard Evaluation `my_subset(mtcars, mpg > 25 & hp > 90)` ---\n")
print(fast_efficient_cars[, c("mpg", "hp", "wt", "cyl")])

# Inspecting an expression's call tree
expr <- quote(y <- alpha + beta * log(x))
cat("\nAST Call Elements of `y <- alpha + beta * log(x)`:\n")
print(as.list(expr))

