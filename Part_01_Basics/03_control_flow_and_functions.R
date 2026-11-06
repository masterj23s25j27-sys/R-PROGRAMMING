# ==============================================================================
# PART 01: R BASICS & FUNDAMENTALS
# Module 03: Control Flow, Loops, and Functions
# ==============================================================================
# In this module, you will master:
# 1. Conditional Branching (`if`, `else if`, `else`, vectorized `ifelse()`, `switch()`)
# 2. Iteration (`for`, `while`, `repeat`, `break`, `next`, `seq_along()`)
# 3. Writing Custom Functions (Arguments, Defaults, Ellipsis `...`, Scoping)
# 4. Base R Functional Programming (`lapply`, `sapply`, `vapply`, `apply`, `mapply`)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CONDITIONAL BRANCHING
# ------------------------------------------------------------------------------

# 1.1 Standard Scalar `if / else if / else`
temperature_c <- 28

if (temperature_c >= 35) {
  weather_status <- "Extreme Heat"
} else if (temperature_c >= 25) {
  weather_status <- "Warm & Pleasant"
} else if (temperature_c >= 10) {
  weather_status <- "Cool"
} else {
  weather_status <- "Cold"
}
cat("Temp:", temperature_c, "C -> Status:", weather_status, "\n")

# 1.2 Vectorized Conditional: `ifelse(test, yes, no)`
# Never use a scalar `if ()` on a vector of length > 1! Use `ifelse()` instead.
exam_scores <- c(92, 48, 76, 59, 88, 34, 65)
pass_fail <- ifelse(exam_scores >= 60, "PASS", "FAIL")
names(pass_fail) <- exam_scores
print(pass_fail)

# Nested vectorized `ifelse()` for grading tiers
letter_grades <- ifelse(exam_scores >= 90, "A",
                 ifelse(exam_scores >= 80, "B",
                 ifelse(exam_scores >= 70, "C",
                 ifelse(exam_scores >= 60, "D", "F"))))
print(data.frame(score = exam_scores, status = pass_fail, grade = letter_grades))

# 1.3 `switch()` Statement for Clean Multi-Way Dispatch
calculate_metric <- function(x, type = "mean") {
  switch(
    type,
    mean   = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd     = sd(x, na.rm = TRUE),
    iqr    = IQR(x, na.rm = TRUE),
    stop("Unsupported metric type: ", type)
  )
}
cat("Switch (median):", calculate_metric(exam_scores, "median"), "\n")

# ------------------------------------------------------------------------------
# 2. ITERATION & LOOPS
# ------------------------------------------------------------------------------
# Best Practice in R:
# 1. Prefer vectorized operations first.
# 2. When writing `for` loops, ALWAYS pre-allocate the output container (never grow
#    vectors inside a loop with `c(res, new_val)`) and use `seq_along(x)` instead
#    of `1:length(x)` (which fails if `length(x) == 0`, producing `c(1, 0)`!).

# 2.1 Safe `for` Loop with Pre-allocation & `seq_along()`
raw_values <- c(10, 25, 40, 55, 70)
normalized <- numeric(length(raw_values)) # Pre-allocate numeric vector of zeros

min_v <- min(raw_values)
max_v <- max(raw_values)

for (i in seq_along(raw_values)) {
  normalized[i] <- (raw_values[i] - min_v) / (max_v - min_v)
}
cat("Min-Max Normalized:", normalized, "\n")

# 2.2 `next` (skip iteration) and `break` (exit loop early)
cat("Odd squares up to 50: ")
for (k in 1:20) {
  if (k %% 2 == 0) next        # Skip even numbers
  sq <- k^2
  if (sq > 50) break           # Stop once square exceeds 50
  cat(sq, "")
}
cat("\n")

# 2.3 `while` Loop (Newton-Raphson Square Root Approximation)
target <- 64
guess  <- target / 2
tol    <- 1e-7
iter   <- 0L

while (abs(guess^2 - target) > tol) {
  guess <- 0.5 * (guess + target / guess)
  iter  <- iter + 1L
}
cat(sprintf("Newton-Raphson sqrt(%d) = %.6f in %d iterations\n", target, guess, iter))

# ------------------------------------------------------------------------------
# 3. WRITING CUSTOM FUNCTIONS IN R
# ------------------------------------------------------------------------------
# Functions in R are first-class objects: they can be assigned to variables,
# stored in lists, passed as arguments, and returned from other functions.

# 3.1 Anatomy of a Robust Function (Default arguments, Input Validation, Explicit/Implicit Return)
compute_confidence_interval <- function(x, conf_level = 0.95, na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector.")
  }
  if (conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be strictly between 0 and 1.")
  }
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  n <- length(x)
  if (n < 2L) {
    warning("Need at least 2 observations to compute standard error. Returning NA.")
    return(c(lower = NA_real_, mean = NA_real_, upper = NA_real_))
  }

  sample_mean <- mean(x)
  sample_se   <- sd(x) / sqrt(n)
  alpha       <- 1 - conf_level
  t_crit      <- qt(1 - alpha / 2, df = n - 1)
  margin      <- t_crit * sample_se

  # The last evaluated expression is automatically returned
  c(
    lower = sample_mean - margin,
    mean  = sample_mean,
    upper = sample_mean + margin
  )
}

set.seed(42)
sample_data <- rnorm(50, mean = 100, sd = 15)
ci_95 <- compute_confidence_interval(sample_data, conf_level = 0.95)
print(round(ci_95, 2))

# 3.2 Passing Arbitrary Arguments via Ellipsis (`...`)
summarize_and_transform <- function(x, fn = log, ...) {
  transformed <- fn(x)
  mean(transformed, ...)
}
cat("Mean of log with na.rm = TRUE:",
    summarize_and_transform(c(1, 10, 100, NA), fn = log10, na.rm = TRUE), "\n")

# 3.3 Anonymous / Lambda Functions (R 4.1+ shorthand `\()`)
nums <- 1:5
cubed <- sapply(nums, \(x) x^3)
cat("Cubed via R 4.1+ shorthand \\(x):", cubed, "\n")

# ------------------------------------------------------------------------------
# 4. BASE R APPLY FAMILY (Vectorized Functional Iteration)
# ------------------------------------------------------------------------------

# 4.1 `apply(X, MARGIN, FUN)`: Apply over Matrix/Array margins (1 = rows, 2 = cols)
sales_matrix <- matrix(
  c(120, 150, 130,
    200, 180, 210,
    90,  110, 95,
    300, 320, 290),
  nrow = 4, byrow = TRUE,
  dimnames = list(c("North", "South", "East", "West"), c("Q1", "Q2", "Q3"))
)
cat("Row Totals (by Region):", apply(sales_matrix, MARGIN = 1, FUN = sum), "\n")
cat("Col Averages (by Quarter):", round(apply(sales_matrix, MARGIN = 2, FUN = mean), 1), "\n")

# 4.2 `lapply()`, `sapply()`, and Type-Safe `vapply()`
sensor_readings <- list(
  sensor_A = c(21.2, 21.8, 22.1, 21.5),
  sensor_B = c(19.4, 19.9, 20.1, 19.8, 19.6),
  sensor_C = c(25.0, 24.8, 25.3)
)

# lapply ALWAYS returns a list
list_means <- lapply(sensor_readings, mean)
# sapply simplifies to a vector/matrix if possible
vec_means  <- sapply(sensor_readings, mean)
# vapply enforces an explicit output template (safer and faster in production code!)
safe_means <- vapply(sensor_readings, mean, FUN.VALUE = numeric(1))

print(safe_means)

# 4.3 `tapply(X, INDEX, FUN)`: Grouped aggregation by factor levels
 print(tapply(mtcars$mpg, mtcars$cyl, mean))
