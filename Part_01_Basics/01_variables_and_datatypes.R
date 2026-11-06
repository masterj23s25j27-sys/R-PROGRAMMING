# ==============================================================================
# PART 01: R BASICS & FUNDAMENTALS
# Module 01: Variables, Data Types, and Operators
# ==============================================================================
# Welcome to R Programming! In this script, you will learn:
# 1. Variable Assignment (<-, =, assign)
# 2. Core Atomic Data Types (numeric, integer, character, logical, complex, raw)
# 3. Type Inspection & Coercion (class, typeof, is.*, as.*)
# 4. Arithmetic, Relational, Logical, and Miscellaneous Operators
# 5. Special Values in R (NA, NaN, Inf, -Inf, NULL)
# 6. Built-in Math & String Functions
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. VARIABLE ASSIGNMENT & ENVIRONMENT BASICS
# ------------------------------------------------------------------------------
# In R, the idiomatic assignment operator is `<-` (Alt + - in RStudio).
# `=` also works at the top level, but `<-` is universally preferred in R style.

x <- 42
y = 18
assign("z", 100)

# Rightward assignment (rarely used, occasionally seen at the end of pipelines)
250 -> right_assigned

print(x)
cat("x =", x, "| y =", y, "| z =", z, "| right_assigned =", right_assigned, "\n")

# List objects currently in the workspace environment
print(ls())

# ------------------------------------------------------------------------------
# 2. CORE ATOMIC DATA TYPES
# ------------------------------------------------------------------------------

# 2.1 Numeric (Double-precision floating point by default)
num_val <- 3.14159
whole_as_double <- 10
cat("num_val class:", class(num_val), "| typeof:", typeof(num_val), "\n")
cat("whole_as_double class:", class(whole_as_double), "| typeof:", typeof(whole_as_double), "\n")

# 2.2 Integer (Explicitly suffix with `L`)
int_val <- 10L
cat("int_val class:", class(int_val), "| typeof:", typeof(int_val), "\n")

# 2.3 Character (Strings - single or double quotes)
char_val <- "R Programming for Data Science"
char_single <- 'Single quotes work identically'
cat("char_val class:", class(char_val), "| length (elements):", length(char_val), "| characters:", nchar(char_val), "\n")

# 2.4 Logical (Boolean: TRUE or FALSE; T/F are shorthand but can be overwritten, so always use TRUE/FALSE!)
log_true <- TRUE
log_false <- FALSE
cat("log_true class:", class(log_true), "| typeof:", typeof(log_true), "\n")

# 2.5 Complex (Numbers with real and imaginary parts)
comp_val <- 4 + 3i
cat("comp_val:", comp_val, "| Real:", Re(comp_val), "| Imaginary:", Im(comp_val), "| Modulus:", Mod(comp_val), "\n")

# 2.6 Raw (Raw bytes)
raw_val <- charToRaw("Hello")
print(raw_val)
cat("Back to char:", rawToChar(raw_val), "\n")

# ------------------------------------------------------------------------------
# 3. TYPE INSPECTION & COERCION
# ------------------------------------------------------------------------------
# Checking types with `is.*()` functions
cat("is.numeric(num_val):", is.numeric(num_val), "\n")
cat("is.integer(whole_as_double):", is.integer(whole_as_double), "\n")
cat("is.integer(int_val):", is.integer(int_val), "\n")
cat("is.character(char_val):", is.character(char_val), "\n")
cat("is.logical(log_true):", is.logical(log_true), "\n")

# Explicit Coercion with `as.*()` functions
# Coercion hierarchy (most flexible to least): character <- complex <- numeric <- integer <- logical
num_from_char <- as.numeric("123.45")
int_from_num  <- as.integer(99.87)      # Truncates towards zero (99)
char_from_num <- as.character(2026)
log_from_num  <- as.logical(c(0, 1, -5)) # 0 is FALSE, any non-zero number is TRUE
num_from_log  <- as.numeric(c(TRUE, FALSE, TRUE)) # TRUE -> 1, FALSE -> 0

cat("as.numeric('123.45'):", num_from_char, "\n")
cat("as.integer(99.87):", int_from_num, "\n")
cat("as.logical(c(0, 1, -5)):", log_from_num, "\n")
cat("as.numeric(c(TRUE, FALSE, TRUE)):", num_from_log, "\n")

# Failed coercion produces NA with a warning
suppressWarnings({
  bad_coercion <- as.numeric("not_a_number")
  cat("Failed coercion result:", bad_coercion, "\n")
})

# ------------------------------------------------------------------------------
# 4. OPERATORS IN R
# ------------------------------------------------------------------------------

# 4.1 Arithmetic Operators (Vectorized by default!)
a <- 17
b <- 5

cat("Addition (a + b):        ", a + b, "\n")
cat("Subtraction (a - b):     ", a - b, "\n")
cat("Multiplication (a * b):  ", a * b, "\n")
cat("Division (a / b):        ", a / b, "\n")
cat("Exponentiation (a ^ b):  ", a ^ b, " (or a ** b:", a ** b, ")\n")
cat("Modulo / Remainder (%%): ", a %% b, "\n")
cat("Integer Division (%/%):  ", a %/% b, "\n")

# 4.2 Relational (Comparison) Operators
cat("a == b:", a == b, "\n")
cat("a != b:", a != b, "\n")
cat("a > b: ", a > b, "\n")
cat("a <= b:", a <= b, "\n")

# Floating-point comparison pitfall & `all.equal()` / `near()`
fp_check <- (0.1 + 0.2) == 0.3
cat("Direct floating point (0.1 + 0.2 == 0.3):", fp_check, "\n")
cat("Safe floating point check isTRUE(all.equal(0.1 + 0.2, 0.3)):", isTRUE(all.equal(0.1 + 0.2, 0.3)), "\n")

# 4.3 Logical Operators
# `&` and `|` are vectorized (element-wise)
# `&&` and `||` evaluate a single scalar condition (short-circuiting, used in `if ()` statements)
v1 <- c(TRUE, TRUE, FALSE, FALSE)
v2 <- c(TRUE, FALSE, TRUE, FALSE)

cat("Element-wise AND (v1 & v2):", v1 & v2, "\n")
cat("Element-wise OR  (v1 | v2):", v1 | v2, "\n")
cat("Logical NOT      (!v1):    ", !v1, "\n")
cat("Exclusive OR     xor(v1,v2):", xor(v1, v2), "\n")
cat("Scalar short-circuit &&:   ", (a > 10) && (b < 10), "\n")

# 4.4 Miscellaneous Operators
# `:` sequence operator
seq_1_to_10 <- 1:10
# `%in%` membership operator
cat("Is 5 in 1:10? ", 5 %in% seq_1_to_10, "\n")
cat("Are c(3, 15) in 1:10? ", c(3, 15) %in% seq_1_to_10, "\n")

# Native pipe operator `|>` (introduced in R 4.1+)
piped_result <- c(4, 9, 16, 25) |> sqrt() |> mean()
cat("Piped result (mean of sqrt):", piped_result, "\n")

# ------------------------------------------------------------------------------
# 5. SPECIAL VALUES IN R (NA, NaN, Inf, -Inf, NULL)
# ------------------------------------------------------------------------------
# - NA   : Missing / Not Available value (has typed variants: NA_integer_, NA_real_, NA_character_, NA_complex_)
# - NaN  : Not a Number (e.g., 0/0, Inf - Inf). Note: is.na(NaN) is TRUE, but is.nan(NA) is FALSE.
# - Inf  : Positive Infinity (e.g., 1/0)
# - -Inf : Negative Infinity (e.g., -1/0)
# - NULL : Absence of an object / zero-length empty object

missing_val <- NA
not_a_num   <- 0 / 0
pos_inf     <- 1 / 0
neg_inf     <- -1 / 0
null_obj    <- NULL

cat("is.na(NA):       ", is.na(missing_val), "\n")
cat("is.nan(0/0):     ", is.nan(not_a_num), " | is.na(0/0):", is.na(not_a_num), "\n")
cat("is.infinite(1/0):", is.infinite(pos_inf), " | is.finite(1/0):", is.finite(pos_inf), "\n")
cat("is.null(NULL):   ", is.null(null_obj), " | length(NULL):", length(null_obj), "\n")

# NA propagation rule: almost any operation with NA yields NA unless `na.rm = TRUE`
scores <- c(88, 92, NA, 79, 95)
cat("mean(scores) without na.rm:", mean(scores), "\n")
cat("mean(scores, na.rm = TRUE):", mean(scores, na.rm = TRUE), "\n")

# ------------------------------------------------------------------------------
# 6. ESSENTIAL BUILT-IN MATH & STRING FUNCTIONS
# ------------------------------------------------------------------------------
# Math functions
val <- -15.678
cat("abs:", abs(val), "| ceiling:", ceiling(val), "| floor:", floor(val), "| round(2):", round(val, 2), "\n")
cat("sqrt(144):", sqrt(144), "| exp(1):", exp(1), "| log(e):", log(exp(1)), "| log10(1000):", log10(1000), "\n")

# Base string functions
first <- "Ada"
last  <- "Lovelace"
full_name <- paste(first, last)              # Default sep = " "
slug_name <- paste(first, last, sep = "-")   # Custom separator
compact   <- paste0(first, "_", last)        # No separator
formatted <- sprintf("Pioneer: %s %s (Born %d)", first, last, 1815)

cat("paste:   ", full_name, "\n")
cat("paste0:  ", compact, "\n")
cat("sprintf: ", formatted, "\n")
cat("toupper: ", toupper(full_name), " | tolower:", tolower(full_name), "\n")
cat("substr:  ", substr(full_name, start = 1, stop = 3), "\n")
cat("gsub:    ", gsub("Ada", "Countess Ada", full_name), "\n")

# ==============================================================================
# PRACTICE EXERCISES
# ==============================================================================
# Exercise 1: Calculate the compound interest for Principal P = 10000,
# annual rate r = 0.075, compounded n = 12 times per year for t = 5 years:
# Formula: A = P * (1 + r/n)^(n*t)
P <- 10000
r <- 0.075
n <- 12
t <- 5
A <- P * (1 + r / n)^(n * t)
cat(sprintf("Exercise 1 - Final Amount after %d years: $%.2f\n", t, A))

# Exercise 2: Given a vector of raw strings with missing values, coerce to numeric
# and compute the sum of valid numbers.
raw_inputs <- c("45.2", "19.8", "NA", "35.0")
suppressWarnings(clean_nums <- as.numeric(raw_inputs))
cat("Exercise 2 - Sum of valid inputs:", sum(clean_nums, na.rm = TRUE), "\n")
