# ==============================================================================
# PART 05: ADVANCED R PROGRAMMING
# Module 03: Structured Error Handling (`tryCatch`, Custom Conditions),
#            Debugging, Benchmarking, Memory Management, and Parallel Computing
# ==============================================================================
# In this script, you will learn:
# 1. R's Condition System (`message`, `warning`, `stop`, and `tryCatch`, `withCallingHandlers`)
# 2. Custom Condition Classes for Granular Exception Handling
# 3. Performance Benchmarking (`system.time`, Vectorization vs Bad Loop Anti-Patterns)
# 4. Memoisation (Caching Expensive Function Calls via Closures)
# 5. Multi-Core Parallel Computing with Base R `parallel` (`makeCluster`, `parLapply`)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. STRUCTURED ERROR HANDLING (`tryCatch` & `withCallingHandlers`)
# ------------------------------------------------------------------------------
safe_divide_and_log <- function(a, b) {
  tryCatch(
    expr = {
      if (!is.numeric(a) || !is.numeric(b)) {
        stop("Both `a` and `b` must be numeric.", call. = FALSE)
      }
      if (b == 0) {
        stop("Division by zero is undefined.", call. = FALSE)
      }
      ratio <- a / b
      if (ratio < 0) {
        warning("Negative ratio encountered; taking log of absolute value.", call. = FALSE)
        return(log(abs(ratio)))
      }
      log(ratio)
    },
    warning = function(w) {
      cat("[WARN HANDLER]:", conditionMessage(w), "\n")
      suppressWarnings(log(abs(a / b)))
    },
    error = function(e) {
      cat("[ERROR HANDLER]:", conditionMessage(e), "-> Returning NA_real_\n")
      NA_real_
    },
    finally = {
      # Always runs (ideal for closing database/file connections)
    }
  )
}

cat("Valid call (100, 10):  ", safe_divide_and_log(100, 10), "\n")
cat("Negative call (-10, 2):", safe_divide_and_log(-10, 2), "\n")
cat("Zero div (10, 0):      ", safe_divide_and_log(10, 0), "\n")
cat("Bad type ('a', 5):     ", safe_divide_and_log("a", 5), "\n")

# ------------------------------------------------------------------------------
# 2. PERFORMANCE BENCHMARKING: GROWING A VECTOR vs PRE-ALLOCATION vs VECTORIZATION
# ------------------------------------------------------------------------------
n <- 25000L

# Anti-Pattern 1: Growing a vector inside a loop (O(n^2) memory copies!)
t_grow <- system.time({
  res_grow <- numeric(0)
  for (i in seq_len(n)) {
    res_grow <- c(res_grow, sqrt(i))
  }
})

# Pattern 2: Pre-allocating the output vector (O(n))
t_prealloc <- system.time({
  res_pre <- numeric(n)
  for (i in seq_len(n)) {
    res_pre[i] <- sqrt(i)
  }
})

# Pattern 3: Native C-level Vectorization
t_vec <- system.time({
  res_vec <- sqrt(seq_len(n))
})

cat("\n--- Benchmark for n = 25,000 elements (Elapsed Seconds) ---\n")
cat(sprintf("1. Growing vector in loop (Anti-pattern): %.4f sec\n", t_grow["elapsed"]))
cat(sprintf("2. Pre-allocated for-loop:                %.4f sec\n", t_prealloc["elapsed"]))
cat(sprintf("3. Vectorized `sqrt(seq_len(n))`:         %.4f sec\n", t_vec["elapsed"]))

# ------------------------------------------------------------------------------
# 3. MEMOISATION (DYNAMIC PROGRAMMING CACHE VIA R ENVIRONMENTS)
# ------------------------------------------------------------------------------
# R environments (`new.env(hash = TRUE)`) provide O(1) hash-table lookups!
memoise_fn <- function(fn) {
  cache <- new.env(hash = TRUE, parent = emptyenv())
  function(x) {
    key <- as.character(x)
    if (exists(key, envir = cache, inherits = FALSE)) {
      return(get(key, envir = cache, inherits = FALSE))
    }
    val <- fn(x)
    assign(key, val, envir = cache)
    val
  }
}

# Recursive Fibonacci with Memoisation runs in O(n) instead of exponential O(2^n)!
fib_memo <- memoise_fn(function(k) {
  if (k <= 2) return(1)
  fib_memo(k - 1) + fib_memo(k - 2)
})
cat("\nMemoised 50th Fibonacci number:", format(fib_memo(50), scientific = FALSE), "\n")

# ------------------------------------------------------------------------------
# 4. MULTI-CORE PARALLEL COMPUTING (`parallel` Package)
# ------------------------------------------------------------------------------
library(parallel)
detected_cores <- detectCores(logical = TRUE)
cat("Detected CPU logical cores:", detected_cores, "\n")

# Create a 2-worker PSOCK cluster (works cross-platform on Windows, macOS, and Linux!)
cl <- makeCluster(min(2L, detected_cores))

# Run parallel bootstrap simulation across worker processes
parallel_boot_means <- parSapply(cl, 1:4, function(worker_task_id) {
  set.seed(100 + worker_task_id)
  mean(replicate(500, mean(rnorm(100, mean = 50, sd = 10))))
})

stopCluster(cl)
cat("Parallel cluster task results:", round(parallel_boot_means, 4), "\n")

