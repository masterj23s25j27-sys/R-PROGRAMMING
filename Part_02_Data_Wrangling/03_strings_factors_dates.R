# ==============================================================================
# PART 02: DATA WRANGLING & THE TIDYVERSE
# Module 03: Strings, Regular Expressions, Factors, and Dates/Times
#            (`stringr`, `forcats`, `lubridate`, plus Base R equivalents)
# ==============================================================================
# Real-world datasets are full of messy text, unordered categorical labels,
# and inconsistent date/timestamp formats. In this script, you will learn:
# 1. String Manipulation & Regular Expressions (`stringr` & Base R `grep`/`gsub`)
# 2. Factor Manipulation (`forcats`: `fct_reorder`, `fct_lump`, `fct_recode`)
# 3. Date & Time Parsing, Extraction, and Arithmetic (`lubridate` & `POSIXct`)
# ==============================================================================

has_stringr   <- requireNamespace("stringr", quietly = TRUE)
has_forcats   <- requireNamespace("forcats", quietly = TRUE)
has_lubridate <- requireNamespace("lubridate", quietly = TRUE)

# ------------------------------------------------------------------------------
# 1. STRING MANIPULATION & REGULAR EXPRESSIONS (`stringr` & Base R)
# ------------------------------------------------------------------------------
raw_logs <- c(
  "[2026-03-15 09:12:44] INFO  user_id=4821 action=LOGIN ip=192.168.1.10",
  "[2026-03-15 09:14:02] WARN  user_id=1093 action=FAILED_AUTH ip=10.0.0.45",
  "[2026-03-15 09:18:19] ERROR user_id=4821 action=PAYMENT_TIMEOUT ip=192.168.1.10",
  "[2026-03-15 09:21:55] INFO  user_id=7732 action=LOGOUT ip=172.16.0.8"
)

# 1.1 Base R Regex (`grepl`, `regmatches`, `sub`, `gsub`)
has_error_or_warn <- grepl("WARN|ERROR", raw_logs)
cat("Logs with WARN or ERROR:", which(has_error_or_warn), "\n")

# Extracting user_id via Base R capture groups
user_ids_base <- as.integer(sub(".*user_id=([0-9]+).*", "\\1", raw_logs))
cat("Extracted user IDs (Base R):", user_ids_base, "\n")

# 1.2 `stringr` Pipeline (Consistent `str_*` naming, data-first argument)
if (has_stringr) {
  library(stringr)

  parsed_logs <- data.frame(
    timestamp = str_extract(raw_logs, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}"),
    level     = str_extract(raw_logs, "(?<=\\] )\\w+"),
    user_id   = as.integer(str_match(raw_logs, "user_id=(\\d+)")[, 2]),
    action    = str_match(raw_logs, "action=([A-Z_]+)")[, 2],
    ip_addr   = str_extract(raw_logs, "\\d+\\.\\d+\\.\\d+\\.\\d+"),
    stringsAsFactors = FALSE
  )
  cat("\n--- Parsed Server Logs via `stringr` Regex ---\n")
  print(parsed_logs)

  # String cleaning utilities
  messy_names <- c("  dr. jane GOODALL ", "  prof. alan TURING  ", "ada lovelace ")
  clean_names <- messy_names |>
    str_trim() |>
    str_squish() |>
    str_to_title()
  cat("Cleaned Names:", paste(clean_names, collapse = " | "), "\n")
}

# ------------------------------------------------------------------------------
# 2. CATEGORICAL FACTOR WRANGLING (`forcats` & Base R)
# ------------------------------------------------------------------------------
browsers <- c(
  rep("Chrome", 52), rep("Safari", 25), rep("Edge", 12),
  rep("Firefox", 8), rep("Brave", 2), rep("Opera", 1)
)

if (has_forcats) {
  library(forcats)

  browser_fct <- factor(browsers)

  # Order factor levels by frequency (highest first)
  by_freq <- fct_infreq(browser_fct)
  cat("\nFactor levels ordered by frequency:\n")
  print(table(by_freq))

  # Lump rare categories into "Other" (keep top 3)
  lumped <- fct_lump_n(browser_fct, n = 3, other_level = "Other Browsers")
  cat("\nLumped top 3 browsers + Other:\n")
  print(table(lumped))
} else {
  print(sort(table(browsers), decreasing = TRUE))
}

# ------------------------------------------------------------------------------
# 3. DATES, TIMES, AND TIME ZONES (`lubridate` & Base R `Date` / `POSIXct`)
# ------------------------------------------------------------------------------
# Base R has two primary datetime classes:
# - `Date`    : Calendar dates stored internally as days since 1970-01-01 (Unix Epoch)
# - `POSIXct` : Date-times stored as seconds since 1970-01-01 00:00:00 UTC

d1 <- as.Date("2026-01-01")
d2 <- as.Date("2026-11-07")
cat("\nDays between 2026-01-01 and 2026-11-07:", as.numeric(d2 - d1), "days\n")

# Formatting dates with `strftime` codes (%Y = 4-digit year, %B = Full month, %d = day)
cat("Formatted Date:", format(d2, "%A, %B %d, %Y"), "\n")

if (has_lubridate) {
  library(lubridate)

  # Effortless parsing regardless of delimiter
  parsed_ymd <- ymd("2026/07/24")
  parsed_mdy <- mdy("November 7th, 2026")
  parsed_dmy <- dmy("15-Aug-2026")
  parsed_dt  <- ymd_hms("2026-11-07 14:30:00", tz = "Asia/Kolkata")

  cat("Parsed Date-Time (IST):", format(parsed_dt, "%Y-%m-%d %H:%M:%S %Z"), "\n")
  cat("Converted to UTC:      ", format(with_tz(parsed_dt, "UTC"), "%Y-%m-%d %H:%M:%S %Z"), "\n")

  # Extracting components & Date Arithmetic
  cat("Year:", year(parsed_dt), "| Month:", as.character(month(parsed_dt, label = TRUE)),
      "| Weekday:", as.character(wday(parsed_dt, label = TRUE)), "\n")

  # Adding periods vs durations
  next_quarter <- parsed_ymd + months(3) + days(15)
  cat("2026-07-24 + 3 months + 15 days =", as.character(next_quarter), "\n")
}
