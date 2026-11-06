# ==============================================================================
# PART 02: DATA WRANGLING & THE TIDYVERSE
# Module 01: Data Import, Export, and Tidy Reshaping (readr, tidyr, data.table)
# ==============================================================================
# In this module, you will learn:
# 1. Generating & Reading/Writing Delimited Files (Base R vs `readr` vs `data.table`)
# 2. Working with JSON, RDS (Native R Serialization), and Multi-Object `.RData`
# 3. Principles of Tidy Data (Hadley Wickham)
# 4. Reshaping Data with `tidyr`: `pivot_longer()`, `pivot_wider()`
# 5. Splitting, Combining, and Completing Missing Combinations (`separate`, `unite`, `complete`, `drop_na`, `replace_na`)
# ==============================================================================

# Helper: Load tidyverse if installed, otherwise provide pure-Base R equivalents too
has_tidyr <- requireNamespace("tidyr", quietly = TRUE)
has_readr <- requireNamespace("readr", quietly = TRUE)

# ------------------------------------------------------------------------------
# 1. DATA IMPORT & EXPORT (CSV, TSV, RDS)
# ------------------------------------------------------------------------------
# Create a temporary directory for safe, reproducible file I/O demonstrations
io_dir <- file.path(tempdir(), "r_course_io")
if (!dir.exists(io_dir)) dir.create(io_dir, recursive = TRUE)

# Sample dataset to export and re-import
orders_df <- data.frame(
  order_id   = 1001:1006,
  customer   = c("Arjun", "Priya", "Liam", "Sofia", "Kenji", "Elena"),
  order_date = as.Date(c("2026-01-15", "2026-01-18", "2026-02-02",
                         "2026-02-10", "2026-02-14", "2026-03-01")),
  amount_usd = c(249.99, 89.50, 1200.00, 45.00, 670.25, 310.80),
  status     = c("Delivered", "Delivered", "Processing", "Cancelled", "Delivered", "Shipped"),
  stringsAsFactors = FALSE
)

# 1.1 Base R CSV I/O (`write.csv` / `read.csv`)
csv_path <- file.path(io_dir, "orders.csv")
write.csv(orders_df, file = csv_path, row.names = FALSE)
orders_from_csv <- read.csv(csv_path, stringsAsFactors = FALSE)
cat("Read", nrow(orders_from_csv), "rows from CSV via Base R.\n")

# 1.2 Fast & Type-Safe Serialization with `.rds` (`saveRDS` / `readRDS`)
# Why `.rds`? Unlike CSV, `saveRDS()` preserves exact R data types (factors, dates,
# attributes, list-columns) and compresses data on disk!
rds_path <- file.path(io_dir, "orders.rds")
saveRDS(orders_df, file = rds_path)
orders_from_rds <- readRDS(rds_path)
cat("Does RDS preserve exact Date class?", inherits(orders_from_rds$order_date, "Date"), "\n")

# 1.3 `readr` (Tidyverse fast reader) if available
if (has_readr) {
  library(readr)
  readr_path <- file.path(io_dir, "orders_readr.csv")
  write_csv(orders_df, readr_path)
  orders_tibble <- read_csv(readr_path, show_col_types = FALSE)
  print(orders_tibble)
}

# ------------------------------------------------------------------------------
# 2. PRINCIPLES OF TIDY DATA & RESHAPING (`tidyr`)
# ------------------------------------------------------------------------------
# A dataset is "Tidy" if and only if:
# 1. Every variable has its own column.
# 2. Every observation has its own row.
# 3. Every value has its own cell.

# Wide (Untidy) Quarterly Revenue Data: Quarters are spread across column names
wide_revenue <- data.frame(
  store_id = c("S01", "S02", "S03"),
  city     = c("Mumbai", "Bengaluru", "Delhi"),
  Q1_2026  = c(450, 520, 390),
  Q2_2026  = c(480, 560, 410),
  Q3_2026  = c(510, 610, 440),
  Q4_2026  = c(590, 680, 495),
  stringsAsFactors = FALSE
)
cat("\n--- Original Wide Table ---\n")
print(wide_revenue)

if (has_tidyr) {
  library(tidyr)

  # 2.1 `pivot_longer()`: Wide -> Long (Tidy format)
  long_revenue <- wide_revenue |>
    pivot_longer(
      cols      = starts_with("Q"),
      names_to  = c("quarter", "year"),
      names_sep = "_",
      values_to = "revenue_k"
    )
  cat("\n--- Tidy Long Table via `tidyr::pivot_longer()` ---\n")
  print(long_revenue)

  # 2.2 `pivot_wider()`: Long -> Wide (e.g., for summary presentation tables)
  reconstructed_wide <- long_revenue |>
    pivot_wider(
      names_from  = c(quarter, year),
      names_sep   = "_",
      values_from = revenue_k
    )
  cat("\n--- Reconstructed Wide Table via `tidyr::pivot_wider()` ---\n")
  print(reconstructed_wide)

  # 2.3 `separate_wider_delim()` / `separate()` and `unite()`
  sku_df <- tibble(
    sku_code = c("TECH-LAP-2026-01", "HOME-COF-2025-09", "TECH-MON-2026-04"),
    stock    = c(45, 12, NA)
  )

  tidy_sku <- sku_df |>
    separate(sku_code, into = c("category", "product", "year", "month"), sep = "-") |>
    replace_na(list(stock = 0))

  cat("\n--- Separated & Missing-Replaced SKU Table ---\n")
  print(tidy_sku)
} else {
  # Base R `reshape()` equivalent if `tidyr` is not yet installed
  long_base <- reshape(
    wide_revenue,
    varying   = c("Q1_2026", "Q2_2026", "Q3_2026", "Q4_2026"),
    v.names   = "revenue_k",
    timevar   = "quarter",
    times     = c("Q1", "Q2", "Q3", "Q4"),
    direction = "long"
  )
  print(long_base)
}
