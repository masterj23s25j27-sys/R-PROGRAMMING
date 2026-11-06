# ==============================================================================
# PART 02: DATA WRANGLING & THE TIDYVERSE
# Module 02: Data Manipulation & Relational Joins with `dplyr`
# ==============================================================================
# In this script, you will master the core grammar of data manipulation:
# 1. `select()`, `rename()`, and tidy-selection helpers (`starts_with`, `where`)
# 2. `filter()`, `between()`, `%in%`, and `arrange()` / `desc()`
# 3. `mutate()`, `across()`, and `case_when()` for complex feature engineering
# 4. `group_by()`, `summarise()`, `.by` (per-operation grouping in dplyr 1.1+)
# 5. Window Functions (`row_number`, `min_rank`, `lag`, `lead`, `cumsum`)
# 6. Relational Joins (`inner_join`, `left_join`, `full_join`, `anti_join`, `semi_join`)
# ==============================================================================

has_dplyr <- requireNamespace("dplyr", quietly = TRUE)

# Create a rich synthetic e-commerce dataset for reproducibility
set.seed(101)
transactions <- data.frame(
  tx_id       = 1:12,
  customer_id = c(101, 102, 101, 103, 104, 102, 105, 101, 103, 106, 104, 102),
  region      = c("APAC", "NAM", "APAC", "EMEA", "NAM", "NAM",
                  "APAC", "APAC", "EMEA", "LATAM", "NAM", "NAM"),
  category    = c("Cloud", "AI", "Security", "Cloud", "AI", "Cloud",
                  "Analytics", "AI", "Security", "Cloud", "Analytics", "AI"),
  revenue     = c(1200, 3400, 850, 2100, 4800, 1550, 920, 3100, 1750, 640, 2250, 2900),
  cost        = c(400, 1100, 300, 750, 1400, 500, 350, 950, 600, 250, 800, 900),
  discount_pct = c(0.05, 0.10, 0.00, 0.05, 0.15, 0.00, 0.05, 0.10, 0.00, 0.00, 0.08, 0.12),
  stringsAsFactors = FALSE
)

customers <- data.frame(
  customer_id = c(101, 102, 103, 104, 105, 107),
  company     = c("Nexus Corp", "Vertex AI", "Helios Ltd", "Quantum Inc", "Aurora Tech", "Zephyr IO"),
  tier        = c("Enterprise", "Enterprise", "Mid-Market", "Enterprise", "Startup", "Startup"),
  stringsAsFactors = FALSE
)

if (has_dplyr) {
  library(dplyr)

  # ----------------------------------------------------------------------------
  # 1. SELECT, FILTER, AND ARRANGE
  # ----------------------------------------------------------------------------
  filtered_tx <- transactions |>
    filter(region %in% c("APAC", "NAM"), revenue >= 1000) |>
    select(tx_id, customer_id, region, category, revenue) |>
    arrange(region, desc(revenue))

  cat("\n--- 1. Filtered & Sorted Transactions ---\n")
  print(filtered_tx)

  # ----------------------------------------------------------------------------
  # 2. MUTATE, CASE_WHEN, AND ACROSS
  # ----------------------------------------------------------------------------
  enriched_tx <- transactions |>
    mutate(
      net_revenue = revenue * (1 - discount_pct),
      profit      = net_revenue - cost,
      margin_pct  = round((profit / net_revenue) * 100, 1),
      deal_size   = case_when(
        net_revenue >= 3000 ~ "Large",
        net_revenue >= 1500 ~ "Medium",
        TRUE                ~ "Small"
      )
    )

  cat("\n--- 2. Enriched Transactions with `mutate()` & `case_when()` ---\n")
  print(head(enriched_tx, 6))

  # Using `across()` to apply transformations to multiple columns at once
  scaled_summary <- enriched_tx |>
    summarise(
      across(
        .cols = c(revenue, net_revenue, profit),
        .fns  = list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)),
        .names = "{.col}_{.fn}"
      )
    )
  cat("\n--- Multi-Column Aggregation via `across()` ---\n")
  print(round(scaled_summary, 2))

  # ----------------------------------------------------------------------------
  # 3. GROUPED AGGREGATION (`group_by` + `summarise`)
  # ----------------------------------------------------------------------------
  regional_kpis <- enriched_tx |>
    group_by(region, category) |>
    summarise(
      deals_count   = n(),
      total_net_rev = sum(net_revenue),
      avg_margin    = mean(margin_pct),
      .groups       = "drop"
    ) |>
    arrange(desc(total_net_rev))

  cat("\n--- 3. Regional & Category KPIs ---\n")
  print(regional_kpis)

  # ----------------------------------------------------------------------------
  # 4. WINDOW FUNCTIONS (Ranking, Offsets, Cumulative Sums)
  # ----------------------------------------------------------------------------
  customer_window <- enriched_tx |>
    arrange(customer_id, tx_id) |>
    group_by(customer_id) |>
    mutate(
      tx_seq_num     = row_number(),
      prev_tx_rev    = lag(net_revenue, default = NA_real_),
      rev_change     = net_revenue - prev_tx_rev,
      cumulative_rev = cumsum(net_revenue),
      rev_rank_in_cust = min_rank(desc(net_revenue))
    ) |>
    ungroup()

  cat("\n--- 4. Window Functions per Customer ---\n")
  print(customer_window |> select(customer_id, tx_id, net_revenue, prev_tx_rev, cumulative_rev, rev_rank_in_cust))

  # ----------------------------------------------------------------------------
  # 5. RELATIONAL JOINS (Mutating & Filtering Joins)
  # ----------------------------------------------------------------------------
  # 5.1 `left_join()`: Keep all transactions, attach customer metadata where available
  tx_with_cust <- enriched_tx |>
    left_join(customers, by = "customer_id")

  # 5.2 `anti_join()`: Which transactions belong to a `customer_id` NOT in `customers` table? (Data Quality check!)
  orphan_tx <- transactions |>
    anti_join(customers, by = "customer_id")
  cat("\n--- 5.2 Orphan Transactions (`anti_join`) ---\n")
  print(orphan_tx)

  # 5.3 `anti_join()` reverse: Which customers have ZERO transactions?
  inactive_customers <- customers |>
    anti_join(transactions, by = "customer_id")
  cat("\n--- 5.3 Inactive Customers (`anti_join`) ---\n")
  print(inactive_customers)

  # 5.4 `semi_join()`: Customers who purchased at least one "AI" product
  ai_buyers <- customers |>
    semi_join(transactions |> filter(category == "AI"), by = "customer_id")
  cat("\n--- 5.4 Customers who bought AI (`semi_join`) ---\n")
  print(ai_buyers)

} else {
  # Base R fallback demonstration
  transactions$net_revenue <- transactions$revenue * (1 - transactions$discount_pct)
  agg <- aggregate(net_revenue ~ region + category, data = transactions, FUN = sum)
  print(agg[order(-agg$net_revenue), ])
}
