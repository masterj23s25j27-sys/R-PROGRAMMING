# ==============================================================================
# PART 03: DATA VISUALIZATION & GRAPHICS
# Module 03: Advanced Visualizations — Heatmaps, Time-Series Ribbons,
#            Dumbbell Charts, Annotations, and Multi-Plot Layouts (`patchwork`)
# ==============================================================================
# Beyond standard bar and scatter plots, senior data scientists frequently build:
# 1. Correlation Heatmaps with direct text annotations (`geom_tile` + `geom_text`)
# 2. Time-Series Uncertainty Fan / Ribbon Charts (`geom_ribbon` + `geom_line`)
# 3. Cleveland Dot / Dumbbell Plots for Before-vs-After comparisons (`geom_segment`)
# 4. Custom Corporate Themes & Multi-Panel Composition (`patchwork`)
# ==============================================================================

if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  # ----------------------------------------------------------------------------
  # 1. ANNOTATED CORRELATION MATRIX HEATMAP (`geom_tile` + `geom_text`)
  # ----------------------------------------------------------------------------
  num_cols <- mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec")]
  cor_mat  <- round(cor(num_cols), 2)

  # Convert correlation matrix to long data frame without requiring external packages
  cor_long <- as.data.frame(as.table(cor_mat))
  names(cor_long) <- c("Var1", "Var2", "Correlation")

  p_heatmap <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = sprintf("%.2f", Correlation)), color = "black", size = 4, fontface = "bold") +
    scale_fill_gradient2(
      low = "#d73027", mid = "white", high = "#1a9850",
      midpoint = 0, limits = c(-1, 1), name = "Pearson r"
    ) +
    coord_fixed() +
    labs(
      title    = "Motor Trend Car Variables — Pearson Correlation Matrix",
      subtitle = "Annotated heatmap built with geom_tile() and scale_fill_gradient2()",
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank(), plot.title = element_text(face = "bold"))

  # ----------------------------------------------------------------------------
  # 2. TIME-SERIES FORECAST WITH 80% & 95% CONFIDENCE RIBBONS (`geom_ribbon`)
  # ----------------------------------------------------------------------------
  set.seed(42)
  months_seq <- seq(as.Date("2024-01-01"), by = "month", length.out = 36)
  trend      <- seq(100, 185, length.out = 36) + 12 * sin(seq(0, 6 * pi, length.out = 36))
  ts_df <- data.frame(
    date    = months_seq,
    actual  = ifelse(1:36 <= 24, trend + rnorm(36, 0, 4), NA_real_),
    mean_fc = ifelse(1:36 >= 24, trend, NA_real_),
    lo_80   = ifelse(1:36 >= 24, trend - 1.28 * seq(3, 14, length.out = 36), NA_real_),
    hi_80   = ifelse(1:36 >= 24, trend + 1.28 * seq(3, 14, length.out = 36), NA_real_),
    lo_95   = ifelse(1:36 >= 24, trend - 1.96 * seq(3, 14, length.out = 36), NA_real_),
    hi_95   = ifelse(1:36 >= 24, trend + 1.96 * seq(3, 14, length.out = 36), NA_real_)
  )

  p_forecast <- ggplot(ts_df, aes(x = date)) +
    geom_ribbon(aes(ymin = lo_95, ymax = hi_95), fill = "#3182bd", alpha = 0.20, na.rm = TRUE) +
    geom_ribbon(aes(ymin = lo_80, ymax = hi_80), fill = "#3182bd", alpha = 0.35, na.rm = TRUE) +
    geom_line(aes(y = actual), color = "#252525", linewidth = 1, na.rm = TRUE) +
    geom_line(aes(y = mean_fc), color = "#08519c", linewidth = 1.1, linetype = "dashed", na.rm = TRUE) +
    geom_vline(xintercept = as.Date("2025-12-01"), linetype = "dotted", color = "firebrick", linewidth = 0.8) +
    annotate("text", x = as.Date("2025-11-01"), y = 205, label = "Forecast Horizon ->",
             color = "firebrick", fontface = "bold", hjust = 1) +
    labs(
      title    = "Monthly SaaS ARR Trajectory & 12-Month Probabilistic Forecast",
      subtitle = "Historical actuals (solid) with 80% and 95% prediction intervals (shaded ribbons)",
      x        = "Date",
      y        = "ARR Index ($M)"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))

  # ----------------------------------------------------------------------------
  # 3. DUMBBELL / CLEVELAND DOT PLOT FOR BEFORE-AND-AFTER COMPARISONS
  # ----------------------------------------------------------------------------
  dumbbell_df <- data.frame(
    department = factor(c("Engineering", "Product", "Data Science", "Marketing", "Sales", "Customer Success"),
                        levels = c("Customer Success", "Sales", "Marketing", "Product", "Engineering", "Data Science")),
    score_2025 = c(72, 68, 78, 64, 61, 58),
    score_2026 = c(88, 82, 94, 75, 79, 74)
  )

  p_dumbbell <- ggplot(dumbbell_df) +
    geom_segment(aes(x = score_2025, xend = score_2026, y = department, yend = department),
                 color = "gray65", linewidth = 1.8) +
    geom_point(aes(x = score_2025, y = department), color = "#e08214", size = 4.5) +
    geom_point(aes(x = score_2026, y = department), color = "#1b7837", size = 4.5) +
    labs(
      title    = "Employee AI Adoption Score: 2025 (Orange) vs. 2026 (Green)",
      subtitle = "Dumbbell chart highlighting year-over-year gain across departments",
      x        = "Adoption Index (0 - 100)",
      y        = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))

  out_dir <- file.path(tempdir(), "r_course_plots")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  ggsave(file.path(out_dir, "p4_heatmap.png"), p_heatmap, width = 7, height = 6, dpi = 150)
  ggsave(file.path(out_dir, "p5_forecast.png"), p_forecast, width = 9, height = 5, dpi = 150)
  ggsave(file.path(out_dir, "p6_dumbbell.png"), p_dumbbell, width = 8, height = 4.5, dpi = 150)
  cat("Saved advanced visualizations to:", out_dir, "\n")
}

