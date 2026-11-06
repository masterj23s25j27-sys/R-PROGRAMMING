# ==============================================================================
# PART 03: DATA VISUALIZATION & GRAPHICS
# Module 02: Grammar of Graphics with `ggplot2` (Fundamentals to Publication-Grade)
# ==============================================================================
# Wilkinson's "Grammar of Graphics" decomposes any statistical plot into 7 layers:
# 1. Data        (`ggplot(data = ...)`)
# 2. Aesthetics  (`aes(x = ..., y = ..., color = ..., size = ..., fill = ...)`)
# 3. Geometries  (`geom_point()`, `geom_line()`, `geom_col()`, `geom_boxplot()`)
# 4. Facets      (`facet_wrap()`, `facet_grid()`)
# 5. Statistics  (`stat_smooth()`, `stat_summary()`)
# 6. Coordinates (`coord_flip()`, `coord_cartesian()`, `coord_polar()`)
# 7. Themes      (`theme_minimal()`, `theme_bw()`, custom `theme()`)
# ==============================================================================

if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  # Prepare a clean copy of `mtcars` with factor labels
  cars_df <- mtcars
  cars_df$cyl_fct  <- factor(cars_df$cyl, labels = c("4 Cylinders", "6 Cylinders", "8 Cylinders"))
  cars_df$trans    <- factor(cars_df$am, labels = c("Automatic", "Manual"))
  cars_df$car_name <- rownames(mtcars)

  # ----------------------------------------------------------------------------
  # 1. MULTIVARIATE SCATTER PLOT WITH CONFIDENCE RIBBONS & AESTHETIC MAPPINGS
  # ----------------------------------------------------------------------------
  p1_scatter <- ggplot(cars_df, aes(x = wt, y = mpg, color = cyl_fct, shape = trans)) +
    geom_point(size = 3.5, alpha = 0.85) +
    geom_smooth(aes(group = 1), method = "lm", formula = y ~ x,
                se = TRUE, color = "gray30", linetype = "dashed", linewidth = 0.9) +
    scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
    labs(
      title    = "Fuel Efficiency vs. Vehicle Weight Across Powertrain Configurations",
      subtitle = "OLS linear trend with 95% confidence interval ribbon (mtcars dataset)",
      x        = "Weight (1,000 lbs)",
      y        = "Fuel Efficiency (Miles Per Gallon)",
      color    = "Engine Size",
      shape    = "Transmission",
      caption  = "Source: 1974 Motor Trend US Magazine | R End-to-End Curriculum"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title      = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    )

  # ----------------------------------------------------------------------------
  # 2. RAIN-CLOUD STYLE DISTRIBUTION: VIOLIN + BOXPLOT + JITTERED POINTS
  # ----------------------------------------------------------------------------
  p2_distribution <- ggplot(iris, aes(x = Species, y = Petal.Length, fill = Species)) +
    geom_violin(alpha = 0.45, trim = FALSE, color = NA) +
    geom_boxplot(width = 0.18, outlier.shape = NA, alpha = 0.8, color = "gray20") +
    geom_jitter(width = 0.12, alpha = 0.5, size = 1.8, color = "gray15") +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title    = "Iris Petal Length Distribution by Species",
      subtitle = "Combining kernel density (violin), quartiles (boxplot), and individual observations",
      x        = "Species",
      y        = "Petal Length (cm)"
    ) +
    theme_bw(base_size = 12) +
    theme(legend.position = "none", plot.title = element_text(face = "bold"))

  # ----------------------------------------------------------------------------
  # 3. FACETED SMALL MULTIPLES (`facet_wrap`)
  # ----------------------------------------------------------------------------
  p3_facets <- ggplot(cars_df, aes(x = hp, y = mpg, color = trans)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    facet_wrap(~ cyl_fct, scales = "free_x") +
    scale_color_manual(values = c("#e41a1c", "#377eb8")) +
    labs(
      title    = "Horsepower vs. MPG Faceted by Engine Cylinder Class",
      subtitle = "Small multiples reveal within-group slopes and transmission differences",
      x        = "Gross Horsepower (hp)",
      y        = "Miles Per Gallon (MPG)",
      color    = "Transmission"
    ) +
    theme_light(base_size = 12)

  # Save plots cleanly to tempdir
  out_dir <- file.path(tempdir(), "r_course_plots")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  ggsave(file.path(out_dir, "p1_scatter.png"), p1_scatter, width = 9, height = 6, dpi = 150)
  ggsave(file.path(out_dir, "p2_distribution.png"), p2_distribution, width = 8, height = 5.5, dpi = 150)
  ggsave(file.path(out_dir, "p3_facets.png"), p3_facets, width = 10, height = 5, dpi = 150)
  cat("Saved ggplot2 figures to:", out_dir, "\n")
} else {
  cat("Install `ggplot2` via install.packages('ggplot2') to render Grammar of Graphics plots.\n")
}

