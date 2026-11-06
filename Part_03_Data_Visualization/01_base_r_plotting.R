# ==============================================================================
# PART 03: DATA VISUALIZATION & GRAPHICS
# Module 01: Base R Graphics System (`plot`, `hist`, `boxplot`, `barplot`, `par`)
# ==============================================================================
# Base R has an extraordinarily flexible "Artist's Palette" plotting model:
# 1. High-level plotting functions (`plot`, `hist`, `boxplot`, `barplot`, `pairs`)
#    initialize a new canvas.
# 2. Low-level plotting functions (`lines`, `points`, `abline`, `polygon`, `text`,
#    `legend`, `axis`) layer additional elements onto the active canvas.
# 3. `par()` controls global graphical parameters (multi-panel grids `mfrow`,
#    margins `mar`, colors, font sizes).
# ==============================================================================

# Save plots to a temporary/output PDF or PNG so this script runs cleanly both
# interactively in RStudio and non-interactively via `Rscript`.
plot_dir <- file.path(tempdir(), "r_course_plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
pdf_path <- file.path(plot_dir, "part03_base_r_graphics.pdf")
pdf(file = pdf_path, width = 11, height = 8.5)

# ------------------------------------------------------------------------------
# 1. MULTI-PANEL DASHBOARD LAYOUT WITH `par(mfrow = c(2, 2))`
# ------------------------------------------------------------------------------
old_par <- par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1.5), las = 1)

# 1.1 Scatter Plot + OLS Regression Line + LOWESS Smooth Curve
plot(
  mtcars$wt, mtcars$mpg,
  pch  = 19,
  cex  = 1.3,
  col  = ifelse(mtcars$cyl == 4, "#1b9e77", ifelse(mtcars$cyl == 6, "#d95f02", "#7570b3")),
  xlab = "Weight (1000 lbs)",
  ylab = "Miles Per Gallon (MPG)",
  main = "1. Scatter Plot with OLS & LOWESS Fit"
)
grid(col = "gray85", lty = "dotted")
# Add linear regression trend line
abline(lm(mpg ~ wt, data = mtcars), col = "firebrick", lwd = 2, lty = 2)
# Add non-parametric LOWESS smooth curve
lines(lowess(mtcars$wt, mtcars$mpg), col = "navy", lwd = 2)
legend(
  "topright",
  legend = c("4 Cyl", "6 Cyl", "8 Cyl", "OLS Fit", "LOWESS"),
  col    = c("#1b9e77", "#d95f02", "#7570b3", "firebrick", "navy"),
  pch    = c(19, 19, 19, NA, NA),
  lty    = c(NA, NA, NA, 2, 1),
  lwd    = c(NA, NA, NA, 2, 2),
  bty    = "n",
  cex    = 0.85
)

# 1.2 Histogram + Kernel Density Overlay + Normal Curve
set.seed(2026)
sim_returns <- rnorm(500, mean = 0.08, sd = 0.15)
hist(
  sim_returns,
  breaks = 25,
  freq   = FALSE, # Plot probability density instead of raw counts
  col    = "steelblue3",
  border = "white",
  xlab   = "Annualized Return",
  ylab   = "Density",
  main   = "2. Histogram & Kernel Density Overlay"
)
lines(density(sim_returns), col = "darkorange2", lwd = 2.5)
abline(v = mean(sim_returns), col = "red", lwd = 2, lty = 2)

# 1.3 Grouped Boxplot with Notches & Jittered Stripchart Overlay
boxplot(
   Sepal.Length ~ Species,
  data   = iris,
  notch  = TRUE,
  col    = c("#66c2a5", "#fc8d62", "#8da0cb"),
  xlab   = "Iris Species",
  ylab   = "Sepal Length (cm)",
  main   = "3. Notched Boxplot + Jittered Points"
)
stripchart(
  Sepal.Length ~ Species,
  data   = iris,
  vertical = TRUE,
  method = "jitter",
  add    = TRUE,
  pch    = 20,
  col    = rgb(0, 0, 0, 0.4)
)

# 1.4 Grouped Barplot with Error Bars / Value Labels
cyl_gear_table <- table(mtcars$cyl, mtcars$gear)
bp <- barplot(
  cyl_gear_table,
  beside = TRUE,
  col    = c("#2b8cbe", "#7bccc4", "#bae4bc"),
  xlab   = "Number of Forward Gears",
  ylab   = "Vehicle Count",
  main   = "4. Grouped Bar Chart (Cylinders by Gear)",
  ylim   = c(0, max(cyl_gear_table) + 3)
)
legend("topright", legend = paste(rownames(cyl_gear_table), "Cyl"),
       fill = c("#2b8cbe", "#7bccc4", "#bae4bc"), bty = "n", cex = 0.85)
# Add count labels above bars
text(x = bp, y = cyl_gear_table + 0.6, labels = cyl_gear_table, cex = 0.85, font = 2)

# Restore default graphical parameters
par(old_par)

# ------------------------------------------------------------------------------
# 2. SCATTERPLOT MATRIX (`pairs`) & CONTOUR / HEATMAP PLOTS
# ------------------------------------------------------------------------------
pairs(
  iris[, 1:4],
  main = "Iris Multivariate Scatterplot Matrix",
  pch  = 21,
  bg   = c("#1b9e77", "#d95f02", "#7570b3")[unclass(iris$Species)]
)

# Topographic 2D Contour Map using built-in `volcano` elevation matrix
filled.contour(
  volcano,
  color.palette = terrain.colors,
  main = "Maunga Whau Volcano Topographic Elevation Heatmap"
)

dev.off()
cat("Saved Base R multi-page graphics gallery to:", pdf_path, "\n")

