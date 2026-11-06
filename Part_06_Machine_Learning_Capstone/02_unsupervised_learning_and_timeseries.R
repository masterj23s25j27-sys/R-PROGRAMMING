# ==============================================================================
# PART 06: MACHINE LEARNING, TIME SERIES & PRODUCTION CAPSTONE
# Module 02: Unsupervised Learning (PCA, K-Means, Hierarchical Clustering)
#            & Time-Series Analysis (`ts`, STL Decomposition, ARIMA, Holt-Winters)
# ==============================================================================
# In this script, you will master:
# 1. Principal Component Analysis (`prcomp`): Eigenvalues, Scree Variance Explained,
#    and Biplot Loadings
# 2. K-Means Clustering (`kmeans`): Within-Cluster Sum of Squares (WCSS) Elbow Method
# 3. Hierarchical Agglomerative Clustering (`dist`, `hclust`, `cutree`)
# 4. Time-Series Objects (`ts`), Seasonal Decomposition (`stl`, `decompose`),
#    Autocorrelation (`acf`, `pacf`), ARIMA (`arima`), and Holt-Winters Forecasting
# ==============================================================================

set.seed(42)

# ------------------------------------------------------------------------------
# 1. PRINCIPAL COMPONENT ANALYSIS (PCA) FOR DIMENSIONALITY REDUCTION (`prcomp`)
# ------------------------------------------------------------------------------
# Always set `center = TRUE` and `scale. = TRUE` when features have different units!
us_pca <- prcomp(USArrests, center = TRUE, scale. = TRUE)

cat("--- 1. Principal Component Analysis (`USArrests`) ---\n")
var_explained <- (us_pca$sdev^2) / sum(us_pca$sdev^2)
pca_summary <- data.frame(
  Component        = paste0("PC", seq_along(var_explained)),
  Eigenvalue       = round(us_pca$sdev^2, 3),
  Pct_Variance     = round(var_explained * 100, 2),
  Cum_Pct_Variance = round(cumsum(var_explained) * 100, 2)
)
print(pca_summary)

cat("\nFeature Loadings (Rotation Matrix) on PC1 & PC2:\n")
print(round(us_pca$rotation[, 1:2], 3))

# ------------------------------------------------------------------------------
# 2. K-MEANS CLUSTERING & ELBOW METHOD (`kmeans`)
# ------------------------------------------------------------------------------
iris_scaled <- scale(iris[, 1:4])

# Compute Total Within-Cluster Sum of Squares (tot.withinss) for K = 1 to 8
k_values <- 1:8
wcss <- vapply(k_values, function(k) {
  kmeans(iris_scaled, centers = k, nstart = 25)$tot.withinss
}, numeric(1))

cat("\n--- 2. K-Means Elbow Profile (Total Within-Cluster SS) ---\n")
print(data.frame(K = k_values, Total_WCSS = round(wcss, 2)))

# Fit final K = 3 model with 25 random initializations (`nstart = 25`)
km_3 <- kmeans(iris_scaled, centers = 3, nstart = 25)
cat("\nCluster Assignment vs True Iris Species Cross-Tabulation:\n")
print(table(Cluster = km_3$cluster, True_Species = iris$Species))

# ------------------------------------------------------------------------------
# 3. HIERARCHICAL AGGLOMERATIVE CLUSTERING (`dist` + `hclust` + `cutree`)
# ------------------------------------------------------------------------------
dist_matrix <- dist(scale(USArrests), method = "euclidean")
hc_ward     <- hclust(dist_matrix, method = "ward.D2")

# Cut dendrogram into 4 clusters
state_clusters <- cutree(hc_ward, k = 4)
cat("\n--- 3. Hierarchical Clustering (Ward's D2, k = 4) State Counts ---\n")
print(table(state_clusters))

# ------------------------------------------------------------------------------
# 4. TIME-SERIES DECOMPOSITION, ARIMA & HOLT-WINTERS FORECASTING
# ------------------------------------------------------------------------------
# Built-in `AirPassengers` dataset: Monthly international airline totals (1949-1960)
ap_ts <- AirPassengers
cat("\n--- 4. Time-Series Analysis (`AirPassengers` 1949-1960) ---\n")
cat("Start:", start(ap_ts), "| End:", end(ap_ts), "| Frequency:", frequency(ap_ts), "\n")

# Log-transform to stabilize multiplicative seasonal variance
log_ap <- log(ap_ts)

# 4.1 STL (Seasonal and Trend decomposition using Loess)
stl_decomp <- stl(log_ap, s.window = "periodic")
cat("\nSTL Decomposition Summary (First 6 months of Trend & Seasonal components):\n")
print(round(head(stl_decomp$time.series, 6), 4))

# 4.2 Holt-Winters Exponential Smoothing with Multiplicative Seasonality
hw_fit      <- HoltWinters(ap_ts, seasonal = "multiplicative")
hw_forecast <- predict(hw_fit, n.ahead = 12, prediction.interval = TRUE, level = 0.95)
cat("\nHolt-Winters 12-Month Ahead Forecast (1961):\n")
print(round(hw_forecast, 1))

# 4.3 Seasonal ARIMA Model (`arima`) on log-transformed series
# Fit ARIMA(0, 1, 1) x (0, 1, 1)_12 (Airline Model)
airline_arima <- arima(
  log_ap,
  order    = c(0, 1, 1),
  seasonal = list(order = c(0, 1, 1), period = 12)
)
cat("\nSeasonal ARIMA(0,1,1)(0,1,1)[12] Coefficients & AIC:\n")
print(airline_arima)

# Ljung-Box test on ARIMA residuals (H0: Residuals are white noise / uncorrelated)
lb_test <- Box.test(residuals(airline_arima), lag = 12, type = "Ljung-Box")
cat(sprintf("Ljung-Box Residual White-Noise Test p-value: %.4f\n", lb_test$p.value))

