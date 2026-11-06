# ==============================================================================
# PART 01: R BASICS & FUNDAMENTALS
# Module 02: Data Structures in R
# ==============================================================================
# R's data structures can be organized by their dimensionality (1D, 2D, nD)
# and whether their contents must be homogeneous (all same type) or can be
# heterogeneous (mixed types):
#
#   Dimension | Homogeneous       | Heterogeneous
#   ----------|-------------------|-------------------
#   1D        | Atomic Vector     | List
#   2D        | Matrix            | Data Frame / Tibble
#   nD        | Array             |
#   (+ Factor for categorical data built on integer vectors)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. ATOMIC VECTORS (1D, Homogeneous)
# ------------------------------------------------------------------------------
# Almost everything in R is a vector! Even a single number `42` is a numeric
# vector of length 1.

# 1.1 Creating Vectors
v_num  <- c(10.5, 20.3, 30.1, 40.8, 50.0)
v_int  <- 1:10
v_seq  <- seq(from = 0, to = 100, by = 20)
v_len  <- seq(from = 1, to = 5, length.out = 9)
v_rep1 <- rep(c("A", "B"), times = 3)  # "A" "B" "A" "B" "A" "B"
v_rep2 <- rep(c("A", "B"), each = 3)   # "A" "A" "A" "B" "B" "B"

# 1.2 Naming Vector Elements
monthly_rev <- c(Jan = 12000, Feb = 15400, Mar = 13800, Apr = 18200)
print(monthly_rev)
cat("Names:", names(monthly_rev), "\n")

# 1.3 Vectorized Arithmetic & Recycling Rule
# If two vectors are of unequal length, R recycles (repeats) the shorter vector!
# (Always be careful when lengths are not multiples of one another.)
prices <- c(100, 200, 300, 400)
tax_multiplier <- 1.18 # Length 1 recycled to length 4
prices_with_tax <- prices * tax_multiplier
cat("Prices with tax:", prices_with_tax, "\n")

# 1.4 Subsetting Vectors (1-based indexing in R!)
# - Positive integers: select elements at those positions
# - Negative integers: exclude elements at those positions
# - Logical vector:    select elements where TRUE
# - Character vector:  select elements by name

cat("1st and 3rd month:", monthly_rev[c(1, 3)], "\n")
cat("All except 2nd:   ", monthly_rev[-2], "\n")
cat("Rev > 14000:      ", monthly_rev[monthly_rev > 14000], "\n")
cat("By name ('Mar'):  ", monthly_rev["Mar"], "\n")
cat("which(rev > 14000) indices:", which(monthly_rev > 14000), "\n")

# ------------------------------------------------------------------------------
# 2. FACTORS (Categorical Data)
# ------------------------------------------------------------------------------
# Factors store categorical variables (nominal or ordinal) as integers with a
# `levels` character attribute. Crucial for statistical modeling and plotting!

edu_raw <- c("Bachelors", "Masters", "PhD", "High School", "Masters", "Bachelors")

# Unordered (Nominal) Factor
edu_factor <- factor(edu_raw)
print(edu_factor)
cat("Levels:", levels(edu_factor), "\n")
cat("Underlying storage typeof:", typeof(edu_factor), "\n")

# Ordered (Ordinal) Factor
edu_ordered <- factor(
  edu_raw,
  levels = c("High School", "Bachelors", "Masters", "PhD"),
  ordered = TRUE
)
print(edu_ordered)
cat("Is person 2 > person 1 in education?", edu_ordered[2] > edu_ordered[1], "\n")
print(table(edu_ordered))

# ------------------------------------------------------------------------------
# 3. MATRICES & ARRAYS (2D & nD, Homogeneous)
# ------------------------------------------------------------------------------

# 3.1 Creating a Matrix (filled column-wise by default unless `byrow = TRUE`)
mat_col <- matrix(1:12, nrow = 3, ncol = 4)
mat_row <- matrix(1:12, nrow = 3, ncol = 4, byrow = TRUE,
                  dimnames = list(c("R1", "R2", "R3"), c("C1", "C2", "C3", "C4")))
print(mat_row)

# 3.2 Matrix Properties & Subsetting `[row, col]`
cat("Dimensions:", dim(mat_row), "| Rows:", nrow(mat_row), "| Cols:", ncol(mat_row), "\n")
cat("Element [2, 3]:", mat_row[2, 3], "\n")
cat("Entire Row 1:  ", mat_row[1, ], "\n")
cat("Entire Col 2:  ", mat_row[, 2], "\n")

# Note: Subsetting a single row/column drops the 2D matrix dimension by default!
# Use `drop = FALSE` to preserve it as a 1-row or 1-col matrix:
col2_preserved <- mat_row[, 2, drop = FALSE]
cat("Class with drop = FALSE:", class(col2_preserved), "\n")

# 3.3 Matrix Operations & Linear Algebra
A <- matrix(c(2, 1, 5, 3), nrow = 2, byrow = TRUE)
B <- matrix(c(1, 4, 2, 6), nrow = 2, byrow = TRUE)

cat("Element-wise multiplication (A * B):\n")
print(A * B)

cat("True Matrix Multiplication (A %*% B):\n")
print(A %*% B)

cat("Transpose t(A):\n")
print(t(A))

cat("Determinant det(A):", det(A), "\n")
cat("Matrix Inverse solve(A):\n")
print(solve(A))

cat("Row Sums:", rowSums(mat_row), "| Col Means:", colMeans(mat_row), "\n")

# 3.4 Multi-dimensional Arrays (nD)
arr_3d <- array(1:24, dim = c(3, 4, 2))
cat("3D Array dimensions (3 rows, 4 cols, 2 slices):", dim(arr_3d), "\n")

# ------------------------------------------------------------------------------
# 4. LISTS (1D, Heterogeneous & Recursive)
# ------------------------------------------------------------------------------
# Lists can hold elements of any type and length, including other lists,
# functions, models, and data frames!

employee_profile <- list(
  id = 1042L,
  name = "Grace Hopper",
  roles = c("Rear Admiral", "Computer Scientist"),
  metrics = matrix(c(98, 99, 95, 100), nrow = 2),
  active = TRUE
)

# Inspecting structure with `str()`
str(employee_profile)

# Subsetting Lists: `[` vs `[[` vs `$`
# - `[`   returns a smaller SUB-LIST (preserves list container)
# - `[[`  extracts the actual ELEMENT inside the list
# - `$`   extracts a single element by name (shorthand for `[["name"]]`)

sub_list <- employee_profile[1:2]
cat("Class of employee_profile[1]:  ", class(employee_profile[1]), "\n")
cat("Class of employee_profile[[1]]:", class(employee_profile[[1]]), "\n")
cat("Access via $name:              ", employee_profile$name, "\n")
cat("Chained extraction (2nd role): ", employee_profile[["roles"]][2], "\n")

# ------------------------------------------------------------------------------
# 5. DATA FRAMES (2D, Heterogeneous across columns, Homogeneous within columns)
# ------------------------------------------------------------------------------
# Under the hood, a `data.frame` is a named list of equal-length vectors!

students_df <- data.frame(
  student_id = 101:105,
  name       = c("Alice", "Bob", "Charlie", "Diana", "Evan"),
  major      = factor(c("CS", "Math", "Physics", "CS", "Stats")),
  gpa        = c(3.92, 3.45, 3.78, 3.98, 3.61),
  graduated  = c(FALSE, TRUE, FALSE, FALSE, TRUE),
  stringsAsFactors = FALSE # Default in R >= 4.0.0
)

print(students_df)
str(students_df)
summary(students_df)

# Subsetting Data Frames
# Filter rows where GPA > 3.7 and select `name` and `gpa` columns
high_achievers <- students_df[students_df$gpa > 3.7, c("name", "gpa")]
print(high_achievers)

# Adding a new calculated column
students_df$honors <- students_df$gpa >= 3.75
print(students_df)

# Binding rows (`rbind`) and columns (`cbind`)
new_student <- data.frame(
  student_id = 106L,
  name       = "Fiona",
  major      = factor("Math", levels = levels(students_df$major)),
  gpa        = 3.88,
  graduated  = FALSE,
  honors     = TRUE
)
students_df <- rbind(students_df, new_student)
cat("Updated row count:", nrow(students_df), "\n")

