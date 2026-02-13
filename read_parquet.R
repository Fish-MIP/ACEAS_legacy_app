# Read parquet file in R
# Install arrow if needed
if (!require("arrow", quietly = TRUE)) {
  install.packages("arrow", repos = "https://cloud.r-project.org")
}

library(arrow)

# Read the parquet file
df <- read_parquet("simulated_observed_catches_all.parquet")

# Display basic information
cat("Successfully read parquet file!\n\n")
cat("Dimensions:", nrow(df), "rows x", ncol(df), "columns\n\n")
cat("Column names:\n")
print(names(df))
cat("\n")
cat("First 10 rows:\n")
print(head(df, 10))
cat("\n")
cat("Summary:\n")
print(summary(df))

# Check for region column
if ("region" %in% names(df)) {
  cat("\nUnique regions:\n")
  print(unique(df$region))
}

# Save to CSV for easier use
write.csv(df, "simulated_observed_catches_all.csv", row.names = FALSE)
cat("\nSaved to CSV: simulated_observed_catches_all.csv\n")
