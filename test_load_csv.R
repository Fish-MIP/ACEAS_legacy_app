# Test loading the CSV file created by Python
library(ggplot2)

# Load the data
dbpm_data <- read.csv('dbpm_biomass_clean.csv')

# Check the data
print("Data loaded successfully!")
print(paste("Total rows:", nrow(dbpm_data)))
print(paste("Columns:", paste(names(dbpm_data), collapse=", ")))
print("\nFirst few rows:")
print(head(dbpm_data))

print("\nSummary by region:")
print(table(dbpm_data$region))

# Create a simple plot
print("\nCreating plot...")
p <- ggplot(dbpm_data, aes(x=lon, y=lat, fill=biomass)) +
  geom_tile() +
  scale_fill_viridis_c() +
  facet_wrap(~region) +
  coord_equal() +
  theme_minimal() +
  labs(title="DBPM Biomass by Region (from CSV)",
       fill="Biomass\n(tonnes/km²)")

# Save plot
ggsave("dbpm_from_csv_test.png", p, width=12, height=4, dpi=150)
print("Plot saved as: dbpm_from_csv_test.png")
print("\nYou can now use this CSV in your Shiny app!")
