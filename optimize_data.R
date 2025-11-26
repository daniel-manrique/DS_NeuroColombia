data <- read_csv(input_path, show_col_types = FALSE)

# Aggregate
message("Aggregating data...")
data_agg <- data |>
    group_by(Year, Department, Capital, Diagnostic, Age, Sex) |>
    summarise(
        Cases = sum(Cases, na.rm = TRUE),
        Population = sum(Population, na.rm = TRUE),
        .groups = "drop"
    )

# Save
message("Saving optimized data to ", output_path)
write_csv(data_agg, output_path)
message("Done.")
