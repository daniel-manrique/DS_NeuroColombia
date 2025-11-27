data <- read_csv(input_path, show_col_types = FALSE)
data <- read_csv(input_path, show_col_types = FALSE)

# Function to download and read RDS map safely
read_map <- function(url) {
}

colombia_map <- read_map(map_url)

# Normalize department names in the main data
data <- data |>
    dplyr::mutate(
        Department = dplyr::case_when(
            Department == "Bogota" ~ "Bogota",
            Department == "Archipelago of Saint Andrews" ~ "Archipielago de San Andres, Providencia Y Santa Catalina",
            Department == "North Santander" ~ "Norte de Santander",
            TRUE ~ Department
        )
    ) |>
    dplyr::mutate(Department = stringi::stri_trans_general(Department, "Latin-ASCII"))

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
