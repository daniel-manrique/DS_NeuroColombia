#|
#| label: common-setup
#| include: false
#|
#| This script is SOURCED by all other .qmd files in this project.
#| It contains all common package loads, a custom plot theme, and
#| loads the main dataset to ensure consistency.

# --- 1. PACKAGE MANAGEMENT ---
# Install packages if they are not already present
if (!requireNamespace("brms", quietly = TRUE)) install.packages("brms")
if (!requireNamespace("flextable", quietly = TRUE)) install.packages("flextable")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("ggridges", quietly = TRUE)) install.packages("ggridges")
if (!requireNamespace("ggbeeswarm", quietly = TRUE)) install.packages("ggbeeswarm")
if (!requireNamespace("gt", quietly = TRUE)) install.packages("gt")
if (!requireNamespace("gtsummary", quietly = TRUE)) install.packages("gtsummary")
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
if (!requireNamespace("hrbrthemes", quietly = TRUE)) install.packages("hrbrthemes")
if (!requireNamespace("tibble", quietly = TRUE)) install.packages("tibble")
if (!requireNamespace("tidybayes", quietly = TRUE)) install.packages("tidybayes")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("stringi", quietly = TRUE)) install.packages("stringi")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis")
if (!requireNamespace("scales", quietly = TRUE)) install.packages("scales")
if (!requireNamespace("rnaturalearth", quietly = TRUE)) install.packages("rnaturalearth")
if (!requireNamespace("officer", quietly = TRUE)) install.packages("officer")

# Load all required libraries
library(brms)
library(flextable)
library(dplyr)
library(ggplot2)
library(ggridges)
library(ggbeeswarm)
library(gt)
library(gtsummary)
library(here)
library(hrbrthemes)
library(officer)
library(rnaturalearth) 
library(tibble)
library(tidybayes)
library(tidyr)
library(stringi) 
library(stringr) 
library(viridis)
library(scales) 

# --- 2. GLOBAL PLOT THEME ---
# Define a consistent theme for all plots in the project
Plot_theme <- theme_classic() +
  theme(
    plot.title = element_text(size=16, face="bold", vjust = 2),
    plot.subtitle = element_text(size = 10, color = "black"),
    plot.caption = element_text(size = 8, color = "black", hjust = 1, vjust = -2),
    
    axis.line = element_line(colour = "black", linewidth = 1.5, linetype = "solid"),
    axis.ticks.length=unit(7,"pt"),
    
    axis.title.x = element_text(colour = "black", size = 16, vjust = -1),
    axis.text.x = element_text(colour = "black", size = 16, angle = 0, hjust = 0.5),
    axis.ticks.x = element_line(colour = "black", linewidth = 1),
    
    axis.title.y = element_text(colour = "black", size = 16, vjust = 3),
    axis.text.y = element_text(colour = "black", size = 16),
    axis.ticks.y = element_line(colour = "black", linewidth = 1),
    
    legend.position="right",
    legend.direction="vertical",
    legend.title = element_text(colour="black", face="bold", size=12),
    legend.text = element_text(colour="black", size=10),
    
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10) 
  ) 

# --- 3. MAIN DATA LOADING ---
# Load the primary dataset so it's available in all notebooks
Neuro_Data <- read.csv("Data_Processed/Ministery_DiagnosticData_Compiled.csv")

# Load and clean the Colombia map data so it's available for plotting
Col_map <- ne_states(country = "Colombia", returnclass = "sf") %>%
  select(name_en, geometry) %>%
  mutate(
    Department = stri_trans_general(name_en, "Latin-ASCII")
  ) %>%
  select(Department, geometry)

# Fix known name mismatches between map data and diagnostic data
Col_map_fixed <- Col_map %>%
  mutate(
    Department = recode(
      Department,
      "Bogota"                       = "Bogota D.C",
      "Archipelago of Saint Andrews" = "Archipielago de San Andres, Providencia Y Santa Catalina",
      .default = Department
    )
  )

# --- 4. TABLE THEMES ---

# APA Theme for gt tables
theme_apa_gt <- function(gt_tbl) {
  gt_tbl %>%
    tab_options(
      table.border.top.width = px(2),
      table.border.top.color = "black",
      table.border.bottom.width = px(2),
      table.border.bottom.color = "black",
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "black",
      table_body.border.top.style = "none",
      table_body.border.bottom.style = "none",
      table.border.left.style = "none",
      table.border.right.style = "none",
      table_body.vlines.style = "none",
      column_labels.vlines.style = "none",
      row_group.border.top.style = "none"
    )
}

# APA Theme for flextable (Word)
theme_apa_flextable <- function(ft_tbl) {
  ft_tbl %>%
    autofit() %>%
    border_remove() %>%
    border_outer(
      part = "all",
      border = fp_border(color = "black", width = 2)
    ) %>%
    hline_bottom(
      part = "header",
      border = fp_border(color = "black", width = 2)
    ) %>%
    align(part = "header", align = "center") %>%
    bold(part = "header", bold = TRUE) %>%
    align(part = "footer", align = "left")
}