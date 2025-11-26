#
# NeuroColombia interactive explorer
#

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(readr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(rnaturalearth)
library(scales)
library(DT)
library(glue)
library(bsicons)

# ---- Data --------------------------------------------------------------------

find_data_file <- function(filename) {
  candidates <- c(
    file.path("data", filename),
    file.path("Data_Processed", filename),
    filename
  )

  existing <- candidates[file.exists(candidates)]

  if (length(existing) == 0) {
    stop(
      "Could not locate ", filename,
      ". Looked in:\n",
      paste0("  - ", normalizePath(candidates, winslash = "/", mustWork = FALSE), collapse = "\n"),
      "\n\nCopy the dataset into either 'app/data' (recommended) or 'app/Data_Processed'."
    )
  }

  normalizePath(existing[[1]], winslash = "/")
}

data_path <- find_data_file("Ministery_DiagnosticData_Compiled.csv")

Neuro_Data <- readr::read_csv(
  data_path,
  show_col_types = FALSE,
  progress = FALSE
) |>
  mutate(
    Year = as.integer(Year),
    Age = suppressWarnings(as.numeric(Age)),
    Cases = as.numeric(Cases),
    Population = as.numeric(Population),
    Diagnostic_Group = if_else(
      Diagnostic_Group == "" | is.na(Diagnostic_Group),
      "Other / NA",
      Diagnostic_Group
    ),
    Capital = if_else(Capital == "Yes", "Yes", "No")
  )

diagnostic_choices <- sort(unique(Neuro_Data$Diagnostic))
diag_default <- intersect(c("F900", "F809", "F808"), diagnostic_choices)

year_range <- range(Neuro_Data$Year, na.rm = TRUE)
age_range <- range(Neuro_Data$Age, na.rm = TRUE)

# Build Colombia map
colombia_map <- rnaturalearth::ne_states(country = "Colombia", returnclass = "sf") |>
  dplyr::select(name_en, geometry) |>
  mutate(
    Department = stringi::stri_trans_general(name_en, "Latin-ASCII")
  ) |>
  dplyr::select(Department, geometry) |>
  mutate(
    Department = dplyr::recode(
      Department,
      "Bogota" = "Bogota, D.C.",
      "Archipelago of Saint Andrews" = "Archipielago de San Andres, Providencia Y Santa Catalina",
      .default = Department
    )
  )

# Custom Theme
my_theme <- bs_theme(
  bootswatch = "zephyr",
  base_font = font_google("Inter"),
  heading_font = font_google("Outfit"),
  primary = "#0a4f63",
  secondary = "#4DB6D0",
  success = "#69b3a2"
)

# Plot Theme (Reactive to match light/dark would be ideal, but keeping simple for now)
plot_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# ---- UI ----------------------------------------------------------------------

ui <- page_sidebar(
  theme = my_theme,
  title = "NeuroColombia Explorer",
  
  sidebar = sidebar(
    title = "Filters",
    width = 300,
    sliderInput("year_range", "Year range", min = year_range[1], max = year_range[2], value = year_range, sep = ""),
    sliderInput("age_range", "Age range", min = age_range[1], max = age_range[2], value = age_range),
    checkboxGroupInput("sex", "Sex", choices = sort(unique(Neuro_Data$Sex)), selected = sort(unique(Neuro_Data$Sex))),
    checkboxGroupInput("capital", "Capital city status", choices = c("Yes", "No"), selected = c("Yes", "No"), inline = TRUE),
    selectizeInput("diagnostics", "ICD-10 diagnostics", choices = diagnostic_choices, selected = diag_default, multiple = TRUE, options = list(placeholder = "All diagnostics", maxOptions = 500)),
    actionLink("clear_diags", "Clear selection"),
    hr(),
    downloadButton("download_filtered", "Download Data")
  ),

  navset_card_underline(
    title = "Analysis Modules",
    
    # --- Tab 1: Overview ---
    nav_panel(
      "Overview",
      layout_columns(
        fill = FALSE,
        value_box(
          title = "Total Consultations",
          value = textOutput("vb_total_cases"),
          showcase = bs_icon("people-fill"),
          theme_color = "primary"
        ),
        value_box(
          title = "Departments",
          value = textOutput("vb_departments"),
          showcase = bs_icon("geo-alt-fill"),
          theme_color = "secondary"
        ),
        value_box(
          title = "Top Diagnostic",
          value = textOutput("vb_top_diag"),
          showcase = bs_icon("activity"),
          theme_color = "success"
        )
      ),
      layout_columns(
        card(
          full_screen = TRUE,
          card_header("Consultations over Time"),
          plotlyOutput("cases_over_time")
        ),
        card(
          full_screen = TRUE,
          card_header("Age & Sex Distribution"),
          plotlyOutput("age_sex_plot")
        )
      )
    ),

    # --- Tab 2: Geography ---
    nav_panel(
      "Geography",
      layout_columns(
        col_widths = c(8, 4),
        card(
          full_screen = TRUE,
          card_header(
            class = "d-flex justify-content-between align-items-center",
            "Geographic Distribution",
            input_switch("map_rate_toggle", "Show Rate per 100k", value = FALSE)
          ),
          leafletOutput("department_map", height = "600px")
        ),
        card(
          full_screen = TRUE,
          card_header("Top Departments"),
          plotlyOutput("department_bar", height = "600px")
        )
      )
    ),

    # --- Tab 3: Distributions (NEW) ---
    nav_panel(
      "Distributions",
      layout_columns(
        card(
          full_screen = TRUE,
          card_header("Departmental Variability (Box Plot)"),
          plotlyOutput("dept_boxplot")
        ),
        card(
          full_screen = TRUE,
          card_header("Age Distribution by Sex (Box Plot)"),
          plotlyOutput("age_sex_boxplot")
        )
      )
    ),

    # --- Tab 4: Diagnostics ---
    nav_panel(
      "Diagnostics",
      layout_columns(
        card(
          full_screen = TRUE,
          card_header("Most Frequent Diagnostics"),
          plotlyOutput("diagnostic_bar")
        ),
        card(
          full_screen = TRUE,
          card_header("Detailed Statistics"),
          DTOutput("diagnostic_table")
        )
      )
    ),

    # --- Tab 5: Capital & Access ---
    nav_panel(
      "Capital & Access",
      layout_columns(
        card(
          full_screen = TRUE,
          card_header("Total Cases: Capital vs Non-Capital"),
          plotlyOutput("capital_distribution")
        ),
        card(
          full_screen = TRUE,
          card_header("Consultation Rates: Capital vs Non-Capital"),
          plotlyOutput("capital_rates")
        )
      )
    ),

    # --- Tab 6: Data ---
    nav_panel(
      "Data Explorer",
      card(
        card_header("Filtered Dataset"),
        DTOutput("data_table")
      )
    )
  )
)

# ---- Server ------------------------------------------------------------------

server <- function(input, output, session) {

  observeEvent(input$clear_diags, {
    updateSelectizeInput(session, "diagnostics", selected = character(0))
  })

  filtered_data <- reactive({
    req(Neuro_Data)
    sexes <- if (length(input$sex) > 0) input$sex else unique(Neuro_Data$Sex)
    capitals <- if (length(input$capital) > 0) input$capital else c("Yes", "No")

    Neuro_Data |>
      filter(
        Year >= input$year_range[1],
        Year <= input$year_range[2],
        Age >= input$age_range[1],
        Age <= input$age_range[2],
        Sex %in% sexes,
        Capital %in% capitals
      ) |>
      { if (length(input$diagnostics) > 0) filter(., Diagnostic %in% input$diagnostics) else . }
  })

  output$download_filtered <- downloadHandler(
    filename = function() { glue("NeuroColombia_filtered_{Sys.Date()}.csv") },
    content = function(file) { readr::write_csv(filtered_data(), file) }
  )

  # ---- Value Boxes ----
  overview_metrics <- reactive({
    data <- filtered_data()
    list(
      total = sum(data$Cases, na.rm = TRUE),
      depts = n_distinct(data$Department),
      top = data |> count(Diagnostic, wt = Cases, sort = TRUE) |> slice(1)
    )
  })

  output$vb_total_cases <- renderText({ comma(overview_metrics()$total) })
  output$vb_departments <- renderText({ overview_metrics()$depts })
  output$vb_top_diag <- renderText({
    m <- overview_metrics()$top
    if(nrow(m) > 0) paste0(m$Diagnostic, " (", comma(m$n), ")") else "N/A"
  })

  # ---- Plots: Overview ----
  output$cases_over_time <- renderPlotly({
    d <- filtered_data() |> group_by(Year) |> summarise(Total = sum(Cases, na.rm = TRUE))
    validate(need(nrow(d) > 0, "No data"))
    
    p <- ggplot(d, aes(x = Year, y = Total)) +
      geom_line(color = "#4DB6D0", linewidth = 1.2) +
      geom_point(color = "#0a4f63", size = 3) +
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
      labs(x = NULL, y = "Consultations") +
      plot_theme
    
    ggplotly(p) |> config(displayModeBar = FALSE)
  })

  output$age_sex_plot <- renderPlotly({
    d <- filtered_data() |> group_by(Age, Sex) |> summarise(Total = sum(Cases, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(d) > 0, "No data"))
    
    p <- ggplot(d, aes(x = Age, y = Total, color = Sex)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = c("Men" = "#00BFC4", "Women" = "#F8766D")) +
      labs(x = "Age", y = "Consultations") +
      plot_theme
    
    ggplotly(p) |> config(displayModeBar = FALSE)
  })

  # ---- Geography ----
  dept_summary <- reactive({
    filtered_data() |>
      group_by(Department) |>
      summarise(
        Total = sum(Cases, na.rm = TRUE),
        Rate = ifelse(sum(Population) > 0, (sum(Cases)/sum(Population))*1e5, 0),
        .groups = "drop"
      )
  })

  output$department_map <- renderLeaflet({
    d <- dept_summary()
    map_data <- colombia_map |> left_join(d, by = "Department")
    
    metric <- if(input$map_rate_toggle) "Rate" else "Total"
    vals <- map_data[[metric]]
    pal <- colorNumeric("YlOrRd", domain = vals, na.color = "#eee")
    
    leaflet(map_data) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor = ~pal(vals), weight = 1, color = "#666", fillOpacity = 0.8,
        label = ~paste0(Department, ": ", comma(round(vals, 1)), if(input$map_rate_toggle) " per 100k" else " cases"),
        highlight = highlightOptions(weight = 3, color = "#333", bringToFront = TRUE)
      ) |>
      addLegend(pal = pal, values = vals, title = if(input$map_rate_toggle) "Rate / 100k" else "Total Cases")
  })

  output$department_bar <- renderPlotly({
    d <- dept_summary() |> arrange(desc(Total)) |> slice_head(n = 15)
    validate(need(nrow(d) > 0, "No data"))
    
    p <- ggplot(d, aes(x = Total, y = reorder(Department, Total))) +
      geom_col(fill = "#69b3a2") +
      geom_text(aes(label = comma(Total)), hjust = -0.1, size = 3) +
      labs(x = "Total Cases", y = NULL) +
      plot_theme +
      scale_x_continuous(expand = expansion(mult = c(0, 0.2)))
    
    ggplotly(p) |> config(displayModeBar = FALSE)
  })

  # ---- Distributions (Box Plots) ----
  output$dept_boxplot <- renderPlotly({
    # Boxplot of cases per department (distribution across years)
    d <- filtered_data() |>
      group_by(Department, Year) |>
      summarise(Total = sum(Cases, na.rm = TRUE), .groups = "drop")
    
    validate(need(nrow(d) > 0, "No data"))
    
    p <- ggplot(d, aes(x = reorder(Department, Total, median), y = Total)) +
      geom_boxplot(fill = "#4DB6D0", alpha = 0.7, outlier.size = 1) +
      coord_flip() +
      labs(x = NULL, y = "Annual Cases") +
      plot_theme
    
    ggplotly(p) |> config(displayModeBar = FALSE)
  })

  output$age_sex_boxplot <- renderPlotly({
    # Boxplot of Age distribution weighted by cases is tricky in aggregated data.
    # Since we have 'Cases' counts per Age, we can visualize the distribution of 'Cases' across Ages.
    # Alternatively, we can expand the data or just plot the summary.
    # Let's plot the distribution of Cases count per Age group to show which ages have high variability.
    
    d <- filtered_data() |>
      group_by(Age, Sex, Year) |>
      summarise(Cases = sum(Cases, na.rm = TRUE), .groups = "drop")
      
    validate(need(nrow(d) > 0, "No data"))

    p <- ggplot(d, aes(x = factor(Age), y = Cases, fill = Sex)) +
      geom_boxplot(outlier.size = 0.5) +
      scale_fill_manual(values = c("Men" = "#00BFC4", "Women" = "#F8766D")) +
      labs(x = "Age", y = "Annual Cases") +
      plot_theme +
      theme(legend.position = "none")
      
    ggplotly(p) |> config(displayModeBar = FALSE)
  })

  # ---- Diagnostics ----
  output$diagnostic_bar <- renderPlotly({
    d <- filtered_data() |> group_by(Diagnostic) |> summarise(Total = sum(Cases, na.rm = TRUE)) |> arrange(desc(Total)) |> slice_head(n = 15)
    validate(need(nrow(d) > 0, "No data"))
    
    p <- ggplot(d, aes(x = Total, y = reorder(Diagnostic, Total), fill = Total)) +
      geom_col(show.legend = FALSE) +
      scale_fill_distiller(palette = "Blues", direction = 1) +
      labs(x = "Total Cases", y = NULL) +
      plot_theme
    
    ggplotly(p) |> config(displayModeBar = FALSE)
  })

  output$diagnostic_table <- renderDT({
    filtered_data() |>
      group_by(Diagnostic) |>
      summarise(Total = sum(Cases, na.rm = TRUE)) |>
      arrange(desc(Total)) |>
      mutate(Total = comma(Total)) |>
      datatable(options = list(pageLength = 10))
  })

  # ---- Capital ----
  output$capital_distribution <- renderPlotly({
    d <- filtered_data() |> group_by(Capital) |> summarise(Total = sum(Cases, na.rm = TRUE))
    p <- ggplot(d, aes(x = Capital, y = Total, fill = Capital)) +
      geom_col() + scale_fill_manual(values = c("Yes" = "#D9717D", "No" = "#4DB6D0")) +
      labs(y = "Total Cases") + plot_theme
    ggplotly(p) |> config(displayModeBar = FALSE)
  })

  output$capital_rates <- renderPlotly({
    d <- filtered_data() |> group_by(Capital) |> summarise(Total = sum(Cases), Pop = sum(Population)) |> mutate(Rate = (Total/Pop)*1e4)
    p <- ggplot(d, aes(x = Capital, y = Rate, fill = Capital)) +
      geom_col() + scale_fill_manual(values = c("Yes" = "#D9717D", "No" = "#4DB6D0")) +
      labs(y = "Cases per 10k") + plot_theme
    ggplotly(p) |> config(displayModeBar = FALSE)
  })

  # ---- Data ----
  output$data_table <- renderDT({
    filtered_data() |> datatable(extensions = "Buttons", options = list(dom = "Bfrtip", buttons = c("csv", "excel")))
  })
}

shinyApp(ui, server)
