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

# Build Colombia map (same cleaning applied in notebooks)
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

# A lighter version of the custom plot theme from _common.R
plot_theme <- theme_classic() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# ---- UI ----------------------------------------------------------------------

ui <- fluidPage(
  theme = bs_theme(
    bootswatch = "spacelab",
    base_font = font_google("Source Sans 3")
  ),
  titlePanel("NeuroColombia consultation explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput(
        "year_range",
        "Year range",
        min = year_range[1],
        max = year_range[2],
        value = year_range,
        sep = ""
      ),
      sliderInput(
        "age_range",
        "Age range",
        min = age_range[1],
        max = age_range[2],
        value = age_range
      ),
      checkboxGroupInput(
        "sex",
        "Sex",
        choices = sort(unique(Neuro_Data$Sex)),
        selected = sort(unique(Neuro_Data$Sex))
      ),
      checkboxGroupInput(
        "capital",
        "Capital city status",
        choices = c("Yes", "No"),
        selected = c("Yes", "No"),
        inline = TRUE
      ),
      selectizeInput(
        "diagnostics",
        "ICD-10 diagnostics (leave empty for all)",
        choices = diagnostic_choices,
        selected = diag_default,
        multiple = TRUE,
        options = list(
          placeholder = "All diagnostics",
          plugins = list("remove_button"),
          maxOptions = 500
        )
      ),
      actionLink("clear_diags", "Clear diagnostic selection"),
      hr(),
      downloadButton("download_filtered", "Download filtered data")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Overview",
          br(),
          fluidRow(
            column(4, uiOutput("total_cases_box")),
            column(4, uiOutput("departments_box")),
            column(4, uiOutput("top_diag_box"))
          ),
          br(),
          plotlyOutput("cases_over_time", height = "320px"),
          br(),
          plotlyOutput("age_sex_plot", height = "320px")
        ),
        tabPanel(
          "Geography",
          br(),
          leafletOutput("department_map", height = "540px"),
          br(),
          plotlyOutput("department_bar", height = "320px")
        ),
        tabPanel(
          "Diagnostics",
          br(),
          plotlyOutput("diagnostic_bar", height = "360px"),
          br(),
          DTOutput("diagnostic_table")
        ),
        tabPanel(
          "Capital & Access",
          br(),
          plotlyOutput("capital_distribution", height = "320px"),
          br(),
          plotlyOutput("capital_rates", height = "320px")
        ),
        tabPanel(
          "Filtered data",
          br(),
          DTOutput("data_table")
        )
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
    filename = function() {
      glue("NeuroColombia_filtered_{Sys.Date()}.csv")
    },
    content = function(file) {
      readr::write_csv(filtered_data(), file)
    }
  )

  # ---- Overview cards ----
  overview_metrics <- reactive({
    data <- filtered_data()
    total_cases <- sum(data$Cases, na.rm = TRUE)
    total_population <- sum(data$Population, na.rm = TRUE)
    unique_departments <- dplyr::n_distinct(data$Department)
    top_diag <- data |>
      count(Diagnostic, wt = Cases, sort = TRUE) |>
      slice_head(n = 1)

    list(
      total_cases = total_cases,
      unique_departments = unique_departments,
      incidence = ifelse(
        total_population > 0,
        total_cases / total_population,
        NA_real_
      ),
      top_diagnostic = if (nrow(top_diag) == 0) NA_character_ else top_diag$Diagnostic,
      top_diagnostic_cases = if (nrow(top_diag) == 0) NA_real_ else top_diag$n
    )
  })

  output$total_cases_box <- renderUI({
    metrics <- overview_metrics()
    value_box(
      title = "Total consultations",
      value = comma(round(metrics$total_cases, 0))
    )
  })

  output$departments_box <- renderUI({
    metrics <- overview_metrics()
    incidence_txt <- if (is.na(metrics$incidence)) {
      "Cases per 100k: NA"
    } else {
      paste0("Cases per 100k: ", comma(round(metrics$incidence * 1e5, 1)))
    }
    value_box(
      title = "Departments represented",
      value = metrics$unique_departments,
      p(incidence_txt)
    )
  })

  output$top_diag_box <- renderUI({
    metrics <- overview_metrics()
    diag_label <- ifelse(
      is.na(metrics$top_diagnostic),
      "No diagnostic available",
      paste0(metrics$top_diagnostic, ": ", scales::comma(metrics$top_diagnostic_cases), " cases")
    )
    value_box(
      title = "Most frequent diagnostic",
      value = diag_label
    )
  })

  # ---- Plots: Overview ----
  output$cases_over_time <- renderPlotly({
    yearly <- filtered_data() |>
      group_by(Year) |>
      summarise(Total_Cases = sum(Cases, na.rm = TRUE), .groups = "drop")

    validate(need(nrow(yearly) > 0, "No data available for these filters."))

    p <- ggplot(yearly, aes(x = Year, y = Total_Cases)) +
      geom_line(color = "#4DB6D0", linewidth = 1.2) +
      geom_point(color = "#0a4f63", size = 2.5) +
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
      labs(
        title = "Consultations per year",
        x = "Year",
        y = "Number of consultations"
      ) +
      plot_theme

    ggplotly(p, tooltip = c("x", "y")) |> config(displayModeBar = FALSE)
  })

  output$age_sex_plot <- renderPlotly({
    age_sex <- filtered_data() |>
      group_by(Age, Sex) |>
      summarise(Total_Cases = sum(Cases, na.rm = TRUE), .groups = "drop")

    validate(need(nrow(age_sex) > 0, "No data available for these filters."))

    p <- ggplot(age_sex, aes(x = Age, y = Total_Cases, color = Sex)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = c("Men" = "#00BFC4", "Women" = "#F8766D")) +
      labs(
        title = "Distribution by age and sex",
        x = "Age",
        y = "Number of consultations",
        color = "Sex"
      ) +
      plot_theme

    ggplotly(p, tooltip = c("x", "y", "color")) |> config(displayModeBar = FALSE)
  })

  # ---- Geography tab ----
  department_summary <- reactive({
    filtered_data() |>
      group_by(Department) |>
      summarise(
        Total_Cases = sum(Cases, na.rm = TRUE),
        Cases_per_100k = ifelse(
          sum(Population, na.rm = TRUE) > 0,
          (sum(Cases, na.rm = TRUE) / sum(Population, na.rm = TRUE)) * 1e5,
          NA_real_
        ),
        .groups = "drop"
      )
  })

  department_map_data <- reactive({
    colombia_map |>
      left_join(department_summary(), by = "Department")
  })

  output$department_map <- renderLeaflet({
    map_data <- department_map_data()

    validate(need(any(!is.na(map_data$Total_Cases)), "No geographical data for these filters."))

    pal <- colorNumeric("YlOrRd", domain = map_data$Total_Cases, na.color = "#f0f0f0")

    leaflet(map_data, options = leafletOptions(minZoom = 4, maxZoom = 10)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor = ~pal(Total_Cases),
        weight = 1,
        color = "#636363",
        opacity = 0.9,
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(weight = 3, color = "#252525", bringToFront = TRUE),
        label = ~htmltools::HTML(glue(
          "<strong>{Department}</strong><br/>",
          "Total cases: {scales::comma(Total_Cases)}<br/>",
          "Cases per 100k: {ifelse(is.na(Cases_per_100k), 'NA', scales::comma(round(Cases_per_100k, 1)))}"
        ))
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~Total_Cases,
        title = "Total consultations",
        opacity = 0.9,
        labFormat = label_number(scale_cut = cut_short_scale())
      )
  })

  output$department_bar <- renderPlotly({
    dept <- department_summary() |>
      arrange(desc(Total_Cases)) |>
      slice_head(n = 10)

    validate(need(nrow(dept) > 0, "No departmental data to plot."))

    p <- ggplot(dept, aes(x = Total_Cases, y = reorder(Department, Total_Cases))) +
      geom_col(fill = "#69b3a2") +
      geom_text(
        aes(label = label_number(scale_cut = cut_short_scale())(Total_Cases)),
        hjust = -0.1,
        size = 3.2
      ) +
      labs(
        title = "Top departments by consultations",
        x = "Total cases",
        y = NULL
      ) +
      plot_theme +
      theme(axis.text.y = element_text(size = 11)) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

    ggplotly(p, tooltip = c("y", "x")) |> config(displayModeBar = FALSE)
  })

  # ---- Diagnostics tab ----
  diagnostic_summary <- reactive({
    filtered_data() |>
      group_by(Diagnostic) |>
      summarise(Total_Cases = sum(Cases, na.rm = TRUE), .groups = "drop") |>
      mutate(Percent = Total_Cases / sum(Total_Cases, na.rm = TRUE))
  })

  output$diagnostic_bar <- renderPlotly({
    diag_top <- diagnostic_summary() |>
      arrange(desc(Total_Cases)) |>
      slice_head(n = 15)

    validate(need(nrow(diag_top) > 0, "No diagnostic data available."))

    p <- ggplot(diag_top, aes(x = Total_Cases, y = reorder(Diagnostic, Total_Cases), fill = Total_Cases)) +
      geom_col(show.legend = FALSE) +
      labs(
        title = "Most frequent diagnostics",
        x = "Total cases",
        y = "ICD-10 code"
      ) +
      scale_fill_distiller(palette = "Blues", direction = 1) +
      plot_theme

    ggplotly(p, tooltip = c("y", "x")) |> config(displayModeBar = FALSE)
  })

  output$diagnostic_table <- renderDT({
    diagnostic_summary() |>
      arrange(desc(Total_Cases)) |>
      mutate(
        Total_Cases = scales::comma(Total_Cases),
        Percent = scales::percent(Percent, accuracy = 0.1)
      ) |>
      datatable(
        rownames = FALSE,
        options = list(
          pageLength = 15,
          dom = "tip",
          order = list(list(1, "desc"))
        ),
        colnames = c("Diagnostic", "Total cases", "Share of filtered cases")
      )
  })

  # ---- Capital tab ----
  capital_summary <- reactive({
    filtered_data() |>
      group_by(Capital) |>
      summarise(
        Total_Cases = sum(Cases, na.rm = TRUE),
        Population = sum(Population, na.rm = TRUE),
        Cases_per_10k = ifelse(Population > 0, (Total_Cases / Population) * 1e4, NA_real_),
        .groups = "drop"
      )
  })

  output$capital_distribution <- renderPlotly({
    capital_data <- capital_summary()

    validate(need(nrow(capital_data) > 0, "No capital vs non-capital data available."))

    p <- ggplot(capital_data, aes(x = Capital, y = Total_Cases, fill = Capital)) +
      geom_col() +
      scale_fill_manual(values = c("Yes" = "#D9717D", "No" = "#4DB6D0"), guide = "none") +
      labs(
        title = "Consultations by capital status",
        x = NULL,
        y = "Total cases"
      ) +
      plot_theme

    ggplotly(p, tooltip = c("x", "y")) |> config(displayModeBar = FALSE)
  })

  output$capital_rates <- renderPlotly({
    capital_data <- capital_summary()

    validate(need(nrow(capital_data) > 0, "No capital vs non-capital data available."))

    p <- ggplot(capital_data, aes(x = Capital, y = Cases_per_10k, fill = Capital)) +
      geom_col() +
      scale_fill_manual(values = c("Yes" = "#D9717D", "No" = "#4DB6D0"), guide = "none") +
      labs(
        title = "Consultations per 10k residents",
        x = NULL,
        y = "Cases per 10,000"
      ) +
      plot_theme

    ggplotly(p, tooltip = c("x", "y")) |> config(displayModeBar = FALSE)
  })

  # ---- Filtered data table ----
  output$data_table <- renderDT({
    filtered_data() |>
      datatable(
        extensions = "Buttons",
        options = list(
          pageLength = 20,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel")
        )
      )
  })
}

shinyApp(ui, server)
