# NeuroColombia Data Analysis

![License](https://img.shields.io/github/license/daniel-manrique/DS_NeuroColombia)
![Build Status](https://github.com/daniel-manrique/DS_NeuroColombia/actions/workflows/publish.yml/badge.svg)

[**Leer en Español**](#análisis-de-datos-neurocolombia)

This repository contains the source code and data analysis for the **NeuroColombia** project. The goal of this project is to explore the frequency, geographical distribution, and demographic patterns of neurodevelopmental disorder consultations in Colombia using public health data.

The project is structured as a **Quarto Book**, which integrates data cleaning, statistical modeling, and interactive visualizations into a cohesive narrative.

## 📘 Project Structure

The analysis is divided into the following chapters:

- **`0_NeuroColombia_DataCleaning.qmd`**: The data preprocessing pipeline. It handles loading raw data, cleaning text fields, standardized department names, and merging with population data.
- **`1_Consultation_Frequency.qmd`**: An analysis of consultation trends over time (yearly evolution) and geography (maps of consultation rates).
- **`2_Consultation_DiagnosticFrecuency.qmd`**: A breakdown of consultations by specific ICD-10 diagnostic categories.
- **`3_Consultation_MostFrequent.qmd`**: A deep dive into the top 3 most frequent diagnoses (F900, F809, F808), including age/sex distribution and statistical modeling.
- **`4_Interactive_App.qmd`**: An embedded **Shinylive** application that allows users to interactively explore the data directly in the browser.

## 🚀 Usage

### View the Book Online

The latest version of the analysis is deployed as a static website:
👉 **[Visit NeuroColombia Data Analysis](https://daniel-manrique.github.io/DS_NeuroColombia/)**

### Run Locally

To reproduce the analysis or run the code on your local machine:

1.  **Clone the repository**:
    ```bash
    git clone https://github.com/daniel-manrique/DS_NeuroColombia.git
    ```
2.  **Open the project**:
    Open the `DS_NeuroColombia.Rproj` file in RStudio.
3.  **Install Dependencies**:
    The project uses `renv` (or standard package management). Ensure you have the required packages installed (see `_common.R` or `.github/workflows/publish.yml` for the list).
    Key packages: `tidyverse`, `brms`, `sf`, `leaflet`, `plotly`, `bslib`, `gt`, `flextable`.
4.  **Render the Book**:
    Run the following command in the terminal:
    ```bash
    quarto preview
    ```

### Run the Shiny App

The repository includes a standalone Shiny app in the `app/` directory.

1.  Open `app/app.R` in RStudio.
2.  Click the **"Run App"** button.

## 🛠️ Requirements

- **R**: Version 4.4.0 or higher is recommended.
- **Quarto CLI**: Latest version.

---

# Análisis de Datos NeuroColombia

Este repositorio contiene el código fuente y el análisis de datos del proyecto **NeuroColombia**. El objetivo de este proyecto es explorar la frecuencia, distribución geográfica y patrones demográficos de las consultas por trastornos del neurodesarrollo en Colombia, utilizando datos de salud pública.

El proyecto está estructurado como un **Libro de Quarto**, que integra la limpieza de datos, el modelado estadístico y visualizaciones interactivas en una narrativa cohesiva.

## 📘 Estructura del Proyecto

El análisis se divide en los siguientes capítulos:

- **`0_NeuroColombia_DataCleaning.qmd`**: El flujo de preprocesamiento de datos. Se encarga de cargar los datos crudos, limpiar campos de texto, estandarizar nombres de departamentos y unir con datos poblacionales.
- **`1_Consultation_Frequency.qmd`**: Un análisis de las tendencias de consulta a lo largo del tiempo (evolución anual) y geografía (mapas de tasas de consulta).
- **`2_Consultation_DiagnosticFrecuency.qmd`**: Un desglose de las consultas por categorías diagnósticas específicas CIE-10.
- **`3_Consultation_MostFrequent.qmd`**: Un análisis profundo de los 3 diagnósticos más frecuentes (F900, F809, F808), incluyendo distribución por edad/sexo y modelado estadístico.
- **`4_Interactive_App.qmd`**: Una aplicación **Shinylive** integrada que permite a los usuarios explorar interactivamente los datos directamente en el navegador.

## 🚀 Uso

### Ver el Libro en Línea

La última versión del análisis está desplegada como un sitio web estático:
👉 **[Visitar Análisis de Datos NeuroColombia](https://daniel-manrique.github.io/DS_NeuroColombia/)**

### Ejecutar Localmente

Para reproducir el análisis o ejecutar el código en tu máquina local:

1.  **Clonar el repositorio**:
    ```bash
    git clone https://github.com/daniel-manrique/DS_NeuroColombia.git
    ```
2.  **Abrir el proyecto**:
    Abre el archivo `DS_NeuroColombia.Rproj` en RStudio.
3.  **Instalar Dependencias**:
    Asegúrate de tener instalados los paquetes requeridos (ver `_common.R` o `.github/workflows/publish.yml` para la lista).
    Paquetes clave: `tidyverse`, `brms`, `sf`, `leaflet`, `plotly`, `bslib`, `gt`, `flextable`.
4.  **Renderizar el Libro**:
    Ejecuta el siguiente comando en la terminal:
    ```bash
    quarto preview
    ```

### Ejecutar la App Shiny

El repositorio incluye una aplicación Shiny independiente en el directorio `app/`.

1.  Abre `app/app.R` en RStudio.
2.  Haz clic en el botón **"Run App"**.

## 🛠️ Requisitos

- **R**: Se recomienda la versión 4.4.0 o superior.
- **Quarto CLI**: Última versión.
