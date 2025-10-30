# 📊 Shiny Demographic & Persona Dashboard

This repository contains a **Shiny web application** for visualizing demographic data, personas, and related analytics. The app provides an interactive interface for exploring key indicators, visual summaries, and downloadable reports through a clean and responsive UI.

---

## 🚀 Features

- **Interactive Landing Page:**  
  Navigate between demographics, persona insights, and additional visual modules.

- **Dynamic Visualizations:**  
  Interactive plots, tables, and UI elements built using Shiny reactive programming.

- **Custom Styling:**  
  A clean, professional look powered by `styles.css`, including hover effects, responsive layouts, and consistent theming.

- **Downloadable Data:**  
  Export tables and plots using download buttons styled with `.down` class.

- **Modular Structure:**  
  Each component (Demographics, Persona, etc.) is modularized for maintainability.

---

## 🧩 Repository Structure

```
.
├── global.R              # Loads libraries, global variables, and data sources
├── ui.R                  # Defines the main UI layout and navigation
├── server.R              # Contains server logic and reactivity
├── demographicsPageNew.R # Module for demographics data and visualizations
├── personaPage.R         # Module for persona-level insights
├── styles.css            # Custom CSS for styling the UI
└── README.md             # Documentation (this file)
```

---

## ⚙️ Installation & Setup

### Prerequisites

- **R ≥ 4.1**
- **Shiny ≥ 1.7**
- Recommended packages:
  ```r
  install.packages(c(
    "shiny", "tidyverse", "plotly", "DT", "shinythemes",
    "shinydashboard", "readr", "dplyr", "ggplot2"
  ))
  ```

### Running the App

Clone the repository and run:

```r
# In R console
library(shiny)
runApp("path/to/this/repo")
```

Or directly from within RStudio:

```r
shiny::runApp()
```

---

## 🎨 UI & Styling

The `styles.css` file defines consistent design rules across the app:
- `.landing-page-box`, `.landing-page-icon`: hover-enabled cards with icons  
- `.depr-text-box`: styled informational boxes  
- `.down`: custom buttons for downloads  
- `.navbar`, `.well`, `.definitionbox`: styled navigation and layout elements  

---

## 🧠 Modules Overview

| Module | File | Description |
|--------|------|--------------|
| **Demographics** | `demographicsPageNew.R` | Displays demographic data summaries and charts |
| **Persona** | `personaPage.R` | Visualizes personas and behavioral patterns |
| **Global** | `global.R` | Defines global variables, functions, and loads data |
| **Server/UI** | `server.R` / `ui.R` | Core Shiny logic and layout definition |

---

## 📁 Data Inputs

You can connect the app to your data by updating the **data loading section** in `global.R`. Ensure CSVs or APIs used are accessible in your environment.

---

## 🧪 Development Notes

- CSS errors are suppressed in Shiny using:
  ```css
  .shiny-output-error { visibility: hidden; }
  .shiny-output-error:before { visibility: hidden; }
  ```
- Follows modular design principles — easy to extend with new pages.
- Supports deployment on [shinyapps.io](https://www.shinyapps.io/) or internal Shiny Server.

---

## 🙌 Acknowledgments

- Built with ❤️ using [R Shiny](https://shiny.posit.co/).  
- Inspired by public health and demographic visualization dashboards.
