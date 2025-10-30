# Code to create Catalina Shiny profile platform
# In this script include packages, functions, datasets and anyting that will be 
# used both by UI and server

##################################################################.
# Packages ----
library(shiny)
library(shinyBS) #modals
library(shinythemes) # layouts for shiny
library(shinyWidgets) # for extra widgets
library(shinyjs)
library(shinydashboard) #for valuebox on techdoc tab
library(shinycssloaders) #for loading icons, see line below
# library(tidyverse) # data manipulation
library(dplyr)
library (DT) # for data tables
library(htmltools) # merge columns in DT
# library(plotly) #interactive graphs
# library(leaflet) #javascript maps
library(tibble) # rownames to column in techdoc
library(sp)
# library(lubridate) #for automated list of dates in welcome modal
library(webshot) #to download plotly charts
library(rintrojs) # for help intros 
library(scales) # number format
library(glue) # glue query
# library(promises)
# library(future)

##################################################################.
# configs ---- 

options(warn = -1) #suppress warnings
options(scipen = 999) # print fixed notation

Sys.setenv(TZ='America/New_York')

# detect app root directory automatically
app_dir <- tryCatch(
  {
    # When running from RStudio
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      dirname(rstudioapi::getActiveDocumentContext()$path)
    } else {
      # When deployed on Shiny Server or shinyapps.io
      normalizePath(".")
    }
  },
  error = function(e) normalizePath(".") # fallback
)

# Print to confirm
cat("App directory detected:", app_dir, "\n")

# load data
load(file.path(app_dir, "data/demo_data.RData"))

# load all modules
source("modules/personaPage.R")
source("modules/demographicsPageNew.R")

##################################################################.
# UI helping function -----

#Creating big boxes for main tabs in the landing page (see ui for formatting css)
lp_main_box <- function(title_box, image_name, button_name, description) {
  div(class="landing-page-box",
    div(title_box, class = "landing-page-box-title"),
    div(description, class = "landing-page-box-description"),
    div(class = "landing-page-icon", style= paste0("background-image: url(", image_name, ".png);
          background-size: auto 90%; background-position: center; background-repeat: no-repeat; ")),
    actionButton(button_name, NULL, class="landing-page-button")
  )
}

###############################################.
# Plot parameters ----

#Common parameters for plots
xaxis_plots <- list(title = FALSE, tickfont = list(size=14), titlefont = list(size=14), 
  showline = TRUE, tickangle = 'auto', fixedrange=TRUE)
yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE, size = 4, 
  tickfont = list(size=14), titlefont = list(size=14)) 
font_plots <- list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif')
# colors for up to 8 classes
my_colors <- c('rgb(2, 32, 48)', 'rgb(68, 84, 106)', 'rgb(49, 54, 149)', 'rgb(69, 117, 180)', 'rgb(116,173, 209)',
  'rgb(171,217,233)', 'rgb(181, 181, 181)', 'rgb(179, 198, 231)')

