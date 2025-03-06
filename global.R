# Load helper functions
folder_path <- "helper_functions/"
if (dir.exists(folder_path)) {
  files <- list.files(folder_path, pattern = "\\.R$", full.names = TRUE)
  lapply(files, source)
} else {
  warning("Directory 'helper_functions/' not found.")
}

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(profvis)
library(data.table)
library(mrgsolve)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(stringr)
library(shinycssloaders)
library(plotly)
library(DT)
library(bslib)
library(shinyalert)
library(prompter)
library(gt)
library(cowplot)
library(introdataviz)
library(kableExtra)
library(leaflet)
library(RColorBrewer)
library(htmltools)
library(conflicted)


# Resolve function conflicts
conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::filter)
