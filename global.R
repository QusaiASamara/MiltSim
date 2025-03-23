# Function to check and install missing packages
check_and_install_packages <- function(required_packages) {
  # Convert to data.table for efficiency with large package lists
  pkg_status <- data.table(
    package = required_packages,
    installed = sapply(required_packages, requireNamespace, quietly = TRUE)
  )
  
  # Filter missing packages
  missing_pkgs <- pkg_status[installed == FALSE, package]
  
  # Install missing packages if any
  if (length(missing_pkgs) > 0) {
    message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
    install.packages(missing_pkgs, dependencies = TRUE)
    
    # Verify installation success
    still_missing <- sapply(missing_pkgs, function(pkg) !requireNamespace(pkg, quietly = TRUE))
    if (any(still_missing)) {
      warning("Failed to install: ", paste(missing_pkgs[still_missing], collapse = ", "))
    }
  }
  
  # Load all packages at once
  invisible(sapply(required_packages, library, character.only = TRUE))
}

# Define required packages
required_packages <- c(
  "shiny", "ggplot2", "dplyr", "tidyr", "data.table", 
  "mrgsolve", "shinythemes", "plotly", "DT", "gt", 
  "kableExtra", "leaflet", "htmltools", "conflicted"
)

# Check, install if needed, and load packages
check_and_install_packages(required_packages)

# Load helper functions
folder_path <- "helper_functions/"
if (dir.exists(folder_path)) {
  files <- list.files(folder_path, pattern = "\\.R$", full.names = TRUE)
  lapply(files, source)
} else {
  warning("Directory 'helper_functions/' not found.")
}

# Resolve function conflicts
conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::filter)