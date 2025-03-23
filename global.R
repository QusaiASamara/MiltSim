check_and_install_packages <- function(required_packages) {
  # Check which packages are not installed
  is_installed <- sapply(required_packages, requireNamespace, quietly = TRUE)
  missing_pkgs <- required_packages[!is_installed]
  
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
  
  # Load all packages - wrap in tryCatch to continue even if some fail
  for (pkg in required_packages) {
    tryCatch({
      library(pkg, character.only = TRUE)
    }, error = function(e) {
      warning("Could not load package: ", pkg)
    })
  }
}

# Define required packages
required_packages <- c(
  "shiny", "useShinyjs", "useShinyjs", "ggplot2", "dplyr", "tidyr", 
  "data.table", "mrgsolve", "shinythemes", "plotly", "DT", "gt", 
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
