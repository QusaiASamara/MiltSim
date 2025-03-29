create_temp_model_dir <- function() {
  # Create a unique temporary directory for this session
  temp_dir <- file.path(tempdir(), paste0("nonmem_model_", 
                                          format(Sys.time(), "%Y%m%d_%H%M%S"), 
                                          "_", 
                                          sample(1:10000, 1)))
  
  # Create the directory
  dir.create(temp_dir, recursive = TRUE)
  
  return(temp_dir)
}