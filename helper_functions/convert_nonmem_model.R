convert_nonmem_model <- function(files, temp_dir) {
  # Validate input files
  if (length(files) == 0) {
    stop("No files uploaded")
  }
  
  # Identify control and extension files
  control_files <- files$name[grepl("\\.ctl$|\\.mod$", files$name)]
  ext_files <- files$name[grepl("\\.ext$", files$name)]
  
  if (length(control_files) == 0 || length(ext_files) == 0) {
    stop("Need both a NONMEM control file (.ctl or .mod) and .ext file")
  }
  
  # Remove file extension to get base filename
  control_file_base <- tools::file_path_sans_ext(control_files[1])
  
  # Perform translation
  nonmem2mrgsolve::nonmem2mrgsolve(
    filename = control_file_base,
    dir = temp_dir,
    sigdig = 4,
    write = TRUE,
    return.orig = FALSE
  )
  
  # Find the generated R file
  generated_files <- list.files(
    path = temp_dir, 
    pattern = "\\.R$",
    full.names = TRUE
  )
  
  if (length(generated_files) == 0) {
    stop("No translated file found")
  }
  
  return(generated_files[1])
}