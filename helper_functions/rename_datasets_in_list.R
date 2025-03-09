rename_columns <- function(df, list_name, exclude_cols = c("BINNED_WT", "BIN", "COUNT_BIN")) {
  df %>% 
    rename_with(~ ifelse(.x %in% exclude_cols, 
                         .x, 
                         paste0(list_name, "_", .x)))
}

# Function to apply renaming to all datasets in the list
rename_datasets_in_list <- function(data_list) {
  lapply(names(data_list), function(list_name) {
    data_list[[list_name]] <- lapply(data_list[[list_name]], function(df) 
      rename_columns(df, list_name))
    return(data_list[[list_name]])
  }) %>% 
    setNames(names(data_list))
}
