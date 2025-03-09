AUC_TOEC90_summary <- function(data, upper_ci_obs, lower_ci_obs) {
  data_name <- deparse(substitute(data))
  
  AUC_TOEC90_EOT <- data %>% 
    mutate(
      UPPER = ifelse(AUC > upper_ci_obs, 1, 0), 
      LOWER_TOEC90 = ifelse(TEC90 < lower_ci_obs, 1, 0)
    )
  
  basic_summary <- AUC_TOEC90_EOT %>% 
    group_by(BINNED_WT, FLAG) %>% 
    mutate(
      !!paste0("COUNT_UPPER_", data_name) := sum(UPPER), 
      !!paste0("COUNT_LOWER_", data_name) := sum(LOWER_TOEC90), 
      COUNT_BIN = n()
    ) %>% 
    filter(row_number() == 1) %>% 
    ungroup() %>% 
    mutate(
      !!paste0("PER_UPPER_", data_name) := 100 - (!!sym(paste0("COUNT_UPPER_", data_name)) / COUNT_BIN * 100), 
      !!paste0("PER_LOWER_", data_name) := 100 - (!!sym(paste0("COUNT_LOWER_", data_name)) / COUNT_BIN * 100)
    ) %>% 
    arrange(BINNED_WT) %>% 
    rename(
      !!paste0("DOSE_", data_name) := DOSE_G, 
      !!paste0("FLAG_", data_name) := FLAG
    ) %>% 
    dplyr::select(
      BINNED_WT, BIN, starts_with("WT_BAND_") , COUNT_BIN, 
      !!paste0("DOSE_", data_name), 
      !!paste0("COUNT_UPPER_", data_name), 
      !!paste0("PER_UPPER_", data_name), 
      !!paste0("COUNT_LOWER_", data_name), 
      !!paste0("PER_LOWER_", data_name), 
      !!paste0("FLAG_", data_name))
  
  return(basic_summary)
}
