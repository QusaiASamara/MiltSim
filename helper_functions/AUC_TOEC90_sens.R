AUC_TOEC90_sens <- function(data, custom_doses, upper_ci_obs, lower_ci_obs) {
  
  data_name <- deparse(substitute(data))
  
  weight_bands <- custom_doses$weight_bands
  
  weight_band_ranges <- lapply(weight_bands, function(band) {
    # Extract numeric ranges from the weight band
    range_vals <- gsub("[^0-9.-]", "", band)
    range_vals <- strsplit(range_vals, "-")[[1]] %>% as.numeric()
    return(range_vals)
  })
  
  AUC_TOEC90_EOT <- data %>% 
    mutate(UPPER = ifelse(AUC > upper_ci_obs, 1, 0), 
           LOWER_TOEC90 = ifelse(TEC90 < lower_ci_obs, 1, 0)) %>% 
    mutate(WT_BAND_Allometric_WB_custom = {
      case_when_expr <- purrr::map2(weight_band_ranges, seq_along(weight_band_ranges), function(range, idx) {
        if (!is.na(range[1]) && !is.na(range[2])) {
          paste0("BIN >= ", range[1], " & BIN < ", range[2], " ~ '", range[1], " to ", range[2], " kg'")
        } else {
          NULL
        }
      }) %>% 
        purrr::compact() %>% 
        paste(collapse = ", ")
      eval(parse(text = paste0("case_when(", case_when_expr, ", TRUE ~ NA_character_)")))
    }) %>% 
    arrange(ID)
  
  basic_summary <- AUC_TOEC90_EOT %>% 
    group_by(BINNED_WT, FLAG) %>% 
    mutate(!!paste0("COUNT_UPPER_", data_name) := sum(UPPER), 
           !!paste0("COUNT_LOWER_", data_name) := sum(LOWER_TOEC90), 
           COUNT_BIN = n()) %>% 
    filter(row_number() == 1) %>% 
    ungroup() %>% 
    mutate(!!paste0("PER_UPPER_", data_name) := 100 - (!!sym(paste0("COUNT_UPPER_", data_name)) / COUNT_BIN * 100), 
           !!paste0("PER_LOWER_", data_name) := 100 - (!!sym(paste0("COUNT_LOWER_", data_name)) / COUNT_BIN * 100)) %>% 
    arrange(BINNED_WT) %>% ungroup() %>%
    group_by(FLAG, WT_BAND_Allometric_WB_custom) %>%
    mutate(SUM_BIN_custom = !!sym(paste0("COUNT_UPPER_", data_name)) + !!sym(paste0("COUNT_LOWER_", data_name)),
           SUM_WT_BAND_custom = sum(SUM_BIN_custom)) %>%
    ungroup() %>%
    group_by(BINNED_WT) %>%
    filter(SUM_WT_BAND_custom == min(SUM_WT_BAND_custom)) %>%
    filter(row_number() == 1) %>%
    rename(!!paste0("DOSE_", data_name) := DOSE_G,
           !!paste0("FLAG_", data_name) := FLAG) %>%
    dplyr::select(BINNED_WT, BIN,WT_BAND_Allometric_WB_custom, COUNT_BIN, 
                  !!paste0("DOSE_", data_name), 
                  !!paste0("COUNT_UPPER_", data_name),
                  !!paste0("PER_UPPER_", data_name),
                  !!paste0("COUNT_LOWER_", data_name),
                  !!paste0("PER_LOWER_", data_name),
                  !!paste0("FLAG_", data_name)) 

  DOSE_summary <- basic_summary %>%
    group_by(WT_BAND_Allometric_WB_custom,  !!sym(paste0("DOSE_", data_name))) %>%
    mutate(COUNT_BIN_dose = sum(COUNT_BIN)) %>%
    filter(COUNT_BIN_dose == max(COUNT_BIN_dose), row_number() == 1) %>%
    select(WT_BAND_Allometric_WB_custom, starts_with("DOSE"))
  

  
  return(list(basic_summary = basic_summary, DOSE_summary = DOSE_summary))
}
