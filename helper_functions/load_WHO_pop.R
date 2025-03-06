load_WHO_pop <- function(subj_num, seed, minage, maxage,minWT,maxWT,minHT,maxHT, age_unit) {
  # Read and combine WHO data
  WA_who_total <- bind_rows(
    fread("WHO_Data/female_LMS_values.csv") %>% mutate(SEX = 1),
    fread("WHO_Data/male_LMS_values.csv") %>% mutate(SEX = 0)
  ) %>% arrange(SEX, AGE) %>% rename(W_L = L, W_M = M, W_S = S)
  
  HA_who_total <- bind_rows(
    fread("WHO_Data/lhfa_boys_0-to-2-years_zscores.csv") %>% dplyr::select(Month, L, M, S) %>% mutate(SEX = 0),
    fread("WHO_Data/lhfa_boys_2-to-5-years_zscores.csv") %>% dplyr::select(Month, L, M, S) %>% mutate(SEX = 0),
    fread("WHO_Data/lhfa_girls_0-to-2-years_zscores.csv") %>% dplyr::select(Month, L, M, S) %>% mutate(SEX = 1),
    fread("WHO_Data/lhfa_girls_2-to-5-years_zscores.csv") %>% dplyr::select(Month, L, M, S) %>% mutate(SEX = 1),
    fread("WHO_Data/hfa-boys-z-who-2007-exp.csv") %>% dplyr::select(Month, L, M, S) %>% mutate(SEX = 0),
    fread("WHO_Data/hfa-girls-z-who-2007-exp.csv") %>% dplyr::select(Month, L, M, S) %>% mutate(SEX = 1)
  ) %>% arrange(SEX, Month) %>% rename(H_L = L, H_M = M, H_S = S, AGE = Month)
  
  # Combine and correct data
  comb_data <- inner_join(WA_who_total, HA_who_total, by = c("AGE", "SEX")) %>%
    mutate(
      W_L = if_else(AGE <= 120, W_L, if_else(SEX == 0, W_L * 0.57, W_L * 0.71)),
      W_M = if_else(AGE <= 120, W_M, if_else(SEX == 0, W_M * 0.97, W_M * 0.96)),
      W_S = if_else(AGE <= 120, W_S, if_else(SEX == 0, W_S * 0.91, W_S * 0.88))
    ) %>%
    dplyr::select(AGE, W_L, W_M, W_S, SEX, H_L, H_M, H_S) %>%
    filter(AGE != dplyr::lag(AGE) | is.na(dplyr::lag(AGE))) 
  
  if (age_unit == "years") {
    comb_data <- comb_data %>%
      mutate(AGE = round(AGE / 12, 0)) %>%
      group_by(AGE, SEX) %>%
      dplyr::summarise(
        W_L = mean(W_L, na.rm = TRUE), 
        W_M = mean(W_M, na.rm = TRUE),
        W_S = mean(W_S, na.rm = TRUE),
        H_L = mean(H_L, na.rm = TRUE), 
        H_M = mean(H_M, na.rm = TRUE), 
        H_S = mean(H_S, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # Generate z-scores and perform initial transformations
  WHO_data_HT_WT <- generate_z_scores(comb_data, 0.6887524, seed = seed, rep_each = subj_num, age_unit) %>%
    mutate(ID = (row_number() - 1) %/% 1 + 1)%>%
    select(ID,AGE,SEX,WT,HT)%>% 
    filter(AGE >= minage & AGE <= maxage) %>%
    filter(WT > minWT & WT < maxWT)%>%
    filter(HT > minHT & HT < maxHT)
 
  # Calculate FFM and perform further transformations
  WHO_data_HT_WT_FFM <- WHO_data_HT_WT %>%
    mutate(ALALPHA = ifelse(SEX == 0, 0.88, 1.11),
           A50 = ifelse(SEX == 0, 13.4, 7.1),
           ALGAMMA = ifelse(SEX == 0, 12.7, 1.1),
           WHSMAX = ifelse(SEX == 0, 42.92, 37.99),
           WHS50 = ifelse(SEX == 0, 30.93, 35.98),
           AGEGAM = AGE^ALGAMMA,
           A50GAM = A50^ALGAMMA,
           FFM = ((AGEGAM + ALALPHA * A50GAM) / (AGEGAM + A50GAM)) * 
             ((WHSMAX * ((HT / 100)^2) * WT) / (WHS50 * ((HT / 100)^2) + WT))) %>%
    dplyr::select(ID, AGE, SEX, HT, WT, FFM) 
  
  
  WHO_data_HT_WT_FFM_long <- WHO_data_HT_WT_FFM %>%
    pivot_longer(cols = c("HT", "WT", "FFM"), names_to = "METRIC", values_to = "VAL") %>%
    mutate(
      GENDER = if_else(SEX == 0, "Boys", "Girls"),
      METRIC = case_when(
        startsWith(METRIC, "HT") ~ "Height (cm)",
        startsWith(METRIC, "WT") ~ "Weight (kg)",
        startsWith(METRIC, "FFM") ~ "Fat free mass (kg)")) %>%
    dplyr::select(-SEX)
    

  return(list(WHO_data_HT_WT_FFM_long = WHO_data_HT_WT_FFM_long, WHO_data_HT_WT_FFM = WHO_data_HT_WT_FFM))
}
