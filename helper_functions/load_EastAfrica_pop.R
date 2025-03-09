

source("helper_functions/Z_scores_generator.R")
source("helper_functions/calculate_ffm.R")
source("helper_functions/create_time_points.R")


#----- WT Boys coefficient-----
WT_BOY_INT <- 8.986205e-01
WT_BOY_1 <- -1.058781e-01
WT_BOY_2 <- 3.767608e-02
WT_BOY_3 <- -5.156523e-03
WT_BOY_4 <- 2.882342e-04
WT_BOY_5 <- -5.634462e-06 
#----- HT Boys coefficient-----
HT_BOY_INT <- 1.059742e+00
HT_BOY_1 <- -7.604413e-02
HT_BOY_2 <- 2.387956e-02
HT_BOY_3 <- -3.002445e-03
HT_BOY_4 <- 1.622542e-04 
HT_BOY_5 <- -3.168780e-06 
#----- WT Girls coefficient-----
WT_GIRL_INT <- 8.576468e-01 
WT_GIRL_1 <- -6.356099e-02
WT_GIRL_2 <- 2.254520e-02
WT_GIRL_3 <- -3.339971e-03
WT_GIRL_4 <- 2.087267e-04
WT_GIRL_5 <- -4.548547e-06
#----- HT Girls coefficient-----
HT_GIRL_INT <- 1.039742e+00 
HT_GIRL_1 <- -7.530014e-02 
HT_GIRL_2 <- 2.693704e-02
HT_GIRL_3 <- -3.668021e-03 
HT_GIRL_4 <- 2.134928e-04
HT_GIRL_5 <- -4.442716e-06


load_East_Africa_pop <- function(subj_num, seed, minage, maxage, minWT, maxWT, minHT, maxHT, age_unit = c("months","years")) {
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
    comb_data <- comb_data %>% mutate(AGE = round(AGE / 12, 0)) %>% group_by(AGE, SEX) %>% 
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
  WHO_data_HT_WT <- generate_z_scores(comb_data, cor_var = 0.6887524, seed = seed, rep_each = subj_num , age_unit) %>%
    mutate(
      BIN = round(AGE, 0),
      AGE2 = AGE^2,
      AGE3 = AGE^3,
      AGE4 = AGE^4,
      AGE5 = AGE^5
    ) %>%
    pivot_longer(cols = c("HT", "WT"), names_to = "METRIC", values_to = "VAL") %>%
    mutate(
      FACTOR = case_when(
        SEX == 0 & METRIC == "WT" ~ WT_BOY_INT + WT_BOY_1 * AGE + WT_BOY_2 * AGE2 + WT_BOY_3 * AGE3 + WT_BOY_4 * AGE4 + WT_BOY_5 * AGE5,
        SEX == 0 & METRIC == "HT" ~ HT_BOY_INT + HT_BOY_1 * AGE + HT_BOY_2 * AGE2 + HT_BOY_3 * AGE3 + HT_BOY_4 * AGE4 + HT_BOY_5 * AGE5,
        SEX == 1 & METRIC == "WT" ~ WT_GIRL_INT + WT_GIRL_1 * AGE + WT_GIRL_2 * AGE2 + WT_GIRL_3 * AGE3 + WT_GIRL_4 * AGE4 + WT_GIRL_5 * AGE5,
        SEX == 1 & METRIC == "HT" ~ HT_GIRL_INT + HT_GIRL_1 * AGE + HT_GIRL_2 * AGE2 + HT_GIRL_3 * AGE3 + HT_GIRL_4 * AGE4 + HT_GIRL_5 * AGE5
      ),
      CORR_VAL = VAL * FACTOR
    ) %>%
    dplyr::select(AGE, SEX, METRIC, FACTOR, VAL, CORR_VAL) %>%
    pivot_longer(cols = c("VAL", "CORR_VAL"), names_to = "CORR", values_to = "VAL") %>%
    mutate(ID = (row_number() - 1) %/% 4 + 1) %>%
    pivot_wider(id_cols = c("AGE", "SEX", "ID"), names_from = c("METRIC", "CORR"), values_from = c("VAL", "FACTOR")) %>%
    dplyr::select(-FACTOR_HT_VAL, -FACTOR_WT_VAL) %>%
    rename(HT = VAL_HT_VAL, HTc = VAL_HT_CORR_VAL, WT = VAL_WT_VAL, WTc = VAL_WT_CORR_VAL, 
           FACTOR_HT = FACTOR_HT_CORR_VAL, FACTOR_WT = FACTOR_WT_CORR_VAL)
  
  # Calculate FFM and perform further transformations
  WHO_data_HT_WT_FFM <- calculate_ffm_2015(WHO_data_HT_WT) %>%
    filter(AGE >= minage & AGE <= maxage) %>%
    filter(WTc > minWT & WTc < maxWT) %>%
    filter(HTc > minHT & HTc < maxHT) %>%
    dplyr::select(ID, AGE, SEX, HT, HTc, WT,WTc, FFM,FFMc) 

  WHO_data_HT_WT_FFM_long <- WHO_data_HT_WT_FFM %>%
    dplyr::select(ID, AGE, SEX, HTc, WTc, FFMc)  %>%
    pivot_longer(cols = c("HTc","WTc","FFMc"), names_to = "METRIC", values_to = "VAL") %>%
    mutate(
      GENDER = factor(if_else(SEX == 0, "Boys", "Girls")),
      METRIC = case_when(
        startsWith(METRIC, "HT") ~ "Height (cm)",
        startsWith(METRIC, "WT") ~ "Weight (kg)",
        startsWith(METRIC, "FFM") ~ "Fat free mass (kg)"
      ),
      DTA_DES = "Eastern African virtual population"
    ) %>%
    dplyr::select(-SEX)
  

  
  return(list(WHO_data_HT_WT_FFM_long = WHO_data_HT_WT_FFM_long, WHO_data_HT_WT_FFM = WHO_data_HT_WT_FFM))
}

