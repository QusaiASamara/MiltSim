load_imported_pop <- function(data,minage,maxage,minWT,maxWT,minHT,maxHT) {
  
  # Load the data
  imp_data <- fread(data)
  
  WHO_data_HT_WT_FFM <- imp_data %>%
    mutate(ALALPHA = ifelse(SEX == 0, 0.88, 1.11),
           A50 = ifelse(SEX == 0, 13.4, 7.1),
           ALGAMMA = ifelse(SEX == 0, 12.7, 1.1),
           WHSMAX = ifelse(SEX == 0, 42.92, 37.99),
           WHS50 = ifelse(SEX == 0, 30.93, 35.98),
           AGEGAM = AGE^ALGAMMA,
           A50GAM = A50^ALGAMMA,
           FFM = ((AGEGAM + ALALPHA * A50GAM) / (AGEGAM + A50GAM)) * 
             ((WHSMAX * ((HT / 100)^2) * WT) / (WHS50 * ((HT / 100)^2) + WT))) %>%
    dplyr::select(ID, AGE, SEX, HT, WT, FFM)%>%
    filter(AGE >= minage & AGE <= maxage) %>%
    filter(WT > minWT & WT < maxWT)%>%
    filter(HT > minHT & HT < maxHT)
  
  
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

