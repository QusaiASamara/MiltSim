calculate_ffm_2015 <- function(data) {
  data %>%
    mutate(ALALPHA = ifelse(SEX == 0, 0.88, 1.11),
           A50 = ifelse(SEX == 0, 13.4, 7.1),
           ALGAMMA = ifelse(SEX == 0, 12.7, 1.1),
           WHSMAX = ifelse(SEX == 0, 42.92, 37.99),
           WHS50 = ifelse(SEX == 0, 30.93, 35.98),
           AGEGAM = AGE^ALGAMMA,
           A50GAM = A50^ALGAMMA,
           FFM = ((AGEGAM + ALALPHA * A50GAM) / (AGEGAM + A50GAM)) * 
             ((WHSMAX * ((HT / 100)^2) * WT) / (WHS50 * ((HT / 100)^2) + WT)),
           FFMc = ((AGEGAM + ALALPHA * A50GAM) / (AGEGAM + A50GAM)) * 
             ((WHSMAX * ((HTc / 100)^2) * WTc) / (WHS50 * ((HTc / 100)^2) + WTc)))
    
}
