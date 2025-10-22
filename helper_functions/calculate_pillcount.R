calculate_pillcount <- function(dose_g, custom_strength = NULL) {
  if (is.null(custom_strength) || is.na(custom_strength)) {
    # Reference treatment: 50 mg and 10 mg
    (dose_g %/% 50) + ((dose_g %% 50) / 10)
  } else {
    # Custom single strength (scorable tablets)
    dose_g / custom_strength
  }
}