

generate_z_scores <- function(data, cor_var = 0.6887524, seed, rep_each, age_unit = c("months","years")) {
  
  # Replicate the data
  rep_dat <- slice(data, rep(1:nrow(data), each = rep_each))
  
  
  
  
  # Define the correlation matrix
  cor.wt.ht <- matrix(c(1.0, cor_var, cor_var, 1.0), nrow = 2, dimnames = list(c("zwfa", "zhfa"), c("zwfa", "zhfa")))
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Generate z-scores for height and weight
  rep_dat1 <- bind_cols(rep_dat, as.data.frame(MASS::mvrnorm(n = nrow(rep_dat), mu = c(0, 0), Sigma = cor.wt.ht)))
  
  if (age_unit == "months") {
    rep_dat1 <- rep_dat1 %>% mutate(AGE = AGE / 12)
  }
  # Generate unadjusted weight and height
  rep_dat1 %<>% mutate(
    WT = W_M * (1 + zwfa * W_L * W_S)^(1 / W_L),  # unadjusted weight
    HT = H_M * (1 + zhfa * H_L * H_S)^(1 / H_L)  # unadjusted height
  ) %>% dplyr::select(-zwfa, -zhfa) %>% as.data.frame()
  
  return(rep_dat1)
}
