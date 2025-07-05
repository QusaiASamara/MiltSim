update_model_variability <- function(model, include_iiv, include_ruv) {
  if (!include_iiv) {
    model <- update(model, omega = matrix(0, nrow = 2, ncol = 2))
  }
  
  if (!include_ruv) {
    model <- update(model, sigma = matrix(0, nrow = 1, ncol = 1))
  }
  
  return(model)
}
