load_predefined_model <- function(model_name, model_path, include_iiv, include_ruv) {
  mod_MF_pk <- mread(model_path)
  mod_MF_pk <- update_model_variability(mod_MF_pk, include_iiv, include_ruv)
  loadso(mod_MF_pk)
  return(mod_MF_pk)
}