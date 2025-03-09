run_sensitivity_analysis <- function(model, data, param_name, step_size) {
  results <- list()
  original_value <- param(model)[[param_name]]
  
  for (i in seq(-step_size, step_size, by = step_size)) {
    new_value <- original_value * (1 + i / 100)
    model <- param(model, setNames(list(new_value), param_name))
    sim <- model %>% 
      data_set(data) %>% 
      carry.out(a.u.g) %>% 
      obsaug %>% 
      mrgsim() %>% 
      as.data.frame()
    results[[paste0(param_name, "_", i)]] <- sim
  }
  
  model <- param(model, setNames(list(original_value), param_name))
  return(results)
}

