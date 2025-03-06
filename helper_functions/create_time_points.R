
create_time_points <- function(data, treatment_duration) {
  
  dosing <- (treatment_duration - 1)
  nondosing <- (112 - (dosing*2))
  
  # Create time points
  times_MF <- 0:672
  times_post_MF <- 672:1344
  
  # Generate ID values
  ID_vals <- rep(1:length(data$ID), each = 113)
  
  # Generate dose times
  dose_times <- c(1, rep(seq(0, 1), times = dosing), rep(0, times = nondosing))
  
  # Extract unique time points
  extracted_times <- unique(c(times_MF[seq(1, length(times_MF), 12)], 
                              times_post_MF[seq(1, length(times_post_MF), 12)]))
  
  # Create a data.table directly with repeated time points and dose times
  n_ids <- length(unique(data$ID))
  times_df <- data.table(
    ID = rep(1:n_ids, each = length(extracted_times)), 
    TIME = rep(extracted_times, times = n_ids),
    EVID = rep(dose_times, times = n_ids)
  )
  
  # Add CMT column
  times_df[, CMT := ifelse(EVID == 1, 1, 2)]
  
  return(times_df)
}
