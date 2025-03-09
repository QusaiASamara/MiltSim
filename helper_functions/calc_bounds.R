# Calculate bounds once
calc_bounds <- function(dt, col) {
  
  if (!is.data.table(dt)) {
    dt <- as.data.table(dt)
  }
  stats <- dt[, .(
    q1 = quantile(get(col), 0.25),
    q3 = quantile(get(col), 0.75),
    min_val = min(get(col)),
    max_val = max(get(col))
  )]
  
  iqr <- stats$q3 - stats$q1
  lower <- stats$q1 - 1.5 * iqr
  upper <- stats$q3 + 1.5 * iqr
  
  c(
    ifelse(stats$min_val > lower, stats$min_val, lower),
    ifelse(stats$max_val < upper, stats$max_val, upper)
  )
  
}