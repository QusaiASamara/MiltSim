target_attainment_sumplots <- function(ref_data, binned_df, dosing_strategy) {
  # Pre-process data once
  required_cols <- c("ID", "AUC", "TEC90", "BIN", "BINNED_WT")
  
  # More efficient data preprocessing using data.table
  library(data.table)
  
  # Convert to data.table and rename column in one step
  ref_dt <- setDT(ref_data)[, .(
    ID, 
    WT_BAND = get(grep("^WT_BAND", names(ref_data), value = TRUE)[1]),
    AUC, 
    TEC90, 
    BIN, 
    BINNED_WT
  )]
  
  binned_dt <- setDT(binned_df)[, .(
    ID, 
    WT_BAND = get(grep("^WT_BAND", names(binned_df), value = TRUE)[1]),
    AUC, 
    TEC90, 
    BIN, 
    BINNED_WT
  )]
  
  # Calculate bounds once
  calc_bounds <- function(dt, col) {
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
  
  # Calculate bounds for both metrics at once
  bounds <- list(
    AUC = calc_bounds(ref_dt, "AUC"),
    TEC90 = calc_bounds(ref_dt, "TEC90")
  )
  
  # Create base theme once
  base_theme <- theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "right"
    )
  
  # Create reference theme once
  ref_theme <- theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "none"
    )
  
  # Function to create main plots
  create_main_plot <- function(data, metric, bounds, title) {
    ggplot(data, aes(x = as.factor(BIN), y = get(metric), fill = WT_BAND)) +
      geom_rect(
        xmin = -Inf, xmax = Inf,
        ymin = bounds[1], ymax = bounds[2],
        fill = "lightyellow", alpha = 0.3
      ) +
      geom_boxplot(color = "black", size = 0.5) +
      labs(
        title = paste(dosing_strategy, "dosing strategy"),
        x = "Weight (kg)",
        y = ifelse(metric == "AUC", 
                   "AUC at EOT (mg*day/L)", 
                   "T>EC90 at EOT (days)"),
        fill = "WT_BAND"
      ) +
      base_theme
  }
  
  # Function to create reference plots
  create_ref_plot <- function(data, metric, bounds, title) {
    ggplot(data, aes(x = "Ref", y = get(metric), fill = "ref")) +
      geom_rect(
        xmin = -Inf, xmax = Inf,
        ymin = bounds[1], ymax = bounds[2],
        fill = "lightyellow", alpha = 0.3
      ) +
      geom_boxplot(color = "black", size = 0.5) +
      labs(
        title = "Reference chart",
        y = ifelse(metric == "AUC", 
                   "AUC at EOT (mg*day/L)", 
                   "T>EC90 at EOT (days)")
      ) +
      ref_theme
  }
  
  # Create all plots efficiently
  plots <- list(
    p1 = create_main_plot(binned_dt, "AUC", bounds$AUC),
    p2 = create_main_plot(binned_dt, "TEC90", bounds$TEC90),
    ref_p1 = create_ref_plot(ref_dt, "AUC", bounds$AUC),
    ref_p2 = create_ref_plot(ref_dt, "TEC90", bounds$TEC90)
  )
  
  # Combine plots
  top_row <- plot_grid(plots$p1, plots$ref_p1, ncol = 2, rel_widths = c(4, 1))
  bottom_row <- plot_grid(plots$p2, plots$ref_p2, ncol = 2, rel_widths = c(4, 1))
  
  plot_grid(top_row, bottom_row, ncol = 1)
}
