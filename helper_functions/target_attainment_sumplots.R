target_attainment_sumplots <- function(ref_data, binned_df, dosing_strategy, ref_regimen_name) {
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
  
  # Function to create main plots
  create_main_plot <- function(data, metric, bounds, title) {
    ggplot(data, aes(x = as.factor(BIN), y = get(metric), fill = WT_BAND)) +
      geom_rect(
        xmin = -Inf, xmax = Inf,
        ymin = bounds[1], ymax = bounds[2],
        fill = "lightyellow", alpha = 0.3
      ) +
      geom_boxplot(color = "black", size = 0.5, outlier.shape = NA) +
      labs(
        title = dosing_strategy,
        x = "Weight (kg)",
        y = ifelse(metric == "AUC", 
                   "AUC at EOT (mg*day/L)", 
                   "T>EC90 at EOT (days)"),
        fill = "WT_BAND"
      ) +
      base_theme
  }
  
  # Create all plots efficiently
    p1 <- create_main_plot(binned_dt, "AUC", bounds$AUC, dosing_strategy)
    p2 <- create_main_plot(binned_dt, "TEC90", bounds$TEC90, dosing_strategy) +
      labs(caption = paste0(
        "The yellow shaded area represents the Tukey's hinges range for ", ref_regimen_name, "\n",
        "Box plots represent the distribution of AUC and TEC90 values across weights for ", dosing_strategy, " (excluding outliers)."
      )) +
      theme(plot.caption = element_text(hjust = 0.5, size = 7))


    plot_grid(p1, p2, ncol = 1, align = "v")
}
