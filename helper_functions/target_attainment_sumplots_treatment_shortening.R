target_attainment_sumplots_treatment_shortening <- function(ref_data, binned_df, dosing_strategy, ref_regimen_name) {
  # Pre-process data once
  required_cols <- c("ID", "AUC", "TEC90", "BIN", "BINNED_WT","T_EC90")
  
  # More efficient data preprocessing using data.table
  library(data.table)
  
  # Convert to data.table and rename column in one step
  ref_dt <- setDT(ref_data)[, .(
    ID, 
    WT_BAND = get(grep("^WT_BAND", names(ref_data), value = TRUE)[1]),
    AUC, 
    TEC90, 
    BIN, 
    BINNED_WT,
    T_EC90
  )]
  
  binned_dt <- setDT(binned_df)[, .(
    ID, 
    WT_BAND = get(grep("^WT_BAND", names(binned_df), value = TRUE)[1]),
    AUC, 
    TEC90, 
    BIN, 
    BINNED_WT,
    COUNT_BIN_bin,
    T_EC90
  )]
  
  # Calculate bounds for both metrics at once
  bounds <- list(
    AUC = calc_bounds(ref_dt, "AUC"),
    TEC90 = calc_bounds(ref_dt, "TEC90"),
    T_EC90 = calc_bounds(ref_dt, "T_EC90"))
  
  # Function to create main plots
  create_main_plot <- function(data, metric, bounds, title) {
    pct_in_bounds <- round(100 * sum(data[[metric]] >= bounds[1] & 
                                       data[[metric]] <= bounds[2], na.rm = TRUE) /
                             sum(!is.na(data[[metric]])), 1)
    
    y_range <- range(data[[metric]], na.rm = TRUE)
    y_label_pos <- y_range[2] + (y_range[2] - y_range[1]) * 0.15
    
    ggplot(data, aes(x = as.factor(BIN), y = get(metric), fill = WT_BAND)) +
      geom_rect(
        xmin = -Inf, xmax = Inf,
        ymin = bounds[1], ymax = bounds[2],
        fill = "lightyellow", alpha = 0.3,
        color = "gold", size = 0.2, linetype = "dashed"
      ) +
      geom_boxplot(color = "black", size = 0.5, outlier.shape = NA) +
      labs(
        x = "Weight (kg)",
        subtitle = sprintf("%s - %.1f%% within reference bounds", metric, pct_in_bounds),
        y = ifelse(metric == "AUC",
                   "AUC at EOT (mg*day/L)",
                   "T>EC90 (days)"),
        fill = "WT_BAND"
      ) +
      geom_hline(
        yintercept = ifelse(metric == "AUC", upper_ci_obs_AUC, lower_ci_obs_TOEC90), 
        linetype = "dashed", 
        color = "yellowgreen", 
        size = 0.8
      ) +
      geom_text(
        data = data,
        aes(x = as.factor(BIN), y = y_label_pos, label = COUNT_BIN_bin),
        size = 3,
        vjust = 0.5
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(face = "bold", size = 12, hjust = 0),
        plot.subtitle = element_text(hjust = 0, size = 13, color = "gray40"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5),
        legend.position = "right"
      )
  }
  
  # Create third plot for T_EC90
  create_tec90_plot <- function(data, title) {
    # Calculate bounds for T_EC90
    tec90_bounds <- bounds$T_EC90
    
    pct_in_bounds <- round(100 * sum(data$T_EC90 >= tec90_bounds[1] & 
                                       data$T_EC90 <= tec90_bounds[2], na.rm = TRUE) /
                             sum(!is.na(data$T_EC90)), 1)
    
    y_range <- range(data$T_EC90, na.rm = TRUE)
    y_label_pos <- y_range[2] + (y_range[2] - y_range[1]) * 0.15
    
    ggplot(data, aes(x = as.factor(BIN), y = T_EC90, fill = WT_BAND)) +
      geom_rect(
        xmin = -Inf, xmax = Inf,
        ymin = tec90_bounds[1], ymax = tec90_bounds[2],
        fill = "lightyellow", alpha = 0.3,
        color = "gold", size = 0.2, linetype = "dashed"
      ) +
      geom_boxplot(color = "black", size = 0.5, outlier.shape = NA) +
      labs(
        x = "Weight (kg)",
        subtitle = sprintf("T_EC90 - %.1f%% within reference bounds", pct_in_bounds),
        y = "T_EC90 (days)",
        fill = "WT_BAND"
      ) +
      geom_hline(
        yintercept = 3, 
        linetype = "dashed", 
        color = "yellowgreen", 
        size = 0.8
      ) +
      geom_text(
        data = data,
        aes(x = as.factor(BIN), y = y_label_pos, label = COUNT_BIN_bin),
        size = 3,
        vjust = 0.5
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(face = "bold", size = 12, hjust = 0),
        plot.subtitle = element_text(hjust = 0, size = 13, color = "gray40"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5),
        legend.position = "right"
      )
  }
  
  # Create all plots efficiently
  p1 <- create_main_plot(binned_dt, "AUC", bounds$AUC, paste(dosing_strategy, "- AUC Distribution"))
  p2 <- create_main_plot(binned_dt, "TEC90", bounds$TEC90, paste(dosing_strategy, "- T>EC90 Distribution"))
  p3 <- create_tec90_plot(binned_dt, paste(dosing_strategy, "- T to EC90 Distribution"))
  
  # Combine all three plots vertically
  plot_grid(p1, p2, p3, ncol = 1, align = "v")
}
