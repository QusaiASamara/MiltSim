plot_pk_profiles <- function(data, log_y = FALSE, treatment_duration, use_loading_dose) {
  treatment_duration <- if (use_loading_dose) treatment_duration + 1 else treatment_duration
  
  y_min <- if (log_y) min(data$p5, na.rm = TRUE) else 0
  y_max <- max(data$p95, na.rm = TRUE)
  ec90_position <- 10.6
  eot_rect_ymin <- if (log_y) y_min * 1.1 else y_min + (y_max - y_min) * 0.05
  eot_rect_ymax <- if (log_y) y_min * 1.6 else y_min + (y_max - y_min) * 0.15
  ec90_rect_ymin <- if (log_y) ec90_position * 0.8 else ec90_position - (y_max - y_min) * 0.05
  ec90_rect_ymax <- if (log_y) ec90_position * 1.2 else ec90_position + (y_max - y_min) * 0.05
  
  p <- ggplot(data, aes(x = TIME, color = TYPE, fill = TYPE)) +
    geom_ribbon(aes(ymin = p5, ymax = p95), alpha = 0.3) +
    geom_line(aes(y = median), size = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", size = 0.5) +
    labs(
      title = "Population PK Profile",
      subtitle = "Median with 5th and 95th Percentiles",
      caption = "Source: Simulated Data",
      x = "Time (days)",
      y = "Drug Concentration (µg/mL)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 12, color = "gray40"),
      plot.caption = element_text(size = 10, face = "italic", color = "gray60"),
      axis.title = element_text(face = "bold", size = 14),
      axis.text = element_text(color = "black", size = 12),
      panel.grid.major = element_line(color = "gray85", size = 0.5),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    ) +
    geom_vline(xintercept = treatment_duration, color = "red", size = 0.5, linetype = "solid") +
    geom_rect(
      aes(
        xmin = treatment_duration - 5,
        xmax = treatment_duration + 5,
        ymin = eot_rect_ymin,
        ymax = eot_rect_ymax
      ),
      fill = "white", color = "black"
    ) +
    geom_text(
      aes(
        x = treatment_duration, 
        y = (eot_rect_ymin + eot_rect_ymax) / 2,
        label = paste0("EOT = ", treatment_duration, " days")
      ),
      color = "black", fontface = "bold", size = 4, hjust = 0.5, vjust = 0.5
    ) +
    geom_hline(yintercept = ec90_position, color = "blue", size = 0.5, linetype = "solid") +
    geom_rect(
      aes(
        xmin = max(TIME) - 12,
        xmax = max(TIME) + 2,
        ymin = ec90_rect_ymin,
        ymax = ec90_rect_ymax
      ),
      fill = "white", color = "black"
    ) +
    geom_text(
      aes(
        x = max(TIME) - 5, 
        y = (ec90_rect_ymin + ec90_rect_ymax) / 2,
        label = "EC90 = 10.6 µg/mL"
      ),
      color = "black", fontface = "bold", size = 4, hjust = 0.5, vjust = 0.5
    )
  
  if (log_y) {
    p <- p + scale_y_log10()
  }
  
  return(p)
}