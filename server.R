server <- function(input, output, session) {
  
  #################################
  ####    virtual population   ####
  #################################
  
  observeEvent(input$details_pop, {
    showModal(modalDialog(
      title = "Simulation Settings Details",
      "Here you can find more information about the simulation settings.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$show_inputs, {
    toggle("covariate_panel")
  })
  
  model <- eventReactive(input$model, {
    shiny::req(input$model)
    if (input$model == "L. Verrest (2023)") {
      mod_MF_pk <- mread("helper_functions/Verrest_pk_model.cpp")
      loadso(mod_MF_pk)  
    } else if (input$model == "Upload Own Model") {
      shiny::req(input$pk_model_file)
      model_path <- input$pk_model_file$datapath
      own_model <- mread(model_path)
      loadso(own_model)
    } else {
      NA
    }
  })
  
  data <- eventReactive(input$go_button, {
    if (input$population_type == "predefined") {
      shiny::req(input$population)
      
      if (input$use_seed) {
        shiny::validate(
          shiny::need(input$seed_value, "Please enter a seed value.")
        )
      }
      
      # Determine seed value
      seed_value <- if (input$use_seed) {
        as.integer(input$seed_value)
      } else {
        as.integer(sample.int(1e6, 1))
      }
      
      
      params <- list(
        subj_num = input$subj_num,
        seed = seed_value,
        minage = input$min_age,
        maxage = input$max_age,
        minWT = input$min_WT,
        maxWT = input$max_WT,
        minHT = input$min_HT,
        maxHT = input$max_HT,
        age_unit = input$age_unit
      )
      
      # Load data based on selected population
      data_list <- switch(input$population,
                          "Pediatric Eastern African Virtual Population" = do.call(load_East_Africa_pop, params),
                          "WHO Virtual Population" = do.call(load_WHO_pop, params))
      
      return(data_list)
      
      
    } else if (input$population_type == "import") {
      shiny::req(input$upload_csv)
      
      # Read the uploaded file
      file_path <- input$upload_csv$datapath
      
      # Load the imported population data
      data_list <- load_imported_pop(file_path)
      
      return(data_list)
    }
  })
  
  output$model_loaded <- renderText({
    shiny::req(input$go_button)
    
    if (input$use_seed) {
      shiny::validate(
        shiny::need(input$seed_value, "Please enter a seed value.")
      )
    }
    
    "Population is loaded successfully!"
  })
  
  output$download_template <- downloadHandler(
    filename = function() { "population_template.csv" },
    content = function(file) {
      file.copy("Virtual_population_template.csv", file)
    }
  )
  
  output$virtual_pop <- renderPlot({
    shiny::req(input$explore)
    data_list <- data()
    
    mean_se <- function(x) {
      m <- mean(x, na.rm = TRUE)
      se <- sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
      data.frame(y = m, ymin = m - se, ymax = m + se)
    }
    
    rain_height <- 0.1
    
    
    ggplot(data_list$WHO_data_HT_WT_FFM_long, aes(x = "", y = VAL, fill = GENDER)) +
      # Cloud: flat violins (with slight nudge to the right)
      introdataviz::geom_flat_violin(trim = FALSE, alpha = 0.2,
                                     position = position_nudge(x = rain_height + 0.05)) +
      # Rain: jittered points representing individual observations
      geom_point(aes(colour = GENDER), size = 2, alpha = 0.09, show.legend = FALSE,
                 position = position_jitter(width = rain_height, height = 0)) +
      # Boxplots: nudged to the left to separate from the violins and points
      geom_boxplot(width = rain_height, 
                   alpha = 0.9, 
                   show.legend = FALSE,
                   outlier.shape = NA,
                   position = position_dodge(width = 0.1)) +
      # Mean and SE: summarized as a point-range and nudged to further separate layers
      stat_summary(fun.data = mean_se, geom = "pointrange",
                   aes(color = GENDER), show.legend = FALSE,
                   position = position_nudge(x = rain_height * 3)) +
      # Faceting by metric; adjust the facet variable if needed
      facet_wrap(~ METRIC, nrow = 3,scales = "free_x") +
      # X-axis: minimal since we are using coord_flip()
      scale_x_discrete(name = "", expand = c(rain_height * 3, 0, 0, 0.7)) +
      # Y-axis: using pretty breaks based on data
      scale_y_continuous(name = "", breaks = scales::pretty_breaks(n = 20),limits = c(0, NA),
                         expand = expansion(mult = c(0, 0.1))) +
      # Flip coordinates for a horizontal layout
      coord_flip() +
      # Color scales for fill and colour
      scale_fill_brewer(palette = "Dark2", name = "Gender") +
      scale_colour_brewer(palette = "Dark2") +
      # Add a caption with a description of the plot
      labs(
        caption = "The flat violins (clouds) represent the full distribution of each covariate, 
        the individual dots indicate the raw data points, the boxplots summarize the distribution 
        (median and interquartile range), and the point-range markers within the clouds display the 
        mean and its standard error.") +
      # Use a minimal theme with professional touches
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),            # Remove horizontal grid lines
        strip.text = element_text(face = "bold", size = 12), # Bold facet labels
        strip.background = element_rect(fill = "gray90", color = NA),
        legend.position = c(0.85, 0.85),                   # Position legend inside at top-right
        legend.position.inside = TRUE,
        legend.background = element_rect(fill = "white", color = "white"),
        axis.title.y = element_blank(),                    # Remove Y-axis title if desired
        axis.text = element_text(color = "gray20"),
        # Adjust the caption appearance
        plot.caption = element_text(size = 12, face = "italic", hjust = 0)
      )
    
  }) 
  
  
  output$id_count <- renderValueBox({
    shiny::req(input$explore)
    data_list <- data()
    id_count <- length(unique(data_list$WHO_data_HT_WT_FFM_long$ID))
    valueBox(
      value = id_count,
      subtitle = "Total Individuals count",
      icon = icon("id-badge"))
  })
  
  output$gender_distribution <- renderValueBox({
    shiny::req(input$explore)
    data_list <- data()
    unique_data <- data_list$WHO_data_HT_WT_FFM_long[!duplicated(data_list$WHO_data_HT_WT_FFM_long$ID), ]
    gender_counts <- table(unique_data$GENDER)    
    valueBox(
      value = paste0("Males: ", gender_counts["Boys"], ", Females: ", gender_counts["Girls"]),
      subtitle = "Gender Distribution",
      icon = icon("venus-mars"),
    )
  })
  
  output$demographics_map <- renderLeaflet({
    shiny::req(
      input$go_button,
      input$population == "Pediatric Eastern African Virtual Population",
      input$pop_sim_res == "Demographics"
    )
    
    # Generate demographic map
    create_demographic_map()
  })
  
  
  
  ################################
  ####    dosing strategy     ####
  ################################
  regimen_data <- regimen_server("regimen")
  
  
  combined_regimens <- eventReactive(input$run_model, {
    shiny::req(input$weight)
    # Get all saved regimens
    all_regimens <- regimen_data$regimens()
    
    validate(
      need(!is.null(input$weight), "Please enter a weight value."))
    
    if (is.null(all_regimens) || length(all_regimens) == 0) {
      showNotification("No regimens found. Please add a regimen before running the model.", type = "warning")
      return(NULL)
    }
    regimen_list <- list()
    
    
    first_regimen <- all_regimens[[1]]  
    
    # Extract maintenance dose frequency and interval
    mainta_freq <- first_regimen$maintenance_dose$frequency
    mainta_interval <- first_regimen$maintenance_dose$interval
    
    
    
    ref_data <- create_allom_dataset(
      data = data()$WHO_data_HT_WT_FFM,
      model = model(),
      weight = input$weight,
      seed = 9119,
      use_loading_dose = FALSE,
      fixed_load_dose = NULL,
      load_freq = NULL,
      load_interval = NULL,
      main_freq = mainta_freq,
      main_interval = mainta_interval,
      weight_bands = NULL, 
      type = "ref", 
      upper = NULL,
      lower = NULL)
    
    
    
    
    upper_ci_obs_AUC_ref <- ref_data$upper_ci_obs_AUC
    lower_ci_obs_TOEC90_ref <- ref_data$lower_ci_obs_TOEC90
    ref_sumplot <- ref_data$sumplot
    
    assign ("upper_ci_obs_AUC", if (!is.na(input$upp_limit) && input$custom_limit) {
      input$upp_limit
    } else {
      upper_ci_obs_AUC_ref  # Use stored reactive value if available
    }, envir = .GlobalEnv)
    
    assign("lower_ci_obs_TOEC90", if (!is.na(input$lower_limit) && input$custom_limit) {
      input$lower_limit
    } else {
      lower_ci_obs_TOEC90_ref  # Use stored reactive value if available
    }, envir = .GlobalEnv)
    # 
    # assign("upper_ci_obs_AUC", upper_ci_obs_AUC, envir = .GlobalEnv)
    # assign("lower_ci_obs_TOEC90", lower_ci_obs_TOEC90, envir = .GlobalEnv)
    assign("ref_data", ref_sumplot, envir = .GlobalEnv)
    
    
    # Iterate over each saved regimen
    for (regimen_name in names(all_regimens)) {
      
      current_regimen <- all_regimens[[regimen_name]]
      regimen_label <- current_regimen$name  # Ensure this is the correct field
      
      # Extract values for current regimen
      dosing_strategy <- current_regimen$strategy
      use_loading_dose <- !is.null(current_regimen$loading_dose)
      loading_dose_fixed <- current_regimen$loading_dose$fixed_dose
      loading_freq <- ifelse(use_loading_dose, current_regimen$loading_dose$frequency, 0)
      loading_interval <- ifelse(use_loading_dose, current_regimen$loading_dose$interval, 0)
      maint_freq <- current_regimen$maintenance_dose$frequency
      maint_interval <- current_regimen$maintenance_dose$interval
      weight_bands <- current_regimen$loading_dose$weight_bands
      custom_doses <- current_regimen$custom_doses
      
      
      
      # Generate regimen based on dosing strategy
      generated_regimen <- switch(dosing_strategy,
                                  "Allometric_FFM" = create_allom_dataset(
                                    data = data()$WHO_data_HT_WT_FFM,
                                    model = model(),
                                    weight = input$weight,
                                    seed = 9119,
                                    use_loading_dose = use_loading_dose,
                                    fixed_load_dose = loading_dose_fixed,
                                    load_freq = loading_freq,
                                    load_interval = loading_interval,
                                    main_freq = maint_freq,
                                    main_interval = maint_interval,
                                    weight_bands = weight_bands,
                                    type = "calc",
                                    upper = upper_ci_obs_AUC,
                                    lower = lower_ci_obs_TOEC90),
                                  "Conventional" = create_lin_dataset(
                                    data = data()$WHO_data_HT_WT_FFM,
                                    model = model(),
                                    weight = input$weight,
                                    seed = 9119,
                                    use_loading_dose = use_loading_dose,
                                    fixed_load_dose = loading_dose_fixed,
                                    load_freq = loading_freq,
                                    load_interval = loading_interval,
                                    main_freq = maint_freq,
                                    main_interval = maint_interval,
                                    weight_bands = weight_bands),
                                  "Allometric_WB" = create_allometric_WB_dosing(
                                    data = data()$WHO_data_HT_WT_FFM,
                                    model = model(),
                                    weight = input$weight,
                                    seed = 9119,
                                    use_loading_dose = use_loading_dose,
                                    fixed_load_dose = loading_dose_fixed,
                                    load_freq = loading_freq,
                                    load_interval = loading_interval,
                                    main_freq = maint_freq,
                                    main_interval = maint_interval,
                                    weight_bands = weight_bands),
                                  "costum_allometric_WB" = create_costum_allometric_WB_dosing(
                                    data = data()$WHO_data_HT_WT_FFM,
                                    model = model(),
                                    custom_doses = custom_doses,
                                    weight = input$weight,
                                    seed = 9119,
                                    use_loading_dose = use_loading_dose,
                                    fixed_load_dose = loading_dose_fixed,
                                    load_freq = loading_freq,
                                    load_interval = loading_interval,
                                    main_freq = maint_freq,
                                    main_interval = maint_interval,
                                    weight_bands_load = weight_bands,
                                    upper_limit = upper_ci_obs_AUC,
                                    lower_limit = lower_ci_obs_TOEC90
                                  ))
      
      # Add the generated regimen to the list with its name
      regimen_list[[regimen_label]] <- generated_regimen
    }
    return(regimen_list)
  })
  
  observeEvent(combined_regimens(), {
    shiny::req(combined_regimens())
    
    updateSelectInput(session,
                      inputId = "select_hazard_sumplot",
                      choices = names(combined_regimens()),
                      selected = names(combined_regimens())[1])
  })
  
  observeEvent(combined_regimens(), {
    shiny::req(combined_regimens())
    
    updateSelectInput(session,
                      inputId = "select_sum_plot",
                      choices = names(combined_regimens()),
                      selected = names(combined_regimens())[1])
  })
  
  observeEvent(combined_regimens(), {
    shiny::req(combined_regimens())
    
    updateSelectInput(session,
                      inputId = "pk_profiles_choice",
                      choices = names(combined_regimens()),
                      selected = names(combined_regimens())[1])
  })
  
  
  
  #####################################
  ####    target attainment plot   ####
  #####################################
  
  
  output$target_attainment_plot <- renderPlotly({
    shiny::req(input$run_model)
    data_frames <- combined_regimens()
    
    
    AUC_TOEC90_datasets <- lapply(data_frames, function(lst) {
      lst[grep("^AUC_TOEC90_", names(lst))]
    })
    
    renamed_datasets <- rename_datasets_in_list(AUC_TOEC90_datasets)
    
    all_dataframes <- do.call(c, renamed_datasets)
    
    
    combined_df <- purrr::reduce(all_dataframes, full_join, by = c("BINNED_WT", "BIN"))
    
    target_attainment <- combined_df %>%
      gather(TYPE, VALUE, matches("PER_UPPER_|PER_LOWER_"), factor_key = FALSE) %>%
      mutate(
        # Extract regimen name (everything before first underscore "_")
        REGIMEN = sub("_.*", "", TYPE),
        
        # Label for upper/lower limit
        TYPE_LIMIT = if_else(
          str_detect(TYPE, "PER_UPPER_"), 
          "Upper limit of AUC", 
          "Lower limit of T>EC90"
        ),
        
        # Define limit values
        LIMIT = if_else(
          str_detect(TYPE, "PER_UPPER_"),
          paste0("limit =", round(upper_ci_obs_AUC), " ug*day/mL"),
          paste0("limit =", round(lower_ci_obs_TOEC90), " days")
        ),
        
        # Rename TYPE based on regimen
        TYPE = case_when(
          str_detect(TYPE, "Allometric_FFM$") ~ paste(REGIMEN, "Allometric FFM-based"),
          str_detect(TYPE, "conventional$") ~ paste(REGIMEN, "Conventional (mg/kg)"),
          str_detect(TYPE, "Allometric_WB$") ~ paste(REGIMEN, "Allometric WB-based"),
          str_detect(TYPE, "Allometric_WB_custom$") ~ paste(REGIMEN, "Custom Allometric WB-based"),
          TRUE ~ paste(REGIMEN, "Other")
        )
      ) %>%
      # Use row-wise transformation to get the correct DOSE column
      rowwise() %>%
      mutate(
        DOSE = case_when(
          TYPE == paste(REGIMEN, "Allometric FFM-based") & paste0(REGIMEN, "_DOSE_Allometric_FFM") %in% names(pick(everything())) ~ 
            paste0("Dose: ", getElement(pick(everything()), paste0(REGIMEN, "_DOSE_Allometric_FFM"))),
          
          TYPE == paste(REGIMEN, "Conventional (mg/kg)") & paste0(REGIMEN, "_DOSE_conventional") %in% names(pick(everything())) ~ 
            paste0("Dose: ", getElement(pick(everything()), paste0(REGIMEN, "_DOSE_conventional"))),
          
          TYPE == paste(REGIMEN, "Allometric WB-based") & paste0(REGIMEN, "_DOSE_Allometric_WB") %in% names(pick(everything())) ~ 
            paste0("Dose: ", getElement(pick(everything()), paste0(REGIMEN, "_DOSE_Allometric_WB"))),
          
          TYPE == paste(REGIMEN, "Custom Allometric WB-based") & paste0(REGIMEN, "_DOSE_Allometric_WB_custom") %in% names(pick(everything())) ~ 
            paste0("Dose: ", getElement(pick(everything()), paste0(REGIMEN, "_DOSE_Allometric_WB_custom"))),
          
          TRUE ~ "Other"
        )
      ) %>%
      ungroup()
    
    
    plot <- ggplot(target_attainment, aes(x = BIN, y = VALUE, color = TYPE)) + 
      geom_line(na.rm = TRUE) + 
      geom_point(aes(text = DOSE)) +
      facet_wrap(~TYPE_LIMIT) +
      theme_bw() +  
      labs(x = "Weight (kg)", y = "% of patients within limits") +
      ggtitle("Target achievement of simulated patients' profiles") + 
      theme(legend.position = "bottom", strip.text = element_text(size = 12)) +
      scale_color_discrete(name = "")
    
    ggplotly(plot)
    
  }) 
  
  
  
  output$proposed_doses <- renderUI({
    shiny::req(input$run_model)
    shiny::req(input$select_sum_plot)
    shiny::req(combined_regimens())
    data_frames <- combined_regimens()
    
    DOSE_datasets <- lapply(data_frames, function(lst) {
      lst[grep("^DOSE", names(lst))]
    })
    
    Dose_sum <- DOSE_datasets[[input$select_sum_plot]][["DOSE_summary"]]
    
    shiny::validate(
      shiny::need(!is.null(Dose_sum), "No dosing information found for the selected regimen.")
    )
    
    # Generate a properly formatted table with kableExtra
    table_html <- knitr::kable(Dose_sum, col.names = c("Weight Band", "Proposed Dose (mg)"), align = c("l", "c")) %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
    
    HTML(table_html)  # Return as HTML to be displayed
  })
  
  
  
  
  #####################################
  ####        pk profile plot      ####
  #####################################
  
  output$pk_profiles_plot <- renderPlotly({
    shiny::req(combined_regimens())
    shiny::req(input$select_sum_plot)
    
    data_frames <- combined_regimens()
    
    pk_profile_data <- lapply(data_frames, function(lst) {
      lst[grep("^pk_profile", names(lst))]
    })
    
    validate(
      need(pk_profile_data[[input$select_sum_plot]], "No PK profile data found for selected regimen"),
      need(pk_profile_data[[input$select_sum_plot]][["pk_profile"]], "PK profile data is missing")
    )
    
    
    plot <- plot_pk_profiles(data = pk_profile_data[[input$select_sum_plot]][["pk_profile"]],
                             log_y = FALSE, 
                             treatment_duration = data_frames[[input$select_sum_plot]][["treatment_duration"]],use_loading_dose = F)
    
    
    ggplotly(plot)
  }) %>% bindCache(input$select_sum_plot, combined_regimens())
  
  output$pk_stat <- render_gt({
    shiny::req(combined_regimens())
    shiny::req(input$select_sum_plot)
    
    
    data_frames <- combined_regimens()
    
    pk_stat_data <- lapply(data_frames, function(lst) {
      lst[grep("^stat_data", names(lst))]
    })
    validate(
      need(pk_stat_data[[input$select_sum_plot]], "No PK data found for selected regimen"),
      need(pk_stat_data[[input$select_sum_plot]][["stat_data"]], "PK data is missing")
    )
    
    
    
    
    pk_statistic <- pk_stat_data[[input$select_sum_plot]][["stat_data"]] %>% as.data.frame()
    
    column_names <- colnames(pk_statistic)
    
    extracted_numbers <- str_extract(column_names, "\\d+")
    
    # Convert the extracted numbers to numeric
    extracted_numbers <- as.numeric(extracted_numbers)
    
    pk_statistic <- pk_statistic %>%
      rename_with(~ paste0("children (", extracted_numbers [2] , ")"), .cols = starts_with("Children")) %>%
      rename_with(~ paste0("Adults (", extracted_numbers [3], ")"), .cols = starts_with("Adults")) %>%
      rename_with(~ paste0("Total (", extracted_numbers [4], ")"), .cols = starts_with("Total"))
    
    pk_statistic <- pk_statistic %>%
      gt() %>%
      # Add a title and subtitle
      tab_header(
        title = md("**Miltefosine Exposure, Median (IQR)**"),
        subtitle = md(paste0(
          "Summary of exposure parameters for the **",
          input$select_sum_plot,
          "** dosing regimen"
        ))
      ) %>%
      # Add a footnote explaining abbreviations
      tab_footnote(
        footnote = md(paste0(
          "Data are presented as median (IQR). Abbreviations: **AUC0_D14**, ",
          "area under the concentration-time curve from day 0 to day 14; ",
          "**AUC0_EOT**, area under the concentration-time curve until the end of treatment; ",
          "**Cmax**, maximum observed concentration; **T>EC90**, ",
          "time above the EC90 value (10.6 mg/L); ",
          "**TEC90**, time point at which the concentration first reaches the EC90 value; ",
          "**Tmax**, time to reach maximum concentration."
        )),
        locations = cells_title(groups = "subtitle")
      ) %>%
      tab_footnote(
        footnote = "Children: ≤12 years; adults: >12 years.",
        locations = cells_column_labels(columns = starts_with("Children"))
      ) %>%
      tab_footnote(
        footnote = "Children: ≤12 years; adults: >12 years.",
        locations = cells_column_labels(columns = starts_with("Adults"))
      ) %>%
      # Apply custom styling for a professional look
      tab_options(
        table.font.size = "small",          # Set a smaller font size
        heading.align = "center",          # Center-align the title and subtitle
        column_labels.font.size = "medium",# Set medium size for column labels
        table.border.top.color = "black",  # Add a top border for the table
        table.border.bottom.color = "black",
        data_row.padding = px(5),          # Add padding between rows
        row.striping.include_table_body = TRUE, # Add row striping for readability
      ) %>%
      # Add alignment and format
      cols_align(
        align = "center",
        columns = everything()
      ) %>%
      fmt_markdown(columns = everything())
    
    return(pk_statistic)
  })
  
  output$pk_model_stat <- render_gt({
    shiny::req(model)
    
    if (input$model == "L. Verrest (2023)") {
      pk_model_param <- model_parameter_verrest()
      return(pk_model_param)
    } else {
      return(gt::gt(data.frame(Message = "The summary of the selected PK model is not available")))
    }
  })
  
  ###########################
  ###   Pharmacodynamics  ###
  ###########################
  output$hazard_sumplot <- renderPlot({
    shiny::req(input$run_model)
    shiny::req(input$select_hazard_sumplot)
    shiny::req(combined_regimens())
    
    data_frames <- combined_regimens()
    
    
    sumplot_datasets <- lapply(data_frames, function(lst) {
      lst[grep("^sumplot", names(lst))]
    })
    
    
    # Validate data exists for selected regimen
    shiny::validate(
      shiny::need(!is.null(sumplot_datasets[[input$select_hazard_sumplot]]), 
                  "No data found for the selected regimen."),
      shiny::need(!is.null(sumplot_datasets[[input$select_hazard_sumplot]][["sumplot"]]), 
                  "No summary plot data found for the selected regimen.")
    )
    
    sumplot_datasets[[input$select_hazard_sumplot]][["sumplot"]] <- sumplot_datasets[[input$select_hazard_sumplot]][["sumplot"]] %>%
      rename_with(~ "WT_BAND", .cols = starts_with("WT_BAND"))
    
    plot <- ggplot(sumplot_datasets[[input$select_hazard_sumplot]][["sumplot"]], 
           aes(x = BINNED_WT, y = hazard, fill = WT_BAND)) +
      geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.size = 2) +
      labs(
        title = "Relapse Hazard Ratio",
        subtitle = "Distribution of hazard ratios across weight bands at the end of treatment",
        x = "Weight (kg)",
        y = "Hazard Ratio",
        caption = paste("regimen:", input$select_hazard_sumplot)) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12, color = "gray50"),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        legend.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
      )
    
    return(plot)
  },res = 144) %>% bindCache(input$select_hazard_sumplot, combined_regimens())
  
  
  
  ###########################
  ###   Summary Table     ###
  ###########################
  
  output$combined_output <- renderUI({
    shiny::req(input$run_model)
    shiny::req(combined_regimens())
    
    data_frames <- combined_regimens()
    
    
    AUC_TOEC90_datasets <- lapply(data_frames, function(lst) {
      lst[grep("^AUC_TOEC90_", names(lst))]
    })
    
    regimens <- names(data_frames)
    lows <- c()
    upps <- c()
    
    # Calculate statistics for each regimen
    for (regimen in regimens) {
      df <- AUC_TOEC90_datasets[[regimen]][[1]]  # Get the data for current regimen
      
      # Calculate percentages
      low_percent <- round(100 - (sum(df[, grep("^COUNT_LOWER", names(df))]) / 
                                    sum(df[, grep("^COUNT_BIN", names(df))]) * 100), 2)
      
      upp_percent <- round(100 - (sum(df[, grep("^COUNT_UPPER", names(df))]) / 
                                    sum(df[, grep("^COUNT_BIN", names(df))]) * 100), 2)
      
      lows <- c(lows, low_percent)
      upps <- c(upps, upp_percent)
    } 
    
    result_df <- data.frame(
      regimen_name = regimens,
      lower_limit_TEC90 = lows,
      upper_limit_AUC = upps
    ) %>%
      mutate(TOTAL = 100 - ((100 - lower_limit_TEC90) + (100 - upper_limit_AUC)))
    
    table_html <- result_df %>%
      knitr::kable(
        col.names = c(
          "Regimen Name",
          "Lower Limit of T>EC90 (%)",
          "Upper Limit of AUC (%)",
          "Total (%)"
        ),
        align = c('l', 'c', 'c', 'c'),
        digits = 2,
        format = "html"
      ) %>%
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "responsive"),
        full_width = TRUE,
        position = "center"
      ) %>%
      kableExtra::row_spec(0, bold = TRUE, background = "#f8f9fa") %>%
      kableExtra::column_spec(1, bold = TRUE) %>%
      kableExtra::add_header_above(c(" " = 1, "Target Attainment Metrics" = 3))
    
    # Return as HTML
    HTML(table_html)
  })
  output$sum_plot <- renderPlot({
    shiny::req(input$run_model)
    shiny::req(input$select_sum_plot)
    shiny::req(combined_regimens())
    
    data_frames <- combined_regimens()
    
    
    sumplot_datasets <- lapply(data_frames, function(lst) {
      lst[grep("^sumplot", names(lst))]
    })
    
    
    # Validate data exists for selected regimen
    shiny::validate(
      shiny::need(!is.null(sumplot_datasets[[input$select_sum_plot]]), 
                  "No data found for the selected regimen."),
      shiny::need(!is.null(sumplot_datasets[[input$select_sum_plot]][["sumplot"]]), 
                  "No summary plot data found for the selected regimen.")
    )
    
    
    
    plot <- target_attainment_sumplots(ref_data = ref_data,
                                       binned_df = sumplot_datasets[[input$select_sum_plot]][["sumplot"]],
                                       dosing_strategy = names(data_frames)[names(data_frames) == input$select_sum_plot])
    
    return(plot)
  },res = 144) %>% bindCache(input$select_sum_plot, combined_regimens())
  
  output$tec90_limit <- renderValueBox({
    shiny::req(input$run_model)
    
    valueBox(
      value = paste0(lower_ci_obs_TOEC90, " days"),
      subtitle = "Limit of the lower boundary of T>EC90",
      icon = icon("chart-line"))
  })
  
  output$auc_limit <- renderValueBox({
    shiny::req(input$run_model)
    valueBox(
      value = paste0(upper_ci_obs_AUC, " mg/L*day"),
      subtitle = "Limit of the higher boundary of AUC",
      icon = icon("chart-bar"))
  })
  
  
  ##############################
  ###  Sensitivity Analysis  ###
  ##############################
  
  
  model_sens <- eventReactive(input$model_sens, {
    shiny::req(input$model_sens)
    input$model_sens
    if (input$model_sens == "L. Verrest (2023)") {
      mod_MF_pk <- mread("helper_functions/Verrest_pk_model.cpp")
      loadso(mod_MF_pk)  
    } else if (input$model_sens == "Upload Own Model") {
      shiny::req(input$pk_model_file_sens)
      model_path <- input$pk_model_file_sens$datapath
      own_model <- mread(model_path)
      loadso(own_model)
    } else {
      NA
    }
  })
  
  regimen_data_sens <- regimen_server_sens("regimen_sens")
  
  combined_regimens_sens <- eventReactive(input$run_sens, {
    shiny::req(input$weight_sens)
    # Get the single saved regimen
    current_regimen <- regimen_data_sens$get_regimen()
    
    validate(
      need(!is.null(input$weight_sens), "Please enter a weight value."))
    
    if (is.null(current_regimen)) {
      showNotification("No regimen found. Please add a regimen before running the model.", type = "warning")
      return(NULL)
    }
    regimen_list <- list()
    
    
    # Extract maintenance dose frequency and interval from the single regimen
    mainta_freq <- current_regimen$maintenance_dose$frequency
    mainta_interval <- current_regimen$maintenance_dose$interval
    
    
    ref_data_sens <- create_allom_dataset(
      data = data()$WHO_data_HT_WT_FFM,
      model = model_sens(),
      weight = input$weight_sens,
      seed = 9119,
      use_loading_dose = FALSE,
      fixed_load_dose = NULL,
      load_freq = NULL,
      load_interval = NULL,
      main_freq = mainta_freq,
      main_interval = mainta_interval,
      weight_bands = NULL, 
      type = "ref", 
      upper = NULL,
      lower = NULL)
    
    
    upper_ci_obs_AUC_ref <- ref_data_sens$upper_ci_obs_AUC
    lower_ci_obs_TOEC90_ref <- ref_data_sens$lower_ci_obs_TOEC90
    
    assign("upper_ci_obs_AUC_sens", if (!is.na(input$upp_limit_sens) && input$custom_limit_sens) {
      input$upp_limit_sens
    } else {
      upper_ci_obs_AUC_ref  # Use stored reactive value if available
    }, envir = .GlobalEnv)
    
    assign("lower_ci_obs_TOEC90_sens", if (!is.na(input$lower_limit_sens) && input$custom_limit_sens) {
      input$lower_limit_sens
    } else {
      lower_ci_obs_TOEC90_ref  # Use stored reactive value if available
    }, envir = .GlobalEnv)
    
    
    
    # Process the single regimen
    regimen_label <- current_regimen$name
    
    # Extract values for current regimen
    dosing_strategy <- current_regimen$strategy
    use_loading_dose <- !is.null(current_regimen$loading_dose)
    loading_dose_fixed <- if(use_loading_dose) current_regimen$loading_dose$fixed_dose else NULL
    loading_freq <- if(use_loading_dose) current_regimen$loading_dose$frequency else 0
    loading_interval <- if(use_loading_dose) current_regimen$loading_dose$interval else 0
    maint_freq <- current_regimen$maintenance_dose$frequency
    maint_interval <- current_regimen$maintenance_dose$interval
    weight_bands <- if(use_loading_dose) current_regimen$loading_dose$weight_bands else NULL
    custom_doses <- current_regimen$custom_doses
    
    
    # Generate regimen based on dosing strategy
    generated_regimen <- create_costum_allometric_WB_dosing(
                                  data = data()$WHO_data_HT_WT_FFM,
                                  model = model_sens(),
                                  custom_doses = custom_doses,
                                  weight = input$weight_sens,
                                  seed = 9119,
                                  use_loading_dose = use_loading_dose,
                                  fixed_load_dose = loading_dose_fixed,
                                  load_freq = loading_freq,
                                  load_interval = loading_interval,
                                  main_freq = maint_freq,
                                  main_interval = maint_interval,
                                  weight_bands_load = weight_bands,
                                  upper_limit = upper_ci_obs_AUC_sens,
                                  lower_limit = lower_ci_obs_TOEC90_sens)
    

    return(generated_regimen)
  })
  
  
  sens_model <- eventReactive(input$run_sens, {
    shiny::req(combined_regimens_sens())
    shiny::req(input$param_to_analyze)
    shiny::req(input$sensitivity_step)
    
    sens_data <- run_sensitivity_analysis(model_sens(), combined_regimens_sens()$sens_df,
                                          input$param_to_analyze, input$sensitivity_step)
    return(sens_data)
  })
  
  sensitivity_data <- eventReactive(input$run_sens, {
    shiny::req(sens_model())
    
    current_regimen <- regimen_data_sens$get_regimen()
    
    model_data1 <- sens_model()[[paste0(input$param_to_analyze, "_", input$sensitivity_step)]] 
    model_data2 <- sens_model()[[paste0(input$param_to_analyze, "_", "0")]]
    model_data3 <- sens_model()[[paste0(input$param_to_analyze, "_", "-", input$sensitivity_step)]]
    
    if(!is.null(current_regimen$loading_dose)) {
      model_data1 <- model_data1 %>% group_by(ID) %>%
        filter(TIME > 0) %>%
        mutate( AMT = max(AMT))
    } else {
      model_data1 <- model_data1 %>% group_by(ID) %>%
        mutate( AMT = max(AMT))
    }
    
    if(!is.null(current_regimen$loading_dose)) {
      model_data2 <- model_data2 %>% group_by(ID) %>%
        filter(TIME > 0) %>%
        mutate( AMT = max(AMT))
    } else {
      model_data2 <- model_data2 %>% group_by(ID) %>%
        mutate( AMT = max(AMT))
    }
    
    if(!is.null(current_regimen$loading_dose)) {
      model_data3 <- model_data3 %>% group_by(ID) %>%
        filter(TIME > 0) %>%
        mutate( AMT = max(AMT))
    } else {
      model_data3 <- model_data3 %>% group_by(ID) %>%
        mutate( AMT = max(AMT))
    }
    
    
    Allometric_WB_custom_h <- create_bin(model_data1,Time = combined_regimens_sens()$treatment_duration,
                                         input$weight_sens)
    Allometric_WB_custom_n <- create_bin(model_data2, Time = combined_regimens_sens()$treatment_duration,
                                         input$weight_sens)
    Allometric_WB_custom_l <- create_bin(model_data3, Time = combined_regimens_sens()$treatment_duration,
                                         input$weight_sens)
    
    AUC_TOEC90_summary_1 <- AUC_TOEC90_sens(Allometric_WB_custom_h, current_regimen$custom_doses,
                                            upper_ci_obs_AUC_sens, lower_ci_obs_TOEC90_sens)
    AUC_TOEC90_summary_2 <- AUC_TOEC90_sens(Allometric_WB_custom_n, current_regimen$custom_doses,
                                            upper_ci_obs_AUC_sens, lower_ci_obs_TOEC90_sens)
    AUC_TOEC90_summary_3 <- AUC_TOEC90_sens(Allometric_WB_custom_l, current_regimen$custom_doses,
                                            upper_ci_obs_AUC_sens, lower_ci_obs_TOEC90_sens)
    
    DOSE_sum_1 <- AUC_TOEC90_summary_1$DOSE_summary
    DOSE_sum_2 <- AUC_TOEC90_summary_2$DOSE_summary
    DOSE_sum_3 <- AUC_TOEC90_summary_3$DOSE_summary
    
    AUC_TOEC90_summary_1 <- AUC_TOEC90_summary_1$basic_summary
    AUC_TOEC90_summary_2 <- AUC_TOEC90_summary_2$basic_summary
    AUC_TOEC90_summary_3 <- AUC_TOEC90_summary_3$basic_summary 
    
    merged_df <- AUC_TOEC90_summary_1 %>%
      full_join(AUC_TOEC90_summary_2, by = c("BINNED_WT", "BIN", "WT_BAND_Allometric_WB_custom")) %>%
      full_join(AUC_TOEC90_summary_3, by = c("BINNED_WT", "BIN", "WT_BAND_Allometric_WB_custom"))
    
    
    merged_df_dose <- DOSE_sum_1 %>%
      full_join(DOSE_sum_2, by = "WT_BAND_Allometric_WB_custom") %>%
      full_join(DOSE_sum_3, by = "WT_BAND_Allometric_WB_custom")
   
    return(list(merged_df = merged_df, merged_df_dose = merged_df_dose, model_data1 = model_data1, 
                model_data2 = model_data2, model_data3 = model_data3))
  })
  
  output$target_attainment_plot_sens <- renderPlotly({
    shiny::req(input$run_sens)
    target_attainment <- sensitivity_data()$merged_df
    target_attainment %<>%
      gather(TYPE, VALUE, starts_with("PER_UPPER_"), starts_with("PER_LOWER_"), 
             factor_key = FALSE) %>%
      mutate(
        TYPE_LIMIT = if_else(
          startsWith(TYPE, "PER_UPPER_"), 
          "% of patients under upper limit of AUC", 
          "% of patients over lower limit of T>EC90"
        ),
        LIMIT = if_else(
          startsWith(TYPE, "PER_UPPER_"),
          paste0("limit = ", round(upper_ci_obs_AUC_sens), " ug*day/mL"),
          paste0("limit = ", round(lower_ci_obs_TOEC90_sens), " days")
        ),
        TYPE = case_when(
          stringr::str_detect(TYPE, "Allometric_WB_custom_h$") ~ paste0(input$sensitivity_step, "%"),
          stringr::str_detect(TYPE, "Allometric_WB_custom_n$") ~ "no change",
          stringr::str_detect(TYPE, "Allometric_WB_custom_l$") ~ paste0("-", input$sensitivity_step, "%"),
          TRUE ~ "Other"
        ),
        DOSE = case_when(
          stringr::str_detect(TYPE, paste0(input$sensitivity_step, "%")) ~ paste0("Dose: ", DOSE_Allometric_WB_custom_h),
          stringr::str_detect(TYPE, "no change") ~ paste0("Dose: ", DOSE_Allometric_WB_custom_n),
          stringr::str_detect(TYPE, paste0("-", input$sensitivity_step, "%")) ~ paste0("Dose: ", DOSE_Allometric_WB_custom_l),
          TRUE ~ "other"
        )
      )
    
    plot <- ggplot(target_attainment, aes(x = BIN, y = VALUE, color = TYPE)) + 
      geom_line(na.rm = TRUE) + 
      geom_point(aes(text = paste0(DOSE," mg"))) +
      facet_wrap(~TYPE_LIMIT) +
      theme_bw() +  
      labs(x = "Weight in kg", y = "% of patients within limits") +
      ggtitle("Target achievement of simulated patients' profiles") + 
      theme(legend.position = "bottom", strip.text = element_text(size = 12)) +
      scale_color_discrete(name = "")
    
    ggplotly(plot)
  }) 
  
  output$summary_sens <- renderDT({
    shiny::req(input$run_sens)
    target_attainment <- sensitivity_data()$merged_df_dose
    
    target_attainment %>%
      select(WT_BAND_Allometric_WB_custom, DOSE_Allometric_WB_custom_l, DOSE_Allometric_WB_custom_n, 
             DOSE_Allometric_WB_custom_h) %>%
      datatable(rownames = FALSE,
                colnames = c("Weight Band", 
                             paste0("- ", input$sensitivity_step, "%"),
                             "Default",
                             paste0("+ ",input$sensitivity_step, "%")),
                options = list(
                  pageLength = 6,
                  autoWidth = TRUE,
                  dom = 't'
                ))
  })
  
  
  output$pk_profiles_plot_sens <- renderPlotly({
    shiny::req(input$run_sens)
    shiny::req(sensitivity_data())
    
    model_data1 <- sensitivity_data()$model_data1
    model_data2 <- sensitivity_data()$model_data2
    model_data3 <- sensitivity_data()$model_data3
    
    # Summarize and mutate each dataset
    pk_plot_pos <- model_data1 %>%
      group_by(TIME) %>%
      summarise(
        median = median(CONC_CENT, na.rm = TRUE),
        p5 = quantile(CONC_CENT, 0.05, na.rm = TRUE),
        p95 = quantile(CONC_CENT, 0.95, na.rm = TRUE)
      ) %>%
      mutate(TIME = TIME / 24, TYPE = paste0("+", input$sensitivity_step, "%"))
    
    pk_plot_nochange <- model_data2 %>%
      group_by(TIME) %>%
      summarise(
        median = median(CONC_CENT, na.rm = TRUE),
        p5 = quantile(CONC_CENT, 0.05, na.rm = TRUE),
        p95 = quantile(CONC_CENT, 0.95, na.rm = TRUE)
      ) %>%
      mutate(TIME = TIME / 24, TYPE = "Default")
    
    pk_plot_neg <- model_data3 %>%
      group_by(TIME) %>%
      summarise(
        median = median(CONC_CENT, na.rm = TRUE),
        p5 = quantile(CONC_CENT, 0.05, na.rm = TRUE),
        p95 = quantile(CONC_CENT, 0.95, na.rm = TRUE)
      ) %>%
      mutate(TIME = TIME / 24, TYPE = paste0("-", input$sensitivity_step, "%"))
    
    # Combine the datasets
    combined_data <- bind_rows(pk_plot_pos, pk_plot_nochange, pk_plot_neg)
    
    plot <- plot_pk_profiles(combined_data, FALSE,combined_regimens_sens()$treatment_duration,F)
    ggplotly(plot)
    
  })
  
  
  output$pk_stat_sens <- render_gt({
    shiny::req(input$run_sens)
    shiny::req(sensitivity_data())
    sensitivity_data <- sensitivity_data()
    
    
    model_data1 <- pk_summary_sens(sensitivity_data()$model_data1, combined_regimens_sens()$treatment_duration) %>%
      rename("value_pos" = Value)
    model_data2 <- pk_summary_sens(sensitivity_data()$model_data2, combined_regimens_sens()$treatment_duration) %>%
      rename("value_nochange" = Value)
    model_data3 <- pk_summary_sens(sensitivity_data()$model_data3, combined_regimens_sens()$treatment_duration) %>%
      rename("value_neg" = Value)
    
    pk_stat <- list(model_data1,model_data2,model_data3)
    comb_data <- Reduce(function(x, y) merge(x, y,by = c("Metric")), pk_stat)
    
    gt_table <- gt(comb_data) %>%
      tab_header(
        title = " Miltefosine Exposure and Target Attainment",
        subtitle = "Summary of exposure parameters for the sensitivity analysis"
      ) %>%
      cols_label(
        Metric = "",
        value_pos = paste0("+", input$sensitivity_step, " %"),
        value_nochange = "Default",
        value_neg = paste0("-", input$sensitivity_step, " %")
      ) %>%
      tab_footnote(
        footnote = md(paste0(
          "Data are presented as median (IQR). Abbreviations: **AUC0_D14**, ",
          "area under the concentration-time curve from day 0 to day 14; ",
          "**AUC0_EOT**, area under the concentration-time curve until the end of treatment; ",
          "**Cmax**, maximum observed concentration; **T>EC9**, ",
          "time above the EC90 value (10.6 mg/L); ",
          "**TEC90**, time point at which the concentration first reaches the EC90 value; ",
          "**Tmax**, time to reach maximum concentration. +/- ", input$sensitivity_step,
          " denotes sensitivity analysis adjustments")),
        locations = cells_title(groups = "subtitle")
      ) %>%
      tab_options(
        table.font.size = "small",          # Set a smaller font size
        heading.align = "center",          # Center-align the title and subtitle
        column_labels.font.size = "medium",# Set medium size for column labels
        table.border.top.color = "black",  # Add a top border for the table
        table.border.bottom.color = "black",
        data_row.padding = px(5),          # Add padding between rows
        row.striping.include_table_body = TRUE, # Add row striping for readability
      ) %>%
      # Add alignment and format
      cols_align(
        align = "center", 
        columns = everything()
      ) %>%
      fmt_markdown(columns = everything())
    
    return(gt_table)
    
  })
  
}