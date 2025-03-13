regimen_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    regimens <- reactiveVal(list())
    weight_bands_load <- reactiveVal(list())
    dosing_strategy <- reactiveVal(NULL)
    use_loading_dose <- reactiveVal(FALSE)
    
    loading_dose_fixed <- reactiveVal(NULL)
    loading_freq <- reactiveVal(NULL)
    loading_interval <- reactiveVal(NULL)
    maint_freq <- reactiveVal(NULL)
    maint_interval <- reactiveVal(NULL)
    validation_message <- reactiveVal("")  # Initialize with empty string
    
    # Custom doses reactive values
    custom_doses <- reactiveVal(list())
    
    observeEvent(input$add_regimen, {
      showModal(modalDialog(
        title = "Build Dosing Regimen",
        size = "l",
        easyClose = TRUE,
        
        textInput(ns("regimen_name"), "Regimen Name", ""),
        selectInput(ns("dosing_strategy"), "Dosing Strategy",
                    choices = c("Allometric FFM-based" = "Allometric_FFM",
                                "Allometric WB-based" = "Allometric_WB",
                                "Conventional (mg/kg)" = "Conventional",
                                "Allometric Customized WB-based" = "costum_allometric_WB")),
        
        # Custom Allometric WB Dose Input Section
        conditionalPanel(
          condition = sprintf("input['%s'] == 'costum_allometric_WB'", ns("dosing_strategy")),
          numericInput(ns("num_flags"), "Number of Doses per Weight Band", value = 3, min = 1),
          textInput(ns("custom_weight_bands"), 
                    "Enter custom weight bands (comma-separated):", 
                    placeholder = "e.g., 0-5.9 kg, 6-9.9 kg, 10-14.9 kg"),
          uiOutput(ns("custom_dose_inputs"))
        ),
        
        materialSwitch(ns("include_loading"), "Include a Loading Dose", value = FALSE),
        
        # Loading Dose Section (same as before)
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("include_loading")),
          div(
            h4("Loading Dose"),
            fluidRow(
              column(4, numericInput(ns("load_freq"), "Frequency (number of doses)", value = NA, min = 0)),
              column(4, numericInput(ns("load_interval"), "Time interval between doses (hr)", value = NA, min = 0))
            ),
            uiOutput(ns("loading_dose_ui")),
            hr(),
            uiOutput(ns("weight_bands_ui")),
            actionButton(ns("add_weight_band"), "Add a new Weight Band", class = "btn btn-outline-primary mt-2")
          )
        ),
        
        # Maintenance Dose UI
        div(
          h4("Maintenance Dose"),
          fluidRow(
            column(4, numericInput(ns("maint_freq"), "Frequency (number of doses)", value = NA, min = 0)),
            column(4, numericInput(ns("maint_interval"), "Time interval between doses (hr)", value = NA, min = 0))
          )
        ),
        div(
          style = "color: red; margin-top: 10px; text-align: center;",
          textOutput(ns("validation_message"))
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_regimen"), "Save Regimen", class = "btn btn-primary")
        )
      ))
    })
    
    
    output$loading_dose_ui <- renderUI({
      if (length(weight_bands_load()) == 0) {
        numericInput(ns("loading_dose"), "Fixed Loading Dose across all weights (mg)", value = NA, min = 0)
      } else {
        NULL  # Hide the input when weight bands exist
      }
    })
    
    # Render weight bands dynamically
    output$weight_bands_ui <- renderUI({
      shiny::req(input$include_loading)
      
      tagList(
        lapply(seq_along(weight_bands_load()), function(i) {  
          band <- weight_bands_load()[[i]] 
          fluidRow(
            column(3, numericInput(ns(paste0("band_min_", i)), "Min Weight (kg)", value = band$min, min = 0)),
            column(3, numericInput(ns(paste0("band_max_", i)), "Max Weight (kg)", value = band$max, min = 0)),
            column(3, numericInput(ns(paste0("band_dose_", i)), "Dose (mg)", value = band$dose, min = 0)),
            column(3, actionButton(ns(paste0("remove_band_", i)), "Remove", class = "btn btn-outline-danger btn-sm"))
          )
        })
      )
    })
    
    # Add a new weight band
    observeEvent(input$add_weight_band, {
      current_bands <- weight_bands_load()
      weight_bands_load(c(current_bands, list(list(min = NA, max = NA, dose = NA)))) 
    })
    
    
    
    # Remove a weight band
    observe({
      lapply(seq_along(weight_bands_load()), function(i) {
        observeEvent(input[[paste0("remove_band_", i)]], {
          weight_bands_load(weight_bands_load()[-i])         
        }, ignoreInit = TRUE, once = TRUE)
      })
    })
    
    
    observeEvent(c(input$num_flags, input$custom_weight_bands, input$dosing_strategy), {
      if(input$dosing_strategy == "costum_allometric_WB") {
      shiny::req(input$num_flags)
      
      # Get weight bands
      weight_bands_input <- if (!is.null(input$custom_weight_bands) && input$custom_weight_bands != "") {
        strsplit(input$custom_weight_bands, ",")[[1]] %>% 
          trimws() %>% 
          sapply(function(band) {
            if (!grepl("kg", band)) paste0(band, " kg") else band
          })
      } else {
        c("0-6 kg", "6-10 kg", "10-15 kg", "15-20 kg", "20-25 kg", 
          "25-30 kg", "30-45 kg", "45-100 kg")
      }
      
      # Default values for each weight band
      default_values <- list(
        c(20, 30, 40),  # 0-6 kg
        c(20, 30, 40),  # 6-10 kg
        c(40, 50, 60),  # 10-15 kg
        c(50, 60, 70),  # 15-20 kg
        c(60, 70, 80),  # 20-25 kg
        c(70, 80, 100), # 25-30 kg
        c(80, 100, 150),# 30-45 kg
        c(90, 100, 150) # 45-100 kg
      )
      
      # Initialize doses list
      doses <- vector("list", length(weight_bands_input))
      
      # Collect doses for each weight band
      for(i in seq_along(weight_bands_input)) {
        band <- weight_bands_input[i]
        current_doses <- numeric(input$num_flags)
        
        for(j in 1:input$num_flags) {
          input_id <- paste0("dose_", gsub("[^a-zA-Z0-9]", "_", band), "_", j)
          if (!is.null(input[[input_id]])) {
            current_doses[j] <- as.numeric(input[[input_id]])
          } else {
            # Use default value if available
            if (i <= length(default_values) && j <= length(default_values[[i]])) {
              current_doses[j] <- default_values[[i]][j]
            }
          }
        }
        
        doses[[i]] <- current_doses
      }
      
      # Update custom_doses reactive
      if (length(doses) > 0) {
        custom_doses(list(
          num_flags = input$num_flags,
          weight_bands = weight_bands_input,
          doses = doses
        ))
      }
      }
    })
    
    output$custom_dose_inputs <- renderUI({
      shiny::req(input$dosing_strategy == "costum_allometric_WB")
      validate(
        need(input$num_flags >= 1, "At least one dose per Weight Band should be assigned.")
      )
      
      # Get the current values if they exist
      current_doses <- if (!is.null(custom_doses())) {
        custom_doses()$doses
      } else {
        NULL
      }
      
      weight_bands_input <- if (!is.null(input$custom_weight_bands) && input$custom_weight_bands != "") {
        strsplit(input$custom_weight_bands, ",")[[1]] %>% 
          trimws() %>% 
          sapply(function(band) {
            if (!grepl("kg", band)) paste0(band, " kg") else band
          })
      } else {
        c("0-6 kg", "6-10 kg", "10-15 kg", "15-20 kg", "20-25 kg", 
          "25-30 kg", "30-45 kg", "45-100 kg")
      }
      
      # Determine the total width needed
      # Width for each column (in pixels)
      col_width <- 120
      weight_band_width <- 120
      # Total width needed in pixels
      total_width <- weight_band_width + (col_width * (input$num_flags + 2))
      
      # Create the table with fixed widths
      table_html <- div(
        class = "table-responsive",
        style = paste0("min-width: 100%; overflow-x: auto;"),
        
        div(
          style = paste0("width: ", total_width, "px; min-width: ", total_width, "px;"),
          
          # Header row
          div(
            class = "row bg-light py-2 mb-2 font-weight-bold",
            style = "margin: 0;",
            
            div(
              class = "col px-2 text-center",
              style = paste0("width: ", weight_band_width, "px; min-width: ", weight_band_width, "px; max-width: ", weight_band_width, "px;"),
              "Weight Band"
            ),
            
            # Dose headers
            lapply(1:input$num_flags, function(j) {
              div(
                class = "col px-2 text-center",
                style = paste0("width: ", col_width, "px; min-width: ", col_width, "px; max-width: ", col_width, "px;"),
                paste0("Dose ", j)
              )
            }),
          ),
          
          # Data rows
          lapply(seq_along(weight_bands_input), function(i) {
            band <- weight_bands_input[i]
            
            # Get current values for this band if they exist
            current_band_values <- if (!is.null(current_doses) && i <= length(current_doses)) {
              current_doses[[i]]
            } else {
              NULL
            }
            
            # Define default values
            default_values <- list(
              c(20, 30, 40),  # 0-6 kg
              c(20, 30, 40),  # 6-10 kg
              c(40, 50, 60),  # 10-15 kg
              c(50, 60, 70),  # 15-20 kg
              c(60, 70, 80),  # 20-25 kg
              c(70, 80, 100), # 25-30 kg
              c(80, 100, 150),# 30-45 kg
              c(90, 100, 150) # 45-100 kg
            )
            
            # Create alternating row background
            row_class <- if(i %% 2 == 0) "bg-white" else "bg-light"
            
            div(
              class = paste("row py-2 mb-1", row_class),
              style = "margin: 0;",
              
              # Weight band column
              div(
                class = "col px-2 d-flex align-items-center",
                style = paste0("width: ", weight_band_width, "px; min-width: ", weight_band_width, "px; max-width: ", weight_band_width, "px;"),
                div(class = "font-weight-bold py-1 text-center w-100", band)
              ),
              
              # Dose columns
              lapply(1:input$num_flags, function(j) {
                value <- if (!is.null(current_band_values) && j <= length(current_band_values)) {
                  current_band_values[j]
                } else {
                  if (i <= length(default_values) && j <= length(default_values[[i]])) {
                    default_values[[i]][j]
                  } else {
                    NA
                  }
                }
                
                div(
                  class = "col px-2",
                  style = paste0("width: ", col_width, "px; min-width: ", col_width, "px; max-width: ", col_width, "px;"),
                  numericInput(
                    ns(paste0("dose_", gsub("[^a-zA-Z0-9]", "_", band), "_", j)),
                    NULL,
                    value = value, 
                    min = 0
                  )
                )
              })
            )
          })
        )
      )
      
      # Combine into a card
      div(
        class = "card shadow-sm",
        div(
          class = "card-header bg-primary text-white",
          h4("Custom Dose Configuration", class = "mb-0")
        ),
        div(
          class = "card-body p-3",
          table_html
        )
      )
    })
    
    output$validation_message <- renderText({
      validation_message()
    })
    # Save regimen with proper custom dose handling
    observeEvent(input$save_regimen, {
      
      # Reset validation message
      validation_message(NULL)
      
      # Perform validation
      if (input$regimen_name == "") {
        validation_message("Please provide a name for the regimen.")
        return()
      }
      if (input$dosing_strategy == "") {
        validation_message("Please select a dosing strategy.")
        return()
      }
      if (input$maint_freq <= 0 || is.na(input$maint_freq)) {
        validation_message("Please provide a maintenance dose frequency.")
        return()
      }
      if (input$maint_interval <= 0 || is.na(input$maint_interval)) {
        validation_message("Please provide a maintenance dose interval.")
        return()
      }
      
      if (input$include_loading) {
        if (input$load_freq <= 0 || is.na(input$load_freq)) {
          validation_message("Please provide a loading dose frequency.")
          return()
        }
        if (input$load_interval <= 0 || is.na(input$load_interval)) {
          validation_message("Please provide a loading dose interval.")
          return()
        }
        if (is.na(input$loading_dose) && length(weight_bands_load()) == 0) {
          validation_message("Please provide a loading dose.")
          return()
        }
      }
      
      if (input$dosing_strategy == "costum_allometric_WB") {
        weight_bands_input <- if (!is.null(input$custom_weight_bands) && input$custom_weight_bands != "") {
          strsplit(input$custom_weight_bands, ",")[[1]] %>% 
            trimws() %>% 
            sapply(function(band) {
              if (!grepl("kg", band)) paste0(band, " kg") else band
            })
        } else {
          c("0-6 kg", "6-10 kg", "10-15 kg", "15-20 kg", "20-25 kg", 
            "25-30 kg", "30-45 kg", "45-100 kg")
        }
        
        # Collect the actual current input values, not relying on the custom_doses() reactive
        collected_doses <- vector("list", length(weight_bands_input))
        
        for(i in seq_along(weight_bands_input)) {
          band <- weight_bands_input[i]
          current_doses <- numeric(input$num_flags)
          
          for(j in 1:input$num_flags) {
            input_id <- paste0("dose_", gsub("[^a-zA-Z0-9]", "_", band), "_", j)
            if (!is.null(input[[input_id]])) {
              current_doses[j] <- as.numeric(input[[input_id]])
            }
          }
          
          collected_doses[[i]] <- current_doses
        }
        custom_doses(list(
          num_flags = input$num_flags,
          weight_bands = weight_bands_input,
          doses = collected_doses
        ))
      }
      
      
      new_regimen <- list(
        name = input$regimen_name,
        strategy = input$dosing_strategy,
        loading_dose = if (input$include_loading) {
          list(
            frequency = input$load_freq,
            interval = input$load_interval,
            fixed_dose = if (length(weight_bands_load()) == 0) input$loading_dose else 0,
            weight_bands = if (length(weight_bands_load()) > 0) {
              lapply(seq_along(weight_bands_load()), function(i) {
                list(
                  min = input[[paste0("band_min_", i)]],
                  max = input[[paste0("band_max_", i)]],
                  dose = input[[paste0("band_dose_", i)]]
                )
              })
            } else NULL         
          )
        } else NULL,
        maintenance_dose = list(
          frequency = input$maint_freq,
          interval = input$maint_interval
        )
      )
      
      # Add custom doses if using custom allometric WB strategy
      if (input$dosing_strategy == "costum_allometric_WB" && !is.null(custom_doses())) {
        current_custom_doses <- custom_doses()
        new_regimen$custom_doses <- list(
          num_flags = input$num_flags,
          weight_bands = current_custom_doses$weight_bands,
          doses = current_custom_doses$doses
        )
      }
      
      regimens(c(regimens(), setNames(list(new_regimen), new_regimen$name)))
      removeModal()
      
    })
    
    
    
    output$saved_regimens <- renderUI({
      if (length(regimens()) == 0) return(NULL)
      
      tagList(
        h4(class = "mb-3 text-primary", "Saved Regimens"),  # Section title
        lapply(seq_along(regimens()), function(i) {
          regimen <- regimens()[[i]]
          
          div(
            class = "card shadow-sm border-0 mb-2 px-3 py-2",  # Modern styling
            div(
              class = "card-body p-2",
              fluidRow(
                column(
                  width =8,  # Text occupies most of the row
                  h5(class = "card-title mb-0 text-dark fw-bold", regimen$name),
                  p(class = "card-text text-muted small mb-0", regimen$strategy)
                ),
                column(
                  width = 1,  # Small column for the X button
                  class = "d-flex justify-content-end align-items-center",  
                  actionButton(
                    ns(paste0("delete_regimen_", i)), 
                    HTML("<i class='fa fa-times'></i>"), 
                    class = "btn btn-outline-danger btn-sm p-1"
                  )
                )
              )
            )
          )
        })
      )
    })
    
    
    # Delete a regimen
    observe({
      lapply(seq_along(regimens()), function(i) {
        observeEvent(input[[paste0("delete_regimen_", i)]], {
          updated_regimens <- regimens()
          updated_regimens <- updated_regimens[-i]  # Properly remove the regimen at index i
          regimens(updated_regimens)  # Update the reactiveVal with the new list
        }, ignoreInit = TRUE, once = TRUE)
      })
    })
    
    
    return(list(
      regimens = regimens,
      get_latest_regimen = reactive({
        shiny::req(length(regimens()) > 0)  # Ensure at least one regimen exists
        regimens()[[length(regimens())]]  # Get the most recently added regimen
      })
    ))
  })
  
}
