fluidPage(
  useShinyjs(),
  # Use a more modern theme with custom color palette
  theme = bslib::bs_theme(
    bootswatch = "flatly",
    primary = "#2C3E50",
    secondary = "#95a5a6",
    success = "#18BC9C",
    info = "#3498DB",
    warning = "#F39C12",
    danger = "#E74C3C"
  ),
  
  # Custom CSS for enhanced styling
  tags$head(
    tags$style(HTML("
      .app-header {
        background: linear-gradient(135deg, #2C3E50 0%, #3498DB 100%);
        padding: 1.5rem 0;
        margin-bottom: 2rem;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .app-title {
        font-weight: 700; 
        letter-spacing: 0.5px;
        text-shadow: 1px 1px 3px rgba(0,0,0,0.2);
      }
      .card {
        border-radius: 8px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.05);
        transition: transform 0.2s, box-shadow 0.2s;
        overflow: hidden;
      }
      .card-body {
      overflow: visible !important;
      }

      .card:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 8px rgba(0,0,0,0.1);
      }
      .card-header {
        border-bottom: none;
        padding: 1rem 1.25rem;
      }
      .btn-primary, .btn-secondary, .btn-success {
        border-radius: 6px;
        font-weight: 500;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        transition: all 0.2s;
      }
      .btn-primary:hover, .btn-secondary:hover, .btn-success:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .value-box {
        border-radius: 8px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.05);
      }
      .nav-pills .nav-link {
        border-radius: 6px;
        font-weight: 500;
        padding: 0.75rem 1.5rem;
      }
      .nav-pills .nav-link.active {
        background-color: #2C3E50;
      }
      .tab-content {
        padding-top: 1.5rem;
      }
      .form-control, .selectize-input {
        border-radius: 6px;
        border: 1px solid #ddd;
      }
      #covariate_panel {
        background-color: #f8f9fa;
        border-radius: 8px;
        padding: 15px;
        margin-top: 15px;
      }
      .spinner-grow {
        color: #3498DB !important;
      }
    "))
  ),
  
  # Application Title with improved styling
  div(
    class = "app-header",
    div(
      class = "container-fluid",
      div(
        class = "row justify-content-center",
        div(
          class = "col-md-10",
          h1("Miltefosine Dose Optimization and Population Analysis", 
             class = "text-center text-white app-title m-0")
        )
      )
    )
  ),
  
  # Main navigation with larger, more distinct tabs
  tabsetPanel(
    id = "main_tabs",
    type = "pills",
    tabPanel(
      div(icon("users"), "Population Simulation"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          div(
            class = "card mb-4",
            div(
              class = "card-header bg-primary text-white",
              h4("Simulation Settings", class = "m-0")
            ),
            div(
              class = "card-body",
              # Population type selector with improved styling
              radioButtons("population_type", "Population Type", 
                           choices = c("Predefined Population" = "predefined", 
                                       "Import Population" = "import"),
                           inline = TRUE),
              hr(class = "my-3"),
              
              # Predefined population settings
              conditionalPanel(
                condition = "input.population_type == 'predefined'",
                selectizeInput("population", "Select Population",
                            choices = c("Pediatric Eastern African Virtual VL Population", 
                                        "WHO Virtual Population"), 
                            width = "100%"),
                hr(class = "my-3"),
                
                div(class = "form-group",
                    radioGroupButtons(
                      inputId = "age_unit",
                      label = "Age Unit",
                      choices = c("years", "months"),
                      justified = TRUE,
                      status = "primary",
                      checkIcon = list(yes = icon("check-circle"))
                    )
                ),
                
                numericInput("subj_num", "Individuals per Age Unit", 
                             value = 10, min = 1, step = 1, width = "100%"),
                hr(class = "my-3"),
                
                div(class = "d-flex align-items-center",
                    materialSwitch("use_seed", "Reproduce Population", 
                                   value = FALSE, status = "primary"),
                ),
                
                conditionalPanel(
                  condition = "input.use_seed == true",
                  div(class = "mt-3",
                      numericInput("seed_value", "Seed Value", value = 9119, 
                                   min = 1, step = 1, width = "100%")
                  )
                ),
                
                div(class = "mt-4",
                    actionButton("show_inputs", "Customize Covariates", 
                                 icon = icon("sliders"),
                                 class = "btn btn-outline-primary btn-block")
                ),
                
                div(
                  id = "covariate_panel",
                  style = "display: none;",
                  div(class = "mt-3 mb-2",
                      h5("Covariate Ranges", style = "color: #2C3E50;")
                  ),
                  
                  fluidRow(
                    column(6, numericInput("min_age", "Min Age (Years)", value = 0)),
                    column(6, numericInput("max_age", "Max Age (Years)", value = 18))
                  ),
                  fluidRow(
                    column(6, numericInput("min_WT", "Min Weight (kg)", value = 0)),
                    column(6, numericInput("max_WT", "Max Weight (kg)", value = 150))
                  ),
                  fluidRow(
                    column(6, numericInput("min_HT", "Min Height (cm)", value = 0)),
                    column(6, numericInput("max_HT", "Max Height (cm)", value = 200))
                  )
                )
              ),
              
              # Import dataset settings
              conditionalPanel(
                condition = "input.population_type == 'import'",
                div(
                  class = "card mb-4",
                  div(
                    class = "card-body",
                    h5("Virtual Population Template", class = "mb-3"),
                    downloadButton("download_template", "Download Template", 
                                   class = "btn-primary w-100",
                                   icon = icon("file-download"))
                  )
                ),
                
                div(
                  class = "card mb-4",
                  div(
                    class = "card-body",
                    h5("Upload Dataset (CSV)", class = "mb-3"),
                    fileInput("upload_csv", label = NULL, 
                              placeholder = "No file selected", 
                              buttonLabel = div(icon("upload"), "Browse"), 
                              accept = c(".csv"),
                              width = "100%")
                  )
                ),
                
                div(
                  class = "card",
                  div(
                    class = "card-body bg-light",
                    h5("Instructions", class = "mb-3"),
                    tags$ol(
                      class = "ps-4",
                      tags$li("Download the template CSV file"),
                      tags$li("Fill in your population data"),
                      tags$li("Upload your completed CSV file"),
                      tags$li("Ensure all required columns are present")
                    )
                  )
                )
              ),
              
              # Action buttons
              div(class = "mt-4",
                  actionButton("go_button", "Load Population", 
                               icon = icon("database"),
                               class = "btn btn-primary btn-lg btn-block mb-3"),
                  actionButton("explore", "Explore Demographics", 
                               icon = icon("chart-bar"),
                               class = "btn btn-secondary btn-lg btn-block")
              )
            )
          )
        ),
        
        mainPanel(
          tabsetPanel(
            id = "pop_sim_res",
            tabPanel(
              "Population Simulation",
              value = "simulation",
              h4("Population Simulation Results", class = "text-primary mb-4", style = "color: #0056b3;"),
              withSpinner(verbatimTextOutput("model_loaded"), type = 7),
              hr(),
              withSpinner(plotOutput("virtual_pop", height = "600px"), type = 7),
              hr(),
              fluidRow(
                column(4, valueBoxOutput("id_count", width = NULL)),
                column(8, valueBoxOutput("gender_distribution", width = NULL))
              ),
              hr(),
              withSpinner(plotOutput("age_metric_plot", height = "400px"), type = 7),
            ),
            tabPanel(
              "Demographics Map",
              value = "Demographics",
              h4("Demographics", class = "text-primary"),
              withSpinner(
                leafletOutput("demographics_map", height = "600px"),
                type = 7,
                color = "#0275d8"
              )
            )
          )
        )
      )
    ),
    
    tabPanel(
      div(icon("pills"), "Dose Building"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          div(
            class = "card mb-4",
            div(
              class = "card-header bg-primary text-white",
              h4("Model Selection", class = "m-0")
            ),
            div(
              class = "card-body",
              selectizeInput(
                "model", 
                "Select Model",
                choices = c("L. Verrest (2023)", "Upload Own Model"),
                selected = "L. Verrest (2023)", 
                width = "100%",
                options = list(
                  dropdownParent = "body",
                  openOnFocus = TRUE,
                  maxOptions = 10
                )
              ),
              conditionalPanel(
                condition = "input.model == 'Upload Own Model'",
                fileInput(
                  "pk_model_file", 
                  "Upload Model", 
                  accept = c(".cpp"),
                  buttonLabel = div(icon("upload"), "Browse"),
                  placeholder = "Select .cpp file"
                )
              )
            ),
            conditionalPanel(
              condition = "input.model == 'L. Verrest (2023)'",
            div(
                materialSwitch(
                  inputId = "IIV",
                  label = "Include Interindividual Variability (IIV)",
                  value = TRUE,
                  width = "auto"
                )
            ),
            
            div(
                materialSwitch(
                  inputId = "RUV",
                  label = "Include Residual Unexplained Variability (RUV)",
                  value = TRUE,
                  width = "auto"
                ),
            )
            )
          ),
          
          div(
            class = "card mb-4",
            div(
              class = "card-header bg-primary text-white",
              h4("Dosing Regimen", class = "m-0")
            ),
            div(
              class = "card-body",
              regimen_modal_ui("regimen"),
              hr(class = "my-3"),
              
              numericInput(
                inputId = "weight",
                label = "Maximum weight (kg) for analysis",
                value = 30
              ),
              
              hr(class = "my-3"),
              
              materialSwitch("custom_limit", "Use Custom limits", 
                             value = FALSE, status = "primary"),
              
              conditionalPanel(
                condition = "input.custom_limit == true",
                div(class = "mt-3",
                    numericInput(
                      inputId = "upp_limit",
                      label = "Upper limit of AUC (μg·day/mL)",
                      value = NA
                    ),
                    numericInput(
                      inputId = "lower_limit",
                      label = "Lower limit of T>EC90 (days)",
                      value = NA
                    )
                )
              )
            )
          ),
          
          actionButton("run_model", "Load Dosing Strategy", 
                       icon = icon("play"),
                       class = "btn btn-primary btn-lg w-100 mb-4"),
          
          # Conditional panels for different tabs
          conditionalPanel(
            condition = "input.tab_selected == 'Pharmacokinetics'",
            div(
              class = "card",
              div(
                class = "card-header bg-info text-white",
                h4("Regimen Selection", class = "m-0")
              ),
              div(
                class = "card-body",
                selectizeInput("select_sum_plot", "Select regimen:", 
                            choices = NULL, selected = NULL, width = "100%",
                            options = list(
                              dropdownParent = "body",
                              openOnFocus = TRUE
                            ))
              )
            )
          ),
          
          conditionalPanel(
            condition = "input.tab_selected == 'pd'",
            div(
              class = "card",
              div(
                class = "card-header bg-info text-white",
                h4("Regimen Selection", class = "m-0")
              ),
              div(
                class = "card-body",
                selectizeInput("select_hazard_sumplot", "Select regimen:", 
                            choices = NULL, selected = NULL, width = "100%",
                            options = list(
                              dropdownParent = "body",
                              openOnFocus = TRUE))
              )
            )
          )
        ),
        mainPanel(
                 tabsetPanel(
                   id = "tab_selected",
                   
                   # Combined Pharmacokinetics tab with PK profiles
                   tabPanel(
                     title = "Pharmacokinetics",
                     value = "Pharmacokinetics",
                     
                     h3("Pharmacokinetic Analysis", class = "text-primary mb-4"),
                     
                     # Target attainment plot and metrics in first row
                     fluidRow(
                       column(8,
                              div(class = "card shadow-sm mb-4",
                                  div(class = "card-header bg-primary text-white",
                                      h4("Target Attainment Plot", class = "m-0")),
                                  div(class = "card-body",
                                      withSpinner(plotlyOutput("target_attainment_plot"), type = 7)
                                  )
                              )
                       ),
                       column(4,
                              div(class = "h-100 d-flex flex-column justify-content-between",
                                 div(class = "card shadow-sm",
                                      div(class = "card-header bg-info text-white",
                                          h5("AUC Limit", class = "m-0")),
                                      div(class = "card-body",
                                          valueBoxOutput("auc_limit", width = NULL)
                                      )
                                  ),
                                   div(class = "card shadow-sm mb-3",
                                      div(class = "card-header bg-info text-white",
                                          h5("TEC90 Limit", class = "m-0")),
                                      div(class = "card-body",
                                          valueBoxOutput("tec90_limit", width = NULL)
                                      )
                                  )
                              )
                       )
                     ),
                     
                     # Target attainment summary and proposed doses in second row (side by side)
                     fluidRow(
                       column(6,
                              div(class = "card shadow-sm mb-4",
                                  div(class = "card-header bg-primary text-white",
                                      h4("Target Attainment Summary", class = "m-0")),
                                  div(class = "card-body",
                                      withSpinner(uiOutput("combined_output"), type = 7)
                                  )
                              )
                       ),
                       column(6,
                              div(class = "card shadow-sm mb-4",
                                  div(class = "card-header bg-primary text-white",
                                      h4("Proposed Doses", class = "m-0")),
                                  div(class = "card-body",
                                      withSpinner(uiOutput("proposed_doses"), type = 7)
                                  )
                              )
                       )
                     ),
                     
                     # Summary plot in third row
                     fluidRow(
                       column(12,
                              div(class = "card border-0 shadow-lg mb-4",
                                  div(class = "card-header bg-primary text-white d-flex justify-content-between align-items-center py-3",
                                      h4("Target Attainment by Weight", class = "m-0 fw-bold"),
                                      helpText(class = "text-white-50 m-0", "Select reference regimen below")
                                  ),
                                  div(class = "card-body p-4",
                                      div(class = "mb-3",
                                          selectizeInput("select_sum_plot_ref", 
                                                      label = NULL, 
                                                      choices = NULL, 
                                                      selected = NULL,
                                                      width = "100%")
                                      ),
                                      tags$style(HTML("
        #select_sum_plot_ref {
          height: 45px;
          font-size: 1.1rem;
        }
      ")),
                                      div(class = "plot-container",
                                          withSpinner(
                                            plotOutput("sum_plot", height = "500px"), 
                                            type = 7,
                                            color = "#0d6efd"
                                          ),
                                          uiOutput("attainment_caption")
                                      )
                                  )
                              )
                       )
                     ),
                     
                     fluidRow(
                      column(7,
                              div(class = "card shadow-sm mb-4",
                                  div(class = "card-header bg-primary text-white d-flex justify-content-between align-items-center",
                                      h4("PK Profile Visualization", class = "m-0"),
                                      actionButton("customize_filter", label = div(icon("sliders-h"), "Customize Filter"), 
                                                   class = "btn btn-outline-light btn-sm")
                                  ),
                                  div(class = "card-body",
                                      withSpinner(plotlyOutput("pk_profiles_plot", height = "500px"), type = 7)
                                  )
                              )
                       ),
                       column(5,
                              div(class = "card shadow-sm mb-4 h-100",
                                  div(class = "card-header bg-primary text-white",
                                      h4("PK Statistics", class = "m-0")),
                                  div(class = "card-body",
                                      withSpinner(gt_output("pk_stat"), type = 7)
                                  )
                              )
                       )
                     )
                   ),
                   tabPanel(
                     title = "Pharmacodynamics",
                     value = "pd",
                     column(12,
                            div(class = "card border-0 shadow-lg mb-4",
                                div(class = "card-header bg-primary text-white d-flex justify-content-between align-items-center py-3",
                                    h4("Pharmacodynamic Analysis", class = "m-0 fw-bold"),
                                    helpText(class = "text-white-50 m-0", "Review hazard summary below")
                                ),
                                div(class = "card-body p-4",
                                    div(class = "mb-3",
                                        selectizeInput("select_hazard_sumplot_ref", 
                                                    label = "Select reference regimen:", 
                                                    choices = NULL, 
                                                    selected = NULL,
                                                    width = "100%",
                                                    options = list(
                                                      dropdownParent = "body",
                                                      openOnFocus = TRUE
                                                    ))
                                    ),
                                    tags$style(HTML("
                   #select_hazard_sumplot_ref {
                     height: 45px;
                     font-size: 1.1rem;
                   }
                 ")),
                                    div(class = "plot-container",
                                        withSpinner(
                                          plotOutput("hazard_sumplot", height = "800px", width = "100%"),
                                          type = 7,
                                          color = "#0d6efd"
                                        )
                                    )
                                )
                            )
                     )
                   ),
                   tabPanel(
                     title = "pop PK-PD Model summary",
                     value = "pk_model_summary",
                     div(class = "card shadow-sm",
                         div(class = "card-header bg-primary text-white",
                             h4("Pharmacokinetic Model Parameters", class = "m-0")),
                         div(class = "card-body",
                             withSpinner(gt_output("pk_model_stat"), type = 7)
                         )
                     )
                   )
                 )
               )
             )),
    
    tabPanel(
      "Sensitivity Analysis",
      sidebarLayout(
        sidebarPanel(
          div(class = "card shadow-sm p-3 mb-3",
              selectizeInput("model_sens", "Select Model",
                          choices = c("L. Verrest (2023)", "Upload Own Model"),
                          selected = "L. Verrest (2023)", width = "100%"),
              conditionalPanel(
                condition = "input.model_sens == 'Upload Own Model'",
                fileInput("pk_model_file_sens", "Upload OWN Model", accept = c(".cpp"),
                          placeholder = "Please upload a .cpp file")
              )
          ), 
          div(class = "card shadow-sm p-3 mb-3",
              h4("Build Dosing Regimen", class = "text-primary"),
              tags$head(tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css")),
              regimen_modal_ui_sens("regimen_sens"),
              hr(),
              numericInput(
                inputId = "weight_sens",
                label = "Maximum included weight (kg) in the analysis",
                value = 30
              ),
              hr(),
              materialSwitch("custom_limit_sens", "Use Custom limits", value = FALSE),
              conditionalPanel(
                condition = "input.custom_limit_sens == true",
                div(
                  numericInput(
                    inputId = "upp_limit_sens",
                    label = "Upper limit of AUC (ug*day/mL)",
                    value = NA
                  ),
                  numericInput(
                    inputId = "lower_limit_sens",
                    label = "Lower limit of T>EC90 (days)",
                    value = NA
                  )
                )
              )
          ),
          div(class = "card shadow-sm p-3 mb-3",
              h4("Sensitivity Analysis Settings", class = "text-primary mb-3"),
              selectizeInput("param_to_analyze", "Parameter to Analyze",
                          choices = c("CL" = "THETA1", "V" = "THETA2", "KA" = "THETA3", 
                                      "COV F (WEEK)" = "THETA7", "COV F (DDOS)" = "THETA8"),
                          options = list(
                            dropdownParent = "body",
                            openOnFocus = TRUE
                          )),
              numericInput("sensitivity_step", "Step Size (%)", value = 25, min = 1, max = 100, step = 1),
          ),
          actionButton("run_sens", "Run Sensitivity Analysis", 
                       class = "btn btn-success btn-lg w-100 mt-3")
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Sensitivity Results",
              h4("Results", class = "text-primary mb-3"),
              
              # Target Attainment Plot
              div(class = "card shadow-sm mb-4",
                  div(class = "card-header bg-primary text-white",
                      h4("Target Attainment", class = "m-0")),
                  div(class = "card-body p-0",
                      withSpinner(plotlyOutput("target_attainment_plot_sens"), type = 7)
                  )
              ),
              
              # Proposed Doses Table
              div(class = "card shadow-sm mb-4",
                  div(class = "card-header bg-primary text-white",
                      h4("Proposed doses", class = "m-0")),
                  div(class = "card-body",
                      withSpinner(DTOutput("summary_sens"), type = 7)
                  )
              ),
              
              # PK Profile Visualization (moved from separate tab)
              div(class = "card shadow-sm mb-4",
                  div(class = "card-header bg-primary text-white",
                      h4("PK Profile Visualization", class = "m-0")),
                  div(class = "card-body p-0",
                      withSpinner(plotlyOutput("pk_profiles_plot_sens", height = "600px"), type = 7)
                  )
              ),
              
              # PK Summary Table (moved from separate tab)
              div(class = "card shadow-sm",
                  div(class = "card-header bg-primary text-white",
                      h4("Pharmacokinetic Summary", class = "m-0")),
                  div(class = "card-body",
                      withSpinner(gt_output("pk_stat_sens"), type = 7)
                  )
              )
            )
          )
        )
      )
    )
  )
)
