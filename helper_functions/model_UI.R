model_UI <- function() {
  modalDialog(
    title = "Upload & Edit Model",
    size = "l", 
    easyClose = TRUE,
    
    fluidPage(
      tags$head(
        tags$style(HTML("
          .modal-lg {
            width: 95% !important;
            max-width: 1200px !important;
          }
          .file-input-wrapper {
            display: flex;
            flex-direction: column;
            align-items: center;
            padding: 15px;
            border: 2px dashed #ccc;
            border-radius: 5px;
            transition: background-color 0.3s ease;
          }
          .file-input-wrapper:hover {
            background-color: #f4f4f4;
          }
        "))
      ),
      
      fluidRow(
        column(
          width = 12,
          div(
            class = "model-upload-container",
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
            
            # Instructions and Upload Button
            div(
              class = "upload-header",
              style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 20px;",
              h3(
                icon("file-upload"), 
                "Pharmacometric Model Upload",
                style = "margin: 0; color: #343a40;"
              ),
              actionButton(
                "show_upload_instructions", 
                label = span(icon("question-circle"), "Instructions"),
                class = "btn-outline-info"
              )
            ),
            
            fluidRow(
              # Left Column: Model Input
              column(
                width = 4,
                div(
                  class = "model-input-section",
                  style = "background-color: white; padding: 15px; border-radius: 6px; border: 1px solid #e9ecef;",
                  
                  # File Input
                  div(
                    class = "file-input-wrapper",
                    style = "margin-bottom: 15px;",
                    fileInput(
                      "pk_model_file", 
                      label = div(
                        icon("cloud-upload-alt", class = "text-primary"), 
                        tags$span("Drop or Upload Model Files", style = "margin-left: 10px;")
                      ),
                      multiple = TRUE,
                      accept = c(".cpp", ".mod", ".ctl", ".lst", ".ext"),
                      placeholder = "Upload NONMEM or mrgsolve Files"
                    )
                  ),
                  
                  # File Type Selection
                  selectInput(
                    "file_type", 
                    label = span(icon("file-alt", class = "text-primary"), "File Format"),
                    choices = c(
                      "mrgsolve (.cpp)" = "mrgsolve", 
                      "NONMEM (.ctl/.mod & .ext)" = "nonmem"
                    ),
                    width = "100%"
                  ),
                  
                  # Conversion/View Buttons
                  div(
                    class = "action-buttons",
                    style = "display: flex; justify-content: space-between; margin-top: 15px;",
                    conditionalPanel( 
                      condition = "input.file_type == 'nonmem'",
                      actionButton(
                        "convertModel", 
                        label = span(icon("exchange-alt"), "Convert to mrgsolve"),
                        class = "btn-outline-primary"
                      )
                    ),
                    
                    conditionalPanel( 
                      condition = "input.file_type == 'mrgsolve'",
                      actionButton(
                        "viewModel", 
                        label = span(icon("eye"), "View Model Code"),
                        class = "btn-outline-secondary"
                      )
                    )
                  ),
                  
                  # Download Button
                  div(
                    style = "margin-top: 15px; text-align: center;",
                    downloadButton(
                      "downloadModel", 
                      label = span(icon("download"), "Download Model"),
                      class = "btn-success"
                    )
                  )
                )
              ),
              
              # Right Column: Model Editor
              column(
                width = 8,
                div(
                  class = "model-editor-section",
                  style = "background-color: white; padding: 15px; border-radius: 6px; border: 1px solid #e9ecef;",
                  
                  # Editor Title
                  div(
                    style = "display: flex; align-items: center; margin-bottom: 15px;",
                    icon("code", class = "text-primary", style = "margin-right: 10px; font-size: 1.5em;"),
                    h4("Model Code Editor", style = "margin: 0;")
                  ),withSpinner(
                  # Ace Editor
                  aceEditor(
                    outputId = "modelCodeEditor",
                    value = "",
                    mode = "c_cpp",
                    theme = "tomorrow",
                    height = "600px",
                    fontSize = 14,
                    autoScrollEditorIntoView = TRUE,
                    highlightActiveLine = TRUE,
                    showPrintMargin = FALSE
                  ), type = 7)
                )
              )
            )
          )
        )
      )
    ),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("saveModel", "Save Model", class = "btn-success")
    )
  )
}