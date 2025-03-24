regimen_modal_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    actionButton(ns("add_regimen"), "Add a Regimen", class = "btn btn-primary btn-lg w-100",icon("plus")),
    
    # Modal Dialog
    uiOutput(ns("modal_ui")),
    
    # Display saved regimens
    uiOutput(ns("saved_regimens"))
  )
}
