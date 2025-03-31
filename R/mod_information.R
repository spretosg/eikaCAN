#' information UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_information_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' information Server Functions
#'
#' @noRd 
mod_information_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_information_ui("information_1")
    
## To be copied in the server
# mod_information_server("information_1")
