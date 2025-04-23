#' additional_questions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_additional_questions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Oppfølgingsspørsmål"),
    br(),
    textInput(ns("q1"),"First question"),
    br(),
    textInput(ns("q2"),"Second question"),
    br(),
    uiOutput(ns("cond_btn"))


  )
}

#' additional_questions Server Functions
#'
#' @noRd
mod_additional_questions_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$cond_btn<-renderUI({
      validate(
        need(input$q1 !='',''),
        need(!is.na(input$q2),'')      )
      actionButton(ns("confirm"),"submit and save")
    })

  })
}

## To be copied in the UI
# mod_additional_questions_ui("additional_questions_1")

## To be copied in the server
# mod_additional_questions_server("additional_questions_1")
