#' report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Portefølje rapport"),
    br(),
    fluidRow(
      column(3, shinydashboard::valueBoxOutput(ns("n_proj"))),
      column(2),
      column(3, shinydashboard::valueBoxOutput(ns("green_flag"))),
      column(3, shinydashboard::valueBoxOutput(ns("red_flag")))
    ),
    br(),
    fluidRow(
      column(5, plotly::plotlyOutput(ns("intersection_stats"))),
      column(2),
      column(5,plotly::plotlyOutput(ns("areal_stats")))
    )

  )
}

#' report Server Functions
#'
#' @noRd
mod_report_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    db_path <- file.path("inst/extdata", "CAN_DB.duckdb")
    con <- DBI::dbConnect(duckdb::duckdb(), db_path)

    # Load the table into a dataframe
    report_df <- DBI::dbGetQuery(con, "SELECT * FROM nature_risk")

    output$n_proj <- shinydashboard::renderValueBox({
      valueBox(
        value = nrow(report_df),  # Display the area value
        subtitle = "Prosjekter har blitt vudert",
        icon = icon("leaf"),  # Choose an appropriate FontAwesome icon
        color = "olive"
      )
    })

    output$green_flag <- shinydashboard::renderValueBox({
      valueBox(
        value = nrow(report_df%>%filter(n_intersection_val_nat>0)),  # Display the area value
        subtitle = "Prosjekter ligger utenfor aktsomhetsområder",
        icon = icon("leaf"),  # Choose an appropriate FontAwesome icon
        color = "green"
      )
    })
    output$red_flag <- shinydashboard::renderValueBox({
      valueBox(
        value = nrow(report_df%>%filter(n_intersection_val_nat==0)),  # Display the area value
        subtitle = "Prosjekter har et høyt naturrisiko",
        icon = icon("leaf"),  # Choose an appropriate FontAwesome icon
        color = "red"
      )
    })

  })
}

## To be copied in the UI
# mod_report_ui("report_1")

## To be copied in the server
# mod_report_server("report_1")
