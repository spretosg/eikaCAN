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
    h1("Oversikt portefølje"),
    fluidRow(
      bslib::value_box(
        title = "",
        value = "",
        h4("Statistikk og oversikt over alle prosjekter og hele portefølje"),
        theme = bslib::value_box_theme(bg = "#D3D3D3", fg = "black"),
        showcase= bsicons::bs_icon("book")
      )
    ),
    br(),
    fluidRow(
      column(3, shinydashboard::valueBoxOutput(ns("n_proj"))),
      column(2),
      column(3, shinydashboard::valueBoxOutput(ns("natur"))),
      column(3, shinydashboard::valueBoxOutput(ns("klima")))
    ),
    # br(),
    # fluidRow(
    #   column(5, plotly::plotlyOutput(ns("intersection_stats"))),
    #   column(2),
    #   column(5,plotly::plotlyOutput(ns("areal_stats")))
    # )

  )
}

#' report Server Functions
#'
#' @noRd
mod_report_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # db_path <- file.path("inst/extdata", "CAN_DB.duckdb")
    # con <- DBI::dbConnect(duckdb::duckdb(), db_path)

    # Load the table into a dataframe
    # report_df <- DBI::dbGetQuery(con, "SELECT * FROM nature_risk1")

    output$n_proj <- shinydashboard::renderValueBox({
      valueBox(
        value = 6,  # Display the area value
        subtitle = "Prosjekter har blitt vudert",
        icon = icon("number"),  # Choose an appropriate FontAwesome icon
        color = "black"
      )
    })

    output$klima <- shinydashboard::renderValueBox({
      valueBox(
        value = 2,  # Display the area value
        subtitle = "Prosjekter har et høyt klimarisiko",
        icon = icon("cloud"),  # Choose an appropriate FontAwesome icon
        color = "blue"
      )
    })
    output$natur <- shinydashboard::renderValueBox({
      valueBox(
        #value = nrow(report_df%>%filter(n_intersection_val_nat!=0)),
        value = 3,# Display the area value
        subtitle = "Prosjekter har et høyt naturrisiko",
        icon = icon("leafe"),  # Choose an appropriate FontAwesome icon
        color = "olive"
      )
    })

  })
}

## To be copied in the UI
# mod_report_ui("report_1")

## To be copied in the server
# mod_report_server("report_1")
