#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
main_green <- "#8abe23"
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      theme = shinythemes::shinytheme("flatly"),
      titlePanel(title = div(img(src="www/eika_logo.PNG", width ='120'), ".CAN - klima & naturrisiko"),windowTitle = "eika.CAN"),

      br(),
      tabsetPanel(id = "inTabset",

                  tabPanel("Hva er særlig viktig natur?", value = "p1",
                           mod_data_ui("data")
                           ),

                  tabPanel("Aktsomhetsvurdering", value = "p2",
                           mod_matrikkel_screen_ui("screen_main")
                           ),
                  #
                  #
                  # # tabPanel("Sammenlign prosjekter", value = "p3",
                  # #          br(),
                  # #          map_screenUI("compare")),
                  # # tabPanel("Arealstatistikk", value = "p4",
                  # #          # uiOutput("bygg_stats")
                  # # ),
                  # tabPanel("", value = "p5",
                  #          # upload_screenUI("upload")
                  # ),
                  # tabPanel("", value = "p6",
                  #          # matrikkel_screenUI("matrikkel")
                  # ),
                  # tabPanel("", value = "p7",
                  #          # map_screenUI("draw")
                  # )

      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "eikaCAN"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
