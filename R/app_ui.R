#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinythemes
#' @import shinyjs
#' @noRd

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources

    golem_add_external_resources(),
    fluidPage(
      titlePanel(title = div(img(src="www/eika_logo.PNG", width ='120'), ".CAN - klima & naturrisiko"),windowTitle = "eika.CAN"),
      useShinyjs(),
      div(id = "app-content",
          dashboardPage(
            dashboardHeader(
              tags$li(class = "dropdown",
                      style = "display: flex; gap: 15px; align-items: center; padding-right: 15px;",
                      actionLink("help_button", label = NULL, icon = icon("question-circle"),
                                 style = "font-size: 20px;"),
                      actionLink("info_button", label = NULL, icon = icon("info-circle"),
                                 style = "font-size: 20px;"),
                      actionLink("home_button", label = NULL, icon = icon("home"),
                                 style = "font-size: 20px;")
              )
            ),
            dashboardSidebar(
              sidebarMenu(
                menuItem("Naturrisiko", tabName = "nat_tab", icon = icon("leaf")),
                menuItem("Klimarisiko", tabName = "klim_tab", icon = icon("cloud")),
                menuItem("Aktsomhetsvurdering", tabName = "eval", icon = icon("clipboard-check")),
                menuItem("Rapportering", tabName = "rep", icon = icon("file-alt"))
              )
            ),
            dashboardBody(
              tabItems(
                tabItem(tabName = "nat_tab",

                          mod_data_ui("data")

                ),
                tabItem(tabName = "klim_tab",
                  mod_data_klima_ui("data_klim")

                ),
                tabItem(tabName = "eval",
                          mod_matrikkel_screen_ui("screen_main")

                ),
                tabItem(tabName = "rep",

                           mod_report_ui("report")

                )
              )
            )
          )

      ),
      uiOutput("shortcut")

    )#/fluid page
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
  add_resource_path(
    "extdata",
    app_sys("extdata")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "eikaCAN"
    )
  )
}
