#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinythemes
#' @import shinyjs
#' @noRd
Sys.setlocale("LC_ALL", "Norwegian")
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources

    golem_add_external_resources(),
    fluidPage(
      tags$head(
        tags$style(HTML("

      body {
        padding-bottom: 100px; /* Adjust this value to match the footer height */
        font-size: 18px;
      }
    "))
      ),
      div(
        style = "background-color: white; padding: 15px 20px; border-bottom: 1px solid #ddd;",
        titlePanel(
          title = div(
            img(src = "www/eika_logo.PNG", width = "120", style = "margin-right: 10px; vertical-align: middle;"),
            span("CAN - klima & naturrisiko", style = "vertical-align: middle; font-size: 26px; font-weight: bold; color: #1E3A5F;")
          ),
          windowTitle = "eika.CAN"
        )
      ),

      useShinyjs(),
      div(id = "app-content",
          dashboardPage(
            skin = "black",
            dashboardHeader(
              tags$li(class = "dropdown",
                      style = "display: flex; gap: 15px; align-items: center; padding-right: 15px;",
                      actionLink("help_button", label = NULL, icon = icon("question-circle"),
                                 style = "font-size: 20px;"),
                      actionLink("info_button", label = NULL, icon = icon("info-circle"),
                                 style = "font-size: 20px;",onclick = "window.open('https://rpubs.com/retospielhofer/1292830/', '_blank')"),
                      actionLink("home_button", label = NULL, icon = icon("home"),
                                 style = "font-size: 20px;")
              )
            ),
            dashboardSidebar(width = 400,
              tags$head(
                tags$style(HTML("
      /* Change sidebar background color */
      .main-sidebar {
        background-color: #1E3A5F !important;
      }

      /* Make menu text bigger */
      .sidebar-menu > li > a {
        font-size: 22px !important;
        font-weight: bold !important;
      }

      /* Change menu item background when hovered */
      .sidebar-menu > li:hover {
        background-color: #2C5B8F !important;
      }


      /* Change icon color */
      .sidebar-menu i {
        color: white !important;
      }
    "))
              ),

              sidebarMenu(
                menuItem("Se kriterier for naturrisiko", tabName = "nat_tab", icon = icon("leaf")),
                menuItem("Se kriterier for klimarisiko", tabName = "klim_tab", icon = icon("cloud")),
                menuItem("Gjør en akstomhetsvurdering", tabName = "eval", icon = icon("clipboard-check")),
                menuItem("Oversikt portefølje", tabName = "rep", icon = icon("file-alt"))
              )
            )
            ,
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
      uiOutput("shortcut"),
      tags$footer(
        style = "position:fixed; bottom:0; left:0; width:100%;
             background-color:#f8f9fa; color:#333; padding:10px; text-align:center;
             font-size: 12px;
             border-top:1px solid #ddd; z-index: -1;",
        HTML("
      <p>Licensed under <a href='https://www.gnu.org/licenses/gpl-3.0.txt' target='_blank'>GNU General Public License v3.0</a>.</p>
    "),
        div(
          style = "display: flex; justify-content: center; align-items: center;",
          img(src = "www/NINA_logo.png", height = "40px", style = "margin-right:10px;"),
          img(src = "www/cicero_logo.png", height = "40px", style = "margin-right:10px;"),
          span("NINA & CICERO © 2025 CAN.tools")
        )
      )

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
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "eikaCAN"
    )
  )
}
