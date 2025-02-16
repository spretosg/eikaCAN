#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  hideTab(inputId = "inTabset",
          target = "p1")
  hideTab(inputId = "inTabset",
          target = "p2")
  hideTab(inputId = "inTabset",
          target = "p3")
  hideTab(inputId = "inTabset",
          target = "p4")
  hideTab(inputId = "inTabset",
          target = "p5")
  hideTab(inputId = "inTabset",
          target = "p6")
  hideTab(inputId = "inTabset",
          target = "p7")

  ## shiny alert to let the user choose the municipality
  shinyalert::shinyalert(
    title = "Velkommen til prototypen av eika.CAN verktøy",
    #type = "info",
    html = TRUE,
    text = tags$div(
      tags$img(src = "eika_logo.PNG", width = "100%", height = "auto"),
      h4("eika.CAN er et verktøy for aktsomhetsvurdering av vesentlig klima & naturrisiko i virksomheten"),
      br(),
      h5("Prototypen er en showcase og inneholder foreløpig ingen konkrete analyser av naturinngrep. Data og analyser vil bli definert og implementert i løpet av 2025-prosjektet."),
      br(),
      h4(HTML("<b>Velg kommune prosjektet befinner seg i</b>")),
      selectInput("kommune", "",
                  choices = c("Lillestrøm", "Verdal", "Åfjord", "Grong")),
      actionButton("confirm_btn","Bekreft"),
    ),
    showConfirmButton = FALSE,
    closeOnEsc = F,
    closeOnClickOutside = F,
    showCancelButton = FALSE,
    animation = "slide-from-bottom",
    size = "s"
  )

}
