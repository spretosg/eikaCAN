#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @import dplyr
#' @import shinyalert
#' @import shinybusy
#' @import sf
#' @import terra
#' @import shinythemes
#' @import bslib
#' @import bsicons
#' @import leaflet
#' @import shinydashboard
app_server <- function(input, output, session) {
  hideTab(inputId = "inTabset",
          target = "p0")
  hideTab(inputId = "inTabset",
          target = "p1")
  hideTab(inputId = "inTabset",
          target = "p2")
  hideTab(inputId = "inTabset",
          target = "p3")





  shinyalert::shinyalert(
    title = "Velkommen til prototypen av eika.CAN verktøy",
    #type = "info",
    html = TRUE,
    text = tags$div(
      tags$img(src="www/eika_logo.PNG", width = "100%", height = "auto"),
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




  ## load local data according to selected community
  in_dat<-eventReactive(input$confirm_btn,{
    shinybusy::show_modal_spinner(text = "hent data", color = main_green)
    # Get the path to the folder
    folder_path <- system.file(paste0("extdata/",input$kommune), package = "eikaCAN")


    ## save reading of files
    # List of file names to import
    gpkg_files <- c("friluft_filter_84.gpkg",
                    "kommune_84.gpkg",
                    "strandsone_84.gpkg",
                    "vassdrag_100m_84.gpkg",
                    "inon_84.gpkg",
                    "nat_typer_ku_84.gpkg",
                    "vern_84.gpkg",
                    "red_listed_84.gpkg",
                    "teig_84.gpkg",
                    "kvikkleire_84.gpkg",
                    "flomsone_200klima_84.gpkg")

    rast_files <- c("main_ecotypes_25833.tif","myr_25833.tif","natur_skog_25833.tif")

    # Function to safely read a file
    safe_read_gpkg <- function(file_name) {
      tryCatch(
        {
          sf::st_read(paste0(folder_path, "/", file_name))
        },
        error = function(e) {
          message(paste("File not found:", file_name, "- Assigning NULL"))
          NULL
        }
      )
    }

    safe_read_tif <- function(file_name) {
      tryCatch(
        {
          rast<-terra::rast(paste0(folder_path, "/", file_name))
          terra::crs(rast) <- "EPSG:25833"
          return(rast)
        },
        error = function(e) {
          message(paste("File not found:", file_name, "- Assigning NULL"))
          NULL
        }
      )
    }

    # Apply the function to all files
    in_gpkg <- lapply(gpkg_files, safe_read_gpkg)
    in_rast <- lapply(rast_files, safe_read_tif)

    #change matrikkeln header in parcels
    if ("matrikkelnummerTekst" %in% colnames(in_gpkg[[9]])) {
      colnames(in_gpkg[[9]])[colnames(in_gpkg[[9]]) == "matrikkelnummerTekst"] <- "matrikkeln"
    }
    #cast parcel to multipolygon
    in_gpkg[[9]] <- sf::st_cast(in_gpkg[[9]], "MULTIPOLYGON")

    ## change kommune to WGS84
    if (sf::st_crs(in_gpkg[[2]])$epsg != 4326) {
      in_gpkg[[2]] <- sf::st_transform(in_gpkg[[2]], crs = 4326)
    }
    # Get bounding box of the commune
    bbox <- sf::st_bbox(in_gpkg[[2]])
    shinybusy::remove_modal_spinner()
    list(
      kom_dat = in_gpkg[[2]],
      vern = in_gpkg[[7]],
      bbox = bbox,
      lulc = in_rast[[1]],
      nat_ku = in_gpkg[[6]],
      parcel = in_gpkg[[9]],
      inon = in_gpkg[[5]]%>%dplyr::filter(vsone == "1"),
      vassdrag = in_gpkg[[4]],
      strand = in_gpkg[[3]],
      myr = in_rast[[2]],
      red_listed = in_gpkg[[8]],
      friluft = in_gpkg[[1]],
      kvikk = in_gpkg[[10]],
      flom = in_gpkg[[1]],
      nat_skog = in_rast[[3]]
    )
  })

  ## call module to view important nature in selected municipalities and show other tabs
  observeEvent(input$confirm_btn,{
    #req(in_dat)
    # show_modal_spinner(text = "hent data", color = main_green)
    showTab(inputId = "inTabset", target = "p0")
    showTab(inputId = "inTabset", target = "p1")
    showTab(inputId = "inTabset", target = "p2")
    showTab(inputId = "inTabset", target = "p3")
    updateTabsetPanel(session, "inTabset",
                      selected = "p0")

    # adm_name<-as.character(input$kommune)
    # in_files <- in_dat()
    # ## valuable nature data module
    # mod_data_server("data",adm_name, in_files)

    ## screening server module
    #mod_matrikkel_screen_server("screen_main", in_files)
  })

  ## if tabset panel changed to p2 call screening module
  observeEvent(input$inTabset, {
    adm_name<-as.character(input$kommune)
    in_files <- in_dat()
    if(input$inTabset == "p0"){
      updateTabsetPanel(session, "inTabset",
                        selected = "p0")

      ## valuable nature data module
      mod_data_klima_server("data_klim",adm_name, in_files)

    }else if(input$inTabset == "p1"){
      updateTabsetPanel(session, "inTabset",
                        selected = "p1")

      ## valuable nature data module
      mod_data_server("data",adm_name, in_files)

    }else if(input$inTabset == "p2"){
      updateTabsetPanel(session, "inTabset",
                        selected = "p2")

      ## valuable nature data module
      mod_matrikkel_screen_server("screen_main", in_files)
    }else{
      updateTabsetPanel(session, "inTabset",
                        selected = "p3")
      mod_report_server("report")

    }
  })

}
