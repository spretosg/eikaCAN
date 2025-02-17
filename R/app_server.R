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
    # Get the path to the folder
    folder_path <- system.file(paste0("extdata/",input$kommune), package = "eikaCAN")
    #folder_path <- system.file(paste0("extdata/","Verdal"), package = "eikaCAN")

    #folder_path<-"inst/extdata/Verdal"
    # List all .gpkg files in the folder
    gpkg_files <- list.files(folder_path, pattern = "\\.gpkg$", full.names = TRUE)
    raster_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)


    # Load all files into a list
    gpk_list <- lapply(gpkg_files, sf::st_read)
    raster_list <- lapply(raster_files, terra::rast)

    shinybusy::show_modal_spinner(text = "hent data", color = main_green)
    kom_dat<-gpk_list[[2]]
    inon<-gpk_list[[1]]
    lulc<-raster_list[[1]]
    nat_ku<-gpk_list[[3]]
    nin<-gpk_list[[4]]
    parcel<-gpk_list[[5]]
    vassdrag<-gpk_list[[6]]
    vern<-gpk_list[[7]]
    if ("matrikkelnummerTekst" %in% colnames(parcel)) {
      colnames(parcel)[colnames(parcel) == "matrikkelnummerTekst"] <- "matrikkeln"
    }
    parcel <- sf::st_cast(parcel, "MULTIPOLYGON")
    terra::crs(lulc) <- "EPSG:25833"
    if (sf::st_crs(kom_dat)$epsg != 4326) {
      kom_dat <- sf::st_transform(kom_dat, crs = 4326)
    }
    # Get bounding box of the commune
    bbox <- sf::st_bbox(kom_dat)
    shinybusy::remove_modal_spinner()
    list(
      kom_dat = kom_dat,
      vern = vern,
      bbox = bbox,
      lulc = lulc,
      nat_ku = nat_ku,
      parcel = parcel,
      inon = inon%>%dplyr::filter(vsone == "1"),
      nin = nin,
      vassdrag = vassdrag
    )
  })

  ## call module to view important nature in selected municipalities and show other tabs
  observeEvent(input$confirm_btn,{
    #req(in_dat)
    # show_modal_spinner(text = "hent data", color = main_green)
    updateTabsetPanel(session, "inTabset",
                      selected = "p1")
    showTab(inputId = "inTabset", target = "p1")
    #showTab(inputId = "inTabset", target = "p2")
    #showTab(inputId = "inTabset", target = "p4")

    adm_name<-as.character(input$kommune)
    files <- in_dat()
    ## render dashboard like ui

    mod_data_server("data",adm_name, files$kom_dat, files$vern, files$bbox, files$nat_ku, files$inon, files$vassdrag, files$nin)
  })

}
