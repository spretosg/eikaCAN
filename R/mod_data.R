#' data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
mod_data_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("title")),
    br(),
    bslib::value_box(
      title = "",
      value = "",
      h4("Her kan du se ulike kart som viser forekomsten av viktig natur. Hvis et prosjekt ligger innenfor et av disse områdene, er prosjektet merket som «risikolokalitet for tap av svært viktig natur». Det er imidlertid viktig å merke seg at dette er ikke nødvendigvis i samsvar med gjeldende byggeforskrifter."),
      theme = bslib::value_box_theme(bg = main_green, fg = "black"),
      showcase= bsicons::bs_icon("book"),
      # iframe <- tags$iframe(
      #   src = "https://github.com/NINAnor/nature_fakta_ark/utva_truede_nattype.html", # Replace with the actual URL
      #   height = "600px",
      #   width = "100%"
      # ),
    ),
    br(),
    sidebarPanel(
      selectInput(
        ns("layer_select"),
        "Velg et kartlag for ”særlig viktig natur”",
        choices = c("", "Naturvernområder" = "nat_vern",
                    "Utvalgte og truede naturtyper" = "nat_ku",
                    "Myrområder" = "myr",
                    "Villreinområder" = "rein",
                    "Pressområder i strandsonen"="press_strand",
                    "Inngrepsfri natur"="inon",
                    "Vassdragsnatur"="water",
                    "Bymarker, regionale friluftslivområder" = "friluft",
                    "Naturskog" = "forest",
                    "Naturtyper av forvaltningsinteresse" = "nat_type"
        ),
        selected = NULL
      ),
      tags$hr(),
      imageOutput(ns("layer_image"), height = "300px"),
      textOutput(ns("layer_description_short"))
    ),

    mainPanel(

      leaflet::leafletOutput(ns("map"), height = "600px"),
      shinydashboard::box(title = "Enkel forklaring ",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          textOutput(ns("layer_description_easy")),
                          collapsed = TRUE,
                          width = 12),
      shinydashboard::box(title = "Litt mer vitenskapelig forklart",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          #includeHTML("nature_types_text/utv_true_nat.html"),
                          textOutput(ns("layer_description_long")),
                          collapsed = TRUE,
                          width = 12)


    )
  )
}

#' data Server Functions
#'
#' @noRd
mod_data_server <- function(id, adm_unit, kom_dat, vern, bbox, nat_ku, inon, vassdrag, nin){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$title<-renderUI({
      h2(paste0("Naturverdier i ", as.character(adm_unit)," kommune"))
    })

    vectors <- list(
      "nat_vern" = vern,
      "nat_ku" = nat_ku,
      "inon" = inon,
      "water" = vassdrag
    )

    # Placeholder for layer descriptions
    layer_description_short <- list(
      "nat_vern" = "Kort beskrivelse for Naturvernområder: This is the first data layer.",
      "nat_ku" = "Kort beskrivelse for Utvalgte og truede naturtyper: This is the second data layer.",
      "press_strand" = "Kort beskrivelse for pressoner i strandsone: This is the third data layer.",
      "myr" = "Kort beskrivelse for Utvalgte og truede naturtyper: This is the fourth data layer.",
      "inon" = "Kort beskrivelse for Inngrepsfrie naturområder: This is the second data layer.",
      "villrein" = "Kort beskrivelse for Villrein: This is the second data layer."
    )

    layer_description_long <- list(
      "nat_vern" = "Long description for Naturvernområder",
      "nat_ku" = "Description for Layer 2: This is the second data layer."
    )

    layer_description_easy <- list(
      "nat_vern" = "Easy description for Naturvernområder",
      "nat_ku" = "Før en utbygging skal utbygger gjøre en konsekvensvurdering for ulike naturtyper.  Miljødirektoratet har samlet de viktigste naturtypene i et kart for å gjør det enklere å konsekvensvurdere.  Likevel viste NRK saken Norge i Rødt Hvitt og Grått at vi fortsatt bygger på truet natur og natur som er sentrale levesteder for truede arter. Eksempler på en truet naturtype er slåtteeng, kystgranskog, grisehalekorallbunn og kalksjø .  Nesten halvparten av alle truede arter har sitt leveområde helt eller delvis i skog."
    )



    # Placeholder for layer images
    layer_images <- list(
      "nat_vern" = "data/layer_pictures/vern.jpg",
      "nat_ku" = "data/layer_pictures/utv_trued_nattype.png",
      "friluft" = "data/layer_pictures/friluft.jpg",
      "inon" = "data/layer_pictures/inon.jpg",
      "myr" = "data/layer_pictures/myr.jpg",
      "press_strand" = "data/layer_pictures/press_strand.jpg",
      "water" = "data/layer_pictures/vassdrag.jpg",
      "skog" = "data/layer_pictures/vernskog.jpg",
      "rein" = "data/layer_pictures/villrein.jpg"
    )

    # Render Leaflet map
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addPolygons(data= kom_dat,color = "orange", weight = 3, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0)%>%
        leaflet::setView(lng =mean(bbox$xmin,bbox$xmax) , lat = mean(bbox$ymin,bbox$ymax), zoom = 9)
    })

    # Observe layer selection
    observe({
      req(input$layer_select)
      selected_layer <- input$layer_select

      vectors_selected <- vectors[[selected_layer]]
      if(!is.null(class(vectors_selected))){
        vectors_selected <- vectors[[selected_layer]]
      }else{
        vectors_selected<-vectors[[1]]
      }

      vectors_selected <- sf::st_transform(vectors_selected, sp::CRS("+proj=longlat +datum=WGS84"))

      leaflet::leafletProxy("map") %>%
        leaflet::clearShapes()%>%
        leaflet::addPolygons(data= kom_dat, color = "orange", weight = 3, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0)%>%
        leaflet::addPolygons(data = vectors_selected,
                    color = "green",  # Customize color if needed
                    weight = 2,      # Adjust line weight if desired
                    fillOpacity = 0.5)
    })

    # Update image and description based on selected layer
    output$layer_image <- renderImage({
      req(input$layer_select)
      selected_layer <- input$layer_select
      list(src = layer_images[[selected_layer]], alt = selected_layer)
    }, deleteFile = FALSE)

    output$layer_description_short <- renderText({
      req(input$layer_select)
      selected_layer <- input$layer_select
      layer_description_short[[selected_layer]]
    })

    output$layer_description_easy <- renderText({
      req(input$layer_select)
      selected_layer <- input$layer_select
      layer_description_easy[[selected_layer]]
    })

    output$layer_description_long <- renderText({
      req(input$layer_select)
      selected_layer <- input$layer_select
      layer_description_long[[selected_layer]]
    })

  })
}

## To be copied in the UI
# mod_data_ui("data_1")

## To be copied in the server
# mod_data_server("data_1")
