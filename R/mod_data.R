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
#' @import shinyalert
#' @import shinybusy
#' @import sf
#' @import terra
#' @import shinythemes
#' @import bslib
#' @import bsicons
#' @import leaflet
#' @import shinydashboard
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
    ),
    br(),
    sidebarPanel(
      selectInput(
        ns("layer_select"),
        "Velg et kartlag for ”særlig viktig natur”",
        choices = list(
          "Særlig viktig natur" = c("Naturvernområder" = "nat_vern",
                                    "Myrområder" = "myr",
                                    "Villreinområder" = "rein",
                                    "Pressområder i strandsonen"="press_strand",
                                    "Inngrepsfri natur"="inon",
                                    "Vassdragsnatur"="water"),
          "Andre viktige naturverdier" = c("Utvalgte og truede naturtyper" = "nat_ku",
                                           "Friluftslivsområder" = "friluft",
                                           "Naturskog" = "forest")
        ),

        selected = NULL
      ),
      tags$hr(),
      tags$head(tags$style(HTML("
      #layer_image img { max-width: 100%; height: auto; }
    "))),
      imageOutput(ns("layer_image"), height = "200px"),
      textOutput(ns("layer_description_short"))
    ),

    mainPanel(

      leaflet::leafletOutput(ns("data_map"), height = "600px"),
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
mod_data_server <- function(id, adm_unit, in_files){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

      output$title<-renderUI({
      h2(paste0("Naturverdier i ", as.character(adm_unit)," kommune"))
    })


      kom_dat<-in_files$kom_dat
      bbox<-in_files$bbox

    wms_url <- list(
      "inon" = "https://kart.miljodirektoratet.no/geoserver/inngrepsfrinatur/wms",
      "press_strand"="https://wms.geonorge.no/skwms1/wms.spr_strandsoner?service=wms&request=getcapabilities",
      "nat_vern"= "https://kart.miljodirektoratet.no/arcgis/services/vern/mapserver/WMSServer",
      "rein" = "https://kart.miljodirektoratet.no/arcgis/services/villrein/mapserver/WMSServer",
      "nat_ku" = "https://kart.miljodirektoratet.no/arcgis/services/naturtyper_kuverdi/mapserver/WMSServer",
      "friluft" = "https://kart.miljodirektoratet.no/arcgis/services/friluftsliv_kartlagt/mapserver/WMSServer",
      "forest" = "https://image001.miljodirektoratet.no:443/arcgis/services/naturskog/naturskog_v1/MapServer/WMSServer"
    )

    legend_url <- list(
      "inon" = "https://kart.miljodirektoratet.no/geoserver/inngrepsfrinatur/wms?request=GetLegendGraphic&version=1.1.1&format=image%2Fpng&width=20&height=20&layer=status",
      "press_strand"="https://wms.geonorge.no/skwms1/wms.spr_strandsoner?version=1.3.0&service=WMS&request=GetLegendGraphic&sld_version=1.1.0&layer=spr_strandsoner&format=image/png&STYLE=default",
      "nat_vern" = "https://kart.miljodirektoratet.no/arcgis/services/vern/MapServer/WmsServer?request=GetLegendGraphic&version=1.3.0&format=image/png&layer=naturvern_klasser_omrade",
      "rein" = "https://kart.miljodirektoratet.no/arcgis/services/villrein/MapServer/WmsServer?request=GetLegendGraphic&version=1.3.0&format=image/png&layer=villrein_leveomrade",
      "nat_ku" = "https://kart.miljodirektoratet.no/arcgis/services/naturtyper_kuverdi/MapServer/WmsServer?request=GetLegendGraphic&version=1.3.0&format=image/png&layer=kuverdi_naturtype_alle",
      "friluft"="https://kart.miljodirektoratet.no/arcgis/services/friluftsliv_kartlagt/MapServer/WmsServer?request=GetLegendGraphic%26version=1.3.0%26format=image/png%26layer=friluftsliv_kartlagt_verdi",
      "forest" = "https://image001.miljodirektoratet.no:443/arcgis/services/naturskog/naturskog_v1/MapServer/WMSServer?request=GetLegendGraphic%26version=1.3.0%26format=image/png%26layer=skog_etablert_foer_1940_ikke_flatehogd"
    )
    layer <-list(
      "inon" = "status",
      "press_strand" = "spr_strandsoner",
      "nat_vern" = "naturvern_klasser_omrade",
      "rein" = "villrein_leveomrade",
      "nat_ku" = "kuverdi_naturtype_alle",
      "friluft" = "friluftsliv_kartlagt_verdi",
      "forest" = "skog_etablert_foer_1940_ikke_flatehogd"
    )
    attr_list <-list(
      "inon" = "@Milj.dir. Geoserver",
      "press_strand" = "@Geonorge",
      "nat_vern"= "@Milj.dir. Arcgis",
      "rein"= "@Milj.dir. Arcgis",
      "nat_ku" = "@Milj.dir. Arcgis",
      "friluft" = "@Milj.dir. Arcgis",
      "forest" = "@Milj.dir. Arcgis"
    )

    # Placeholder for layer descriptions
    layer_description_short <- list(
      "nat_vern" = "Kort beskrivelse for Naturvernområder data layer.",
      "nat_ku" = "Kort beskrivelse for Naturvernområder data layer.",
      "press_strand" = "Kort beskrivelse for pressområder i strandsonen data layer.",
      "myr" = "Kort beskrivelse for myr data layer.",
      "inon" = "Kort beskrivelse for inngrepsfri natur data layer.",
      "rein" = "Kort beskrivelse for villreinområder data layer.",
      "water" = "Kort beskrivelse for vassdragsnatur data layer.",
      "friluft" = "Kort beskrivelse for verdsatte friluftsliv områder data layer.",
      "forest" = "Kort beskrivelse for vernskog data layer."
    )

    layer_description_long <- list(
      "nat_vern" = "Long beskrivelse for Naturvernområder data layer.",
      "nat_ku" = "Long beskrivelse for Naturvernområder data layer.",
      "press_strand" = "Long beskrivelse for pressområder i strandsonen data layer.",
      "myr" = "Long beskrivelse for myr data layer.",
      "inon" = "Long beskrivelse for inngrepsfri natur data layer.",
      "rein" = "Long beskrivelse for villreinområder data layer.",
      "water" = "Long beskrivelse for vassdragsnatur data layer.",
      "friluft" = "Long beskrivelse for verdsatte friluftsliv områder data layer.",
      "forest" = "Long beskrivelse for vernskog data layer."
    )

    layer_description_easy <- list(
      "nat_vern" = "Easy beskrivelse for Naturvernområder data layer.",
      "nat_ku" = "Easy beskrivelse for Naturvernområder data layer.",
      "press_strand" = "Easy beskrivelse for pressområder i strandsonen data layer.",
      "myr" = "Easy beskrivelse for myr data layer.",
      "inon" = "Easy beskrivelse for inngrepsfri natur data layer.",
      "rein" = "Easy beskrivelse for villreinområder data layer.",
      "water" = "Easy beskrivelse for vassdragsnatur data layer.",
      "friluft" = "Easy beskrivelse for verdsatte friluftsliv områder data layer.",
      "forest" = "Easy beskrivelse for vernskog data layer."
      )



    # Placeholder for layer images
    layer_images <- list(
      "nat_vern" = "layer_pictures/vern.jpg",
      "nat_ku" = "layer_pictures/utv_trued_nattype.png",
      "friluft" = "layer_pictures/friluft.jpg",
      "inon" = "layer_pictures/inon.jpg",
      "myr" = "layer_pictures/myr.jpg",
      "press_strand" = "layer_pictures/press_strand.jpg",
      "water" = "layer_pictures/vassdrag.jpg",
      "forest" = "layer_pictures/vernskog.jpg",
      "rein" = "layer_pictures/villrein.jpg"
    )

    # Render Leaflet map
    output$data_map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addPolygons(data= kom_dat,color = "orange", weight = 3, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0)%>%
        leaflet::setView(lng =mean(bbox$xmin,bbox$xmax) , lat = mean(bbox$ymin,bbox$ymax), zoom = 10)
    })

    # Observe layer selection
    observe({
      req(input$layer_select)
      selected_layer <- input$layer_select

      if(selected_layer == "water"){
        leaflet::leafletProxy("data_map") %>%
          leaflet::clearTiles()%>%
          leaflet::clearControls()%>%
          addWMSTiles(
            baseUrl = "https://wms.geonorge.no/skwms1/wms.norges_grunnkart?service=wms&request=getcapabilities",
            layers = "norges_grunnkart",
            options = WMSTileOptions(format = "image/png", transparent = TRUE),
            attribution = "© Kartverket",
            group = "Kartverket basiskart"
          )%>%
          leaflet::addTiles(
            urlTemplate = "https://maps-test.nina.no/titiler/cog/tiles/WebMercatorQuad/{z}/{x}/{y}@1x?colormap_name=blues&rescale=0%2C1&bidx=1&url=https%3A%2F%2Fmaps-test.nina.no%2Fmedia%2Fmaps%2Fsources%2F37%2Fvassdrag_100m.tif.cog",
            options = tileOptions(tms = FALSE)  # Keep tms=FALSE for Web Mercator Quad tiles
          )


      }else{

        wms_selected <- wms_url[[selected_layer]]
        legend_selected <-legend_url[[selected_layer]]
        layer_selected <-layer[[selected_layer]]
        attribution <- attr_list[[selected_layer]]

        if(is.null(wms_selected) | is.null(legend_selected)){
          #empty leaflet proxy map
          leaflet::leafletProxy("data_map")%>%
            leaflet::clearTiles()%>%
            leaflet::clearControls()%>%
            leaflet::addTiles()%>%
            leaflet::setView(lng =mean(bbox$xmin,bbox$xmax) , lat = mean(bbox$ymin,bbox$ymax), zoom = 10)
        }else{
          leaflet::leafletProxy("data_map") %>%
            leaflet::clearTiles()%>%
            leaflet::clearControls()%>%
            leaflet::addTiles() %>%
            leaflet::addWMSTiles(
              baseUrl = wms_selected,
              layers = layer_selected,
              options = leaflet::WMSTileOptions(
                format = "image/png",
                transparent = TRUE
              ),
              attribution = attribution,
              group = legend_selected
            ) %>%

            # # Add a layer control to toggle the WMS layer
            leaflet::addLayersControl(
              baseGroups = c("Base Map"),
              overlayGroups = layer_selected,
              options = leaflet::layersControlOptions(collapsed = FALSE)
            ) %>%

            # Add the legend manually
            leaflet::addControl(html = paste0("<img src='", legend_selected, "' style='width:250px;'>"),
                                position = "bottomright")
        }

      }




    })

    # Update image and description based on selected layer
    output$layer_image <- renderImage({
      req(input$layer_select)
      selected_layer <- input$layer_select
      image_path <- system.file("extdata", layer_images[[selected_layer]], package = "eikaCAN")

      list(
        src = image_path,
        contentType = "image/jpeg",
        alt = "Image from extdata"
      )

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
