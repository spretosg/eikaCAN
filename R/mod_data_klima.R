#' data_klima UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_klima_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      h1("Klimarisiko områder"),
      h3("På denne siden kan du lese mer om kriteriene eika.CAN bruker for å identifisere klimarisiko i en prosjektlokalitet."),
      br(),
      fluidRow(
        bslib::value_box(
          title = "",
          value = "",
          h4("Her kan du se ulike kart som viser forekomsten av klimarisiko områder. Hvis et prosjekt ligger innenfor et av disse områdene, er prosjektet merket som «klimarisikolokalitet». Det er imidlertid viktig å merke seg at dette er ikke nødvendigvis i samsvar med gjeldende byggeforskrifter."),
          theme = bslib::value_box_theme(bg = "#D3D3D3", fg = "black"),
          showcase= bsicons::bs_icon("book")
        )
      ),#/row1,
      br(),
      fluidRow(
        column(5,
               tags$div(
                 selectInput(
                   ns("layer_select"),
                   h4(
                     icon("cloud", class = "fa-beat", style = "color: #78BE20;"),  # Add icon here
                     "Velg et kartlag for klimarisiko områder",
                     style = "color: black;"
                   ),
                   choices = list(
                     "",
                     "Flom soner" = c("Flomsoner 1000-årsflom" = "flom_1000",
                                               "Flomsoner 200-årsflom" = "flom_200",
                                               "Flomsoner 20-årsflom"= "Flom_20",
                                               "Flomsoner 200-årsflom i 2010 med klimaendring" ="flom_200_klima",
                                               "Flomaktsomhetsområde" = "flom_akt"

                     ),
                     "Skred soner" = c("Skred 100-års" = "skred_100",
                                       "Skred 1000-års" = "skred_1000"),
                     "Kvikkleire risiko"="kvikk"
                   ),

                   selected = ""
                 ),
                 actionLink(ns("info_button"), label = NULL, icon = icon("info-circle"),
                            style = "font-size: 20px; color:  #78BE20; margin-left: 10px;"),
                 style = "display: flex; align-items: center;"
               )
        ),
        column(6,
               tags$hr(),
               tags$head(tags$style(HTML("
      #layer_image img { max-width: 100%; height: 100%; }
    "))),
               imageOutput(ns("layer_image"), height = "auto"),
               br(),
               textOutput(ns("layer_description_short")))



      ),#/row2
      br(),
      fluidRow(
        leaflet::leafletOutput(ns("data_map"))
      ),
      br(),
      fluidRow(
        shinydashboard::box(title = "Litt mer vitenskapelig forklart",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            #includeHTML("nature_types_text/utv_true_nat.html"),
                            textOutput(ns("layer_description_long")),
                            collapsed = TRUE,
                            width = 12),
        shinydashboard::box(title = "Mer informasjon ",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            textOutput(ns("layer_mer")),
                            collapsed = TRUE,
                            width = 12)
      )#/row3
    )

  )
}

#' data_klima Server Functions
#'
#' @noRd
mod_data_klima_server <- function(id, adm_unit, in_files){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$info_button, {
      showModal(modalDialog(
        title = "Klimarisiko områder",
        "Omfattes følgende kartlagte områder: ",
        h5("-Flom"),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })


    kom_dat<-in_files$kom_dat
    bbox<-in_files$bbox

    wms_url <- list(
      "flom_20" = "https://nve.geodataonline.no:443/arcgis/services/Flomsoner1/MapServer/WmsServer",
      "flom_200" = "https://nve.geodataonline.no:443/arcgis/services/Flomsoner1/MapServer/WmsServer",
      "flom_1000"= "https://nve.geodataonline.no:443/arcgis/services/Flomsoner1/MapServer/WmsServer",
      "flom_200_klima" ="https://nve.geodataonline.no:443/arcgis/services/Flomsoner1/MapServer/WmsServer",
      "flom_akt"="https://nve.geodataonline.no:443/arcgis/services/FlomAktsomhet/MapServer/WmsServer",
      "skred_100"= "https://nve.geodataonline.no:443/arcgis/services/Skredfaresoner1/MapServer/WmsServer",
      "skred_1000"= "https://nve.geodataonline.no:443/arcgis/services/Skredfaresoner1/MapServer/WmsServer",
      "kvikk" = "https://nve.geodataonline.no:443/arcgis/services/SkredKvikkleire2/MapServer/WmsServer"
      )

    legend_url <- list(
      "flom_20" = "https://nve.geodataonline.no:443/arcgis/services/Flomsoner1/MapServer/WmsServer?request=GetLegendGraphic%26version=1.3.0%26format=image/png%26layer=Flomsone_20arsflom",
      "flom_200" = "https://nve.geodataonline.no:443/arcgis/services/Flomsoner1/MapServer/WmsServer?request=GetLegendGraphic%26version=1.3.0%26format=image/png%26layer=Flomsone_200arsflom",
      "flom_1000" = "https://nve.geodataonline.no:443/arcgis/services/Flomsoner1/MapServer/WmsServer?request=GetLegendGraphic%26version=1.3.0%26format=image/png%26layer=Flomsone_1000arsflom",
      "flom_200_klima" = "https://nve.geodataonline.no:443/arcgis/services/Flomsoner1/MapServer/WmsServer?request=GetLegendGraphic%26version=1.3.0%26format=image/png%26layer=Flomsone_200arsflom_klima",
      "flom_akt"="https://nve.geodataonline.no:443/arcgis/services/FlomAktsomhet/MapServer/WmsServer?request=GetLegendGraphic%26version=1.3.0%26format=image/png%26layer=Flom_aktsomhetsomrade ",
      "skred_100" = "https://nve.geodataonline.no:443/arcgis/services/Skredfaresoner1/MapServer/WmsServer?request=GetLegendGraphic%26version=1.3.0%26format=image/png%26layer=Skredsoner_100",
      "skred_1000" = "https://nve.geodataonline.no:443/arcgis/services/Skredfaresoner1/MapServer/WmsServer?request=GetLegendGraphic%26version=1.3.0%26format=image/png%26layer=Skredsoner_1000",
      "kvikk" = "https://nve.geodataonline.no:443/arcgis/services/SkredKvikkleire2/MapServer/WmsServer?request=GetLegendGraphic%26version=1.3.0%26format=image/png%26layer=KvikkleireRisiko"
      )

    layer <-list(
      "flom_20" = "Flomsone_20arsflom",
      "flom_200" = "Flomsone_200arsflom",
      "flom_1000" = "Flomsone_1000arsflom",
      "flom_200_klima" = "Flomsone_200arsflom_klima",
      "flom_akt" = "Flom_aktsomhetsomrade",
      "skred_100" = "Skredsoner_100",
      "skred_1000" = "Skredsoner_1000",
      "kvikk" = "KvikkleireRisiko"
      )

    attr_list <-list(
      "flom_20" = "@NVE Arcgis",
      "flom_200" = "@NVE Arcgis",
      "flom_1000" = "@NVE Arcgis",
      "flom_200_klima" = "@NVE Arcgis",
      "flom_akt" = "@NVE Arcgis",
      "skred_100" = "@NVE Arcgis",
      "skred_1000" = "@NVE Arcgis",
      "kvikk" = "@NVE Arcgis"
    )

    # Placeholder for layer descriptions
    layer_description_short <- list(
      "flom_20" = "Flomsoner viser arealer som oversvømmes ved ulike
flomstørrelser (gjentaksintervall). Det blir utarbeidet
flomsoner for 20-, 200- og 1000-årsflommene (NVE, 2025).",
      "flom_200" = "Flomsoner viser arealer som oversvømmes ved ulike
flomstørrelser (gjentaksintervall). Det blir utarbeidet
flomsoner for 20-, 200- og 1000-årsflommene (NVE, 2025).",
      "flom_1000" = "Flomsoner viser arealer som oversvømmes ved ulike
flomstørrelser (gjentaksintervall). Det blir utarbeidet
flomsoner for 20-, 200- og 1000-årsflommene (NVE, 2025).",
      "flom_200_klima" = "Flomsoner viser arealer som oversvømmes ved ulike
flomstørrelser (gjentaksintervall). Det blir utarbeidet
flomsoner for 20-, 200- og 1000-årsflommene. I områder
der klimaendringene gir en forventet økning i
vannføringen på mer enn 20 %, utarbeides det flomsone
for 200-årsflommen i år 2100. (NVE, 2025).",
      "flom_akt" = "NVEs aktsomhetskart for flom er et nasjonalt datasett
som på oversiktsnivå viser hvilke arealer som kan
være utsatt for flomfare (NVE, 2025)",
      "skred_100" = "NVE gjennomfører faresonekartlegging av skred i bratt terreng for utvalgte områder prioritert for kartlegging, jfr Plan for skredfarekartlegging (NVE rapport 14/2011). Kartleggingen dekker skredtypene snøskred, sørpeskred, steinsprang, jordskred og flomskred (NVE, 2025).",
      "skred_1000" = "NVE gjennomfører faresonekartlegging av skred i bratt terreng for utvalgte områder prioritert for kartlegging, jfr Plan for skredfarekartlegging (NVE rapport 14/2011). Kartleggingen dekker skredtypene snøskred, sørpeskred, steinsprang, jordskred og flomskred (NVE 2025).",
      "kvikk" = "Sonene omfatter løsneområder for kvikkleireskred (områder som kan gli ut) og utløpsområder der slike er registrert (områder som kan rammes av skredmasser). For soner som kun inneholder løsneområder, må utløpsområdene vurderes særskilt i forbindelse med videre utredning, eller i arealplan eller byggesak (NVE,2025)."
    )

    layer_description_long <- list(
      "flom_20" = "@NVE Arcgis",
      "flom_200" = "@NVE Arcgis",
      "flom_1000" = "@NVE Arcgis",
      "flom_200_klima" = "@NVE Arcgis",
      "flom_akt" = "Detaljeringsgraden på aktsomhetskart for flom er tilpasset kommuneplannivået (kommunenes
oversiktsplanlegging), der det er egnet til bruk som et
første vurderingsgrunnlag i konsekvensutredninger
og/eller risiko- og sårbarhetsanalyser tilknyttet
kommuneplanen for å identifisere
aktsomhetsområder for flom. Aktsomhetsområdene
skal legges til grunn ved fastsetting av
flomhensynssoner og planbestemmelser (NVE, 2025).",
      "skred_100" = "Plan for skredfarekartlegging legger grunnlaget for prioriteringene med hensyn på farekartlegging for ulike typer skred.
NVEs kartlegging retter seg først og fremst mot eksisterende bebyggelse. Ved identifisering og prioritering av områder er det derfor lagt vekt på hvor det bor og oppholder seg mennesker innenfor potensielt skredfareutsatte områder.
For hver skredtype er det utarbeidet prioriteringslister for farekartlegging, jfr Plan for skredfarekartlegging kap 6.
Ved faresonekartlegging vil områdene med prioritet 1 normalt bli kartlagt først, deretter områdene med prioritet 2. For områder med høy prioritet for flere skredtyper, vil dette også kunne gi samlet høyere prioritet.
Områder som skulle vise seg allerede kartlagt eller tilstrekkelig sikret, vil kunne nedrangeres eller også fjernes fra prioriteringslisten. På samme vis vil nye skredhendelser og registreringer kunne føre til en høyere prioritet (NVE, 2025).",
      "skred_1000" = "Plan for skredfarekartlegging legger grunnlaget for prioriteringene med hensyn på farekartlegging for ulike typer skred.
NVEs kartlegging retter seg først og fremst mot eksisterende bebyggelse. Ved identifisering og prioritering av områder er det derfor lagt vekt på hvor det bor og oppholder seg mennesker innenfor potensielt skredfareutsatte områder.
For hver skredtype er det utarbeidet prioriteringslister for farekartlegging, jfr Plan for skredfarekartlegging kap 6.
Ved faresonekartlegging vil områdene med prioritet 1 normalt bli kartlagt først, deretter områdene med prioritet 2. For områder med høy prioritet for flere skredtyper, vil dette også kunne gi samlet høyere prioritet.
Områder som skulle vise seg allerede kartlagt eller tilstrekkelig sikret, vil kunne nedrangeres eller også fjernes fra prioriteringslisten. På samme vis vil nye skredhendelser og registreringer kunne føre til en høyere prioritet (NVE, 2025).",
      "kvikk" = "Faregrad er klassifisert i tre faregradsklasser (høy-, middels- og lav faregrad), basert på topografiske, geotekniske og hydrologiske kriterier. Sonene er videre klassifisert i tre skadekonsekvensklasser (mindre alvorlig, alvorlig og meget alvorlig) avhengig av konsekvenser som et skred i sonen vil ha på bebyggelse og infrastruktur. Sonene er deretter klassifisert i fem risikoklasser, utledet fra faregrads- og konsekvensklassifiseringen (NVE, 2025)."
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
          ) %>%
          leaflet::setView(lng =mean(bbox$xmin,bbox$xmax) , lat = mean(bbox$ymin,bbox$ymax), zoom = 10)  # Adjust center and zoom as needed


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
# mod_data_klima_ui("data_klima_1")

## To be copied in the server
# mod_data_klima_server("data_klima_1")
