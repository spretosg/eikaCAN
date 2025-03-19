#' matrikkel_screen UI Function
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
#' @import shiny

mod_matrikkel_screen_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        textInput(ns("eika_id"),"Saks nummer (kreditportalen)"),
        br(),
        selectInput(
          ns("proj_type"),
          "Velg prosjekttype",
          choices = c("", "Bolig" = "house", "Næring" = "industry","Landbruk"="agri"),
          selected = ""
        ),
        numericInput(ns("bruks_nr"),"Bruksnummer",NA),
        br(),
        numericInput(ns("gards_nr"),"Gårdsnummer",NA),
        br(),
        uiOutput(ns("cond_btn"))

      ),
      mainPanel(
        leafletOutput(ns("map_parcel"), height = 600),
        uiOutput(ns("dashboard"))

      )
    )
  )
}

#' matrikkel_screen Server Functions
#'
#' @noRd
mod_matrikkel_screen_server <- function(id, in_files){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #get the in_files for valuable nature
    parcel<-in_files$parcel

    #render a conditional btn to be sure that necessary input is provided
    output$cond_btn<-renderUI({
      validate(
        need(input$proj_type !='',''),
        need(!is.na(input$bruks_nr),''),
        need(!is.na(input$gards_nr),''),
      )
      actionButton(ns("confirm"),"Søk teig")
    })

    #select the parcel based on input data

    parcels_sel<-eventReactive(input$confirm,{
      conc_num<-paste0(input$gards_nr,"/",input$bruks_nr)
      parcels_sel<-parcel%>%filter(matrikkeln == conc_num)
      parcels_sel<-sf::st_as_sf(parcels_sel)
    })

    #show parcel, ev valuable nature layer and calculate impact!
    observeEvent(input$confirm,{
      parcels_sel<-parcels_sel()
      #if no parcel is found
      if(nrow(parcels_sel)==0){
        shinyalert(
          title = "",
          #type = "info",
          html = TRUE,
          text = tags$div(

            h4("Vi finner matrikkelen ikke"),

          ),
          showConfirmButton = T,
          closeOnEsc = T,
          closeOnClickOutside = T,
          showCancelButton = FALSE,
          animation = "slide-from-bottom",
          size = "s"
        )

      }else{
        bbox <- st_bbox(parcels_sel)
        output$map_parcel <- renderLeaflet({
          leaflet(parcels_sel) %>%
            addProviderTiles(providers$Esri.WorldImagery,options = tileOptions(minZoom = 8, maxZoom = 15),group = "World image")%>%
            addWMSTiles(
              baseUrl = "https://wms.geonorge.no/skwms1/wms.norges_grunnkart?service=wms&request=getcapabilities",
              layers = "norges_grunnkart",  # Choose from 'topo4', 'norges_grunnkart', etc.
              options = WMSTileOptions(format = "image/png", transparent = TRUE),
              attribution = "© Kartverket",
              group = "Kartverket basiskart"
            )%>%
            addPolygons(color = "orange", weight = 3, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0)%>%
            addLayersControl(baseGroups = c("Kartverket basiskart","World image"),
                             options = layersControlOptions(collapsed = FALSE))
        })
        shinybusy::show_modal_spinner(text = "hent data", color = main_green)
        ## calculations:
        output$dashboard<-renderUI(
          tagList(
            br(),
            #data point E4.SBM-3_03  & E4.SBM-3_04:
            fluidRow(
              shinydashboard::valueBoxOutput(ns("vbox1")),
              shinydashboard::valueBoxOutput(ns("vbox2")),
            ),
            br(),
            uiOutput(ns("screening_light")),
            br(),
            h4("Aktsomhetsarealer"),
            DT::DTOutput(ns("nature_layers_out")),
            br(),
            h4("Påvirket areal"),
            plotly::plotlyOutput(ns("area_stats"))

          ))

        # here calculate the stats for the parcel
        #E4-5_10
        results <- calc_spat_stats(parcels_sel, in_files)
        tot_proj_area_m2 <- sum(sapply(results, function(x) x$project_area_m2))

        # Extract intersections E4.IRO-1_14
        distances <- do.call(rbind, lapply(results, function(x) x$distances_intersection))%>%group_by(valuable_nat)%>%
          summarize(min_dist_m = min(closest_distances))%>%
          mutate(intersections = min_dist_m <= 0)
        #easy number to check number of intersections
        n_intersections<-nrow(distances%>%filter(intersections == T))


        # m2 of parcel that is not bebygged E4.SBM-3_05(natural area loss)
        nat_loss_m2 <- sum(sapply(results, function(x) x$m2_nat_loss))

        # LULC alteration
        lulc_stats <- do.call(rbind, lapply(results, function(x) x$area_stats))%>%
          filter(class>0)%>%group_by(label)%>%
          summarize(area_m2 = sum(area_m2),
                    fraction = sum(area_m2)/tot_proj_area_m2)
        remove_modal_spinner()

        ## visualize on dashboard
        #E4.SBM-3_05
        output$vbox2 <- shinydashboard::renderValueBox({

          valueBox(
            value = paste0(round(nat_loss_m2,0), " m²"),  # Display the area value
            subtitle = paste0(round(nat_loss_m2/tot_proj_area_m2,0)*100, "% av totalareal er naturareal"),
            icon = icon("brid"),  # Choose an appropriate FontAwesome icon
            color = "olive"
          )
        })
        #E4-5_10
        output$vbox1 <- shinydashboard::renderValueBox({

          valueBox(
            value = paste0(round(tot_proj_area_m2,0), " m²"),  # Display the area value
            subtitle = "Prosjektets total areal",
            icon = icon("map"),  # Choose an appropriate FontAwesome icon
            color = "red"
          )
        })

        ## screening light:
        output$screening_light<-renderUI({

          if(n_intersections==0){
            bslib::value_box(
              title = "",
              value = "",
              h4("Prosjekt-areal ligger utenfor aktsomhetsområdene"),
              br(),
              actionButton(ns("save"),"Oppdater portefølje"),
              theme = value_box_theme(bg = main_green, fg = "black"),
              showcase= bs_icon("check"))

          }else{
            bslib::value_box(
              title = "",
              value = "",
              h4("Prosjekt-areal ligger innenfor et aktsomhetsområde"),
              br(),
              actionButton(ns("questions"),"Vis oppfølgingsspørsmål"),
              theme = value_box_theme(bg = "red", fg = "black"),
              showcase= bs_icon("exclamation"))

          }

        })

        # report on all layers
        output$nature_layers_out<-DT::renderDT({
          DT::datatable(distances)
        })

        ## report on land cover change
        output$area_stats<-plotly::renderPlotly({
          plotly::plot_ly(
            data = lulc_stats,
            x = ~label,  # Land cover classes on x-axis
            y = ~area_m2,  # Area in square meters on y-axis
            type = "bar"
          )
        })
        # save to duckDB for reporting





      }

    })



  })
}

## To be copied in the UI
# mod_matrikkel_screen_ui("matrikkel_screen_1")

## To be copied in the server
# mod_matrikkel_screen_server("matrikkel_screen_1")
