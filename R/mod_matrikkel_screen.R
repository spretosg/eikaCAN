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
        uiOutput(ns("evaluation"))

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
      parcels_sel<-st_as_sf(parcels_sel)
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
            addTiles()%>%
            addPolygons(color = "orange", weight = 3, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0)
        })

        ## calculations:
        output$evaluation<-renderUI(
          tagList(
            #data point E4.SBM-3_03  & E4.SBM-3_04:
            uiOutput(ns("inters_verdifull_nat")),
            br(),
            uiOutput(ns("KPI_1")),
            br(),
            plotlyOutput(ns("area_stats")),
            #textInput(ns("comments"), label = "Komentarer")%>%
            br(),
            actionButton(ns("download"),"download results")

          ))

        # here calculate the stats for the parcel
        results <- calc_spat_stats(parcels_sel, in_files)


        # shinyalert(
        #   title = "",
        #   #type = "info",
        #   html = TRUE,
        #   text = tags$div(
        #
        #     h4("Vil du vurdere hele teig eller presisere området"),
        #     br(),
        #     actionButton(ns("full_parcel"),"Hele teig"),
        #     br(),
        #     actionButton(ns("precise"),"Bruk kart og presiser"),
        #   ),
        #   showConfirmButton = FALSE,
        #   closeOnEsc = F,
        #   closeOnClickOutside = F,
        #   showCancelButton = FALSE,
        #   animation = "slide-from-bottom",
        #   size = "s"
        # )

      }

    })



  })
}

## To be copied in the UI
# mod_matrikkel_screen_ui("matrikkel_screen_1")

## To be copied in the server
# mod_matrikkel_screen_server("matrikkel_screen_1")
