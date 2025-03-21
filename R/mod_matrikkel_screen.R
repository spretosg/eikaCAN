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
    dashboardPage(
      dashboardHeader(title = ""),
      dashboardSidebar(
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
      dashboardBody(
        #leafletOutput(ns("map_parcel"), height = 600),
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
      actionButton(ns("confirm"),"Vuder teig")
    })

    #select the parcel based on input data

    parcels_sel<-eventReactive(input$confirm,{
      conc_num<-paste0(input$gards_nr,"/",input$bruks_nr)
      parcels_sel<-parcel%>%filter(matrikkeln == conc_num)
      parcels_sel<-sf::st_as_sf(parcels_sel)
    })

    #show parcel, ev valuable nature layer and calculate impact!
    results<-eventReactive(input$confirm,{
      parcels_sel<-parcels_sel()
      shinybusy::show_modal_spinner(text = "beregn klima- og naturrisiko", color = main_green)
      results <- calc_spat_stats(parcels_sel, in_files)
      remove_modal_spinner()
      return(results)

    })
    observeEvent(input$confirm,{
      parcels_sel<-parcels_sel()
      req(results())
      results<-results()
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
        ## dashboard and data curation of results
        output$dashboard<-renderUI(
          tagList(
            tabsetPanel(id = "screen_tabs",

                        tabPanel("Naturrisiko", value = "nat",
                                 fluidRow(column(5,
                                                 h2("Naturrisiko"),
                                                 br(),
                                                 #flag
                                                 uiOutput(ns("screening_light"))),
                                          column(3,
                                                   shinydashboard::valueBoxOutput(ns("vbox1")),
                                                   shinydashboard::valueBoxOutput(ns("vbox2"))),
                                          #map
                                          column(4,
                                                 leafletOutput(ns("map_parcel")))#close map col
                                          ),#close row
                                 actionButton(ns("save"),"Lagre analyse"),
                                 fluidRow(
                                   shinydashboard::box(title = "Details", collapsible = TRUE, collapsed = T, width = 12,
                                                           column(6,
                                                                  h4("Aktsomhetsarealer"),
                                                                  DT::DTOutput(ns("nature_layers_out"))),
                                                       column(6,
                                                              h4("Påvirket areal"),
                                                              plotly::plotlyOutput(ns("area_stats")))
                                                       )#collapsible details
                                 )


                        ),
                        tabPanel("Klimarisiko", value = "clim",
                                 h2("Klimarisiko"),

                        )
                )#close tabset


          ))

        ## the map ev, some wms layers?
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

        # here calculate the stats for the parcel
        #E4-5_10

        tot_proj_area_m2 <- sum(sapply(results, function(x) x$project_area_m2))

        # Extract intersections E4.IRO-1_14
        distances <- do.call(rbind, lapply(results, function(x) x$distances_intersection))%>%group_by(valuable_nat)%>%
          summarize(min_dist_m = min(as.integer(min_dist)),
                    intersect_area_m2 = sum(as.integer(unlist(intersection_area)), na.rm=T))

        ## add skog and myr
        # myr<-results[[1]]$myr_stats
        myr <- do.call(rbind, lapply(results, function(x) x$myr_stats))%>%group_by(label,class)%>%summarize(
          area_m2 = sum(area_m2)
        )%>%filter(class == 1)

        if(1 %in% myr$class){
          sub_myr<-myr%>%filter(class == 1)%>%select(area_m2)
          myr<-c("Myr eller våtmark",0,sub_myr$area_m2,TRUE)
          distances<-rbind(distances,myr)
        }
        skog <- do.call(rbind, lapply(results, function(x) x$skog_stats))%>%group_by(label,class)%>%summarize(
          area_m2 = sum(area_m2)
        )%>%filter(!class == 99)
        if(1 %in% skog$class){
          sub_skog<-skog%>%filter(class == 1)%>%select(area_m2)
          skog<-c("Naturskog",0,sub_skog$area_m2,TRUE)
          distances<-rbind(distances,skog)
        }

        #easy number to check number of intersections
        n_intersections<-nrow(distances%>%filter(min_dist_m == "0"))

        #names of val_nat which intersects to be displayed in KPI
        KPI_nat<-as.vector(unlist(distances%>%filter(min_dist_m == "0")%>%select(valuable_nat)))


        # m2 of parcel that is not bebygged/aggriculture E4.SBM-3_05(natural area loss)
        nat_loss_m2 <- sum(sapply(results, function(x) x$m2_nat_loss))

        # LULC alteration
        lulc_stats <- do.call(rbind, lapply(results, function(x) x$lulc_stats))%>%
          filter(class>0)%>%group_by(label)%>%
          summarize(area_m2 = sum(area_m2),
                    fraction = sum(area_m2)/tot_proj_area_m2)


        ## visualize on dashboard
        #E4.SBM-3_05
        output$vbox2 <- shinydashboard::renderValueBox({

          valueBox(
            value = paste0(round(nat_loss_m2,0), " m²"),  # Display the area value
            subtitle = paste0(round(nat_loss_m2/tot_proj_area_m2,0)*100, "% av totalareal er naturareal"),
            icon = icon("leaf"),  # Choose an appropriate FontAwesome icon
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
              downloadButton(ns("save"),"Lagre og oppdater portefølje"),
              # actionButton(ns("save"),"Lagre og oppdater portefølje"),
              theme = value_box_theme(bg = main_green, fg = "black"),
              showcase= bs_icon("check"))

          }else{
            bslib::value_box(
              title = "",
              value = "",
              h4(paste0("Prosjekt-areal ligger innenfor ", paste(KPI_nat, collapse = ", "))),
              br(),
              h5("Den naturrisikoen er dermed høy"),
              actionButton(ns("questions"),"Vis oppfølgingsspørsmål"),
              downloadButton(ns("save"),"Lagre og oppdater portefølje"),
              theme = value_box_theme(bg = "red", fg = "black"),
              showcase= bs_icon("exclamation"))

          }

        })

        # report on all layers
        output$nature_layers_out<-DT::renderDT({
          DT::datatable(distances)
        })
        #
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
    # final save btn
    # out_file<-eventReactive(input$save,{
    #
    #
    #
    # })

    output$save <- downloadHandler(
      filename = function() { "data_export.csv" },  # File name
      content = function(file) {
        results<-results()

        #tot area
        tot_proj_area_m2 <- sum(sapply(results, function(x) x$project_area_m2))

        # out_file<-data.frame(
        #   proj_intern_id<-as.character(input$eika_id),
        #   municipality<-as.character(input$kommune),
        #   proj_type<-as.character(input$proj_type),
        #   bruks_nr<-as.integer(input$bruks_nr),
        #   gards_nummer<-as.integer(input$gards_nr),
        #   tot_proj_area_m2<-as.integer(0)
        # )
        out_file<-data.frame(
          proj_intern_id<-as.character("AQ"),
          tot_proj_area_m2<-as.integer(0)
        )
        #colnames(out_file)<-c("proj_intern_id","municipality","proj_type","bruks_nr","gards_nummer","tot_proj_area_m2")
        #write to duckDB
        db_path <- file.path("inst/extdata", "CAN_DB.duckdb")
        con <- DBI::dbConnect(duckdb::duckdb(), db_path)  # Connect to the database

        # Check if the table exists
        table_exists <- duckdb::dbExistsTable(con, "test_1")

        # Append or Create Table
        if (table_exists) {
          duckdb::dbAppendTable(con, "test_1", out_file)  # Append new data
        } else {
          duckdb::dbWriteTable(con, "test_1", out_file, overwrite = FALSE)  # Create table
        }

        duckdb::dbDisconnect(con)  # Close connection

        write.csv(out_file, file, row.names = FALSE)  # Save DataFrame as CSV
      }
    )



  })
}

## To be copied in the UI
# mod_matrikkel_screen_ui("matrikkel_screen_1")

## To be copied in the server
# mod_matrikkel_screen_server("matrikkel_screen_1")
