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
#' @import DT

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

        actionButton(ns("confirm"),"beregn klima- og naturrisiko")

    })

    #select the parcel based on input data

    parcels_sel<-eventReactive(input$confirm,{
      conc_num<-paste0(input$gards_nr,"/",input$bruks_nr)
      parcels_sel<-parcel%>%filter(matrikkeln == conc_num)
      parcels_sel<-sf::st_as_sf(parcels_sel)
    })


    #show parcel, ev valuable nature layer and calculate impact!
    results<-eventReactive(input$confirm,{
      req(parcels_sel())
      parcels_sel<-parcels_sel()
      shinybusy::show_modal_spinner(text = "beregn klima- og naturrisiko", color = main_green)
      if(!is.null(parcels_sel)){
        results <- calc_spat_stats(parcels_sel, in_files)
      }

      remove_modal_spinner()
      return(results)

    })

    distances<-eventReactive(input$confirm,{
      req(results())
      results<-results()
      if(nrow(parcels_sel())>0){
        # Extract intersections E4.IRO-1_14
        distances <- do.call(rbind, lapply(results, function(x) x$distances_intersection))%>%group_by(valuable_areas)%>%
          summarize(min_dist_m = min(as.integer(min_dist)),
                    intersect_area_m2 = sum(as.integer(unlist(intersection_area)), na.rm=T))

        distances$valuable_areas<-unlist(distances$valuable_areas)
        ## add skog and myr
        # myr<-results[[1]]$myr_stats
        myr <- as.data.frame(do.call(rbind, lapply(results, function(x) x$myr_stats))%>%group_by(label,class)%>%summarize(
          area_m2 = sum(area_m2)
        )%>%filter(class == 1))

        if(1 %in% myr$class){
          sub_myr<-myr%>%filter(class == 1)%>%select(area_m2)
          myr<-c("Myr eller våtmark",0,sub_myr$area_m2)
          distances<-rbind(distances,myr)
        }
        skog <- as.data.frame(do.call(rbind, lapply(results, function(x) x$skog_stats))%>%group_by(label,class)%>%summarize(
          area_m2 = sum(area_m2)
        )%>%filter(!class == 99))
        if(1 %in% skog$class){
          sub_skog<-skog%>%filter(class == 1)%>%select(area_m2)
          skog<-c("Naturskog",0,sub_skog$area_m2)
          distances<-rbind(distances,skog)
        }
        distances<-distances%>%filter(!is.na(min_dist_m))

        return(distances)

      }

    })

    # intersection_geoms<-eventReactive(input$confirm,{
    #   req(distances)
    #   req(results)
    #   results<-results()
    #   geoms <- do.call(rbind, lapply(results, function(x) x$distances_intersection))%>%group_by(valuable_areas)%>%filter(geometry != "NULL")

    # })




    observeEvent(input$confirm,{
      req(parcels_sel())
      distances<-distances()
      distances$min_dist_m<-as.integer(distances$min_dist_m)
      distances$intersect_area_m2<-as.integer(distances$intersect_area_m2)
      print(distances)
      results<-results()
      parcels_sel<-parcels_sel()
      #if no parcel is found
      if(is.null(parcels_sel) | nrow(parcels_sel)==0){
        shinyalert::shinyalert(
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
              # main KPI of both
              fluidRow(column(5,
                              h2("Klima og naturrisiko"),
                              br(),
                              #flag
                              uiOutput(ns("screening_light")),
                              br(),
                              uiOutput(ns("screening_light_klim"))),

                       column(3,
                              shinydashboard::valueBoxOutput(ns("vbox1")),
                              shinydashboard::valueBoxOutput(ns("vbox2")),
                              shinydashboard::valueBoxOutput(ns("vbox3"))),
                       #map
                       column(4,
                              leafletOutput(ns("map_parcel")))#close map col
              ),#close row
              uiOutput(ns("dynamic_btn")),
              br(),
              fluidRow(
                shinydashboard::box(title = "Detailjer klimarisiko", collapsible = TRUE, collapsed = T, width = 12,
                                    column(12,
                                           h4("Aktsomhetsarealer klima"),
                                           DT::DTOutput(ns("klima_layers_out"))),
                                    # column(6,
                                    #        h4("Påvirket areal"),
                                    #        plotly::plotlyOutput(ns("area_stats")))
                )#collapsible details nature
              ),

              fluidRow(
                shinydashboard::box(title = "Detailjer naturrisiko", collapsible = TRUE, collapsed = T, width = 12,
                                    column(6,
                                           h4("Aktsomhetsarealer natur"),
                                           DT::DTOutput(ns("nature_layers_out"))),
                                    column(6,
                                           h4("Påvirket areal"),
                                           plotly::plotlyOutput(ns("area_stats")))
                )#collapsible details nature
              ),




          ))

        ## the map ev, some wms layers?
        output$map_parcel <- renderLeaflet({
          leaflet(parcels_sel) %>%
            addProviderTiles(providers$Esri.WorldImagery,options = tileOptions(minZoom = 8, maxZoom = 18),group = "World image")%>%
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

        ## climate vector for filtering nature and climate distances
        klim_vec<-c("Flomsoner 200år klima","Kvikkleire risikoområde")

        #easy number to check number of intersections nature
        n_inter_nat<-nrow(distances%>%filter(min_dist_m == 0 & !valuable_areas %in% klim_vec))

        #and climate
        n_inter_klim<-nrow(distances%>%filter(min_dist_m == 0 & valuable_areas %in% klim_vec))

        #names of val_nat which intersects to be displayed in KPI
        KPI_nat<-as.vector(unlist(distances%>%filter(min_dist_m == 0 & !valuable_areas %in% klim_vec)%>%select(valuable_areas)))
        KPI_klim<-as.vector(unlist(distances%>%filter(min_dist_m == 0 & valuable_areas %in% klim_vec)%>%select(valuable_areas)))
        # print(KPI_klim)

        # m2 of parcel that is not bebygged/aggriculture E4.SBM-3_05(natural area loss)
        nat_loss_m2 <- sum(sapply(results, function(x) x$m2_nat_loss))

        #m2 of parcel that is lost for climate important areas E1...
        klim_loss_m2<-unlist(distances%>%filter(as.integer(min_dist_m) == 0 & valuable_areas %in% klim_vec)%>%select(intersect_area_m2))
        klim_loss_m2<-sum(as.integer(klim_loss_m2))


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
            subtitle = paste0(round(nat_loss_m2/tot_proj_area_m2,0)*100, " % av totalareal er naturareal"),
            icon = icon("leaf"),  # Choose an appropriate FontAwesome icon
            color = "olive"
          )
        })

        ## klimate relevant affected area
        output$vbox3 <- shinydashboard::renderValueBox({

          valueBox(
            value = paste0(round(klim_loss_m2,0), " m²"),  # Display the area value
            subtitle = paste0(round(klim_loss_m2/tot_proj_area_m2,0)*100, " % av totalareal er klima relevant areal"),
            icon = icon("leaf"),  # Choose an appropriate FontAwesome icon
            color = "light-blue"
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

          if(n_inter_nat==0){
            bslib::value_box(
              title = "Prosjekt ligger",
              value = "utenfor natur aktsomhetsområder",
              showcase= bs_icon("check"),
              #downloadButton(ns("save"),"Lagre og oppdater portefølje"),
              theme = value_box_theme(bg = main_green)
              )

          }else{
            bslib::value_box(
              title = "Prosjekt ligger",
              value = "innenfor natur aktsomhetsområder",
              showcase= bs_icon("exclamation"),
              # downloadButton(ns("save"),"Lagre og oppdater portefølje"),
              # actionButton(ns("questions"),"Vis oppfølgingsspørsmål"),
              theme = value_box_theme(bg = "red"))

          }

        })

        ## climate flag
        output$screening_light_klim<-renderUI({

          if(n_inter_klim==0){
            bslib::value_box(
              title = "Prosjekt ligger",
              value = "utenfor klima aktsomhetsområder",
              showcase= bs_icon("check"),
              theme = value_box_theme(bg = main_green)
              )

          }else{
            bslib::value_box(
              title = "Prosjekt ligger",
              value = "innenfor klima aktsomhetsområder",
              showcase= bs_icon("exclamation"),
              # downloadButton(ns("save"),"Lagre og oppdater portefølje"),
              # actionButton(ns("questions"),"Vis oppfølgingsspørsmål"),
              theme = value_box_theme(bg = "red")
              )

          }

        })

        output$dynamic_btn<-renderUI({
          if(n_inter_nat==0 & n_inter_klim==0){

              downloadButton(ns("save"),"Lagre og oppdater portefølje")

          }else if(n_inter_nat==0 & n_inter_klim!=0){
            downloadButton(ns("save"),"Lagre og oppdater portefølje")
          }else{
            tagList(
              #just for testing remove this
              downloadButton(ns("save"),"Lagre og oppdater portefølje"),
              actionButton(ns("questions"),"Vis oppfølgingsspørsmål")
            )

          }
        })


        output$cond_btn2<-renderUI({
          validate(
            need(input$q1 !='',''),
            need(input$q1 !='','')     )
          actionButton(ns("confirm_quest"),"submit and save")
        })

        observeEvent(input$questions,{
          shinyalert::shinyalert(
            title = "oppfølgingsspørsmål",
            #type = "info",
            html = TRUE,
            text = tags$div(
              br(),
              textInput(ns("q1"),"Hvordan har dere identifisert og vurdert overgangsrisikoer og fysiske risikoer knyttet til biologisk mangfold og økosystemer i virksomheten deres? "),
              br(),
              textInput(ns("q2"),"Hvilke muligheter har dere funnet i forhold til biologisk mangfold og økosystemer i løpet av risikovurderingsprosessen? "),
              br(),
              uiOutput(ns("cond_btn2"))
            ),
            showConfirmButton = FALSE,
            closeOnEsc = F,
            closeOnClickOutside = F,
            showCancelButton = FALSE,
            animation = "slide-from-bottom",
            size = "s"
          )






        })

        # report on all nature layers
        output$nature_layers_out<-DT::renderDT({
          DT::datatable(distances%>%filter(!valuable_areas %in% klim_vec))%>% DT::formatStyle(
            'min_dist_m',
            target = 'row',
            backgroundColor = styleEqual(0, "red")
          )
        })

        # report on all climate layers
        output$klima_layers_out<-DT::renderDT({
          DT::datatable(distances%>%filter(valuable_areas %in% klim_vec))%>% DT::formatStyle(
            'min_dist_m',
            target = 'row',
            backgroundColor = styleEqual(0, "red")
          )
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

    # download xls for project and save on duckDB

    output$save <- downloadHandler(
      filename = function() { "data_export.csv" },  # File name
      content = function(file) {
        results<-results()
        distances<-distances()
        if(nrow(parcels_sel())>0){
          #tot area
          tot_proj_area_m2 <- sum(sapply(results, function(x) x$project_area_m2))
          print(tot_proj_area_m2)

          #distances & intersections for reporting
          # Pivot Wider
          dist_wide <- distances%>%
            tidyr::pivot_wider(names_from = valuable_areas, values_from = c(min_dist_m, intersect_area_m2), names_sep = "_")
          # lulc statistics
          lulc_stats <- do.call(rbind, lapply(results, function(x) x$lulc_stats))%>%
            filter(class>0)%>%group_by(label)%>%
            summarize(area_m2 = sum(area_m2),
                      fraction = sum(area_m2)/tot_proj_area_m2)

          lulc_wide <- lulc_stats%>%select(label,area_m2)%>%
            tidyr::pivot_wider(names_from = label, values_from = area_m2, names_sep = "_")
          print(lulc_wide)

          klim_vec<-c("Flomsoner 200år klima","Kvikkleire risikoområde")
          #easy number to check number of intersections nature
          n_inter_nat<-nrow(distances%>%filter(min_dist_m == "0" & !valuable_areas %in% klim_vec))

          #and climate
          n_inter_klim<-nrow(distances%>%filter(min_dist_m == "0" & valuable_areas %in% klim_vec))



          # m2 of parcel that is not bebygged/aggriculture E4.SBM-3_05(natural area loss)
          nat_loss_m2 <- sum(sapply(results, function(x) x$m2_nat_loss))



          proj_param<-a <- data.frame(
            proj_intern_id  = ifelse(length(input$eika_id) > 0, input$eika_id, NA),
            municipality = ifelse(length(input$kommune) > 0, input$kommune, NA),
            proj_type = ifelse(length(input$proj_type) > 0, as.character(input$proj_type), NA),
            bruks_nr = ifelse(length(input$bruks_nr) > 0, as.integer(input$bruks_nr), NA),
            gards_nummer = ifelse(length(input$gards_nr) > 0, as.integer(input$gards_nr), NA),
            tot_proj_area_m2 = ifelse(!is.null(tot_proj_area_m2), as.integer(tot_proj_area_m2), NA),
            n_intersection_val_nat = ifelse(!is.null(n_inter_nat), as.integer(n_inter_nat), NA),
            n_intersection_val_klim = ifelse(!is.null(n_inter_klim), as.integer(n_inter_klim), NA),
            nat_loss_m2 = ifelse(!is.null(nat_loss_m2), as.integer(nat_loss_m2), NA)
          )


          out_file<-cbind(proj_param, dist_wide, lulc_wide)

          #colnames(out_file)<-c("proj_intern_id","municipality","proj_type","bruks_nr","gards_nummer","tot_proj_area_m2")
          #write to duckDB
          db_path <- file.path("inst/extdata", "CAN_DB.duckdb")
          con <- DBI::dbConnect(duckdb::duckdb(), db_path)  # Connect to the database

          # Check if the table exists
          table_exists <- duckdb::dbExistsTable(con, "nature_risk1")

          # Append or Create Table
          if (table_exists) {
            duckdb::dbAppendTable(con, "nature_risk1", out_file)  # Append new data
          } else {
            duckdb::dbWriteTable(con, "nature_risk1", out_file, overwrite = FALSE)  # Create table
          }

          duckdb::dbDisconnect(con)  # Close connection

          write.csv(out_file, file, row.names = FALSE)  # Save DataFrame as CSV

        }


      }
    )



  })
}

## To be copied in the UI
# mod_matrikkel_screen_ui("matrikkel_screen_1")

## To be copied in the server
# mod_matrikkel_screen_server("matrikkel_screen_1")
