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
  # tagList(
    fluidPage(
      h1("Klima og naturrisiko - Aktsomhetsvurdering av prosjekt"),
      br(),
      fluidRow(

             bslib::value_box(
                 title = "",
                 value = "",
                 h3("Med bruks- og gårdsnummer bestemmer matrikkelenehet og lokaliseres prosjektområdet. Deretter bergegnes klima- og naturrisiko eksplisitt for ditt prosjekt. Til slutt kan du lagre statistikk til rapporterings data base og csv filer."),
                 br(),
                 h5(tags$a(href = "https://seeiendom.kartverket.no/", "Du finner disse numrene ved å gjøre et adressesøk", target = "_blank")
                    ),
                 h5(tags$a(href = "https://rpubs.com/retospielhofer/1293207", "Se på bruksanvisningen avsnitt 3.1 for noen eksempler", target = "_blank")
                 ),
                 br(),
                 div(
                   style = "font-size: 22px; padding: 10px;",
                   textInput(ns("eika_id"), "Saksnummer (f.eks. bankens, kredittportalen)")
                 ),
                 br(),
                 div(
                   style = "font-size: 20px; padding: 10px;",
                   selectInput(
                     ns("proj_type"),
                     "Velg prosjekttype",
                     choices = c("", "Bolig" = "house", "Næring" = "industry","Landbruk"="agri"),
                     selected = ""
                   )
                 ),
                 br(),
                 div(
                   style = "font-size: 20px; padding: 10px;",
                   numericInput(ns("gards_nr"),"Gårdsnummer",NA, min = 1, step = 1)
                 ),
                 br(),
                 div(
                   style = "font-size: 20px; padding: 10px;",
                   numericInput(ns("bruks_nr"), "Bruksnummer", NA, min = 1, step = 1)
                 ),
                 br(),
                 # tags$div(
                 #   style = "font-size: 20px; padding: 10px;",
                 #   numericInput(ns("teig_nr"),"Teignummer",NULL, min = 1, step = 1),
                 #   actionLink(ns("info_teig"), label = NULL, icon = icon("info-circle"),
                 #              style = "font-size: 20px; color:  #78BE20; margin-left: 10px;"),
                 #   style = "display: flex; align-items: center;"
                 # ),
                 uiOutput(ns("cond_btn")),
                 theme = bslib::value_box_theme(bg = "#D3D3D3", fg = "black"),
                 showcase= bsicons::bs_icon("book"),
               )

      ),
      br(),
      uiOutput(ns("dashboard"))
    )
}

#' matrikkel_screen Server Functions
#'
#' @noRd
mod_matrikkel_screen_server <- function(id, in_files){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #info teig - matrikkel
    # observeEvent(input$info_teig, {
    #   showModal(modalDialog(
    #     title = "Matrikkel med flere teig",
    #     h4("Husk at en matrikkel kan inneholde flere teiger, så hvis det er mulig, kan du også definere teignummer. Hvis ikke, beregnes risikoen for hele matrikkelen og eventuelt flere teiger."),
    #     easyClose = TRUE,
    #     footer = modalButton("Close")
    #   ))
    # })

    #get the in_files for valuable nature
    parcel<-in_files$parcel

    #render a conditional btn to be sure that necessary input is provided
    output$cond_btn<-renderUI({
      validate(
        need(input$proj_type !='',''),
        need(!is.na(input$bruks_nr),''),
        need(!is.na(input$gards_nr),''),
        need(input$eika_id !='','')
      )

        actionButton(ns("confirm"),"beregn klima- og naturrisiko")

    })

    #select the parcel based on input data

    parcels_sel<-eventReactive(input$confirm,{
      conc_num<-paste0(input$gards_nr,"/",input$bruks_nr)
      #print(input$teig_nr)
      # if(!is.na(input$teig_nr)){
      #   parcels_sel<-parcel%>%filter(matrikkeln == conc_num & teigId ==input$teig_nr)
      # }else{
        parcels_sel<-parcel%>%filter(matrikkeln == conc_num)
      # }
      parcels_sel<-sf::st_as_sf(parcels_sel)
    })


    #show parcel, ev valuable nature layer and calculate impact!
    results<-eventReactive(input$confirm,{
      req(parcels_sel())
      parcels_sel<-parcels_sel()
      shinybusy::show_modal_spinner(text = "beregn klima- og naturrisiko", color = "green")
      if(nrow(parcels_sel)>0){
        results <- calc_spat_stats(parcels_sel, in_files)
      }

      remove_modal_spinner()
      return(results)

    })

    distances<-eventReactive(input$confirm,{
      req(results())
      results<-results()
      if(nrow(parcels_sel())>0 & !is.null(results)){
        # Extract intersections E4.IRO-1_14
        distances <- do.call(rbind, lapply(results, function(x) x$distances_intersection))

        distances<-distances%>%group_by(valuable_areas)%>%
          summarize(min_dist_m = min(as.integer(min_dist),na.rm = T),
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

        # distances<-distances%>%filter(!is.na(min_dist_m))

        return(distances)

      }

    })

    polygons<-eventReactive(input$confirm,{
      req(results())
      results<-results()
      req(distances())
      distances<-distances()
      #print(distances)
      distances$min_dist_m<-as.integer(distances$min_dist_m)

      if(max(distances$min_dist_m,na.rm = T)>0 & !is.null(distances)){
        inter_poly <- do.call(rbind, lapply(results, function(x) x$polygon_geom_df))
       # print(inter_poly)
        #print("here2")
        if(!is.null(inter_poly)){
                  inter_poly <- inter_poly %>%
          group_by(layer_id) %>%
          summarize(geometry = st_union(geom)) %>%
          ungroup()%>%st_set_crs( 25833)
        #print("here3")
        }else{
          inter_poly<-NULL
          #print("here4")
        }


      }else{
        inter_poly<-NULL
      }
      return(inter_poly)
    })


    layer_ids<-eventReactive(input$confirm,{
      req(results())
      results<-results()
      req(distances())
      distances<-distances()
      distances$min_dist_m<-as.integer(distances$min_dist_m)

      if(max(distances$min_dist_m,na.rm = T)>0 & !is.null(distances)){
        layer_ids <- do.call(rbind, lapply(results, function(x) x$layers_id))
        layer_ids <- as.vector(unique(layer_ids))

      }else{
        layer_ids<-NULL
      }
      return(layer_ids)
    })

    ## the UI and the plots
    observeEvent(input$confirm,{
      req(parcels_sel())
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
        distances<-distances()
        polygons<-polygons()
        layer_ids<-layer_ids()
        distances$min_dist_m<-as.integer(distances$min_dist_m)
        distances$intersect_area_m2<-as.integer(distances$intersect_area_m2)
        results<-results()
        ## dashboard and data curation of results
        output$dashboard<-renderUI(
          tagList(
            fluidPage(
              fluidRow(
                h3("Prosjektområdet"),
                h4("I rød ser man arealer av prosjektområder som er innenfor klima eller naturaktsomhetsområder."),
                br(),
                leafletOutput(ns("map_parcel")),
                bslib::value_box(
                  title = NULL,
                  value = NULL,
                  h4("Vær oppmerksom på at kartdataene som ligger til grunn for beregningen av klima- og naturrisiko, er de beste tilgjengelige dataene i Norge, men at de kan inneholde feil eller unøyaktigheter. som en del av aktsomhetsvurderingen er det derfor være viktig å sjekke kart med lokalkunnskap om området. Avvik mellom lokalkunnskap og kartene kan rapporteres nederst på siden"),
                  h5("En slik rapportering kan gjøres nederst på siden."),
                  theme = bslib::value_box_theme(
                    bg = "#F08080",       # Light grey background
                    fg = "black"
                  ),
                  showcase = bsicons::bs_icon("exclamation-triangle") # Exclamation icon
                )

              ),
              # main KPI of both
              fluidRow(column(6,
                              h3("Resultat aktsomhetsvurdering"),
                              br(),
                                shinydashboard::valueBoxOutput(ns("screening_light")),
                                shinydashboard::valueBoxOutput(ns("screening_light_klim"))
              ),
              column(6,
                     h3("Statistikk"),
                     shinydashboard::valueBoxOutput(ns("vbox1")),
                     shinydashboard::valueBoxOutput(ns("vbox2")),
                     shinydashboard::valueBoxOutput(ns("vbox3"))
                     ),
              ),
              uiOutput(ns("dynamic_btn")),
              br(),
              fluidRow(
                shinydashboard::box(title = "Detailjer klimarisiko",
                                    collapsible = TRUE, collapsed = T, width = 12, status = "primary",
                                    column(12,
                                           DT::DTOutput(ns("klima_layers_out"))),
                                    # column(6,
                                    #        h4("Påvirket areal"),
                                    #        plotly::plotlyOutput(ns("area_stats")))
                )#collapsible details nature
              ),

              fluidRow(
                shinydashboard::box(title = "Detailjer naturrisiko",
                                    collapsible = TRUE, collapsed = T, width = 12, status = "primary",
                                    column(6,
                                           DT::DTOutput(ns("nature_layers_out"))),
                                    column(6,
                                           h4("Påvirket arealdekke"),
                                           plotly::plotlyOutput(ns("area_stats")))
                )#collapsible details nature
              ),
              br(),
              fluidRow(
                bslib::value_box(
                  title = NULL,
                  value = NULL,
                  div(
                    style = "font-size: 20px; padding: 10px;",
                  textInput(ns("report_data_layer"),label = "Digitale kart kan ha usikkerheter eller feil. Beskriv eventuelle avvik mellom lokalkunnskap og kartdata her:")),
                  theme = bslib::value_box_theme(
                    bg = "white",       # Light grey background
                    fg = "black"
                  ),
                  showcase = bsicons::bs_icon("pencil-square") # Exclamation icon
                ),
              ),
            )
          ))

        #base map
        ## add the layers that are intersecting to the map
        #print(layer_ids)
        exclude_names <-c("kom_dat","parcel")
        sub_in_files<-in_files[layer_ids]
        sub_in_files <- sub_in_files[setdiff(names(sub_in_files), exclude_names)]
        n_layers <- length(sub_in_files)
        layer_colors <- RColorBrewer::brewer.pal(min(n_layers, 8), "Set1")

        # If more than 8 layers, you can repeat or use a different palette
        if (n_layers > 8) {
          layer_colors <- colorRampPalette(brewer.pal(8, "Set1"))(n_layers)
        }
        #print(names(sub_in_files))
        # Get centroid (returns POINT geometry)
        center <- st_centroid(st_union(parcels_sel))  # st_union ensures single geometry

        # Extract coordinates
        center_coords <- st_coordinates(center)


        ## the map
        output$map_parcel <- renderLeaflet({

          if(!is.null(polygons)){


            polygons<-st_transform(polygons,st_crs(parcels_sel))
            base_map<-leaflet(parcels_sel) %>%
              addProviderTiles(providers$Esri.WorldImagery,options = tileOptions(minZoom = 8, maxZoom = 18),group = "World image")%>%
              addWMSTiles(
                baseUrl = "https://wms.geonorge.no/skwms1/wms.norges_grunnkart?service=wms&request=getcapabilities",
                layers = "norges_grunnkart",  # Choose from 'topo4', 'norges_grunnkart', etc.
                options = WMSTileOptions(format = "image/png", transparent = TRUE),
                attribution = "© Kartverket",
                group = "Kartverket basiskart"
              )

            # Initialize a vector to collect group names
            overlay_groups <- c("Klima-/naturrisiko område")

            for(i in seq_along(sub_in_files)){

              lookup <- list(
                "inon" = "Inngrepsfri natur",
                "nat_ku" = "Truede & sårbare naturtyper",
                "vern" = "Vernområder",
                "strand" = "Strandsone i pressområder",
                "myr" = "Myr",
                "red_listed" = "Rødliste-arter",
                "friluft" = "Viktige og svært viktige friluftslivsområder",
                "kvikk" = "Kvikkleire risikoområde",
                "flom_klima" = "Sone 200-årsflom klimaendring",
                "nat_skog" = "Naturskog",
                "flom_20" = "Sone 20-årsflom",
                "flom_200" = "Sone 200-årsflom",
                "flom_1000" = "Sone 1000-årsflom",
                "flom_akt" = "Flom aktsomhetsområder",
                "skred_100" = "Sone 100-årsskred",
                "skred_1000" = "Sone 1000-årsskred"
              )
              layer <- sub_in_files[[i]]
              layer_id <- names(sub_in_files)[i]

              # Get label from lookup
              label <- lookup[[layer_id]]

              # Optionally add it as a new column to the sf object
              layer$layer_label <- label


              this_color <- layer_colors[i]


              # Append to overlay group vector
              overlay_groups <- c(overlay_groups, label)

                base_map <- base_map %>%
                  addPolygons(data = layer,
                              group = label,
                              fillColor = this_color,
                              weight = 1,
                              opacity = 0.5,
                              color = 'white',
                              fillOpacity = 0.5)

              }


            #
            # if(nrow(distances%>%filter(valuable_areas == "Naturskog" & min_dist_m==0))==1){
            #   raster_layer <- in_files$nat_skog
            #   raster_layer1 <- raster_layer == 1  # this creates a logical raster (TRUE where value == 1)
            #
            #   # Apply the mask to original raster (optional, for retaining values)
            #   nat_skog_subset <- mask(raster_layer, raster_layer1, maskvalue = 0)
            #   base_map <- base_map %>%
            #     addRasterImage(nat_skog_subset,
            #                    colors = "#addd8e",
            #                    group = c("Naturskog"),
            #                    opacity = 0.7,
            #                    project = TRUE)
            #   overlay_groups <- c(overlay_groups, "Naturskog")
            #
            # }
            #
            #
            # if(nrow(distances%>%filter(valuable_areas == "Myr eller våtmark" & min_dist_m==0))==1){
            #   raster_layer <- in_files$myr
            #   raster_layer1 <- raster_layer == 1  # this creates a logical raster (TRUE where value == 1)
            #
            #   # Apply the mask to original raster (optional, for retaining values)
            #   myr_subset <- mask(raster_layer, raster_layer1, maskvalue = 0)
            #   base_map <- base_map %>%
            #     addRasterImage(myr_subset,
            #                    colors = "#9ebcda",
            #                    group = c("Myr eller våtmark"),
            #                    opacity = 0.7,
            #                    project = TRUE)
            #   overlay_groups <- c(overlay_groups, "Myr eller våtmark")
            #
            # }



            base_map <- base_map %>%
              addPolygons(color = "orange",fillColor = "green", weight = 3, smoothFactor = 0.5,
                          opacity = 1.0, fillOpacity = 0)%>%
              addPolygons(data = polygons ,color="red",  fillColor = "red", weight = 0, smoothFactor = 0.5,
                          opacity = 1.0,fillOpacity = 0.5, label = ~ layer_id,
                          highlightOptions = highlightOptions(weight = 1, color = "white", bringToFront = TRUE),
                          group = "Klima-/naturrisiko område")%>%
              addLegend(position = "topright",
                        colors = c( "red"),
                        labels = c("innenfor potensiell klima-/naturrisiko"),
                        title = "",
                        opacity = 1)%>%
              addScaleBar(position = "bottomleft")%>%
              addLayersControl(
                baseGroups = c("Kartverket basiskart", "World image"),
                overlayGroups = overlay_groups,
                options = layersControlOptions(collapsed = FALSE)
              )%>%
              hideGroup(setdiff(overlay_groups, "Klima-/naturrisiko område"))%>%
              setView(lng = center_coords[1], lat = center_coords[2], zoom = 14)



            }else{

            base_map<-leaflet(parcels_sel) %>%
            addProviderTiles(providers$Esri.WorldImagery,options = tileOptions(minZoom = 8, maxZoom = 18),group = "World image")%>%
            addWMSTiles(
              baseUrl = "https://wms.geonorge.no/skwms1/wms.norges_grunnkart?service=wms&request=getcapabilities",
              layers = "norges_grunnkart",  # Choose from 'topo4', 'norges_grunnkart', etc.
              options = WMSTileOptions(format = "image/png", transparent = TRUE),
              attribution = "© Kartverket",
              group = "Kartverket basiskart"
            )%>%
            addPolygons(color = "green",fillColor = "green", weight = 3, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.3, group = "Prosjektområdet" )%>%
            addLayersControl(baseGroups = c("Kartverket basiskart","World image"),
                             overlayGroups = "Prosjektområdet",
                             options = layersControlOptions(collapsed = FALSE))%>%
            addLegend(position = "topright",
                      colors = c("green"),
                      labels = c("utenfor potensiell klima-/naturrisiko"),
                      title = "",
                      opacity = 1)%>%addScaleBar(position = "bottomleft")

          }

        })



        # here calculate the stats for the parcel
        #E4-5_10

        tot_proj_area_m2 <- sum(sapply(results, function(x) x$project_area_m2))
        #print(tot_proj_area_m2)

        ## climate vector for filtering nature and climate distances
        klim_vec<-c("Sone 200-årsflom klimaendring",
                    "Kvikkleire risikoområde",
                    "Sone 20-årsflom",
                    "Sone 200-årsflom",
                    "Sone 1000-årsflom",
                    "Sone 100-årsskred",
                    "Sone 1000-årsskred",
                    "Flom aktsomhetsområder")

        #easy number to check number of intersections nature
        n_inter_nat<-nrow(distances%>%filter(min_dist_m == 0 & !valuable_areas %in% klim_vec))

        #and climate
        n_inter_klim<-nrow(distances%>%filter(min_dist_m == 0 & valuable_areas %in% klim_vec))

        # m2 of parcel that is not bebygged/aggriculture E4.SBM-3_05(natural area loss)
        nat_loss_m2 <- sum(sapply(results, function(x) x$m2_nat_loss))

        #m2 of parcel that is lost for climate important areas E1...
        if(!is.null(polygons)){
          klim_loss_m2<-polygons%>%filter(layer_id %in% klim_vec)%>%st_union()%>%st_area()
        }else{
          klim_loss_m2<-0
        }

        klim_loss_m2<-as.integer(klim_loss_m2)


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
            subtitle = paste0(round(nat_loss_m2/tot_proj_area_m2,4)*100, " % av totalareal er naturareal"),
            #icon = icon("leaf"),  # Choose an appropriate FontAwesome icon
            color = "olive",
            width = 6
          )
        })

        ## klimate relevant affected area
        output$vbox3 <- shinydashboard::renderValueBox({

          valueBox(
            value = paste0(round(klim_loss_m2,0), " m²"),  # Display the area value
            subtitle = paste0(round(klim_loss_m2/tot_proj_area_m2,4)*100, " % av totalareal er klima relevant areal"),
            #icon = icon("leaf"),  # Choose an appropriate FontAwesome icon
            color = "light-blue",
            width = 6
          )
        })

        #E4-5_10
        output$vbox1 <- shinydashboard::renderValueBox({

          valueBox(
            value = paste0(round(tot_proj_area_m2,0), " m²"),  # Display the area value
            subtitle = "Prosjektets total areal",
            #icon = icon("map"),  # Choose an appropriate FontAwesome icon
            color = "black",
            width = 12
          )
        })

        ## screening light:
        output$screening_light<-shinydashboard::renderValueBox({

          if(n_inter_nat==0){
            valueBox(
              value = icon("check"),  # Display the area value
              subtitle = "utenfor område med potensiell naturrisiko",
              #icon = icon("leaf"),  # Choose an appropriate FontAwesome icon
              color = "olive",
              width = 12
            )

          }else{
            valueBox(
              value = icon("xmark"),  # Display the area value
              subtitle = "innenfor område med potensiell naturrisiko",
              #icon = icon("leaf"),  # Choose an appropriate FontAwesome icon
              color = "red",
              width = 12
            )

          }

        })

        ## climate flag
        output$screening_light_klim<-renderUI({

          if(n_inter_klim==0){
            valueBox(
              value = icon("check"),  # Display the area value
              subtitle = "utenfor område med potensiell klimarisiko",
              #icon = icon("cloud"),  # Choose an appropriate FontAwesome icon
              color = "olive",
              width = 12
            )

          }else{
            valueBox(
              value = icon("xmark"),  # Display the area value
              subtitle = "innenfor område med potensiell klimarisiko",
              #icon = icon("cloud"),  # Choose an appropriate FontAwesome icon
              color = "red",
              width = 12
            )

          }

        })

        output$dynamic_btn<-renderUI({
          if(n_inter_nat==0 & n_inter_klim==0){

              downloadButton(ns("save"),"Lagre aktsomhetsvudering og oppdater portefølje")

          }else if(n_inter_nat==0 & n_inter_klim!=0){
            downloadButton(ns("save"),"Lagre aktsomhetsvudering og oppdater portefølje")
          }else{
            tagList(
              actionButton(ns("questions"),"Til oppfølgingsspørsål"),
              h4(
                icon("exclamation-circle", class = "text-danger fa-beat"),  # Add icon here
                "Fyll ut oppfølgingsspørsål for å kunne lagre aktsomhetsvurdering",
                style = "color: red;"
              )
            )


          }
        })


        output$cond_btn2<-renderUI({
          validate(
            need(input$E4.IRO1_04_c !='',''),
            need(input$E4.IRO1_15 !='','')     )
          downloadButton(ns("save"),"Lagre og oppdater portefølje")
        })

        observeEvent(input$questions,{
          shinyalert::shinyalert(
            title = "Oppfølgingsspørsmål",
            html = TRUE,
            text = tags$div(
              br(),
              selectInput(ns("E4.IRO1_03"),
                          label = HTML(
                            'Har prosjekteier foretatt en Unngå-Flytte-Forbedre (UFF) vurdering av prosjektlokalisering og utforming?
     <a href="https://rpubs.com/retospielhofer/1295853"
        target="_blank"
        style="color: #007BFF; text-decoration: underline; margin-left: 5px;">
       (ESRS datapunkt: E4.IRO1_03)
     </a>'), c("","Ja","Nei"), selected = ""),


              br(),
              selectInput(ns("E4.IRO1_04_b"),HTML('Er prosjekteier informert om kommunens mål for arealutvikling/arealreserve/ arealbudsjett?
                            <a href="https://rpubs.com/retospielhofer/1295917"
              target="_blank"
        style="color: #007BFF; text-decoration: underline; margin-left: 5px;">
                            (ESRS datapunkt: E4.IRO1_04)
                          </a>'),c("","Ja","Nei"), selected = ""),
              br(),
              textInput(ns("E4.IRO1_04_c"),HTML('Hvordan vurderer prosjekteier Eika.CAN prosjektets konsekvenser for naturverdier sett i forhold til kommunens historiske naturtap ?
              <a href="https://rpubs.com/retospielhofer/1295917"
              target="_blank"
        style="color: #007BFF; text-decoration: underline; margin-left: 5px;">
                            (ESRS datapunkt: E4.IRO1_04)
                          </a>')),
              br(),
              selectInput(ns("E4.IRO1_05_a"),
                          label = HTML(
                          'Har prosjekteier vurdert prosjektlokalisering ift. arealplanens bestemmelser?
                            <a href="https://rpubs.com/retospielhofer/1295857"
        target="_blank"
        style="color: #007BFF; text-decoration: underline; margin-left: 5px;">
                            (ESRS datapunkt: E4.IRO1_05)
                          </a>'),c("","Ja","Nei"), selected = ""),
              br(),
              selectInput(ns("E4.IRO1_05_b"),label = HTML('Har prosjekteier fått planfaglige råd om prosjektlokalisering fra kommunen?

                            <a href="https://rpubs.com/retospielhofer/1295857"
        target="_blank"
        style="color: #007BFF; text-decoration: underline; margin-left: 5px;">
                            (ESRS datapunkt: E4.IRO1_05)
                          </a>'),c("","Ja","Nei"), selected = ""),
              br(),
              selectInput(ns("E4.IRO1_05_c"),label = HTML('Har prosjekteier informert naboer til eiendommen om prosjektplanen?
                            <a href="https://rpubs.com/retospielhofer/1295857"
        target="_blank"
        style="color: #007BFF; text-decoration: underline; margin-left: 5px;">
                            (ESRS datapunkt: E4.IRO1_05)
                          </a>'),c("","Ja","Nei"), selected = ""),
              br(),
              selectInput(ns("E4.IRO1_06_a"),label = HTML('Har prosjekteier vurdert konsekvenser for eiere og brukere av naboeiendommer av prosjektet?
                                          <a href="https://rpubs.com/retospielhofer/1295905"
        target="_blank"
        style="color: #007BFF; text-decoration: underline; margin-left: 5px;">
                            (ESRS datapunkt: E4.IRO1_06)
                          </a>'),c("","Ja","Nei"), selected = ""),
              br(),
              selectInput(ns("E4.IRO1_06_b"),label = HTML('Har prosjekteier utført en analyse av tiltak for å Unngå-Flytte-Forbedre (UFF) med en konsekvensutredning av prosjektet?                                          <a href="https://rpubs.com/retospielhofer/1295905"
        target="_blank"
        style="color: #007BFF; text-decoration: underline; margin-left: 5px;">
                            (ESRS datapunkt: E4.IRO1_06)
                          </a>'),c("","Ja","Nei"), selected = ""),
              br(),
              selectInput(ns("E4.IRO1_07"),label = HTML('Har prosjekteier informert naboer av prosjektet om prosjektformål og -utforming?
                                         <a href="https://rpubs.com/retospielhofer/1295909"
        target="_blank"
        style="color: #007BFF; text-decoration: underline; margin-left: 5px;">
                            (ESRS datapunkt: E4.IRO1_07)
                          </a>'),c("","Ja","Nei"), selected = ""),
              br(),
              selectInput(ns("E4.IRO1_08_a"),HTML('Har prosjekteier utført en analyse av tiltak for å Unngå-Flytte-Forbedre (UFF) prosjektet, spesielt ift. klimaregnskap?
                                  <a href="https://rpubs.com/retospielhofer/1295913"
        target="_blank"
        style="color: #007BFF; text-decoration: underline; margin-left: 5px;">
                            (ESRS datapunkt: E4.IRO1_08)
                          </a>'),c("","Ja","Nei"), selected = ""),
              br(),
              selectInput(ns("E4.IRO1_08_b"),HTML('Har prosjekteier utført en analyse av tiltak for å Unngå-Flytte-Forbedre (UFF) prosjektet, spesielt ift. konsekvenser for overvannshåndtering, friluftslivs eller andre naturgoder?                                  <a href="https://rpubs.com/retospielhofer/1295913"
        target="_blank"
        style="color: #007BFF; text-decoration: underline; margin-left: 5px;">
                            (ESRS datapunkt: E4.IRO1_08)
                          </a>'),c("","Ja","Nei"), selected = ""),
              br(),
              textInput(ns("E4.IRO1_15"),HTML('Hvordan vil prosjekteier Unngå-Flytte-Forbedre (UFF) prosjektet ift. naturverdiene som er identifisert i aktsomhetsvurderingen?
              <a href="https://rpubs.com/retospielhofer/1295915"
              target="_blank"
        style="color: #007BFF; text-decoration: underline; margin-left: 5px;">
                            (ESRS datapunkt: E4.IRO1_15)
                          </a>')),
              br(),
              uiOutput(ns("cond_btn2"))
            ),
            showConfirmButton = FALSE,
            closeOnEsc = F,
            closeOnClickOutside = T,
            showCancelButton = FALSE,
            animation = "slide-from-bottom",
            size = "m"
          )

        })

        # report on all nature layers
        output$nature_layers_out<-DT::renderDT({
          dt_nat<-distances%>%filter(!valuable_areas %in% klim_vec & !is.na(min_dist_m))%>%
            mutate(
              rel_area = as.integer(intersect_area_m2)/tot_proj_area_m2 ,
              # Replace Inf with 0
              rel_area = ifelse(is.infinite(rel_area), 0, rel_area),
              # Round rel_area to integer
              intersect_area_m2 = as.integer(intersect_area_m2),
              rel_area = round(rel_area*100,2))
          colnames(dt_nat)<-c("Område med potensiell klimarisiko","Kortest distanse [m]","Prosjektareal innenfor risikoareal [m²]","% av total prosjektareal")
          DT::datatable(dt_nat,
                        options = list(
                          searching = FALSE  # Disable the search field
                        ))%>% DT::formatStyle(
            'Kortest distanse [m]',
            target = 'row',
            backgroundColor = styleEqual(0, "red")
          )
        })

        # report on all climate layers
        output$klima_layers_out<-DT::renderDT({
          dt_clim<-distances%>%filter(valuable_areas %in% klim_vec & !is.na(min_dist_m))%>%
            mutate(
              rel_area = as.integer(intersect_area_m2)/tot_proj_area_m2 ,
              # Replace Inf with 0
              rel_area = ifelse(is.infinite(rel_area), 0, rel_area),
              # Round rel_area to integer
              intersect_area_m2 = as.integer(intersect_area_m2),
              rel_area = round(rel_area*100,2))
          colnames(dt_clim)<-c("Område med potensiell klimarisiko","Kortest distanse [m]","Prosjektareal innenfor risikoareal [m²]","% av total prosjektareal")


          DT::datatable(dt_clim,
                        options = list(
                          searching = FALSE  # Disable the search field
                        ))%>% DT::formatStyle(
            'Kortest distanse [m]',
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
            y = ~fraction*100,  # Area in square meters on y-axis
            type = "bar"
          ) %>%
            plotly::layout(
              yaxis = list(title = "Areal (%)")
            )
        })

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
          #print(tot_proj_area_m2)

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
         # print(lulc_wide)

          klim_vec<-c("Flomsoner 200år klima","Kvikkleire risikoområde")
          #easy number to check number of intersections nature
          n_inter_nat<-nrow(distances%>%filter(min_dist_m == "0" & !valuable_areas %in% klim_vec))

          #and climate
          n_inter_klim<-nrow(distances%>%filter(min_dist_m == "0" & valuable_areas %in% klim_vec))



          # m2 of parcel that is not bebygged/aggriculture E4.SBM-3_05(natural area loss)
          nat_loss_m2 <- sum(sapply(results, function(x) x$m2_nat_loss))



          proj_param<- data.frame(
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
            #duckdb::dbAppendTable(con, "nature_risk1", out_file)  # Append new data
          } else {
            #duckdb::dbWriteTable(con, "nature_risk1", out_file, overwrite = FALSE)  # Create table
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
