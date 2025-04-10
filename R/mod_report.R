#' report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_report_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h1("Oversikt portefølje"),
    fluidRow(
      bslib::value_box(
        title = "",
        value = "",
        h4("Statistikk og oversikt over alle prosjekter og hele portefølje"),
        theme = bslib::value_box_theme(bg = "#D3D3D3", fg = "black"),
        showcase= bsicons::bs_icon("book")
      )
    ),
    br(),
    fluidRow(
      shinydashboard::valueBoxOutput(ns("n_proj")),
      shinydashboard::valueBoxOutput(ns("natur")),
      shinydashboard::valueBoxOutput(ns("klima"))
    ),
    br(),
    fluidRow(
      h4("Prosjekter per prosjekttype"),
      column(5,
             plotly::plotlyOutput(ns("n_proj_stats"))),
      column(7,
             leaflet::leafletOutput(ns("res_map")))
    ),
    br(),
    fluidRow(
      h4("Areal i natur / klimarisikoområder"),
      column(6, plotly::plotlyOutput(ns("intersection_nat"))),
      column(6,plotly::plotlyOutput(ns("intersection_klim")))
    ),
    br(),
    fluidRow(
      h4("Påvirket arealdekke"),
      plotly::plotlyOutput(ns("arealstats"))
    )

  )
}

#' report Server Functions
#'
#' @noRd
mod_report_server <- function(id, in_files){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # db_path <- file.path("inst/extdata", "CAN_DB.duckdb")
    # con <- DBI::dbConnect(duckdb::duckdb(), db_path)

    # Load the table into a dataframe
    # report_df <- DBI::dbGetQuery(con, "SELECT * FROM nature_risk1")


    ### generate random data

    #proj type
    prj_type_sum<-data.frame(
      project_type = c("naering","bolig","landbruk"),
      sum_proj_area = c(2100145,310101,805000),
      n_prosjekter = c(12,9,4),
      klim_risiko_prosj = c(77,15,55),
      nat_risiko_prosj = c(34,44,17)
    )


    output$n_proj_stats<-plotly::renderPlotly({
      plotly::plot_ly(
        data = prj_type_sum,
        x = ~project_type,  # Land cover classes on x-axis
        y = ~n_prosjekter,  # Area in square meters on y-axis
        color = ~project_type,
        type = "bar"
      ) %>%
        plotly::layout(
          yaxis = list(title = "Antall prosjekter"),
          xaxis = list(title = "")

        )
    })

    kom_dat<-in_files$kom_dat
    set.seed(123)  # For reproducibility
    random_points <- st_sample(kom_dat, size = 25, type = "random")
    classes <- sample(1:3, 25, replace = TRUE)  # Random assignment of 3 classes
    points_df <- data.frame(id = 1:25, class = classes)

    # Combine the points with their classes
    random_points_sf <- st_sf(points_df, geometry = random_points)
    # Step 4: Convert to a data frame for Leaflet
    random_points_df <- as.data.frame(random_points_sf)
    random_points_df$popup_text <- paste("Saksnummer:", random_points_df$id)



    output$res_map<-renderLeaflet({
      leaflet::leaflet(kom_dat) %>%
        leaflet::addTiles() %>%
        leaflet::addPolygons(color = "orange", weight = 3, smoothFactor = 0.5,
                             opacity = 1.0, fillOpacity = 0)%>%
        leaflet::addCircleMarkers(data = random_points_df,
                                  lng = ~st_coordinates(geometry)[,1],
                                  lat = ~st_coordinates(geometry)[,2],
                                  color = ~ifelse(class == 1, "green", ifelse(class == 2, "orange", "red")),
                                  radius = 6,
                                  fillOpacity = 0.7,
                                  popup = ~popup_text,
                                  group = "Points")%>%  addLegend(position = "topright",  # Position the legend in the top right corner
                                                                  colors = c("green", "orange", "red"),  # Colors for each class
                                                                  labels = c("Ingen klima- og naturrisiko", "Klima- eller naturrisiko", "Klima- og naturrisiko"),  # Corresponding labels
                                                                  title = "Point Classes",  # Title for the legend
                                                                  opacity = 1)

    })

    ## naturrisiko
    set.seed(123)  # For reproducibility

    # Define predefined sum for each proj_type (in m2)
    proj_sums <- c(naering = 1617112, bolig = 136444.4, landbruk = 136850)

    # Define Risikoareal and proj_type
    natur_risiko_arealer <- data.frame(
      Risikoareal = c("Vernområder", "Natur av forvaltningsintersse", "Inngrepsfrie natur",
                      "Vassdragsnatur", "Strandsone", "Rød lista arter", "Friluftslivsområder"),
      proj_type = rep(c("naering", "bolig", "landbruk"), each = 7),
      areal = numeric(21)  # Placeholder for areal values
    )

    # Function to distribute sum randomly
    distribute_sum <- function(sum_value, n) {
      # Generate random proportions
      random_proportions <- runif(n)
      random_proportions <- random_proportions / sum(random_proportions)  # Normalize to sum to 1
      return(random_proportions * sum_value)  # Scale by sum_value
    }

    # Apply to each proj_type
    for(proj in unique(natur_risiko_arealer$proj_type)) {
      # Get rows for this proj_type
      rows <- natur_risiko_arealer$proj_type == proj

      # Distribute the predefined sum to these rows
      natur_risiko_arealer$areal[rows] <- distribute_sum(proj_sums[proj], sum(rows))
    }

    output$intersection_nat<-plotly::renderPlotly({
      plotly::plot_ly(
        data = natur_risiko_arealer,
        x = ~Risikoareal,  # Land cover classes on x-axis
        y = ~areal,  # Area in square meters on y-axis
        color = ~proj_type,
        type = "bar"
      ) %>%
        plotly::layout(
          yaxis = list(title = "m2"),
          xaxis = list(title = "")

        )
    })

    ## klimarisiko

    proj_sum_klim <- c(naering = 1316112, bolig = 135455.4, landbruk = 116850)

    # Define Risikoareal and proj_type
    klim_risiko_arealer <- data.frame(
      Risikoareal = c("Sone 200-årsflom klimaendring",
                      "Kvikkleire risikoområde",
                      "Sone 20-årsflom",
                      "Sone 200-årsflom",
                      "Sone 1000-årsflom",
                      "Sone 100-årsskred",
                      "Sone 1000-årsskred",
                      "Flom aktsomhetsområder"),
      proj_type = rep(c("naering", "bolig", "landbruk"), each = 8),
      areal = numeric(24)  # Placeholder for areal values
    )



    # Apply to each proj_type
    for(proj in unique(klim_risiko_arealer$proj_type)) {
      # Get rows for this proj_type
      rows <- klim_risiko_arealer$proj_type == proj

      # Distribute the predefined sum to these rows
      klim_risiko_arealer$areal[rows] <- distribute_sum(proj_sum_klim[proj], sum(rows))
    }


    output$intersection_klim<-plotly::renderPlotly({
      plotly::plot_ly(
        data = klim_risiko_arealer,
        x = ~Risikoareal,  # Land cover classes on x-axis
        y = ~areal,  # Area in square meters on y-axis
        color = ~proj_type,
        type = "bar"
      ) %>%
        plotly::layout(
          yaxis = list(title = "m2"),
          xaxis = list(title = "")

        )
    })

    #arealendringer
    area_change <-data.frame(
      Arealdekke = c("Bebyggelse/samferdsel", "Dyrket mark", "Grasmark (Innmarksbeite)", "Skog", "Hei og åpen vegetasjon",
                     "Lite vegetert mark", "Våtmark", "Elver/bekker",
                     "Innsjøer/tjern", "Marine bukter og brakkvann", "Svaberg, kyststrender og dyner",
                     "Åpent hav","Uklassifisert areal"),
      proj_type = rep(c("naering", "bolig", "landbruk"), each = 13),
      areal = c(3215246*0.32,3215246*0.08,3215246*0.2,3215246*0.11,3215246*0.02,3215246*0.05,3215246*0.15,3215246*0.07,0,0,0,0,0)

    )

    output$arealstats<-plotly::renderPlotly({
      plotly::plot_ly(
        data = area_change,
        x = ~Arealdekke,  # Land cover classes on x-axis
        y = ~areal,  # Area in square meters on y-axis
        type = "bar"
      ) %>%
        plotly::layout(
          yaxis = list(title = "m2"),
          xaxis = list(title = "")

        )
    })





    output$n_proj <- shinydashboard::renderValueBox({
      valueBox(
        value = 25,  # Display the area value
        subtitle = "Prosjekter har blitt vudert",
        icon = icon("number"),  # Choose an appropriate FontAwesome icon
        color = "black"
      )
    })

    output$klima <- shinydashboard::renderValueBox({
      valueBox(
        value = 13,  # Display the area value
        subtitle = "Prosjekter har et høyt klimarisiko",
        icon = icon("cloud"),  # Choose an appropriate FontAwesome icon
        color = "blue"
      )
    })
    output$natur <- shinydashboard::renderValueBox({
      valueBox(
        #value = nrow(report_df%>%filter(n_intersection_val_nat!=0)),
        value = 18,# Display the area value
        subtitle = "Prosjekter har et høyt naturrisiko",
        icon = icon("leafe"),  # Choose an appropriate FontAwesome icon
        color = "olive"
      )
    })

  })
}

## To be copied in the UI
# mod_report_ui("report_1")

## To be copied in the server
# mod_report_server("report_1")
