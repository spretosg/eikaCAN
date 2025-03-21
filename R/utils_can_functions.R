#' can_functions
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#'

## small function for distance and intersection calc
# Function to calculate intersection and distance between shapes
calc_min_distance <- function(geom, layers) {
  results <- lapply(layers, function(layer) {
    distance <- st_distance(geom, layer) %>% min()  # Closest distance
    if(as.numeric(distance) == 0){
      # Compute intersection AREA
      intersection <- TRUE
      intersection_area <- st_area(st_intersection(geom, layer))

    }else{
      intersection <- FALSE
      intersection_area <- NA
    }

    list(distance = as.integer(distance),
         intersection = intersection,
         intersection_area = as.integer(intersection_area))
  })
  return(results)
}

# calculate overlay m2 of polygon and raster
calc_overlay<-function(geom,raster,stats_fun){
  overlap_stats <- exactextractr::exact_extract(raster, geom)

  # Combine results into a single data frame
  overlap_results <- do.call(rbind, overlap_stats)


  # Filter out NA values
  overlap_results <- overlap_results[!is.na(overlap_results$value), ]
  if(nrow(overlap_results)>0){
    # Group by class and calculate total area
    summary <- aggregate(overlap_results$coverage_fraction,
                         by = list(class = overlap_results$value),
                         FUN = stats_fun)
  }else{
    summary<-data.frame(class = 99,
                           area_fraction = 0)
  }

  colnames(summary) <- c("class", "area_fraction")

  # Calculate actual area in square meters
  pixel_area <- terra::res(raster)[1] * terra::res(raster)[2]
  summary$area_m2 <- summary$area_fraction * pixel_area

  return(summary)
}

transform_sf_objects <- function(obj, crs_target) {
  if (inherits(obj, "sf")) {
    return(sf::st_transform(obj, crs_target))
  } else {
    return(obj)  # Keep raster objects unchanged
  }
}

# a function to calculate spatial statistics for a given parcel to cover reporting data points
calc_spat_stats <- function(drawn_sf, in_files) {
  # Target CRS
  new_crs <- 25833  # EPSG:25833
  drawn_sf <- st_transform(drawn_sf,new_crs)

  # Apply transformation only to sf objects
  in_files <- lapply(in_files, transform_sf_objects, crs_target = new_crs)

  # a subset list of just the objects to calculate distance from parcel
  vern_list<-list(in_files$vern,in_files$nat_ku,in_files$inon,in_files$vassdrag,in_files$strand,in_files$red_listed,in_files$friluft)
  vern_vector<-c("Vernområder","Natur av forvaltningsintersse","Inngrepsfrie natur","Vassdragsnatur","Strandsone","Rød lista arter", "Friluftslivsområder")

  # Initialize an empty list to store results for each polygon
  results_list <- list()

  # Iterate through each polygon in drawn_sf
  for (i in seq_len(nrow(drawn_sf))) {
    single_polygon <- drawn_sf[i, ] # Extract a single polygon

    # Project area m2 E4-5_10
    proj_area<-st_area(single_polygon)

    # Apply function
    spat_stats <- calc_min_distance(single_polygon, vern_list)

    # Extract closest distance E4-5_01
    df<-cbind(sapply(spat_stats, function(x) x$distance),
    sapply(spat_stats, function(x) x$intersection),
    sapply(spat_stats, function(x) x$intersection_area),vern_vector)

    df<-as.data.frame(df)
    colnames(df)<-c("min_dist","intersect","intersection_area","valuable_nat")


    ################## extract overlay with lulc
    lulc_overlay <- calc_overlay(single_polygon,in_files$lulc,sum)
    forest_overlay <- calc_overlay(single_polygon,in_files$nat_skog, sum)
    myr_overlay <- calc_overlay(single_polygon,in_files$myr,sum)

    # Join with ESA WorldCover classes
    lulc_classes <- data.frame(
      class = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,12 ,99),
      label = c("Bebyggelse/samferdsel", "Dyrket mark", "Grasmark (Innmarksbeite)", "Skog", "Hei og åpen vegetasjon",
                "Lite vegetert mark", "Våtmark", "Elver/bekker",
                "Innsjøer/tjern", "Marine bukter og brakkvann", "Svaberg, kyststrender og dyner",
                "Åpent hav","Uklassifisert areal")
    )

    skog_classes <- data.frame(
      class = c(0, 1,99),
      label = c("Skog men ikke naturskog", "Naturskog - Skog etablert før 1940, ikke flatehogd","no data")
    )
    myr_classes <- data.frame(
      class = c(0, 1,99),
      label = c("Ikke myr", "Myr eller våtmark","no_data")
    )

    lulc_summary <- merge(lulc_overlay, lulc_classes, by = "class", all.x = TRUE)
    myr_summary <- merge(myr_overlay, myr_classes, by = "class", all.x = TRUE)
    skog_summary <- merge(forest_overlay, skog_classes, by = "class", all.x = TRUE)


     # amount of summary classes that is not bebygged E4.SBM-3_05
    excluded_classes <- c("Bebyggelse/samferdsel", "Dyrket mark")
    sum_natureloss <- lulc_summary %>%
      filter(!label %in% excluded_classes) %>%
      summarise(sum_A = sum(area_m2)) %>%
      pull(sum_A)

    # Add results for this polygon to the list
    results_list[[i]] <- list(
      polygon_id = i,
      project_area_m2 = as.numeric(proj_area),
      distances_intersection = df,
      m2_nat_loss = sum_natureloss,
      lulc_stats = lulc_summary,
      myr_stats = myr_summary,
      skog_stats = skog_summary
    )
  }

  return(results_list)
}

# calc_spat_stats_single <- function(drawn_sf, vern, lulc) {
#   print("start function ---------")
#   # Transform all geometries to the same CRS (EPSG:25833 - UTM Zone 33N)
#   drawn_sf <- st_transform(drawn_sf, 25833)
#   vern <- st_transform(vern, 25833)
#
#
#   # Initialize an empty list to store results for each polygon
#   results_list <- list()
#
#   # Project area m2
#   proj_area<-st_area(drawn_sf)
#
#   # intersection with vern (change after to valuable nature)
#   intersect_nature = nrow(st_intersection(drawn_sf, vern))
#   print(intersect_nature)
#
#   # Shortest distance calculation
#   shortest_distance <- min(st_distance(drawn_sf, vern))
#
#   # Overlap statistics using exactextractr
#   overlap_stats <- exact_extract(lulc, drawn_sf)
#
#   # Combine results into a single data frame
#   overlap_results <- do.call(rbind, overlap_stats)
#   #print(overlap_results)
#
#   # Filter out NA values
#   overlap_results <- overlap_results[!is.na(overlap_results$value), ]
#
#   # Group by class and calculate total area
#   summary <- aggregate(overlap_results$coverage_fraction,
#                        by = list(class = overlap_results$value),
#                        FUN = sum)
#   colnames(summary) <- c("class", "area_fraction")
#
#   # Calculate actual area in square meters
#   pixel_area <- res(lulc)[1] * res(lulc)[2]
#   summary$area_m2 <- summary$area_fraction * pixel_area
#   print(summary)
#
#   # Join with ESA WorldCover classes
#   esa_classes <- data.frame(
#     class = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 100),
#     label = c("Skog", "Busker", "Jordbruk", "Jordbruk", "Bebygd",
#               "Åpen fastmark", "Snø & Is", "Ferskvann",
#               "Myr", "Myr", "Moss and lichen")
#   )
#   summary <- merge(summary, esa_classes, by = "class", all.x = TRUE)
#   excluded_classes <- c("Jordbruk", "Bebygd")
#   sum_natureloss <- summary %>%
#     filter(!label %in% excluded_classes) %>%
#     summarise(sum_A = sum(area_m2)) %>%
#     pull(sum_A)
#
#   # Add results for this polygon to the list
#   results_list <- list(
#     polygon_id = as.numeric(1),
#     project_area_m2 = as.numeric(proj_area),
#     min_dist_vern = as.numeric(shortest_distance),
#     m2_nat_loss = sum_natureloss,
#     int_vern = as.integer(intersect_nature),
#     area_stats = summary
#   )
#   print("---- end fkt----")
#
#   return(results_list)
# }

write_stats_to_DB <- function(stats){
  #gather the different data points

  #write to parquet file
}
