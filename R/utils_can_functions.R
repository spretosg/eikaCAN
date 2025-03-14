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
calc_intersection_distance <- function(geom, layers) {
  results <- lapply(layers, function(layer) {
    intersection <- st_intersects(geom, layer)  # Intersection
    distance <- st_distance(geom, layer) %>% min()  # Closest distance
    list(intersection = intersection, distance = distance)
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

  # Group by class and calculate total area
  summary <- aggregate(overlap_results$coverage_fraction,
                       by = list(class = overlap_results$value),
                       FUN = stats_fun)
  colnames(summary) <- c("class", "area_fraction")

  # Calculate actual area in square meters
  pixel_area <- terra::res(raster)[1] * terra::res(raster)[2]
  summary$area_m2 <- summary$area_fraction * pixel_area
  return(summary)
}

# a function to calculate spatial statistics for a given parcel to cover reporting data points
calc_spat_stats <- function(drawn_sf, in_files) {
  drawn_sf <- st_transform(drawn_sf,25833)
  # assure that all geometries have the same CRS (EPSG:25833 - UTM Zone 33N)
  vern<-st_transform(in_files$vern, 25833)
  nat_ku<-st_transform(in_files$nat_ku, 25833)
  inon<-st_transform(in_files$inon, 25833)
  vassdrag<-st_transform(in_files$vassdrag, 25833)
  nin<-st_transform(in_files$nin, 25833)
  lulc<-in_files$lulc

  vern_list<-list(vern,nat_ku,inon,vassdrag,nin)
  vern_vector<-c("Vernområder","Natur av forvaltningsintersse","Inngrepsfrie natur","Vassdragsnatur","Natur i Norge")

  # Initialize an empty list to store results for each polygon
  results_list <- list()

  # Iterate through each polygon in drawn_sf
  for (i in seq_len(nrow(drawn_sf))) {
    single_polygon <- drawn_sf[i, ] # Extract a single polygon

    # Project area m2 E4-5_10
    proj_area<-st_area(single_polygon)

    # Apply function
    spat_stats <- calc_intersection_distance(single_polygon, vern_list)

    # Extract closest distance E4-5_01
    closest_distances <- sapply(spat_stats, function(x) x$distance)

    # Extract intersections E4.IRO-1_14
    intersections <- lapply(spat_stats, function(x) x$intersection)

    # extract overlay with lulc
    lulc_overlay <- calc_overlay(single_polygon,lulc,sum)

    # Join with ESA WorldCover classes
    esa_classes <- data.frame(
      class = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 100),
      label = c("Skog", "Busker", "Jordbruk", "Jordbruk", "Bebygd",
                "Åpen fastmark", "Snø & Is", "Ferskvann",
                "Myr", "Myr", "Moss and lichen")
    )
    summary <- merge(lulc_overlay, esa_classes, by = "class", all.x = TRUE)
    excluded_classes <- c("Bebygd")
    # summary E4.SBM-3_05
    sum_natureloss <- summary %>%
      filter(!label %in% excluded_classes) %>%
      summarise(sum_A = sum(area_m2)) %>%
      pull(sum_A)

    # Add results for this polygon to the list
    results_list[[i]] <- list(
      polygon_id = i,
      project_area_m2 = as.numeric(proj_area),
      min_dist_vern = as.numeric(shortest_distance),
      m2_nat_loss = sum_natureloss,
      int_vern = as.integer(intersect_nature),
      area_stats = summary
    )
  }

  return(results_list)
}

calc_spat_stats_single <- function(drawn_sf, vern, lulc) {
  print("start function ---------")
  # Transform all geometries to the same CRS (EPSG:25833 - UTM Zone 33N)
  drawn_sf <- st_transform(drawn_sf, 25833)
  vern <- st_transform(vern, 25833)


  # Initialize an empty list to store results for each polygon
  results_list <- list()

  # Project area m2
  proj_area<-st_area(drawn_sf)

  # intersection with vern (change after to valuable nature)
  intersect_nature = nrow(st_intersection(drawn_sf, vern))
  print(intersect_nature)

  # Shortest distance calculation
  shortest_distance <- min(st_distance(drawn_sf, vern))

  # Overlap statistics using exactextractr
  overlap_stats <- exact_extract(lulc, drawn_sf)

  # Combine results into a single data frame
  overlap_results <- do.call(rbind, overlap_stats)
  #print(overlap_results)

  # Filter out NA values
  overlap_results <- overlap_results[!is.na(overlap_results$value), ]

  # Group by class and calculate total area
  summary <- aggregate(overlap_results$coverage_fraction,
                       by = list(class = overlap_results$value),
                       FUN = sum)
  colnames(summary) <- c("class", "area_fraction")

  # Calculate actual area in square meters
  pixel_area <- res(lulc)[1] * res(lulc)[2]
  summary$area_m2 <- summary$area_fraction * pixel_area
  print(summary)

  # Join with ESA WorldCover classes
  esa_classes <- data.frame(
    class = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 100),
    label = c("Skog", "Busker", "Jordbruk", "Jordbruk", "Bebygd",
              "Åpen fastmark", "Snø & Is", "Ferskvann",
              "Myr", "Myr", "Moss and lichen")
  )
  summary <- merge(summary, esa_classes, by = "class", all.x = TRUE)
  excluded_classes <- c("Jordbruk", "Bebygd")
  sum_natureloss <- summary %>%
    filter(!label %in% excluded_classes) %>%
    summarise(sum_A = sum(area_m2)) %>%
    pull(sum_A)

  # Add results for this polygon to the list
  results_list <- list(
    polygon_id = as.numeric(1),
    project_area_m2 = as.numeric(proj_area),
    min_dist_vern = as.numeric(shortest_distance),
    m2_nat_loss = sum_natureloss,
    int_vern = as.integer(intersect_nature),
    area_stats = summary
  )
  print("---- end fkt----")

  return(results_list)
}
