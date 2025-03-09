
create_demographic_map <- function() {
  demographic_data <- fread("WHO_Data/Joint_demographic_VL_data.csv")
  
  # Calculate FFM for individual records first
  demographic_data <- demographic_data %>%
    mutate(
      ALALPHA = ifelse(SEX == 0, 0.88, 1.11),
      A50 = ifelse(SEX == 0, 13.4, 7.1),
      ALGAMMA = ifelse(SEX == 0, 12.7, 1.1),
      WHSMAX = ifelse(SEX == 0, 42.92, 37.99),
      WHS50 = ifelse(SEX == 0, 30.93, 35.98),
      AGEGAM = AGE^ALGAMMA,
      A50GAM = A50^ALGAMMA,
      FFM = ((AGEGAM + ALALPHA * A50GAM) / (AGEGAM + A50GAM)) * 
        ((WHSMAX * ((HT / 100)^2) * WT) / (WHS50 * ((HT / 100)^2) + WT))
    )
  
  # Create summary statistics
  demographic <- demographic_data %>%
    group_by(COUNTRY) %>%
    filter(region %in% c("Ethiopia", "Kenya", "Sudan", "South Sudan", "Uganda", "Undetermined - Africa")) %>%
    summarise(
      Count = n(),
      # Age statistics
      meanAGE = round(mean(AGE, na.rm = TRUE), 2),
      medianAGE = round(median(AGE, na.rm = TRUE), 2),
      sdAGE = round(sd(AGE, na.rm = TRUE), 2),
      maxAGE = round(max(AGE, na.rm = TRUE), 2),
      minAGE = round(min(AGE, na.rm = TRUE), 2),
      
      # Height statistics
      meanHT = round(mean(HT, na.rm = TRUE), 2),
      medianHT = round(median(HT, na.rm = TRUE), 2),
      sdHT = round(sd(HT, na.rm = TRUE), 2),
      maxHT = round(max(HT, na.rm = TRUE), 2),
      minHT = round(min(HT, na.rm = TRUE), 2),
      
      # Weight statistics
      meanWT = round(mean(WT, na.rm = TRUE), 2),
      medianWT = round(median(WT, na.rm = TRUE), 2),
      sdWT = round(sd(WT, na.rm = TRUE), 2),
      maxWT = round(max(WT, na.rm = TRUE), 2),
      minWT = round(min(WT, na.rm = TRUE), 2),
      
      # FFM statistics
      meanFFM = round(mean(FFM, na.rm = TRUE), 2),
      medianFFM = round(median(FFM, na.rm = TRUE), 2),
      sdFFM = round(sd(FFM, na.rm = TRUE), 2),
      maxFFM = round(max(FFM, na.rm = TRUE), 2),
      minFFM = round(min(FFM, na.rm = TRUE), 2),
      
      # Demographics
      perfemale = round((sum(SEX == 1, na.rm = TRUE) / n()) * 100, 2),
      region = unique(region)
    )
  
  country_coords <- data.frame(
    country = c("Ethiopia", "Kenya", "Sudan", "South Sudan", "Uganda", "Undetermined - Africa"),
    lat = c(9.145, -1.292066, 12.862807, 6.877, 1.373333, 2.7), 
    lon = c(40.489673, 36.821946, 30.217636, 31.307, 32.290275, 21.8) 
  )
  
  # Merge coordinates with demographic data
  demographic_sum <- demographic %>%
    left_join(country_coords, by = c("region" = "country"))
  
  # Create color palette based on Count
  count_pal <- colorNumeric(
    palette = "Spectral",
    domain = demographic_sum$Count,
    reverse = TRUE
  )
  
  # Create dynamic radius scaling function
  radius_scale <- function(x) {
    min_radius <- 10
    max_radius <- 25
    scaled <- (x - min(x)) / (max(x) - min(x))
    min_radius + scaled * (max_radius - min_radius)
  }
  
  # Create custom popup content
  create_popup <- function(data) {
    paste0(
      "<div style='font-family: Arial; max-width: 300px;'>",
      "<h3 style='margin-top: 0;'>", data$region, "</h3>",
      "<hr style='margin: 5px 0;'>",
      "<div style='background-color: #f0f0f0; padding: 8px; border-radius: 4px; margin-bottom: 8px;'>",
      "<strong>Sample Size:</strong> ", data$Count, "<br>",
      "<strong>Female Percentage:</strong> ", data$perfemale, "%",
      "</div>",
      
      "<div style='display: grid; grid-template-columns: 1fr 1fr; gap: 10px;'>",
      
      "<div style='background-color: #e6f3ff; padding: 8px; border-radius: 4px;'>",
      "<strong>Age (years)</strong><br>",
      "Mean: ", data$meanAGE, "<br>",
      "Median: ", data$medianAGE, "<br>",
      "SD: ", data$sdAGE, "<br>",
      "Range: ", data$minAGE, "-", data$maxAGE,
      "</div>",
      
      "<div style='background-color: #fff0e6; padding: 8px; border-radius: 4px;'>",
      "<strong>Height (cm)</strong><br>",
      "Mean: ", data$meanHT, "<br>",
      "Median: ", data$medianHT, "<br>",
      "SD: ", data$sdHT, "<br>",
      "Range: ", data$minHT, "-", data$maxHT,
      "</div>",
      
      "<div style='background-color: #e6ffe6; padding: 8px; border-radius: 4px;'>",
      "<strong>Weight (kg)</strong><br>",
      "Mean: ", data$meanWT, "<br>",
      "Median: ", data$medianWT, "<br>",
      "SD: ", data$sdWT, "<br>",
      "Range: ", data$minWT, "-", data$maxWT,
      "</div>",
      
      "<div style='background-color: #ffe6e6; padding: 8px; border-radius: 4px;'>",
      "<strong>Fat-Free Mass (kg)</strong><br>",
      "Mean: ", data$meanFFM, "<br>",
      "Median: ", data$medianFFM, "<br>",
      "SD: ", data$sdFFM, "<br>",
      "Range: ", data$minFFM, "-", data$maxFFM,
      "</div>",
      
      "</div>",
      "</div>"
    )
  }
  
  maxBounds <- list(
    c(-50, -130),       
    c(50, 150)  
  )
  
  # Create the map with constraints
  map <- leaflet(demographic_sum, 
                 options = leafletOptions(
                   minZoom = 1,
                   maxZoom = 8,
                   maxBounds = maxBounds,
                   maxBoundsViscosity = 1.0  # Makes the bounds "sticky"
                 )) %>%
    # Set initial view to center on Africa
    setView(lng = 30, lat = 5, zoom = 2) %>%
    
    # Add base map
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    # Add circle markers
    addCircleMarkers(
      ~lon, ~lat,
      radius = ~radius_scale(Count),
      fillColor = ~count_pal(Count),
      color = "white",
      weight = 1,
      opacity = 1,
      fillOpacity = 0.7,
      popup = ~lapply(1:nrow(demographic_sum), function(i) {
        HTML(create_popup(demographic_sum[i,]))
      })
    ) %>%
    
    # Add legend for count
    addLegend(
      position = "bottomright",
      pal = count_pal,
      values = ~Count,
      title = "Number of Cases",
      opacity = 0.7
    )
  
  return(map)
}
