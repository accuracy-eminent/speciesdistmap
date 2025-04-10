library(shiny)
library(leaflet)

source('speciesdistmap.R')

selections <- c(
  "BIO1: Mean annual temperature"="bio_1",
  "BIO2: Mean Diurnal Range (Mean of monthly (max temp - min temp))"="bio_2", 
  "BIO3: Isothermality (BIO2/BIO7) (x100)"="bio_3", 
  "BIO4: Temperature Seasonality (standard deviation x100)"="bio_4",
  "BIO5: Max Temperature of Warmest Month"="bio_5", 
  "BIO6: Min Temperature of Coldest Month"="bio_6",
  "BIO7: Temperature Annual Range (BIO5-BIO6)"="bio_7",
  "BIO8: Mean Temperature of Wettest Quarter"="bio_8",
  "BIO9: Mean Temperature of Driest Quarter"="bio_9",
  "BIO10: Mean Temperature of Warmest Quarter"="bio_10",
  "BIO11: Mean Temperature of Coldest Quarter"="bio_11",
  "BIO12: Annual Precipitation"="bio_12",
  "BIO13: Precipitation of Wettest Month"="bio_13",
  "BIO14: Precipitation of Driest Month"="bio_14",
  "BIO15: Precipitation Seasonality (Coefficient of Variation)"="bio_15",
  "BIO16: Precipitation of Wettest Quarter"="bio_16",
  "BIO17: Precipitation of Driest Quarter"="bio_17",
  "BIO18: Precipitation of Warmest Quarter"="bio_18",
  "BIO19: Precipitation of Coldest Quarter"="bio_19",
  "BIO20: De Martonne Aridity Index (BIO12/(BIO1+10))"="bio_20")
ui <- fluidPage(
  p(),
  titlePanel("Species Distribution Model"),
  textInput("species_name","Species name","Taraxacum officinale"),
  numericInput("lat","Location latitude", 35.00, min=-90, max=90),
  numericInput("lon","Location longitude", -80.00, min=-180, max=180),
  checkboxGroupInput("variables",HTML('Variables to calculate distribution (descriptions and climate data from <a href="https://www.worldclim.org/data/bioclim.html">WorldClim</a>)'),
                     choices=selections,
                     selected=selections, width='100%'),
  actionButton("calc","Calculate distribution"),
  textOutput("mean_suitability"),
  textOutput("max_suitability"),
  textOutput("weighted_suitability"),
  textOutput("vars_in_range"),
  htmlOutput("line_break"),
  textOutput("vars_out_2d"),
  textOutput("vars_out_3d"),
  textOutput("vars_out_min_max"),
  leafletOutput("leaflet_map"),
  tableOutput("table"),
)


server <- function(input, output, session){
  create_map <- function(){
    output$line_break <- renderText("<br>")
    # Get species distribution data and location data
    clim_data_filtered <- clim_data %>% select(c("lon","lat", input$variables, "lat_round","lon_round"))
    clim_species_data <- clim_and_species(clim_data_filtered, input$species_name)
    clim_data_sample <- clim_data_filtered %>% sample_n(100)
    loc_data <- clim_data_filtered %>% 
      filter(
        lat_round==round_any(input$lat,  0.1667), 
        lon_round==round_any(input$lon, 0.1667)) %>%
      head(1) %>%
      tibble()
    # Calculate the Z scores for all bioclimatic variables at the location
    suitability_df <- loc_data %>%
      select(!matches(".*z$")) %>%
      pivot_longer(cols = starts_with("bio")) %>%
      rowwise() %>%
      mutate(species_min=min(clim_species_data[[name]])) %>%
      mutate(species_max=max(clim_species_data[[name]])) %>%
      mutate(species_mean=mean(clim_species_data[[name]])) %>%
      mutate(species_sd=sd(clim_species_data[[name]])) %>%
      mutate(world_mean=mean(clim_data_sample[[name]])) %>%
      mutate(world_sd=sd(clim_data_sample[[name]])) %>%
      mutate(var_importance=world_sd/species_sd) %>%
      mutate(in_min_max_range=(value >= species_min && value <= species_max)) %>%
      ungroup() %>%
      mutate(z=(value - species_mean)/species_sd) %>%
      select(name, value, species_min, species_max, species_mean, species_sd, world_mean, world_sd, var_importance, z, in_min_max_range) %>%
      rename(var_name=name, loc_value=value, z_score=z)
    output$table <- renderTable(suitability_df, striped=TRUE)
    # Show suitability map
    suitability_map <- calc_suitability_map(clim_data_filtered, suitability_df)
    r <- crop(suitability_map, ext(-180, 180, -89.5, 89.5))
    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r),
                        na.color = "transparent")
    pal <- colorBin(viridis(6), bins=c(0.25, 0.33, 0.5, 0.75, 10.0), na.color="transparent")
    output$leaflet_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addRasterImage(r, colors=pal, opacity=0.8) %>%
        addLegend(pal=pal, values=values(r), title="Suitability (1/[max Z-score])")
    })
    # Show suitability summary
    # Z-scores
    mean_s_z <- mean(abs(suitability_df$z_score))
    mean_s_p <- pnorm(mean_s_z, lower.tail=TRUE)
    output$mean_suitability <- renderText(sprintf("Mean abs Z-score of location climatic variables according to observed species distribution: z=%.2f, normal %%ile=%.2f%%", mean_s_z, mean_s_p*100))
    max_s_z <- mean(max(suitability_df$z_score))
    max_s_p <- pnorm(max_s_z, lower.tail=TRUE)
    output$max_suitability <- renderText(sprintf("Highest abs Z-score of climatic variables according to observed species distribution: z=%.2f, normal %%ile=%.2f%%",  max_s_z, max_s_p*100))
    weight_s_z <- (suitability_df %>% mutate(z_abs=abs(z_score)) %>% summarize(result=weighted.mean(z_abs, w=var_importance)))$result
    weight_s_p <- pnorm(weight_s_z, lower.tail=TRUE)
    output$weighted_suitability <- renderText(sprintf("Mean (weighted by importance) abs Z-score of location climatic variables according to observed species distribution: z=%.2f, normal %%ile=%.2f%%", weight_s_z, weight_s_p*100))
    # Number of variables in range
    vars_in_range <- suitability_df %>% filter(in_min_max_range == TRUE) %>% nrow()
    total_vars <- suitability_df %>% nrow()
    output$vars_in_range <- renderText(sprintf("Number of climatic variables at location in observed species range: %d of %d", vars_in_range, total_vars))
    # Variables less than N standard deviations away
    clim_var_names <- suitability_df %>% filter(abs(z_score) > 2)
    txt1 <- if(clim_var_names %>% nrow() > 0) reduce(clim_var_names %>% pull(var_name), sprintf, fmt="%s,%s") else "None"
    output$vars_out_2d <- renderText(sprintf("Climatic variables at location more than 2 standard deviations away from species mean: %s", txt1))
    clim_var_names <- suitability_df %>% filter(abs(z_score) > 3)
    txt2 <- if(clim_var_names %>% nrow() > 0) reduce(clim_var_names %>% pull(var_name), sprintf, fmt="%s,%s") else "None"
    output$vars_out_3d <- renderText(sprintf("Climatic variables at location more than 3 standard deviations away from species mean: %s", txt2))
    # Variables outside min/max range
    clim_var_names <- suitability_df %>% filter(in_min_max_range == FALSE)
    txt_out <- if(clim_var_names %>% nrow() > 0) reduce(clim_var_names %>% pull(var_name), sprintf, fmt="%s,%s") else "None"
    output$vars_out_min_max <- renderText(sprintf("Climatic variables outside of observed species range of sample: %s", txt_out))
  }
  p <- observeEvent(input$calc, {create_map()}, ignoreNULL = FALSE)
}

shinyApp(ui = ui, server = server)