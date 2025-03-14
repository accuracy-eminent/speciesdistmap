library(shiny)

source('speciesdistmap.R')

ui <- fluidPage(
  p(),
  titlePanel("Species Distribution Model"),
  textInput("species_name","Species name","Taraxacum officinale"),
  numericInput("lat","Location latitude", 35.00, min=-90, max=90),
  numericInput("lon","Location longitude", -80.00, min=-180, max=180),
  actionButton("calc","Calculate distribution"),
  textOutput("mean_suitability"),
  textOutput("max_suitability"),
  tableOutput("table"),
  plotOutput("plot")
)

server <- function(input, output, session){
  create_map <- function(){
    # Get species distribution data and location data
    clim_species_data <- clim_and_species(clim_data, input$species_name)
    clim_data_sample <- clim_data %>% sample_n(100)
    loc_data <- clim_data %>% 
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
      mutate(species_mean=mean(clim_species_data[[name]])) %>%
      mutate(species_sd=sd(clim_species_data[[name]])) %>%
      mutate(world_mean=mean(clim_data_sample[[name]])) %>%
      mutate(world_sd=sd(clim_data_sample[[name]])) %>%
      mutate(var_importance=world_sd/species_sd) %>%
      ungroup() %>%
      mutate(z=(value - species_mean)/species_sd) %>%
      select(name, value, species_mean, species_sd, world_mean, world_sd, var_importance, z) %>%
      rename(var_name=name, loc_value=value, z_score=z)
    output$table <- renderTable(suitability_df, striped=TRUE)
    # Show suitability summary
    output$mean_suitability <- renderText(sprintf("Mean absolute suitability deviation: %.2f", mean(abs(suitability_df$z_score))))
    output$max_suitability <- renderText(sprintf("Maximum absolute suitability deviation: %.2f",  mean(max(suitability_df$z_score))))
  }
  p <- observeEvent(input$calc, {create_map()}, ignoreNULL = FALSE)
}

shinyApp(ui = ui, server = server)