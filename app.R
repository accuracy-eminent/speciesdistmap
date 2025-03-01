library(shiny)

source('speciesdistmap.R')

ui <- fluidPage(
  p(),
  titlePanel("Species Distribution Model"),
  textInput("species_name","Species name","Taraxacum officinale"),
  actionButton("calc","Calculate distribution")
)

server <- function(input, output, session){
  create_map <- function(){
    clim_species_data <- clim_and_species(clim_data, input$species_name)
    print(clim_species_data %>% head())
  }
  p <- observeEvent(input$calc, {create_map()}, ignoreNULL = FALSE)
}

shinyApp(ui = ui, server = server)