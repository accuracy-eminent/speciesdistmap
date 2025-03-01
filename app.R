library(shiny)

source('speciesdistmap.R')

ui <- fluidPage(
  p(),
  titlePanel("Species Distribution Model"),
  textInput("species_name","Species name","Taraxacum officinale"),
  numericInput("lat","Location latitude", 0.00, min=-90, max=90),
  numericInput("lon","Location longitude", 0.00, min=-180, max=180),
  actionButton("calc","Calculate distribution"),
  plotOutput("plot")
)

server <- function(input, output, session){
  create_map <- function(){
    clim_species_data <- clim_and_species(clim_data, input$species_name)
    combined_data <- merge_out %>% 
      pivot_longer(cols=starts_with("bio_"))
    output$plot <- renderPlot({
      ggplot(combined_data %>% filter(grepl("bio_[0-9]+$",name))) +
      geom_boxplot(aes(x=name,y=value)) +
      theme_bw()
    })
  }
  p <- observeEvent(input$calc, {create_map()}, ignoreNULL = FALSE)
}

shinyApp(ui = ui, server = server)