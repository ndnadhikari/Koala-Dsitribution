source("global.R")

## shiny server
bbox    <- st_bbox(df_states) # Get the bounding box equivalent to our data with states
server <- function(input, output) {
  output$plot <- renderPlot({
    # Filter data based on selected state
    filtered_data <- df_with_states %>%
      filter(states == input$state)
    
    
    # Plot based on selected variable
    if (input$variable == "temp") {
      ggplot() +
        geom_sf(data = df_states %>% dplyr::select("states","geometry"),  fill = NA) +
        geom_point(data = filtered_data, aes(x = long, y = lat, color = temp/10), size = 1) +
        theme_minimal() +
        coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax)) +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              panel.grid = element_blank()) +
        labs(color = "Temperature (C)") +
        scale_color_gradient(low = "blue", high = "red")
    } else if (input$variable == "precip") {
      ggplot() +
        geom_sf(data = df_states %>% dplyr::select("states","geometry"),  fill = NA) +
        geom_point(data = filtered_data, aes(x = long, y = lat, color = precip), size = 1) +
        theme_minimal() +
        coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax)) +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              panel.grid = element_blank()) +
        labs(color = "Precipitation (mm)") +
        scale_color_gradient(low = "blue", high = "red")
    }
  })
}
