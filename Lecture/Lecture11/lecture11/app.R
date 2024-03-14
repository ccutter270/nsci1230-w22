# LECTURE 10: Shiny - with US census data
# Wednesday, January 26, 2022

# ----------------------------------- CODE -----------------------------------



library(shiny)
library("tidyverse")
library("leaflet")
library("geojsonio")


# GOAL: Make a map of the US colored by population where the user can click
#  on a state ad county-level population pops up in a graph


census <- read_csv("Census Population 2020.csv")


# WRANGLE the DATA 

# Get state population data
state.pop <- census %>% 
  filter(COUNTY == 0)

# Load in state polygons
states <- geojson_read("https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_20m.json",
                       what = "sp") 

states.copy <- states

states.copy@data <- states@data %>%
  left_join(state.pop, by = c("NAME" = "STNAME"))


color <- colorNumeric(palette = "YlOrRd",
                      domain = states.copy@data$POPESTIMATE2020)



# lets pretend they clicked "vermont"








ui <- fluidPage(
  leafletOutput(outputId = "map"),
  plotOutput(outputId = "plot")
)

server <- function(input, output, session) {
  
  # MAKE THE MAP
  output$map <- renderLeaflet({ 

    print(input$map_shape_click$id)
    
    states.copy %>%
      leaflet() %>%
      addPolygons(color = "black",
                  opacity = 1,
                  fillColor = ~color(POPESTIMATE2020),
                  fillOpacity = 1,
                  stroke = 1,
                  weight = 1,
                  layerId = ~NAME) %>%
      setView(-96, 37.8, 3.5)
  })
  
  output$plot <- renderPlot({
  
    if(is.null(input$map_shape_click$id)){
      return()
    }
    census %>%
    filter(COUNTY != 0) %>%
    filter(STNAME == input$map_shape_click$id) %>%
    ggplot() + 
    geom_col(aes( y = reorder(CTYNAME, POPESTIMATE2020), x = POPESTIMATE2020),
             fill = "blue",
             color = "black") +
    labs( x = "Population Estimate in 2020",
          y = "County")
    
  })
  
  
  
  
  
  
  
  
  
}

shinyApp(ui, server)