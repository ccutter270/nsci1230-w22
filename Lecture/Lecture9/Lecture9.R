# LECTURE 9: MAPS 
# Tuesday, January 25, 2022

# ----------------------------------- CODE -----------------------------------


# PACKAGES 

#install.packages("")
library("tidyverse")
library("dplyr")
library("tidytext")
library("tidyr")
library("rvest")
library("rnaturalearth")


setwd("~/Desktop/NSCI Data Science/Lecture/Lecture9")


# ----------------------------- NEW TOOLS ----------------------------------

world <- ne_countries(returnclass = "sf")                      


# Make a map just like a ggplot graph 
world %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = pop_est)) +
  theme_bw()


# Let's label Vienna, Austria 
vienna.data <- tibble(longitude = 48.2082,
                      latitude = 16.3738)

world %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = pop_est)) +
  geom_point(data = vienna.data,
             mapping = aes(x = latitude, y = longitude),
             color = "red",
             size = 3) +
  theme_bw()



# The US Map 
library("usmap")

plot_usmap()


# AN ASIDE - mapping other things like the brain

library("ggseg")

ggplot() +
  geom_brain(atlas = dk)




#SCRAPING IN SOME ALCOHOL DATA

url <- "https://worldpopulationreview.com/state-rankings/alcohol-consumption-by-state"


alcohol.data <- url %>%
  read_html() %>%
  html_element(xpath  = '//*[@id="dataTable"]/div[1]/div/div[1]/div/div[1]/div[2]/table') %>%
  html_table() 

# Rename columns 
colnames(alcohol.data) <- c("state","Alcohol")

library(rayshader)
map1 <- plot_usmap(data = alcohol.data,
           values = "Alcohol") +
  scale_fill_viridis_c()  +
  labs(title = "Alcohol consumption in gallons",
       fill = "A")

plot_gg(map1)



# Interactive Maps!

library(leaflet)

vienna.data <- tibble(x = 48.2082,
                      y = 16.3738,
                      name = "Vienna")

vienna.data %>%
  leaflet() %>%
  addTiles() %>%
  addTiles() %>%
  addMarkers(lat = ~x,  # can do this without args, gets data set with coordinates to plot
             lng = ~y,  # must use squiggles for all args    
             label = ~name,
             popup = ~name)  # the popup could be like a website or something


# LETS SCRAPE IN REAL DATA  - taco bells in the US 

fast.food <- read_csv("Datafiniti_Fast_Food_Restaurants.csv")
# Lets cluseter our markers because this isnt useful 

fast.food %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())


# WE only want Taco Bells 


fast.food %>%
  filter(name == "Taco Bell") %>% 
  filter(province %in% c("VT", "NH", "MA", "ME", "CT", 'RI')) %>% 
  leaflet() %>%
  addTiles() %>%
  addMarkers(label = ~address)

# Let's make a choropleth (colored map), perhaps with some markers 

# Introduce .geojson -> standardized way to connect mapping data to a dataframe

library("geojsonio")

states <- geojson_read("https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_20m.json",
                       what = "sp") 


states %>%
  leaflet() %>%
  #addTiles() %>%
  addPolygons(color = "black") %>%
  setView(-96, 38, 3.5)


# REMAKE our alcohol map using leaflet


# Add alcohol to states data
# Each row is uniquely connected to one polygon, so we dont want to change # rows
# So we want to left join this 


# Two ways you don't want to save this 

# 1. DONT OVERWRITE FRAME, because you now don't have polygons anymore 
states <- states@data %>%
  left_join(alcohol.data, by = c("NAME" = "state"))

# 2. DONT OVERITE DATA FRAME, if you keep running it it does it again
states@data <- states@data %>%
  left_join(alcohol.data, by = c("NAME" = "state"))


# SAVE IT LIKE THIS, completely new 
states.copy <- states

states.copy@data <- states@data %>%
  left_join(alcohol.data, by = c("NAME" = "state"))


# NOW GRAPH

# HERE is the PROBLEM - it nees colors not variables 
# Ggplot sclaes by variables, so we need to use colorBin


# Discrete 
color1 <- colorBin(palette = "YlOrRd",
         domain = states.copy@data$Alcohol,
         bins = c(0,1,2,3,4,5))

# Continous 
color2 <- colorBin(palette = "YlOrRd",
                   domain = states.copy@data$Alcohol)


# Check Colors 
color1(states.copy@data$Alcohol)


# Color 1 
states.copy %>%
  leaflet() %>%
  addPolygons(color = "black",
              fillColor = ~color1(Alcohol)) %>%
  setView(-96, 38, 3.5)


# Color 2, with color adjustment 
states.copy %>%
  leaflet() %>%
  addPolygons(color = "black",
              fillColor = ~color2(Alcohol),
              fillOpacity = 1,
              opacity = 1,
              label = ~NAME) %>%
  setView(-96, 38, 3.5)



