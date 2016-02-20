library(dplyr)
library(leaflet)
library(readr)


# Get the data
kenya_coords  <- read_delim( 'data/KenyaEnhanced.csv', delim=',' ) %>% select( SbjNum, Latitude, Longitude )
uganda_coords <- read_delim( 'data/UgandaEnhanced.csv', delim=',' ) %>% select( SbjNum, Latitude, Longitude )

# join the data, filtering out missing values
mapping_data <- rbind( kenya_coords, uganda_coords ) %>% filter( !is.na( Latitude ) )

# create map
cfd_map <- leaflet() %>% 
              addTiles() %>% 
              setView( mapping_data$Longitude[1], mapping_data$Latitude[1], zoom = 5 ) %>% 
              addCircles( data=mapping_data
                          ,lng = ~Longitude
                          ,lat = ~Latitude
                          ,popup = mapping_data$SbjNum )
