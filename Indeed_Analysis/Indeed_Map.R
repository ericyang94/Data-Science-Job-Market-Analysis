library(ggmap)
library(tidyverse)
library(leaflet)
library(maptools)
library(rgdal)
library(lubridate)
library(viridis)
library(sf)

# connect to Google api
myKey <- 'key_here'
register_google(key = myKey, account_type = "standard", day_limit = 2500)

listings <- read.csv('Indeed_Analysis/listings_unitedstates_map.csv')

# get longitude and latitude for each location
listings <- listings %>%
  mutate(coordinates = geocode(Location), lon = coordinates[[1]], lat = coordinates[[2]])

x <- revgeocode(c(-114,39.7))
 
str_remove(str_remove(str_extract(x,", .*, [A-Z]{2} [0-9]{5}"),", [A-Z]{2} [0-9]{5}"),", ")

# get city and state for each coordinate
for(i in 1:length(listings$lon)){
  listings$address[i] = revgeocode(c(listings$lon[i],listings$lat[i]))
  listings$state[i] = substr(str_extract(listings$address[i],"[A-Z]{2} [0-9]{5}"),1,2)
  listings$city[i] = str_remove(str_remove(str_extract(listings$address[i],", .*, [A-Z]{2} [0-9]{5}"),", [A-Z]{2} [0-9]{5}"),", ")
}

# remove NA values
listings <- listings %>%
  filter(!is.na(state) & !is.na(city))

# save new data
write.csv(listings,'Indeed_Analysis/listings_unitedstates_map.csv')

# plot lon/lat coordinates on US map using ggmap
us_map <- get_map(location='united states', zoom=4, source='google',color='bw')

ggmap(us_map) +
  geom_point(data = listings, aes(x = lon, y = lat, color = state), size = 2, alpha = 0.3) +
  labs(title = "Data Science Job Listing Locations") +
  theme(legend.position = "none")


# create leaflet interactive map for US

# make borders using shapefile and get US map coordinates
areas <- readOGR('Indeed_Analysis/us_state')
shp <- spTransform(areas, CRS("+proj=longlat +datum=WGS84"))
us_coordinates <- geocode("United States")
us_lon <- us_coordinates[[1]]
us_lat <- us_coordinates[[2]]

# count jobs in each state
states <- listings %>%
  group_by(state) %>%
  summarize(count = n())

# make barchart
ggplot(states, aes(x = reorder(state,-count), y = count)) +
  geom_bar(stat = "identity", aes(fill = state)) + 
  labs(x = "State", y = "Job Listings", title = "Job Listings in Each State") +
  theme_light() +
  theme(legend.position = "none")

# add count data into shapefile
shp@data <- left_join(shp@data,states, by = c('STUSPS'='state'))

# define color palette
bins <- c(1, 5, 10, 20, 50, 100, 200, 300)
pal <- colorBin("viridis", domain = shp@data$count, bins = bins)

# define labels
labels <- sprintf("<strong>State: %s</strong><br/>Count: %g", shp@data$NAME, shp@data$count) %>% 
  lapply(htmltools::HTML)

# generate map
leaflet(shp) %>% 
  addTiles() %>% 
  setView(lat=us_lat, lng=us_lon,zoom = 4) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=shp,
              weight=1,
              fillColor = ~pal(count),
              fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE),
              label=~labels) %>%
  addLegend(pal = pal, 
            values = ~count,
            opacity = 0.5, 
            title = "Job Listings",
            position = "bottomleft")


# create leaflet interactive map with cities

# count jobs in each city
cities <- listings %>%
  group_by(city) %>%
  summarize(count = n())

cities <- cities %>%
  mutate(coordinates = geocode(paste0(city,", USA")), lon = coordinates[[1]], lat = coordinates[[2]])

write.csv(cities,'Indeed_Analysis/listings_unitedstates_map_2.csv')

# define color palette
bins <- c(1, 5, 10, 20, 40, 60)
pal <- colorBin("viridis", domain = cities$count, bins = bins)

# define labels
labels <- sprintf("<strong>City: %s</strong><br/>Count: %g", cities$city, cities$count) %>% 
  lapply(htmltools::HTML)

# generate map
leaflet(shp) %>% 
  addTiles() %>% 
  setView(lat=us_lat, lng=us_lon,zoom = 4) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=shp,
              weight=1,
              fillColor = "gray",
              fillOpacity = 0.1) %>%
  addCircles(data = cities, lng = ~lon, lat = ~lat, weight = 30, radius = sqrt(cities$count)*2, label=~labels, color = ~pal(count)) %>%
  addLegend(pal = pal, 
            values = ~count,
            opacity = 0.5, 
            title = "Job Listings in California",
            position = "bottomleft")

