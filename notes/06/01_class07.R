## ------------------------------------------------------
## 01_CLASS07.R - R Script for Lecture 07

# version: 1.1
# Author: Bruno Conte Leite @2024-25
# bruno.conte@bse.eu

## ------------------------------------------------------

library(sf) # simple features' library
library(spData) # library of spatial datasets
library(tidyverse) # dplyr, ggplot, ...
library(terra)
library(leaflet)

# 1. The MAP WIDGET ----

# Creates the widget; i.e., the interactive map window

# 1.1. Empty widget:

m <- leaflet() # empty widget
m # show it

# 1.2. Using data: either raw (coordinates)
# or sf objects:

# Important: for points, leaflet (apparently)
# does not work with MULTIPOINTS

# 1.2.1. Adding raw data (long/lat) as
# points with addCircles()
df.coords <- data.frame(
  rbind(c(3.2,4), c(3,4.6), c(3.8,4.4), c(3.5,3.8), c(3.4,3.6), c(3.9,4.5))
  )

# Leaflet with points using
# addCircles():
m <- leaflet() %>% 
  addCircles(
    data = df.coords,
    lng = ~X1,
    lat = ~X2
    )
m

# 1.2.2. Adding sf objects:

# Linestring with addPolylines():
lstring <- st_linestring(rbind(c(0,3),c(0,4),c(1,5),c(2,5)))
m <- leaflet() %>% 
  addPolylines(data = lstring)
m

# Polygons with addPolygons():
pol <-st_polygon(list(rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))))

m <- leaflet() %>% 
  addPolygons(data = pol)
m

# 1.2.3. Using "real data":

# Let us use house prices
# from spData::house

sf.house <- spData::house %>% 
  st_as_sf() %>% 
  sample_n(50) # quite large, random sample 50 obs

m <- leaflet() %>% 
  addCircles(data = sf.house)
m # what is wrong? PROJECTION!

# Reprojecting:
m <- leaflet() %>% 
  addCircles(data = sf.house %>% st_transform('EPSG:4326'))
m

# 2. BACKGROUND MAP, POPUS, AND LABELS: ----

# House data:
sf.house <- spData::house %>% 
  st_as_sf() %>% 
  st_transform('EPSG:4326') %>% 
  sample_n(50) # quite large, random sample 50 obs

# 2.1. Background map:
m <- leaflet() %>% 
  addCircles(data = sf.house) %>% 
  addTiles()
m

# Other backgrounds:
m <- leaflet() %>% 
  addCircles(data = sf.house) %>% 
  addProviderTiles(providers$CartoDB.Positron)
m

# 2.2. Adding popus/labels:

# Let us add the houses' year of
# building.

# It must be a character variable,
# check it:
class(sf.house$yrbuilt)
# Adjust it:
sf.house <- mutate(
  sf.house,
  yrbuilt2 := as.character(yrbuilt)
)
# Plotting it:
m <- leaflet() %>% 
  addMarkers(data = sf.house, popup = ~yrbuilt2) %>% 
  addTiles()
m

# The same holds for labels!

# 3. ADDING RASTERS: ----

# Source here:
# https://www.naturalearthdata.com/downloads/50m-raster-data/50m-manual-shaded-relief/

# Using the elevation raster
# and corpping it over Spain:
r.elev <- rast('sandbox/MSR_50M/MSR_50M.tif') %>% 
  crop(world %>% filter(iso_a2=='ES')) %>% 
  raster::raster() # making it a raster

m <- leaflet() %>% 
  setView(lng = -3.692731, lat = 40.298900, zoom = 5) %>%
  addTiles() %>% 
  addRasterImage(r.elev, opacity = 0.8)# opacity = transparency
m

# Adding a color palette:
pal <- colorNumeric(
  c("#0C2C84", "#41B6C4", "#FFFFCC"),
  values(r.elev),
  na.color = "transparent"
  )

m <- leaflet() %>% 
  setView(lng = -3.692731, lat = 40.298900, zoom = 5) %>%
  addTiles() %>% 
  addRasterImage(r.elev, opacity = 0.8, colors = pal)
m

# ----

# 4. SPATIAL APIs: ----

rm(list = ls())

library(httr)

# Example with https://geocode.maps.co/

# Make a free account at to have an API key!

# Example with UPF Ciutadella ('UPF%20Ciutadella')

# Launch the http request:
htt.req <- GET(
  url = 'https://geocode.maps.co/search?q=Barcelona&api_key=PUT_YOUR_API_KEY_HERE'
  )

# Did request work (200->YES)?
htt.req$status_code

# Retrieving content:
htt.content <- content(htt.req)
htt.content

# Structuring first result:
df.result <- data.frame(
  name = substr(htt.content[[1]]$display_name,1,15),
  longitude = htt.content[[1]]$lon,
  latitude = htt.content[[1]]$lat
)

# Making it a sf:
sf.result <- st_as_sf(
  df.result,
  coords = c('longitude', 'latitude'),
  crs = 'EPSG:4326'
)

# Visualizing it:
ggplot() +
  geom_sf(data = world %>% filter(iso_a2=='ES')) +
  geom_sf(data = sf.result, aes(color = name)) +
  theme_bw()

# ----

# For hands-on:

# Download boundaries of Barcelona here:
# https://opendata-ajuntament.barcelona.cat/data/dataset/a64ae340-d8f3-4fac-a9c8-50b08f9e1d9b/resource/98dc2539-af66-45b3-b4d2-30b4ba8af272/download






















# 
# 
