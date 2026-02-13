## ------------------------------------------------------
## 01_CLASS06.R - R Script for Lecture 06

# version: 1.1
# Author: Bruno Conte Leite @2025-26
# bruno.conte@bse.eu

## ------------------------------------------------------

library(sf) # simple features' library
library(spData) # library of spatial datasets
library(terra)
# library(tidyverse) # dplyr, ggplot, ...


# 1. RASTER BASICS ----

# 1.1. Loading external files:

# Most usual: *.tif files
# Download from NOAA VIIS here:
# https://www.ngdc.noaa.gov/eog/dmsp/downloadV4composites.html
# or here: https://www.dropbox.com/scl/fi/yu7b30oxiypy8q7o8312d/F101992.v4b_web.stable_lights.avg_vis.tif?rlkey=ysegpnnns8oz8khnykhc1ebkz&dl=1

r.nightlights <- rast('direction-to-the-directory-and-tif-file')
r.nightlights
inMemory(r.nightlights) # note: not loaded in memory!

# Plotting it:
plot(r.nightlights)
plot(st_geometry(world),add=T)

# Also normal: NetCDF files (*.nc):
# Download here:
# https://digital.csic.es/handle/10261/268088
# or here: https://www.dropbox.com/scl/fi/kmo2gj0iqvu52rat9ydmt/spei01.nc?rlkey=1ahg7k4cs1uf1v52s5x6d0sxj&dl=1

r.spei <- rast('../../Research/Data/gis data/spei/spei01.nc')
r.spei # note the multilayer!

# Calling specific layers:
r.spei[[1]] # double bracket!

r.spei.layer <- r.spei[[1200]]
plot(r.spei.layer)
plot(st_geometry(world),add=T)

rm(list = ls())

# ----

# 2. VECTOR-RASTER OPERATIONS ----

# 2.1. Creating a raster and an sf object:

# Loading in tif format:
raster <- rast(system.file("raster/elev.tif", package = "spData")) # raster data from spData package
# Creating a sf polygon:
pol <- rbind(c(-1,1), c(-1,-.5), c(1,-1), c(0.5,1), c(-1,1))
pol <-st_polygon(list(pol))
pol <- st_sfc(pol,crs = 'EPSG:4326')
pol <- st_sf(geometry = pol)

# 2.2. Extracting:

# Works only between SpatRasters and
# SpatVectors (vector data on terra format)

# Getting all points from the polygon:
r.points <- st_cast(pol,'POINT')

plot(raster)
plot(r.points,add=T)

# Extracting:
extract(raster,r.points) # note: it transforms sf into a SpatVector: vect()

# Storing it:
r.extract <- extract(raster,r.points)
r.extract # it is a dataframe!

# Adding to sf:
r.points <- r.points %>% 
  mutate(elevation := r.extract$elev)

ggplot() +
  geom_sf(data = r.points, aes(color=elevation)) +
  scale_color_distiller(palette = 'Spectral') +
  theme_bw()

# What if the sf is a line or polygon?
terra::extract(raster,pol)
# Compare with points:
terra::extract(raster,r.points)
# Note the different size!

# 2.3. Zonal statistics:

# Efficiently done with exactextractr package!
library(exactextractr)

r.zonal <- exact_extract(
  x = raster,
  y = pol,
  fun = 'mean')
r.zonal # a number: the average elevation within pol!

# Multi-statitics:
r.zonal <- exact_extract(
  x = raster,
  y = pol,
  fun = c('mean', 'min','max'))
r.zonal

# What if multi-feature?

pol.2 <- rast() %>%
  crop(raster) %>% 
  as.polygons() %>% 
  st_as_sf() %>%
  st_filter(pol)

plot(pol.2)
plot(raster,add=T)
plot(pol.2,add=T)

r.zonal <- exact_extract(
  x = raster,
  y = pol.2,
  fun = c('mean', 'min','max'))
r.zonal

# Adding it to the sf polygon:

pol.2 <- cbind(pol.2,r.zonal)

ggplot() +
  geom_sf(data = pol.2,aes(fill=mean))

# 2.4. Rasterization:

# Let us experiment with the
# Seine river basin:
sf.river <- seine

# To rasterize, one needs a
# 'raster template'. Let us
# create it over the river
# extent:
r.template <- rast() %>% 
  crop(sf.river) # why error? Watch out the projections!

sf.river <- seine %>% 
  st_transform('EPSG:4326')
sf.river

r.template <- rast() %>% 
  crop(sf.river)
r.template

sf.river.rast <- rasterize(sf.river,r.template)
# why error? one needs the sf to be a SpatVector!

sf.river.rast <- rasterize(vect(sf.river),r.template)
plot(sf.river.rast)
# what is the meaning of it? low raster resolution!

# increase resolution:
res(r.template) <- .05
r.template

sf.river.rast <- rasterize(vect(sf.river),r.template)
plot(sf.river.rast)

# 2.5. Distance over raster (friction surface):

# One can use the gdistance package:
library(gdistance)

# Points to calculate distance from:

sf.points <- st_point_on_surface(sf.river)

# Plotting it:
plot(st_geometry(sf.river))
plot(st_geometry(sf.points),add=T,pch=20)

# Euclidean distances - st_gistance
st_distance(sf.points)

# Distance over the (rasterized) rivers:

# First, transform the raster into a transition
# matrix. For that, replace missing values:

vv<-values(sf.river.rast)
vv[is.nan(vv)] <- 1/100 # 100x less likely to cross
values(sf.river.rast) <- vv
rm(vv)
plot(sf.river.rast)

# Comment here: different versions of gdistance might
# require the opposite - higher values in high-cost pixels.
# Double check the output in your applications!

tr.matrix <- transition(
  x = sf.river.rast, # why error? needs a raster() object!
  transitionFunction = mean,
  directions = 8
  )

# Using the (old) raster library for that:
sf.raster <- raster::raster(sf.river.rast)
sf.raster
rm(sf.raster)

tr.matrix <- transition(
  x = raster::raster(sf.river.rast),
  transitionFunction = mean,
  directions = 8
)
# Geocorrection for diagonals:
tr.matrix <- geoCorrection(tr.matrix, type = "c")

# Distance between points 1 and 3:
sp.point.1 <- sf.points %>% slice(1) %>% st_coordinates()
sp.point.2 <- sf.points %>% slice(3) %>% st_coordinates()

sp.distance <- shortestPath(
  x = tr.matrix,
  origin = sp.point.1,
  goal = sp.point.2,
  output = "SpatialLines"
)

# Back to sf:
sf.distance <- st_as_sf(sp.distance)

# Plotting it:
plot(sf.river.rast)
# plot(st_geometry(sf.river),add=T)
plot(st_geometry(sf.points[1,]),pch=20,add=T)
plot(st_geometry(sf.points[3,]),pch=20,add=T)
plot(sp.distance,add=T)

# Retrieving distance with st_length():
st_length(sf.distance)
st_distance(sf.points)[1,3] # compare with Euclidean!

# # Plotting it:
# plot(sf.river.rast)
# plot(st_geometry(sf.river),add=T)
# plot(sf.points,add=T)
# plot(sp.distance,add=T,col='red')

ggplot() +
  geom_sf(data = sf.river) +
  geom_sf(data = sf.points) +
  geom_sf(data = sf.distance,color = 'red') +
  theme_bw()

# How to calculate a bilateral distance
# matrix? Simple idea: loop over all potential
# location pairs (CPU-demanding, though)!

# Empty matrix:
dist.mat.riv <- matrix(
  0,
  nrow = nrow(sf.points),
  ncol = nrow(sf.points)
  )

# Looping over origins-destinations:
for (i in 1:nrow(sf.points)) {
  
  for (j in 1:nrow(sf.points)) {
    
    # Creating the sp points:
    sp.point.1 <- sf.points %>% slice(i) %>% st_coordinates()
    sp.point.2 <- sf.points %>% slice(j) %>% st_coordinates()
    
    # Caluculating path:
    sp.distance <- shortestPath(
      x = tr.matrix,
      origin = sp.point.1,
      goal = sp.point.2,
      output = "SpatialLines"
    )
    
    # Extracting length:
    sf.distance <- st_as_sf(sp.distance)
    
    dist.mat.riv[i,j] <- st_length(sf.distance)
    
  }
  
}

# How does it compare with st_distance()?
dist.mat.riv
st_distance(sf.points)

# More (CPU-) efficient way of doing that:
dist.mat.riv.2 <- costDistance(
  tr.matrix,
  as(sf.points, "Spatial")
  ) %>% 
  as.matrix()

# Comparing them:
dist.mat.riv
dist.mat.riv.2

# This is a great (and much more efficient)
# option! A drawback: it does not yield the
# spatial objects of the network (i.e., the 
# line of the path).

# Final remark: there are many other ways
# of achieving this same task; see e.g., 
# terra::costDist, terra::distance, and others

# ----

# 3. HANDS-ON ----

library(terra)
library(sf)
library(spData)
library(exactextractr)
library(tidyverse)

# 3.1. SPEI index by year USA:

# load in data:
r.spei <- rast('notes/05/spei01.nc')
sf.states <- us_states

# combine states with the SPEI index



# Calculate average state SPEI for different periods (3-4 different years)



# 3.2. Distance between main towns in Spain:
# load in data:
sf.places <- st_read('notes/05/ne_10m_populated_places/ne_10m_populated_places.shp')
r.elevation <- rast('notes/05/MSR_50M/MSR_50M/MSR_50M.tif')
sf.spain <- world %>% 
  filter(name_long == "Spain") %>% 
  select(name_long, geom)

# filter for 10 most population places in Spain

sf.spain_pop10 <- sf.places %>% 
  filter(ADM0NAME == "Spain") %>% 
  arrange(desc(POP_MAX)) %>% 
  select(ADM0NAME, NAME_ES, geometry, POP_MAX) %>% 
  slice(1:10)

# crop elevation within Spain

r.spain <- rast() 
values(r.spain) <- 1:ncell(r.spain)
r.crop_spain <- crop(r.spain, sf.spain)
plot(r.crop_spain)
plot(st_geometry(sf.spain), add=T)

# adjust the bounds
r.spain <- rast()
values(r.spain) <- 1:ncell(r.spain)
ext(r.spain) <- c(-10, 4, 35, 44) # manually cropping


# visualize together



# What is the shortest distance over the geography
# between Madrid and Vigo?


# ----























# 
# 
