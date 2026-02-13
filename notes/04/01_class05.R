## ------------------------------------------------------
## 01_CLASS05.R - R Script for Lecture 05

# version: 1.1
# Author: Bruno Conte Leite @2025-26
# bruno.conte@bse.eu

## ------------------------------------------------------

library(sf) # simple features' library
library(spData) # library of spatial datasets
library(tidyverse) # dplyr, ggplot, ...
library(terra)

# 1. RASTER BASICS ----

# We will work mostly with SpatRaster 
# objects from the terra package

# 1.1 Creating and manipulating a raster:

# Empty raster:
raster <- rast()
raster
plot(raster)

# Assigning values to it:
values(raster) <- 1:ncell(raster) # sequential
plot(raster)
values(raster) <- runif(ncell(raster)) # random unif dist
plot(raster)

# Assigning resolutions:
raster <- rast()
res(raster) <- 10 # lower resolution
values(raster) <- 1:ncell(raster) # sequential
plot(raster)

# Assigining extent:
raster <- rast()
# (min long, min lat, max long, max lat)
ext(raster) <- c(-74, -34, -35, 6) # manually cropping
raster # note the adjustment of resolution; decreases
res(raster) <- 1 # restore our o.g. resolution
raster

values(raster) <- 1:ncell(raster)
plot(raster)
plot(world %>% filter(name_long=='Brazil'),add=T)

# Changing projection/CRS:
r.reprojected <- project(raster, "EPSG:2169")
r.reprojected

rm(list = ls())

# ----

# 2. RASTER OPERATIONS: ----

# 2.1. Cropping:

r <- rast(system.file("ex/elev.tif", package = "terra")) # raster data from terra package

# Creating a sf polygon:
pol <- rbind(c(-1,1), c(-1,-.5), c(1,-1), c(0.5,1), c(-1,1))
pol <-st_polygon(list(pol))
pol <- st_sfc(pol,crs = 'EPSG:4326')
pol <- st_sf(geometry = pol)

# plotting
plot(r)
plot(pol,add=T)

# Cropping:
raster <- rast()
values(raster) <- 1:ncell(raster)
r.crop <- crop(raster, pol)

# Compare them:
r
r.crop

plot(r.crop)
plot(pol,add=T)

# 2.2. Vectorization:

# Raster to points:
r.points <- as.points(r) %>% 
  st_as_sf()
r.points # watch out with the 'layer' column!

r.points <- as.points(r) %>% 
  st_as_sf()

# Plotting it:
plot(r)
plot(r.points,add=T)

# Raster to polygons:
# VERY USEFUL FOR CREATING A GRID!

r.pol <- as.polygons(r) %>% 
  st_as_sf() 
r.pol

# Visualize:
plot(r)
plot(r.pol,add=T)

# ----

# 3. HANDS-ON ----
rm(list=ls())

# 3.1. Vector cropping and vectorizing:

sf.italy <- world %>% 
  filter(name_long == "Italy")

r <- rast()
values(r) <- 1:ncell(r)

# crop only italy area of world
r.crop <- crop(r, sf.italy)

# sanity check
plot(r.crop)
plot(st_geometry(sf.italy), add=T, # use st_geometry to only plot geom col of sf object (sfc)
     col = alpha("gray", 0.4))

# lets adjust the r.crop manually; it seems to cut off parts of italy
# first: check the bounds of our current r.crop object
r.crop

# define a new, custom crop window
r.crop2 <- rast()
ext(r.crop2) <- c(5, 20, 35, 49) # manually cropping
r.crop2  # notice res drops -- need to manually readjust
res(r.crop2) <- 1
# assign vals
values(r.crop2) <- 1:ncell(r.crop2)

# sanity check
plot(r.crop2)
plot(st_geometry(sf.italy), add=T, 
     col = alpha("gray", 0.4))

# create a grid of polygons in this area
r.pol <- as.polygons(r.crop2) %>% 
  st_as_sf() 

# intersection of the gridcells with the "italy" object
# note: both are `sf` objects, so we can use st_intersection

test2 <- st_intersection(r.pol, sf.italy) # cuts off all pts of the grids that dont intersect
plot(r.crop)
plot(test2, add=T) 

# correct way (?) keeps the whole grid that intersects with any pt of our sf object
test3 <- st_filter(r.pol, sf.italy, .predicate = st_intersects) # uses
plot(r.crop)
plot(test3, add=T) 

# Create an 1x1 grid covering Italy:

# now we can plot: cropped raster, italy, and grid overlaying italy
plot(r.crop2)
plot(sf.italy, add=T, 
     col = alpha("salmon", 0.5), lwd=2)
plot(test3, add=T, col=alpha("gray", 0.25), lwd=1.5)



# ----























# 
# 
