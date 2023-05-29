# Get geo characteristics

# for paper
rm(list = ls())


# Read my paths -----------------------------------------------------------
source('myPaths.R')


# Read libs  --------------------------------------------------------------

library(dplyr)
library(data.table)
library(tidyr)
library(rgdal)
library(raster)
library(tidyverse)
library(lubridate)
library(patchwork)
library(fasterize)
library(ggpubr)
library(terra)
library(R.utils)

# Get spatial data for each site:
library(sf)
library(elevatr)  # get elevation

xy        <- vect('C:/Users/ge45lep/Documents/2021_Franconia_mortality/03_plot_sampling/out_fieldData/new_GPS/sites_final_updPassau.shp') # read trap location
# Convert data to DWD coordinate system:
xy2 <- terra::project(xy, "EPSG:3035")  # coordinate system for Europe in m

prj_dd <- "EPSG:3035"

# convert spatVect to sf object to use elevatr library
xy2_sf <- sf::st_as_sf(xy2)

# get elevation raster for Bavaria
elev<- get_elev_raster(xy2_sf, z = 12) # ground resolution in meters: https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution

windows()
plot(elev)
plot(xy2_sf, add = TRUE)

# get elevation
v_elev      <- terra::extract(elev, xy2_sf)

range(v_elev)
#[1] 128 971 m


# Get mean distance beween patches per site 
xy_3035 <- terra::project(xy, "EPSG:3035")  # coordinate system for Europe in m

# https://stackoverflow.com/questions/44187812/5-nearest-neighbors-based-on-given-distance-in-r


xy_3035_df <- as.data.frame(xy_3035, geom = 'WKT') # add XY coordinates

# get coordinates as two separate columns: use sf object
xy2_df <- st_coordinates(xy2_sf)

# Load packages
library(FNN)
library(tidyverse)
library(data.table)

# Create example data frame
dataset <- fread("id    x         y       age
1  1745353   930284.1    30
2  1745317   930343.4    23
3  1745201   930433.9    10
4  1745351   930309.4    5
5  1745342   930335.2    2
6  1746619   929969.7    66
7  1746465   929827.1    7
8  1746731   928779.5    55
9  1746629   929902.6    26
10 1745938   928923.2    22")

# Calculate the nearest ID and distance
near_data <- get.knn(xy2_df[, 1:2], k = 2)

# Extract the nearest ID
nn_index <- as.data.frame(near_data$nn.index)

# Extract the nearest Distance for the closest 2 neighbors
nn_dist <- as.data.frame(near_data$nn.dist)

# get mean:
mean(unlist(nn_dist)) # 372.0173 m
sd(unlist(nn_dist))   # 452.1834 m
