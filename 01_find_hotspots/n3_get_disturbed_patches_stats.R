
# ---------------------------------------------------------
# Get overview of disturbance sizes in Bavaria: 2018-2020
# ---------------------------------------------------------

# How many patches?
# what is their size??

# based on full disturbance extent: 1986:2020
rm(list = ls())
gc()

# Packages ----------------------------------------------------------------

library(tidyverse)
library(raster)
library(lubridate)
library(sf)
library(patchwork)
library(fasterize)
library(landscapemetrics)
library(landscapetools)

# Read my paths -----------------------------------------------------------
source('myPaths.R')

# Calculate the statistics on grid 12 or overall for Bavaria??


# Read files --------------------------------------------------------------
grid    <- read_sf(paste(myPath, 'outSpatial', "grid_12.shp", sep = '/'))


# Get rasters
disturbance <- raster(paste(myPath, inFolder, "disturbance_map_bavaria.tif", sep = "/"))
forest      <- raster(paste(myPath, inFolder, "forestcover_germany.tif", sep = "/"))


# bavaria extent:
# Get Bavaria data ----------------------
bav.shp <- st_read(paste(myPath, inFolder, "outline_bavaria.gpkg", sep = "/"), 
                   layer = 'outline_bavaria') # read watershed


# Simplify polygon to speed up calculation:
bav.simple <- st_simplify(bav.shp, preserveTopology = FALSE, dTolerance = 1500)


# Make sure they have the same projection
st_crs(bav.simple) <- st_crs(bav.shp)

# Crop disturbance raster, snap to near pixel
dist_bav <- crop(disturbance, bav.simple, snap="near")

plot(dist_bav)
# get extend and set correct projectipn
#ext <- as(extent(disturbance), 'SpatialPolygons')
#ext <- st_as_sf(ext)
#st_crs(ext) <- st_crs(grid)



# use new forest raster from Cipernicus - from 2018:
# how to handle virtual raster in R?


# possible: loop oevr hexagon; extract each polygon and extract 
# it from teh disturbance maps;
# calculate landscape characteristics for every disturbance gap








# Get some landscape level statistics for disturbance raster:
# ------------------------------------------------------

# calculate statistics for landscape classes (disturbance years):

# Get overview: 
# https://r-spatialecology.github.io/landscapemetrics/

# instecpct landscape
show_landscape(landscape, discrete = TRUE)
landscape  # ratsre object

# inspect disturbanc raster: no, to slow
# show_landscape(disturbance, discrete = TRUE)

# Calculate all metrics at patch level:
calculate_lsm(landscape, level = "patch") %>% 
  group_by(class) %>% 
  summarize(sum(value), n())


calculate_lsm(landscape, level='patch') %>% 
  filter(metric == 'area') %>% 
  print(n = 30) #
  distinct(metric)
  
  
calculate_lsm(landscape, level = 'class')


# Calculate how many pathces I have ?
lsm_c_area_mn(landscape, directions = 8) # get mean patch size by group

lsm_p_area(landscape, directions = 8)




# Create my raster to understand the measures
r <- raster(ncol = 10, nrow = 10)
#projection(r) <- "+proj=utm +zone=48 +datum=WGS84"

values(r) <- c(1,1,1,1,1,1,1,1,1,1,
               1,1,NA,1,1,1,1,2,1,1,
               1,1,NA,1,1,1,1,2,1,1,
               1,1,1,1,1,1,1,1,1,1,
               1,1,1,1,1,1,1,1,1,1,
               1,1,1,2,1,1,1,1,1,1,
               1,1,1,1,1,1,1,1,1,1,
               1,1,3,1,1,1,1,3,1,1,
               1,3,1,1,1,1,1,3,1,1,
               1,1,1,1,1,1,1,1,1,1)

#res(r) <-30

windows()
show_landscape(r, discrete = TRUE)

lsm_p_area(r, directions = 8)  # the final values are in hectares

# check for disturbance raster
d_patch <- lsm_p_area(disturbance, directions = 8)


# how many patches I have every year?
d_patch %>% 
  group_by(class) %>% 
  tally(id)  # id values is per patch, not within the class!

# Get basic statistics about mean disturbance patches in bavaria:
# important to get how many patches we might exclude given the minimal study size?

qntils = c(0,0.5, 0.75,0.80, 1)

d_patch %>% 
  group_by(class) %>%
  filter(class>2017) %>% 
  summarize(#patch_n = n(),
            mean_size = mean(value),
            #min_size = min(value),
           # max_size = max(value),
            #sum_dist = sum(value),
            #median_p = median(value),
            qs = quantile(value, qntils),
            prob = qntils)  %>%
  pivot_wider(names_from = prob, values_from = qs ) 

library(stringr)
require(sjPlot)
  ggplot(aes(x = class,
             y = sum_dist)) +
  geom_line()


# understand teh metrics:
calculate_lsm(r, level='patch')
check_landscape(r)

# get patches:

patches <- get_patches( landscape, 
                        class = "all", 
                        directions = 8, 
                        to_disk = getOption("to_disk", default = FALSE), return_raster = TRUE )

show_landscape(patches[[1]])


# Identify how many patches I have by year? what is their size??
patches <- get_patches( disturbance, 
                        class = 2020, 
                        directions = 8, # 4 rook neighborhood, 8 queen nbrh
                        to_disk = getOption("to_disk", default = FALSE), 
                        return_raster = TRUE )

