
# -------------------------------
# process disturbance raster
# -------------------------------

# read disturbance data (raster)
# get grid data (sf) 
# read forest data (raster)

# work with rasters intead of polygons to speed up process: e.g. convert grid into rasters

# Process: 
# - create unique grid ids for each pixel: convert the grid into raster
# - Merge data: link grids raster ID  with the values from the forest and 
# disturbances into dataframe, not more as spatial data
# get the statistics

# Outputs: 
# - classified hotspots based on sum mortality >2018
# - yearly mortality classified by grid


rm(list = ls())


# Libs
library(tidyverse)
library(raster)
library(lubridate)
library(sf)
library(patchwork)
library(fasterize)



# Read my paths -----------------------------------------------------------
source('myPaths.R')


# Get rasters -------------------------------------------------------------
disturbance <- raster(paste(myPath, inFolder, "disturbance_map_bavaria.tif", sep = "/"))
forest      <- raster(paste(myPath, inFolder, "forestcover_germany.tif", sep = "/"))



# Single grid example [do not run] -----------------------------------------------------

# Get grid
grid <- read_sf(paste(myPath, outSpatial, 'grid_50.shp', sep = '/'))


# Aggregate disturbances to grid

# get extend and set correct projectipn
ext <- as(extent(disturbance), 'SpatialPolygons')
ext <- st_as_sf(ext)
st_crs(ext) <- st_crs(grid)

# Create grid index for each pixel
grid_sel     <- st_intersection(st_as_sf(grid), st_as_sf(ext))
grid_sel_ras <- fasterize(grid_sel, disturbance, field = "FID") # name the new rasters as polygon ids
grid_values  <- values(grid_sel_ras)     # resolution is still 30 m, grid value for each pixel

# Crop the forest data to disturbance extend, snap to near pixel
forest_bav <- crop(forest, ext, snap="near")

#writeRaster(forest_bav, paste(myPath, inFolder, "forestcover_bav.tif", sep = "/"))

# check if values fit: 
length(values(forest_bav))
length(grid_values)
length(disturbance)



# Create dataframes containing grid id and disturbaance vs forest data
forest_df <-
  data.frame(gridindex = grid_values,
                     forest = values(forest_bav)) %>%
  group_by(gridindex) %>%
  summarize(forest_ha = sum(forest == 1, na.rm = TRUE) * 0.09,
            land_ha = n() * 0.09) %>%
  ungroup(.)


# Get in the same way the disturbance data aggregated by grid
dist_df <-
  data.frame(gridindex = grid_values,
             dist = values(disturbance)) %>%
  na.omit(.) %>%
 # filter(!is.na(disturbance)) %>%
  group_by(gridindex) %>%
  summarize(disturbance_ha = n() * 0.09) %>%
  ungroup(.)

# merge forest and disturbance data
out.df <- forest_df %>% 
   left_join(dist_df, by = "gridindex")


# Export files:
write_csv(out.df, paste(myPath, outTable, "grid_50.csv", sep = "/"))



# ------------------------------------------------------------------------------
# Loop over grids with different sizes: absolute values of disturbances: >=2018
# ------------------------------------------------------------------------------
# lower down: calculate hotspots for all years to get anomalies


# Keep only years from 2018, replace by NA
disturbance18 <- disturbance
values(disturbance18)[values(disturbance18) < 2018 ] <- NA


# Get all grids
grids <- list.files(paste(myPath, outSpatial, sep = "/"), 
                   pattern= "^grid.*shp$") 

grid.ls <- lapply(grids, function(name) {read_sf(paste(myPath, 'outSpatial', name, sep = '/'))})

# Add names
grids_names = gsub('.shp', '', grids)
#grids_names = as.numeric(gsub('grid_', '', grids_names))

# Paste names as new column to each df in a list
grid.ls2 <- map2(grid.ls, grids_names, ~cbind(.x, gridSize = .y))


# Merge disturbance values into individual grids ------------------------------
# outputs is a dataframe
aggregateToGrid <- function(grid, disturbance, ...) {
  
  outName = unique(grid$gridSize)
  
  # Aggregate disturbances to grid
  
  # get extend and set correct projectipn
  ext <- as(extent(disturbance), 'SpatialPolygons')
  ext <- st_as_sf(ext)
  st_crs(ext) <- st_crs(grid)
  
  # Create grid index for each pixel
  grid_sel     <- st_intersection(st_as_sf(grid), st_as_sf(ext))
  grid_sel_ras <- fasterize(grid_sel, disturbance, field = "FID") # name the new rasters as polygon ids
  grid_values  <- values(grid_sel_ras)     # resolution is still 30 m, grid value for each pixel
  
  # Create dataframes containing grid id and disturbaance vs forest data
  forest_df <-
    data.frame(gridindex = grid_values,
               forest = values(forest)) %>%
    group_by(gridindex) %>%
    summarize(forest_ha = sum(forest == 1, na.rm = TRUE) * 0.09,
              land_ha = n() * 0.09) %>%
    ungroup(.)
  
  
  # Get in the same way the disturbance data aggregated by grid
  dist_df <-
    data.frame(gridindex = grid_values,
               dist = values(disturbance)) %>%
    na.omit(.) %>%
    # filter(!is.na(disturbance)) %>%
    group_by(gridindex) %>%
    summarize(disturbance_ha = n() * 0.09) %>%
    ungroup(.)
  
  # merge forest and disturbance data
  out.df <- forest_df %>% 
    left_join(dist_df, by = "gridindex")
  
  return(out.df)
}

# Get data organized by grid, loop over grids
grids_dat <- lapply(grid.ls, function(grid) aggregateToGrid(grid, 
                                                            disturbance = disturbance18))


lapply(grids_dat, head)


# Merge data into one table
out.df <- do.call(rbind, ls2)


# --------------------------------
# Analyze the data: 
# -------------------------------
# get the quantiles by the grid size group
unique(out.df$gridSize)

out.df <- out.df %>% 
  mutate(gridSize = factor(out.df$gridSize,
                           levels =  as.character(sort(as.numeric(unique(out.df$gridSize))))))
                           

# Represent results using quantiles, as they are skewed?
qntils = c(0, 0.01, 0.25, 0.5, 0.75,0.99, 1)

# Get a table with quantiles
qs_tab_year_18 <- 
  out.df %>%  
  group_by(gridSize) %>%
  mutate(mort_rate = disturbance_ha/forest_ha *100) %>%  
  na.omit(mort_rate) %>% 
  summarise(qs_rate = quantile(mort_rate, 
                               qntils),
            qs_area = quantile(disturbance_ha , 
                               qntils),
            mean_rate = mean(mort_rate, rm.na = T),
            mean_area = mean(disturbance_ha, rm.na = T),
            prob = qntils) %>%
  mutate(quant_area_rate    = stringr::str_glue("{round(qs_rate,1)} ({round(qs_area,1)})"),
         Mean               = stringr::str_glue("{round(mean_rate,1)} ({round(mean_area,1)})")) %>% 
  dplyr::select(gridSize, Mean, prob, quant_area_rate) %>% 
  pivot_wider(names_from = prob, values_from = quant_area_rate )


# at level of 75%, there is 2.2% of mortality rate
  # grids of size 20-30 km are having stable results
  # https://stackoverflow.com/questions/35453942/r-quantile-by-groups-with-assignments
library(data.table)
fwrite(qs_tab_year_18, paste( myPath, outTable, 'quantiles_18.csv', sep = "/"))

# Classify the data above 75% quantile as 'high' mortality


# Get mortality rates in a table
out.df2 <- out.df %>%  
    group_by(gridSize) %>%
    mutate(mort_rate = disturbance_ha/forest_ha *100) #%>%  

# Ge classes by quantiles by groups : 4 = high mortality = mortality hotspots
setDT(out.df2)[,quantile := cut(mort_rate, 
                             quantile(mort_rate, probs = 0:4/4, na.rm = TRUE),
                             labels = FALSE, 
                             include.lowest = TRUE), by = gridSize]


# Export the final table
fwrite(out.df2, paste(myPath, outTable, 'hotspots_18_sum.csv', sep = "/"))




# -------------------------------------------------------------------
# -------           Loop to shp, merge values with grid              ------------------------
# -------------------------------------------------------------------


# Export as shp --------------------------------------------------

# look over grids
# loop over the dfs 
# merge them and export as shps

# make a whole new script, as for anomalies


mortality_to_grid <- function(grid, disturbance, ...) {
  
 # grid = grid.ls2[[6]]
  
  # Get out names
  outName = unique(grid$gridSize)
  #outMap = paste(myPath, 'map', paste0('mort18_', outName, '.png'), sep = "/")
  outSHP = paste(myPath, outSpatial, paste0('mort_', outName, '.shp'), sep = "/")
  
  
  # get extend and set correct projectipn
  ext <- as(extent(disturbance), 'SpatialPolygons')
  ext <- st_as_sf(ext)
  st_crs(ext) <- st_crs(grid)
  
  # Create grid index for each pixel
  grid_sel     <- st_intersection(st_as_sf(grid), st_as_sf(ext))
  grid_sel_ras <- fasterize(grid_sel, disturbance, field = "FID") # name the new rasters as polygon ids
  grid_values  <- values(grid_sel_ras)     # resolution is still 30 m, grid value for each pixel
  
  # Crop the forest data to disturbance extend, snap to near pixel
  forest_bav <- crop(forest, ext, snap="near")
  
  
  # Create dataframes containing grid id and disturbaance vs forest data
  forest_df <-
    data.frame(gridindex = grid_values,
               forest = values(forest_bav)) %>%
    group_by(gridindex) %>%
    summarize(forest_ha = sum(forest == 1, na.rm = TRUE) * 0.09,
              land_ha = n() * 0.09) %>%
    ungroup(.)
  
  
  # Get in the same way the disturbance data by year aggregated by grid
  dist_df <-
    data.frame(gridindex = grid_values,
               dist = values(disturbance)) %>%
    na.omit(.) %>%
    group_by(gridindex, dist) %>%   # 'dist' is the year of disturbanec here
    summarize(disturbance_ha = n() * 0.09) %>%
    ungroup(.) %>% 
    rename(year = dist)
  
  # merge forest and disturbance data
  out.df <- forest_df %>% 
    left_join(dist_df, by = "gridindex")
  
  
  # Classify data by quantiles:
  # Get mortality rates in a table
  out.df2 <- out.df %>%  
    #group_by(gridSize) %>%
    mutate(mort_rate = disturbance_ha/forest_ha *100) #%>%  
  
  
  # Get classes by quantiles by groups : 4 = high mortality = mortality hotspots
  setDT(out.df2)[,quantile := cut(mort_rate, 
                                  quantile(mort_rate, probs = 0:4/4, na.rm = TRUE),
                                  labels = FALSE, 
                                  include.lowest = TRUE)] # 
  
  
  # Export the table with quantile indication
  fwrite(out.df2, paste(myPath, outTable, paste('qtls', outName, '.csv'), sep = "/"))
  
  
  
  # merge qunatil mortaliy data with grid
  dat_grid <- grid %>%
    right_join(out.df2, by= c('FID' = "gridindex")) # right join to keep all grids
  
  
  # clip anomalies to the bavaria shp
  dat_grid_bav <- st_intersection(st_as_sf(dat_grid), 
                                  st_as_sf(bav.simple))
  
  # convert back to st shape
  dat_grid_bav2 <- st_collection_extract(dat_grid_bav, "POLYGON")

  # Export the final shp
  st_write(dat_grid_bav2, outSHP, delete_layer = TRUE, overwrite=TRUE )
  
}


# test on one file ----------------------------------------------------------
mortality_to_grid(grid.ls2[[6]], disturbance = disturbance18)


# Loop over all grid sizes --------------------------------------------------
lapply(grid.ls2, function(grid) mortality_to_grid(grid, disturbance = disturbance18))





