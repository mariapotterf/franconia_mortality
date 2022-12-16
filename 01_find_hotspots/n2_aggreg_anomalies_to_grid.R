
# calculate anomalies in mortality rates

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


# Read my paths -----------------------------------------------------------
source('myPaths.R')

# ------------------
# Create final maps 
# ------------------

# grid: 12 km
# calculate anomalies,
# make maps: anomalies - binary maps, for each years and overlayed
# > 300% of disturbance mortality per year



#C:\Users\ge45lep\Documents\2021_Franconia_mortality\01_find_hotspots\outSpatial
# Read files --------------------------------------------------------------
grid    <- read_sf(paste(myPath, '01_find_hotspots/outSpatial', "grid_12.shp", sep = '/'))


# Get rasters
disturbance <- raster(paste(myPath, inFolder, "geodata/disturbance_map_bavaria.tif", sep = "/"))
forest      <- raster(paste(myPath, inFolder, "geodata/forestcover_germany.tif", sep = "/"))


# bavaria extent:
# Get Bavaria data ----------------------
bav.shp <- st_read(paste(myPath, inFolder, "geodata/outline_bavaria.gpkg", sep = "/"), 
                   layer = 'outline_bavaria') # read watershed


# Simplify polygon to speed up calculation:
bav.simple <- st_simplify(bav.shp, preserveTopology = FALSE, dTolerance = 1500)

# remove unnecessary colums:
bav.simple <- bav.simple %>% 
  dplyr::select('geom')


# Make sure they have the same projection
st_crs(bav.simple) <- st_crs(grid)

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


# Create dataframes containing grid id and disturbance vs forest data
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


# Export files:
# write_csv(out.df, paste(myPath, 'outTables', "grid_50.csv", sep = "/"))

# Working example on one size grid!

# get vector of years as reference
reference_period <- 1986:2015
drought_period   <- 2018:2020


# Calculate anomalies: first remove the grids that have less than 1 ha/year of mortality at average:
# is this valid for my hexa data as well? !
# calculate anomalies for the year 2018-2020 as
# calculate anomalies from year 2019-2020
out.df2 <-
  out.df %>%
  group_by(gridindex) %>%
  filter(sum(disturbance_ha) > 35) %>% # Exclude areas with less than 1 ha/yr of disturbances on average
  filter(sum(disturbance_ha[year %in% reference_period]) > 30) %>% # Exclude areas with less than 1 ha/yr of disturbances on average
  mutate(mean_ref    = mean(disturbance_ha[year %in% reference_period], na.rm = T),
         sum_18_20   = sum( disturbance_ha[year %in% drought_period], na.rm = T)/3,
         sum_19_20   = sum( disturbance_ha[year %in% c(2019,2020)], na.rm = T)/2,
         anomaly     = disturbance_ha / mean(disturbance_ha[year %in% reference_period], na.rm = TRUE) - 1,
         anomaly_18_20  = sum_18_20 / mean_ref - 1,
         anomaly_19_20  = sum_19_20 / mean_ref - 1) %>% 
  ungroup()

# Anomalies meaning: 
# anomaly = anomalies per year
# anomaly_18_20 - mean sum per 3 years
# anomaly_19_20 - mean sum per 2 years

# ---------------------------------------------
# Inspect anomalies for the aggregated years  - seems ok
# ---------------------------------------------
out.df2 %>% 
  filter(year >2017) %>% 
  print(n = 20) %>% 
  head()

# Calculate forest cover
out.df2 <- out.df2 %>%
  mutate(forestcover = forest_ha / land_ha) %>% 
  filter(!is.na(forest_ha))


# join anomalies to individual grids:
dat_grid <- grid %>%
  right_join(out.df2, by= c('FID' = "gridindex")) # right join to keep all grids


# clip anomalies to the bavaria shp
dat_grid_bav <- st_intersection(st_as_sf(dat_grid), 
                                st_as_sf(bav.simple))


# clip grid to the bavaria shp
grid_bav <- st_intersection(st_as_sf(grid_sel), 
                            st_as_sf(bav.simple))



# Export table
#fwrite(out.df2, paste(myPath, outTable, 'anomal.csv', sep = "/"))


# --------------------------------
# Make a map of anomalies:
# --------------------------------


# Classify the data :

# Create a new table:
my_breaks = c(-5,3,1000)  # 3 = 300% percent!!
my_cls = c('low', 'high')


# create a new table:
dat_grid_2018_2020 <- dat_grid_bav %>%
  filter(year %in% 2018:2020) %>%
  mutate(disturbance_rate = disturbance_ha / forest_ha,
         disturbance_rate_capped = ifelse(disturbance_rate > 0.05, 0.05, disturbance_rate)) %>%
  mutate(anomaly_capped = ifelse(anomaly > 5, 5, anomaly),
         anomaly_capped_18_20 = ifelse(anomaly_18_20 > 5, 5, anomaly_18_20),
         anomaly_capped_19_20 = ifelse(anomaly_19_20 > 5, 5, anomaly_19_20)) %>% 
  mutate(anomaly_cl =  cut(anomaly, breaks = my_breaks, labels = c('low', 'high')),
         anomaly_cl_18_20 = cut(anomaly_18_20, breaks = my_breaks, labels = c('low', 'high')),
         anomaly_cl_19_20 = cut(anomaly_19_20, breaks = my_breaks, labels = c('low', 'high')))



# Export shp with simpler names to use in qgis, get only one row per whole dataset, 
# as we are using means per 2018-2020: use only one of those years ie.e 2020
dat_grid_2018_2020_2 <- 
  dat_grid_bav %>%
  filter(year == 2020) %>%
  mutate(dist_rate = disturbance_ha / forest_ha) %>%
  mutate(ancl_1820 = cut(anomaly_18_20, 
                        breaks = my_breaks, 
                        labels = c('low', 'high')))  %>% 
  rename(anom18_20 = anomaly_18_20,
         cl_anom18_20 = ancl_1820) %>% 
  dplyr::select(c(FID, anom18_20, cl_anom18_20  )) #%>% 
    
 # rename(dist_ha = disturbance_ha) %>% 
 #   head() #%>% 
 # dplyr::select(-c())


# check why the writing sf does not work??
st_crs(dat_grid_2018_2020_2) <-  st_crs(grid)


# export a new shp: final grid
#st_write(dat_grid_2018_2020_2, 
#         paste('C:/Users/ge45lep/Documents/2021_Franconia_mortality/01_find_hotspots/outSpatial/share', 'anom_12_fin.shp', sep = "/"), delete_layer = TRUE )

# Need to pass the values of the anomaly year into 'hotspot' statement to 
# put them in a one map
# get df of only hotspots
dat_hotspots <- 
  dat_grid_bav %>%
  filter(year %in% 2018:2020) %>%
  mutate(anomaly_cl =  cut(anomaly, 
                           breaks = my_breaks, 
                           labels = c('low', 'hotspot'))) %>% 
    dplyr::select(FID, year, anomaly, anomaly_cl) %>% 
    dplyr::filter(anomaly_cl == 'hotspot')  # select only hotspots!!



# -----------------------------------------
# Plot on map as categorical variable: 
# -----------------------------------------
my_theme <-   theme(panel.spacing = unit(0, "cm"),
                    panel.background = element_rect(fill = "white", color = "black", size = 1.125),
                    legend.key.height = unit(1, "cm"),
                    legend.key.width = unit(0.125, "cm"),
                    legend.position = "bottom",
                    strip.background = element_blank(),
                    strip.text = element_text(size = 9, color = "black"),
                    plot.title = element_text(size = 10),
                    legend.text = element_text(size = 9),
                    axis.text = element_blank(),
                    axis.title = element_blank(),
                    axis.ticks = element_blank()) #+
  



# --------------------------------------------
# aggregated anomalies 2018-2020 
# -------------------------------------------
# FINAL MAP 1 !!!


# add context: rivers, cities
# https://www.earthdatascience.org/courses/earth-analytics/spatial-data-r/make-maps-with-ggplot-in-R/


library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(maptools)
library(ggsn)  # add north arows
library(lubridate)

  

# Get data from Germany:
#sp::plot(ne_states())   # country = 'germany'

# get river, cities and lakes
# need to download them from https://cran.r-project.org/web/packages/rnaturalearth/vignettes/rnaturalearth.html
# lakes
lakes <- ne_download(scale = 10, 
                     type = 'lakes', 
                     category = 'physical', 
                     returnclass = "sf")
lakes <- st_transform(lakes, st_crs(grid))
lakes <- st_intersection(lakes, bav.simple)


# rivers
rivers <- ne_download(scale = 10, type = 'rivers_lake_centerlines', category = 'physical', returnclass = "sf")
rivers <- st_transform(rivers, st_crs(grid))
rivers <- st_intersection(rivers, bav.simple)


# Urban areas:
urban <- ne_download(scale = 10, type = 'urban_areas', category = 'cultural', returnclass = "sf")
urban <- st_transform(urban, st_crs(grid))
urban <- st_intersection(urban, bav.simple)

# Cities:
cities <- ne_download(scale = 10, type = 'populated_places', category = 'cultural', returnclass = "sf")
cities <- st_transform(cities, st_crs(grid))
cities <- st_intersection(cities, bav.simple)


cities %>% 
  filter(SCALERANK <8) %>% 
  distinct(NAME)

# Check which sities to plot?
range(cities$SCALERANK)
nrow(cities)


# Plot data over bavaria: ----------------------------------------
# Classified > 300% of mortality anomaly

# plot with forest as raster:
#forest_bav_r<-as.data.frame(forest_bav, xy = TRUE)
# need to add forest there or not??

ggplot() +
  geom_tile(data = forest_bav_r, aes(fill = forestcover_germany))

ggplot() +
  geom_tile(data=forest_bav_r, aes(x=x, y=y, fill = forestcover_germany))

# !!!!!
# add forest cover: 



ggplot() +
  geom_sf(data = cities) +#, #subset(cities, SCALERANK <8),  
         # fill = 'grey35', 
          #col = NA, 
          #show.legend = 'point')# +
  geom_sf(data = rivers, 
          fill = NA, 
          col = '#74a9cf',#   'blue', 
          lwd = 0.75, 
          show.legend = T) +
  #geom_raster(data = forest_bav, fill = 'grey90', col = NA) +  # add forest layer
  geom_sf(data = subset(dat_grid_2018_2020, 
                        anomaly_cl_18_20 == 'high' & FID !=479),  # exclude grin n 479! falls in Norimberg
          aes(fill = anomaly_cl_18_20), alpha = 0.3, show.legend = T) +
  scale_fill_manual(values = c('red'),
                    labels = c('mortality\nhotspot')) +
  #geom_sf(data = grid_bav, fill = NA, col = 'grey') +
  geom_sf(data = bav.simple, color = "black", fill = NA, lwd = 1.5) +
  theme_linedraw() +
  my_theme +
  coord_sf(expand = FALSE, datum = NA) +
  labs(col = NULL, fill = "Mortality hotspots", 
       title = "Bavaria mortality hotspots (2018-2020)")# +
  

windows()
#p_anomaly_map <- 
ggplot() +
  geom_sf(data = dat_grid_2018_2020,
          aes(fill = anomaly_cl_18_20), 
          col = NA) +
  scale_fill_manual(values = c('white', 'red'),
                    labels = c('', 'mortality\nhotspot')) +
  geom_sf(data = grid_bav, fill = NA, col = 'grey') +
  geom_sf(data = bav.simple, color = "black", fill = NA, lwd = 1.5) +
  theme_linedraw() +
  my_theme +
  coord_sf(expand = FALSE, datum = NA) +
  labs(col = NULL, fill = NULL, 
       title = "Bavaria mortality hotspots (2018-2020)")# +


ggsave(paste(myPath, outReach, "Bavaria_mortality_agg18_20.png", sep = "/"))





# continuous variable: -----------------------------
# Final map 2!!!
# Anomaly map for summed up disturbances: ------------------------------------
#p_anomaly_map <- 
ggplot() +
  geom_sf(data = dat_grid_2018_2020,
          aes(fill = anomaly_capped_18_20 * 100), col = NA) +
  geom_sf(data = bav.simple, color = "black", fill = NA, lwd = 1.5) +
  scale_fill_gradient2(low = "#2166ac", mid = "#FFFFFF", high = "#b2182b",
                       breaks = c(-100, 0, 100, 200, 300, 400, 500),
                       labels = c("-100%", "0%", "100%", "200%", "300%", "400%", ">500%")) +
  scale_color_gradient2(low = "#2166ac", mid = "#FFFFFF", high = "#b2182b",
                        breaks = c(-100, 0, 100, 200, 300, 400, 500),
                        labels = c("-100%", "0%", "100%", "200%", "300%", "400%", ">500%")) +
  theme_linedraw() +
  my_theme +
  coord_sf(expand = FALSE, datum = NA) +
  labs(col = NULL, fill = NULL, 
       title = "a) Forest disturbance anomalies (sum 2018-2020)")# +
#facet_wrap(~year, ncol = 3)










# show only hotspots ---------------
windows()
#p_anomaly_map <- 
ggplot() +
  geom_sf(data = dat_hotspots,
          aes(fill = as.factor(year)), 
          col = NA)  +
  scale_fill_manual(values = c('#ffeda0', '#feb24c', '#f03b20'),
                    labels = c('2018', '2019', '2020')) +
  geom_sf(data = grid_bav, fill = NA, col = 'grey') +
  geom_sf(data = bav.simple, color = "black", fill = NA, lwd = 1.5) +
  theme_linedraw() +
  my_theme +
  coord_sf(expand = FALSE, datum = NA) +
  labs(col = NULL, fill = NULL, 
       title = "Bavaria mortality hotspots (2018-2020)")# +


ggsave(paste(myPath, outReach, "Bavaria_mortality.png", sep = "/"), 
       width = 4, height = 4)







# Need to have two maps: 
# anomaly in 2018, aggregated anomalies in 2019-2020, compare them?
# can i difference them??/

# udenrstand the data: are they in % already? no! need to *100
range(dat_grid_2018_2020$anomaly)
range(dat_grid_2018_2020$anomaly_18_20)
range(dat_grid_2018_2020$anomaly_19_20)



# Make a yearly binary map:

windows()
ggplot() +
  geom_sf(data = subset(dat_grid_2018_2020, year ==2018),
          aes(fill = anomaly_cl), col = NA) +
  scale_fill_manual(values = c('white', 'red'))#  +


windows()
ggplot() +
  geom_sf(data = dat_grid_2018_2020,
          aes(fill = anomaly_cl_19_20), col = NA) +
  scale_fill_manual(values = c('white', 'red'))#  +



# Make map binary for all 3 years: ---------------------------------------
# FINAL MAP 2
windows()
#p_anomaly_map <- 
ggplot() +
  geom_sf(data = dat_grid_2018_2020,
          aes(fill = anomaly_cl), col = NA) +
 
  scale_fill_manual(values = c('white', 'red'),
                    labels = c('', 'mortality\nhotspot')) +
  geom_sf(data = grid_bav, fill = NA, col = 'grey') +
  geom_sf(data = bav.simple, color = "black", fill = NA, lwd = 1.5) +
  theme_linedraw() +
  my_theme +
  coord_sf(expand = FALSE, datum = NA) +
  labs(col = NULL, fill = NULL, 
       title = "Forest disturbance anomalies (2018-2020)") +
  facet_wrap(~year, ncol = 3)

ggsave(paste(myPath, outReach, "Bavaria_mortality_red.png", sep = "/"), dpi = 300,
       width = 8, height = 4)


# FINAL  MAP: 2018 and 2019-2020 


# only 2018
p.18 <- 
  ggplot() +
  geom_sf(data = subset(dat_grid_2018_2020, year == 2018),
          aes(fill = anomaly_cl), col = NA) +
  geom_sf(data = grid_bav, fill = NA, col = 'grey') +
  geom_sf(data = bav.simple, color = "black", fill = NA, lwd = 1.5) +
  scale_fill_manual(values = c('white', 'orange'),
                    labels = c('', 'mortality\nhotspot')) +
  theme_linedraw() +
  my_theme +
  coord_sf(expand = FALSE, datum = NA) +
  labs(col = NULL, fill = NULL, 
       title = "b) Forest disturbance anomalies (2018)")# +
#facet_wrap(~year, ncol = 3)



p.19_20 <- 
  ggplot() +
  geom_sf(data = subset(dat_grid_2018_2020, year == 2018),
          aes(fill = anomaly_cl_19_20), col = NA) +
  geom_sf(data = grid_bav, fill = NA, col = 'grey') +
  geom_sf(data = bav.simple, color = "black", fill = NA, lwd = 1.5) +
  scale_fill_manual(values = c('white', 'red'),
                    labels = c('', 'mortality\nhotspot')) +
  theme_linedraw() +
  my_theme +
  coord_sf(expand = FALSE, datum = NA) +
  labs(col = NULL, fill = NULL, 
       title = "c) Forest disturbance anomalies (2019-2020)")# +




# how often extreme values occurs over years??? ------------------
# get summary
dat_grid_2018_2020 %>% 
  dplyr::select(year, anomaly) %>% 
  split(.$year) %>% 
  map(summary)


# maybe we can consider as wind disturbance everything abouve 24? 
# this is the m,ax anomaly values in 2020, in 2019 max is 19, in 2018 max is 85!



# Inspect anomalies values:  - can the extreme values represent wind???
# Calculated as 'anomaly = disturbance_ha / mean(disturbance_ha[year %in% reference_period], na.rm = TRUE) - 1,'
extreme20_18_FID <- dat_grid_2018_2020 %>% 
  dplyr::select(FID, year, disturbance_ha, mean_ref, anomaly) %>% 
  dplyr::filter(anomaly >=20 & year  == 2018) %>% 
  #dplyr::filter(anomaly > 3) %>% 
  distinct(FID) %>% 
  pull()


extreme3_18_FID <- dat_grid_2018_2020 %>% 
  dplyr::select(FID, year, disturbance_ha, mean_ref, anomaly) %>% 
  dplyr::filter(anomaly >3 & anomaly < 5 & year  == 2018) %>% 
  #dplyr::filter(anomaly > 3) %>% 
  distinct(FID) %>% 
  pull()

# compare the vectors:
setdiff(extreme3_18_FID,  extreme5_18_FID)

# Extremely damaged plots were: 1015,1034,1051,1070 
# plot the data in 2018 with extreme values: 20 of them:
windows()
 
dat_wind <- 
  dat_grid_2018_2020 %>% 
  filter(FID %in% extreme20_18_FID) %>% 
  mutate(d_class = 'windthrow')  %>%
  filter(year == 2018)


p.wind <- 
  ggplot() + 
  geom_sf(data = grid_bav, fill = 'white', col = 'grey') +
  geom_sf(data = dat_wind, aes(fill = d_class)) +
  geom_sf(data = bav.simple, color = "black", fill = NA, lwd = 1.5) +
  scale_fill_manual(values = c('blue'),
                    labels = c('windthrow')) +
  
    #plot_details()
  theme_linedraw() +
  my_theme +
  coord_sf(expand = FALSE, datum = NA) +
  labs(col = NULL, fill = NULL, 
       title = "a) Likely windthrow (2017-2018)")
  

# FINAL MAP 4: mortality spots 2019-2020 minus the 2018 data??
# or make plots:
# disturbances by FID over years:
# which FIDS have the highest anomalies???
dat_grid_2018_2020 %>% 
  filter(anomaly > 10) %>% 
  distinct(FID)



# Export the plot
windows(8,4)
library(ggpubr)
ggarrange(p.wind, p.18, p.19_20, nrow = 1)
ggsave(paste(myPath, outReach, "Bavaria_mortality_hotspots.png", sep = "/"), dpi = 300)





# Grids with high mortality: 
# Let's check grid number 1051: should have high mortality
dat_grid_2018_2020 %>% 
  dplyr::filter(FID == 1051) %>% 
  ggplot(aes(y = ))



# how many grids alltogether?
length(unique(dat_grid_2018_2020$FID))  # 637

nrow(grid_bav) # 656 hexagons



# add data for year as individual layers: -------------------------------------
windows()
#p_anomaly_map <- 
ggplot() +
  geom_sf(data = grid_sel)  

  geom_sf(data = subset(dat_grid_2018_2020, year == 2018),
          aes(fill = anomaly_cl), col = 'white') +
  #scale_fill_manual(values = c('white', 'red'),
   #                 labels = c('', 'mortality\nhotspot')) +
  geom_sf(data = subset(dat_grid_2018_2020, year == 2019),
          aes(fill = anomaly_cl), col = NA) # +
  #scale_fill_manual(data = subset(dat_grid_2018_2020, year == 2018),
   #                 aes(fill = anomaly_cl), col = NA,
    #                values = c('white', 'black'),
     #               labels = c('', 'mortality\nhotspot')) 




+
  geom_sf(data = dat_grid_2018_2020,fill = NA, col = 'grey') +
  geom_sf(data = bav.simple, color = "black", fill = NA, lwd = 1.5) +
  theme_linedraw() +
  theme(panel.spacing = unit(0, "cm"),
        #panel.background = element_rect(fill = "#d1e5f0", color = "black", size = 1.125),
        panel.background = element_rect(fill = "white", color = "black", size = 1.125),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(0.125, "cm"),
        legend.position = "right",
        strip.background = element_blank(),
        strip.text = element_text(size = 9, color = "black"),
        plot.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(expand = FALSE, datum = NA) +
  labs(col = NULL, fill = NULL, 
       title = "Forest disturbance anomalies (2018-2020)") +
  facet_wrap(~year, ncol = 3)# +
#geom_text(aes(x=0, y=10, label='salala'),                 , 
#          color="orange", 
#         size=7 , angle=45, fontface="bold" )






# world libs for nice plottings --------------------------------------------------------------


library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world <- st_transform(world, st_crs(grid))
world <- st_crop(world, st_bbox(grid) + c(-0.05, -0.05, 0.01, 0.01) * as.double(st_bbox(grid)))





# Make a map for anomalies per year (from Cornelius) -----------------------------------------
p_anomaly_map <- 
  ggplot() +
  geom_sf(data = world, color = "black", fill = "lightgray") +
  geom_sf(data = dat_grid_2018_2020,
          aes(fill = anomaly_capped * 100), col = NA) +
  geom_sf(data = world, color = "black", fill = NA) +
  scale_fill_gradient2(low = "#2166ac", mid = "#FFFFFF", high = "#b2182b",
                       breaks = c(-100, 0, 100, 200, 300, 400, 500),
                       labels = c("-100%", "0%", "100%", "200%", "300%", "400%", ">500%")) +
  scale_color_gradient2(low = "#2166ac", mid = "#FFFFFF", high = "#b2182b",
                        breaks = c(-100, 0, 100, 200, 300, 400, 500),
                        labels = c("-100%", "0%", "100%", "200%", "300%", "400%", ">500%")) +
  theme_linedraw() +
  theme(panel.spacing = unit(0, "cm"),
        #panel.background = element_rect(fill = "#d1e5f0", color = "black", size = 1.125),
        panel.background = element_rect(fill = "white", color = "black", size = 1.125),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(0.125, "cm"),
        legend.position = "right",
        strip.background = element_blank(),
        strip.text = element_text(size = 9, color = "black"),
        plot.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(expand = FALSE, datum = NA) +
  labs(col = NULL, fill = NULL, 
       title = "a) Forest disturbance anomalies") +
  facet_wrap(~year, ncol = 3)


(p_anomaly_map)
#ggsave("anom_3.png", width = 7, height = 4)










# Only Bavaria maps of yearly anomalies: -------------------------------------------




#p_anomaly_map <- 
  ggplot() +
  geom_sf(data = dat_grid_2018_2020,
          aes(fill = anomaly_capped * 100), col = NA) +
  geom_sf(data = bav.simple, color = "black", fill = NA, lwd = 1.5) +
  scale_fill_gradient2(low = "#2166ac", mid = "#FFFFFF", high = "#b2182b",
                       breaks = c(-100, 0, 100, 200, 300, 400, 500),
                       labels = c("-100%", "0%", "100%", "200%", "300%", "400%", ">500%")) +
  scale_color_gradient2(low = "#2166ac", mid = "#FFFFFF", high = "#b2182b",
                        breaks = c(-100, 0, 100, 200, 300, 400, 500),
                        labels = c("-100%", "0%", "100%", "200%", "300%", "400%", ">500%")) +
  theme_linedraw() +
  theme(panel.spacing = unit(0, "cm"),
        #panel.background = element_rect(fill = "#d1e5f0", color = "black", size = 1.125),
        panel.background = element_rect(fill = "white", color = "black", size = 1.125),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(0.125, "cm"),
        legend.position = "right",
        strip.background = element_blank(),
        strip.text = element_text(size = 9, color = "black"),
        plot.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(expand = FALSE, datum = NA) +
  labs(col = NULL, fill = NULL, 
       title = "a) Forest disturbance anomalies") +
  facet_wrap(~year, ncol = 3)


(p_anomaly_map)
ggsave("anom_3.png", width = 7, height = 4)





















# ---------------------------- ------------------------------
# !!!!!!!!!!!!!!!!!!
# ----------------------------


# # should be teh same aggregation as assign mortality to grid??


gc()



# Get rasters
disturbance <- raster(paste(myPath, inFolder, "disturbance_map_bavaria.tif", sep = "/"))
forest      <- raster(paste(myPath, inFolder, "forestcover_bav.tif", sep = "/"))

# Get all grids
grids <- list.files(paste(myPath, 'outSpatial', sep = "/"), 
                    pattern= "^grid.*shp$") 

grid.ls <- lapply(grids, function(name) {read_sf(paste(myPath, 'outSpatial', name, sep = '/'))})

# Add names
grids_names = gsub('.shp', '', grids)
#grids_names = as.numeric(gsub('grid_', '', grids_names))

# Paste names as new column to each df in a list
grid.ls2 <- map2(grid.ls, grids_names, ~cbind(.x, gridSize = .y))


# loop over grids


anomalies_to_grid <- function(grid, ...) {
  
  outName = unique(grid$gridSize)
  
  outMap = paste(myPath, 'map', paste0('anom_', outName, '.png'), sep = "/")
  outSHP = paste(myPath, outSpatial, paste0('anom_', outName, '.shp'), sep = "/")
  

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
  
  
  # Working example on one size grid!
  
  # get vector of years as reference
  reference_period <- 1986:2015
  drought_period   <- 2018:2020
  
  
  # Calculate anomalies: first remove the grids that have less than 1 ha/year of mortality at average:
  # is this valid for my hexa data as well? ! 
  out.df2 <- out.df %>%
    group_by(gridindex) %>%
    filter(sum(disturbance_ha) > 35) %>% # Exclude areas with less than 1 ha/yr of disturbances on average
    filter(sum(disturbance_ha[year %in% reference_period]) > 30) %>% # Exclude areas with less than 1 ha/yr of disturbances on average
    mutate(mean_ref    = mean(disturbance_ha[year %in% reference_period], na.rm = T),
           sum_18_20      = sum( disturbance_ha[year %in% drought_period], na.rm = T)/3,
           anomaly     = disturbance_ha / mean(disturbance_ha[year %in% reference_period], na.rm = TRUE) - 1,
           anomaly_18_20  = sum_18_20 /         mean_ref - 1) %>% 
    ungroup()
  
  # Calculate forest cover
  out.df2 <- out.df2 %>%
    mutate(forestcover = forest_ha / land_ha) %>% 
    filter(!is.na(forest_ha))
  
  
  # join anomalies to individual grids:
  dat_grid <- grid %>%
    right_join(out.df2, by= c('FID' = "gridindex")) # right join to keep all grids
  
  
  # clip anomalies to the bavaria shp
  dat_grid_bav <- st_intersection(st_as_sf(dat_grid), 
                                  st_as_sf(bav.simple))
  
  # Export table
  fwrite(out.df2,paste(myPath, outTable, paste0('anom_', outName, '.csv'), sep = "/"))
  
  
  print('make maps')
  
  # --------------------------------
  # Make a map of anomalies:
  # --------------------------------

  library(rnaturalearth)
  library(rnaturalearthdata)
  library(rgeos)
  
  
 # world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
 # world <- st_transform(world, st_crs(grid))
#  world <- st_crop(world, st_bbox(grid) + c(-0.05, -0.05, 0.01, 0.01) * as.double(st_bbox(grid)))
  
  dat_grid_2018_2020 <- dat_grid_bav %>%
    filter(year %in% 2018:2020) %>%
    mutate(disturbance_rate = disturbance_ha / forest_ha,
           disturbance_rate_capped = ifelse(disturbance_rate > 0.05, 0.05, disturbance_rate)) %>%
    mutate(anomaly_capped = ifelse(anomaly > 5, 5, anomaly),
           anomaly_capped_18_20 = ifelse(anomaly_18_20 > 5, 5, anomaly_18_20))
  
  
  # Export the final shp
  st_write(dat_grid_2018_2020, outSHP, delete_layer = TRUE )
  
  
  
  # Make a map ------------------------------------------------
  p_anomaly_map <- ggplot() +
    #geom_sf(data = world, color = "black", fill = "lightgray") +
    geom_sf(data = dat_grid_2018_2020,
            aes(fill = anomaly_capped_18_20 * 100), col = NA) +
    #geom_sf(data = world, color = "black", fill = NA) +
    geom_sf(data = bav.simple, color = "black", fill = NA, lwd = 1.5) +
    scale_fill_gradient2(low = "#2166ac", mid = "#FFFFFF", high = "#b2182b",
                         breaks = c(-100, 0, 100, 200, 300, 400, 500),
                         labels = c("-100%", "0%", "100%", "200%", "300%", "400%", ">500%")) +
    scale_color_gradient2(low = "#2166ac", mid = "#FFFFFF", high = "#b2182b",
                          breaks = c(-100, 0, 100, 200, 300, 400, 500),
                          labels = c("-100%", "0%", "100%", "200%", "300%", "400%", ">500%")) +
    theme_linedraw() +
    theme(panel.spacing = unit(0, "cm"),
          #panel.background = element_rect(fill = "#d1e5f0", color = "black", size = 1.125),
          panel.background = element_rect(fill = "white", color = "black", size = 1.125),
          legend.key.height = unit(1, "cm"),
          legend.key.width = unit(0.125, "cm"),
          legend.position = "right",
          strip.background = element_blank(),
          strip.text = element_text(size = 9, color = "black"),
          plot.title = element_text(size = 10),
          legend.text = element_text(size = 9),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()) +
    coord_sf(expand = FALSE, datum = NA) +
    labs(col = NULL, fill = NULL, 
         title = paste0("a) Forest disturbance anomalies (sum 2018-2020)", gsub('grid_','', outName), ' km')) 
  
  
  (p_anomaly_map)
  ggsave(outMap, width = 7, height = 4)

}


# test on one file ----------------------------------------------------------
anomalies_to_grid(grid.ls2[[6]])


# Loop over all grid sizes --------------------------------------------------
lapply(grid.ls2, anomalies_to_grid)


