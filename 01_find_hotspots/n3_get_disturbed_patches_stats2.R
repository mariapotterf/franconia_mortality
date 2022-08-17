
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


# Read classified hotspots  --------------------------------------------------------------
grid    <- read_sf(paste(myPath, 'outSpatial', 'share', "anom_12_fin.shp", sep = '/'))


# Get rasters
disturbance <- raster(paste(myPath, inFolder, "disturbance_map_bavaria.tif", sep = "/"))
forest      <- raster(paste(myPath, inFolder, "forestcover_germany.tif", sep = "/"))  # can be replaced by the 10 m resolution one??


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



# ----------------------------------------------
# get disturbance characteristics per hexagon 
# ----------------------------------------------


# subset only high mortality patches; 
# loop over rows (individual hexogons)
# crop disturbance raster
# get statistics per hexagon patch

# how many patches?
# area vs perimeter? proximity??

plot(grid)

hotspots <- grid %>% 
  filter(c_18_20 == 'high')

plot(hotspots)


# process disturbance raster: keep only years 2018-2020
dist_18 <- disturbance


# Change raster values
# https://stackoverflow.com/questions/36960974/how-to-replace-raster-values-less-than-0-to-na-in-r-code
values(dist_18)[values(dist_18) < 2018] = NA


# Loop over hotspots, crop disturbance maps by individual hotspots ------------------------------

#
out_ls <- list()

for (i in 1:nrow(hotspots)) {
  print(i)
  hot1 = hotspots[i,]
  
  my_id = hot1$FID
  
  # Crop disturbance raster, snap to near pixel
  dist1   <- crop(dist_18, hot1, snap="near")
  #forest1 <- crop(forest, hot1, snap="near")
  
  # calculate statistics for landscape classes (disturbance years)
  dist_df <- calculate_lsm(dist1, level = "patch", directions = 8)# %>% 
  
  # add unique FID of the hexagon hotspot
  dist_df$FID = my_id
  
  out_ls[[i]] <- dist_df 
  
}


# merge into one dataframe: clean metric, get characteristics by each hexagon
df = do.call("rbind", out_ls)

# get metrics
unique(df$metric)

#get list of all metrics on patch level
list_lsm(level = 'patch')

# para = perimeter-area ratio
range(df[df$metric == 'para',]$value)
# CIRCLE = 0 for a circular patch and approaches CIRCLE = 1 for a linear patch.
# https://r-spatialecology.github.io/landscapemetrics/reference/lsm_p_circle.html


# get patch dimension: 
# a= pi*r*r
# d = sqrt(a/pi)*2

# get statistics per hexahon: how does the average hexagon look per year
hot_df <- df %>% 
  dplyr::filter(metric == 'area' & value > 0.09) %>% 
  group_by(class, FID) %>% # FID is a hexagon ID
  dplyr::summarize(n_patch = n(),
                   sum_area    = sum(value),
                   mean_area   = mean(value),
                   median_area = median(value),
                   min_area = min(value),
                   max_area = max(value)
                   )

# Get only area metric
df_area <- df %>% 
  dplyr::filter(metric == 'area' & value > 0.09) #%>% 



# Get a table with quantiles
# Represent results using quantiles, as they are skewed?
qntils = c(0, 0.01, 0.25, 0.5, 0.75,0.99, 1)

df_qs <- df %>% 
  dplyr::filter(metric == 'area' & value >= 0.09) %>% 
  group_by(class) %>% # FID is a hexagon ID, FID
  summarise(qs_area   = quantile(value, qntils),
            mean_area = mean(value, rm.na = T),
            sum_area  = sum(value),
            n_patch   = n(),
            prob      = qntils) %>%
  #mutate(quant_area_rate    = stringr::str_glue("{round(qs_rate,1)} ({round(qs_area,1)})"),
  #       Mean               = stringr::str_glue("{round(mean_rate,1)} ({round(mean_area,1)})")) %>% 
  dplyr::select(class, qs_area, prob, mean_area, sum_area, n_patch) %>% 
  pivot_wider(names_from = prob, values_from = qs_area ) %>% 
  round(1)




# visualize by boxplots: Variation over hexagons:

p.n <-ggplot(hot_df, aes(x = factor(class), 
                   y = n_patch,
                   color = FID)) + 
  geom_boxplot() + 
  xlab('year') + 
  theme_classic() +
  ylab('# gaps')


p.mean <- ggplot(hot_df, aes(x = factor(class), 
                   y = mean_area,
                   color = FID)) + 
  geom_boxplot() +
  xlab('year') +
  ylab('mean gap area [ha]') +
  theme_classic()

p.median <- ggplot(hot_df, 
                 aes(x = factor(class), 
                             y = median_area,
                             color = FID)) + 
  geom_boxplot() +
  xlab('year') +
  ylab('median gap area [ha]') +
  theme_classic()



library(ggpubr)
windows(7,2.5)
ggarrange(p.n, p.mean, p.median, nrow = 1, ncol = 3, labels="auto")


# how individual patches develop over years??
# get their perimeter and shape to calculate compactness index
df_area <-  df %>% 
  dplyr::filter(metric == 'area')


df_perim <- df %>% 
  dplyr::filter(metric == 'perim') 


dd <- df_area %>% 
  left_join(df_perim, by = c("layer", "level", "class", "id", "FID"))



# Calculate compactness of each patch:
# (C=(4 * π * A)/P2), A = area, P = perimeter, 0 = extremely structured, 1 = circular shape 
dd <- dd %>% 
  mutate(compact = (4*pi*value.x)/(value.y^2))

# get a point plot: shape of gaps ver years:


ggplot(dd, aes(x = value.x,
               y = compact,
               color = factor(FID))) +
  geom_point(alpha = 0.5) +
  facet_grid(.~factor(class))




# Create my raster to understand the measures
# ---------------------------------------------------


r <- raster(ncol = 10, nrow = 10, crs = projection(hex1))
#res(r) <- 10
#projection(r) <-  projection(hex1) #"+proj=utm +zone=48 +datum=WGS84"

values(r) <- c(1,1,1,1,1,1,1,1,1,1,
               1,1,NA,1,1,1,1,2,1,1,
               1,1,NA,1,1,1,1,2,2,1,
               1,1,1,1,1,1,1,1,1,1,
               1,1,1,1,3,3,1,1,1,1,
               1,1,1,2,3,1,1,1,1,1,
               1,1,1,1,1,1,1,1,1,1,
               1,1,3,1,1,1,1,3,1,1,
               1,3,1,1,1,1,1,3,1,1,
               1,1,1,1,1,1,1,1,1,1)

#res(r) <-30

check_landscape(r)

windows()
show_landscape(r, discrete = TRUE)


calculate_lsm(r, level = "patch")

lsm_p_area(r, directions = 8) %>%   # the final values are in hectares
  group_by(class) %>% 
  summarize(sum_area = sum(value),  # sum area in hectares by class
            n = n())                # number of patches by class

  
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

