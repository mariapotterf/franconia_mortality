
# Get data to share with Jorg


# Elevation
# Projected Koordinates X Y
# Treatment
# Number of tree species regeneration – as community matrix, both advanced and small
#                                     - stem density
# Number of tree species canopy – as community matrix
# Forest in the surrounding of 1 km – deciduous, vs coniferous & no forest

# aggregate on SITE level for Jorg, as our were based on PLOTS (we have several plots per site)
# 3 treatments, 


rm(list=ls())


library(sf)
library(dplyr)
library(data.table)
library(tidyr)
library(raster)
library(rgdal)
library(tidyverse)
library(lubridate)
library(patchwork)
library(fasterize)
library(ggpubr)
library(terra)


# Read my paths -----------------------------------------------------------
source('myPaths.R')

getwd()
load(file = paste(getwd(), "outData/dataToPlot.Rdata", sep = '/'))
load(file = paste(getwd(), "outData/dat_restr.Rdata", sep = '/'))


# output file
outPath = paste('C:/Users/ge45lep/Documents/2021_Franconia_mortality/03_plot_sampling/out_fieldData/share_veget_Jorg')


# Get rasters: deciduous vs coniferous,
# calculate the buffer as well;
forest_type <- terra::rast("C:/Users/ge45lep/Documents/2022_BarkBeetles_Bavaria/outSpatial/bav_fortype_ext30_int2u_LZW.tif")
xy          <- st_read(paste(myPath, outSpatial, "xy_3035_elev.gpkg", sep = "/"), 
                layer = 'xy_3035_elev') # read XY data


# need to filter XY data: remove 45, 65 - not possible to plot sample! 
xy <- xy %>% 
  dplyr::filter(!trip_n %in% c("45", "65")) %>% 
  unite(name, c('trip_n','dom_sp', 'manag' )) # %>% 


# check projection
st_is_longlat(xy) # FALSE

# Identify data to use:
head(df_full_corr_mrg)    # - full PLOT based data: df_full_corr, seedlings, advanced, mature - PLOt & surroundings, mature trees filtered 
plot_counts_df_sum        # - number of plots per site & treatment 


# read elevation df with XY coordinates:
# export CSV table 
fwrite(xy2_df_out, paste(myPath, outTable, "xy_3035_elev.csv", sep = "/"))


# filter data to check if really regen is missing: 61 D

df_full_corr_mrg %>% 
  filter(vert_layer == 'regen' ) %>% 
  filter(trip_n == '61' & manag == 'd') #%>%  

# check if data correct: 
# - height classes
# - stem densities
# - tree species 
# - account for plots without vegetation - need to add the full  plot matrix before merging-
#     simply calculate the average per plots to get the SITE value 
# get the 'total' table : combination of trip_n, sub_n, and height classes
v_all_height = unique(df_full_corr_mrg$vert_layer)


# account for plots without some vert layers:
# make master table for species and for vert.layer to include 0 if not found:
# for 27 species
df_master_species_plot <- df_master_species %>% 
  ungroup(.) %>% 
  dplyr::select(-sub_n) %>% 
  distinct()

table(df_master_species_plot$trip_n,df_master_species_plot$manag)


# make master dataframe having both height categories: 
df_master_heights <-   plot_counts_df %>% 
  ungroup(.) %>% 
  dplyr::select(-sub_n) %>% 
  distinct() %>% 
  mutate(vert_layer = 'mature') %>% 
  group_by(trip_n, manag) %>% 
  complete(vert_layer = .env$v_all_height)

table(df_master_heights$trip_n,df_master_heights$manag)

# vert_layer represent both ENV and PLOT level densities for the advanced and mature trees
# change species names to latin
veg_matrix <- 
  df_full_corr_mrg %>% 
  right_join(plot_counts_df_sum, by = c("trip_n", "manag"))  %>% # add sampling effort = plots number
  rename(sampl_effort = n) %>% # number of plots per site&treatment 
  mutate(
    species = case_when(
      species == "Ash"    ~ "Fraxinus excelsior",
      species == "Beech"  ~ "Fagus sylvatica"  ,
      species == "Rowan"  ~ "Sorbus aucuparia" ,
      species == "Maple"  ~ "Acer pseudoplatanus" ,
      species == "Spruce" ~ "Picea abies" ,
      species == "Oak"    ~ "Quercus" ,
      species == "Pine"   ~ "Pinus sylvestris",
      species == "Birch"  ~ "Betula pendula" ,
      species == "Willow" ~ "Salix" ,
      species == "Fir"    ~ "Abies alba" ,
      TRUE ~ species
    )
  ) %>% 
    ungroup(.) %>% 
  group_by(trip_n, sampl_effort, manag, species, vert_layer) %>% 
  summarise(sum_stem = sum(corr_count,rm.na = T)) %>% 
   pivot_wider(names_from = species ,  # convert to matrix by species
   values_from = sum_stem,
             values_fill = 0) %>% # replace NA by 0 if missing speies
  ungroup(.) %>% 
  right_join(df_master_heights, by = c("trip_n", "manag", "vert_layer"),
             values_fill = 0)  %>% # add all 3 height categories
  replace(is.na(.), 0) 
  
    

# split by the vertical layers into individual df
# convert to matrix by species
#
# check missing combination:
table(veg_matrix$manag, veg_matrix$vert_layer)

# split by group
out <- veg_matrix %>% 
  group_split(vert_layer)

# Export matrix table
fwrite(out[[1]], paste(outPath, 'matrix_advanced.csv', sep = '/'))
fwrite(out[[2]], paste(outPath, 'matrix_mature.csv', sep = '/'))
fwrite(out[[3]], paste(outPath, 'matrix_regen.csv', sep = '/'))


# Get deciduous vs coniferous and no-forest
# 1 km buffer -------------------------------------------------------------
xy_vect <- vect(xy)  # convert to terra object
# codes: 2 = coniferous, 1 = deciduous, 0 = no forest 
windows()

# split features in individual objects
buff_ls <- terra::split(xy_vect, "name")


# make a function to loop it over buffers, as they seem to overlap
extract_rst_val <- function(xy_vect, ...) {
  
  name <- xy_vect$name   # get name
  #print(name)
  
  # get buffer
  buff <- buffer(xy_vect, 1000)
  
  # crop data and then mask them to have a circle
  forest_crop <- crop(forest_type, buff)
  forest_mask <- mask(forest_crop, buff)
  
  # Get vector of values
  val <- values(forest_mask)
  
  # count number of cells:
  df <- as.data.frame(table(val))
  
  # add name
  df$name <- name
  
  return(df)
}

out_ls <- lapply(buff_ls, extract_rst_val)

# merge partial tables into one df
forest_df      <-do.call("rbind", out_ls)

# get area
forest_df$area_ha <- forest_df$Freq*0.09  # 30*30 m

forest_df <- forest_df %>% 
  separate(name, c("trip_n", "dom_sp", "manag"), '_') %>% 
  dplyr::select(-Freq)
 
# check if I have reasonable sums? area of 1 buffer (r = 1000 m) is ~323 (314 ha)  
forest_df %>% 
  group_by(trip_n, manag) %>% 
  summarise(sum_area = sum(area_ha))  # here they are ~ correct area


fwrite(forest_df, paste(outPath, 'df_forest_1km.csv', sep = '/'))
