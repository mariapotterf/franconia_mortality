

# Make database with the study sites coordinates and sites attributes table

# process: 

# Read all GPS coordinates
# read all GPS manual data
# Need to have a unique GPS number!!
# Read attribute table
# Merge data


# -------------------------------------

rm(list=ls())

# Libraries
library(rgdal)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(rnaturalearth) # for map data
library(ggspatial)
library(ggpubr)


# Get path ----------------------------------------------------------------
path = 'C:/Users/ge45lep/Documents/2021_Franconia_mortality'


# Get working directory to read all files to gpx points:
setwd(paste(path, "raw/fieldWork/raw", sep = '/'))


# read data -------------------------------------
# Get GPS files 
gps_files <- list.files(pattern = "*.gpx") # 

# get manually created missing GPS data
gps_man <- read_sf(dsn = paste(path, "fieldData/add_GPS.shp", sep = '/')) #, layer="add_GPS.shp"

# Get attribute table: download from teh sync and share
df_att <- read.csv2(paste(path, 'raw/fieldWork/study_sites.csv', sep = '/'))

# Replace  F (forest) by L (living) as D (Dead) is also a forest
df_att <- df_att %>% 
  mutate(Category = replace(Category, Category == 'F', 'L')) #%>% 

# ------------------------------------------------

# Read gpx file
#temp.gpx <- read_sf(dsn = gps_files[3], layer="waypoints")

my_gpx <- lapply(gps_files, function(i) {read_sf(dsn = i, layer="waypoints")})



# Process data :

# merge all GPX data in one file
all_gps <- do.call("rbind", my_gpx)


# Remove names from GPX coordinates 
# 089 = Freising
# 134 = Fabrikschleichach

all_gps <- all_gps %>% 
  filter(name != "089" & name != "134" ) 

plot(all_gps)


# Merge GPX and manual GPS data -------------------------------------------


# First need to make same columns and same names
all_gps <- 
  all_gps %>% 
  select_if(~!all(is.na(.))) %>%
  rename(elevation = ele) %>% 
  select(name, elevation, time, geometry)

gps_man <-
  gps_man %>% 
  mutate(time = NA)  %>% 
    select(name, elevation, time, geometry)

# Add rows to create a final file
all_gps2 = rbind(all_gps, gps_man)

nrow(all_gps2)



# Plot XY data collection on the map ---------------------------------------------

# get and bind country data
de_sf <- ne_states(country = "germany", 
                   returnclass = "sf")

# Get only bavaria
bav_sf <- de_sf %>% 
  dplyr::filter(name_en == "Bavaria")


# Plot GPS points on the map
windows()
ggplot() + 
  geom_sf(data = bav_sf, fill = 'grey') +
  geom_sf(data = all_gps2, alpha = 1) +
  theme_classic() +
  theme(legend.position = 'bottom')



# Make unique names for GPS points -----------------------------------------------

# Put the manes of the GPS on the same forst: 3 digits, need to add leading zero!
sprintf("%08d", as.numeric("14"))


# Add padding zeros on datasets: e.g leading zeros
df_att <- df_att %>%
  mutate(name = sprintf("%03d", as.numeric(GPS.numb)))# %>% 
  #distinct(name)
  
unique(df_att$name)


# 

# Join GPS data with attributes -------------------------------------------
merged_df <- all_gps2  %>%                # first one needs to be sf to keep sf attributes
  select_if(~!all(is.na(.))) %>%         # Remove all columns that are NA from the GPS data
  left_join(df_att, by = 'name')



nrow(merged_df)

unique(merged_df$name)







# Plot data by species -----------------------------------------------------------


# Filter firrst the data - plots by categories:
cat_sf <- merged_df %>% 
  filter(Category %in% c('CC', 'L', "D"))


p.categ <- ggplot() + 
  geom_sf(data = bav_sf, fill = 'grey') +
  geom_sf(data = cat_sf, 
          aes(color = Category), alpha = 0.5) + 
  scale_color_manual(values = c('red', 'orange', 'darkgreen'),
                     labels = c("Salvaged", 'Dead', 'Living'))+
  theme_classic() +
  theme(legend.position = 'bottom')


#  Plot by species:
my_species = c('spruce', 'beech', 'oak', 'pine')

species_sf <- merged_df %>% 
  filter(Species1 %in% my_species)


p.species <- ggplot() + 
  geom_sf(data = bav_sf, fill = 'grey') +
  geom_sf(data = species_sf, 
          aes(color = Species1)) + 
  scale_color_viridis_d() +
  theme_classic() +
  theme(legend.position = 'bottom')


ggarrange(p.categ, p.species)


# Get  stats ---------------------------------------------------------------


# Replace F (forest) by L (Living) as D (dead) is also a forest
# How many triplets by species we have?

# Get corrector by the number of supbplots:
# Triplet has 3, pair has 2
# No need if I have group them first by estimated number of teh triplet/pairs


df_att %>% 
  filter(Triplet %in% c('pair', 'emg pair', 'triplet', 'emg triplet')) %>% 
  filter(Species1 %in% my_species) %>% 
  group_by(Species1, Triplet, Paper_numb ) %>% 
  tally() %>%
  ungroup() %>% 
  group_by(Species1, Triplet) %>% 
  tally() %>% 
  tidyr::spread(Species1, n) #%>% 


# Count how many categories do we have for the F-D or F-CC pairs? ---------

library(dplyr)

df_att %>% 
  filter(Triplet %in% c('pair', 'emg pair', 'triplet', 'emg triplet')) %>% 
  filter(Species1 %in% my_species) %>% 
  group_by(Species1, Triplet, Triplet_num) %>% 
  #%>% 
  #group_by(id) %>% 
  arrange(Category) %>% 
  summarize(combination = paste0(Category, collapse = "-"), .groups = "drop") %>% 
  count(combination)




head(df_att)

# make a dummy example ---------------------------------------------------------

dd <- data.frame(id = c(1,1,2,2,2,3,3,4, 4, 5,5),
                 cat = c('c','f','c','d','f','c','f', 'd', 'f', 'f', 'd'))


library(dplyr)

dd %>% 
  arrange(cat) %>% 
  group_by(id) %>% 
  summarize(combination = paste0(cat, collapse = "-"), .groups = "drop") %>% 
  count(combination)



# Count how many times I have combination of values?
dd %>% 
  group_by(id) %>% 
  count()
  


          