

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
# Keep a copy just in case on online copy fail!
df_att <- read.csv2(paste(path, 'raw/fieldWork/study_sites.csv', sep = '/'), 
                    encoding = "UTF-8")

# Replace  F (forest) by L (living) as D (Dead) is also a forest
df_att <- df_att %>% 
  mutate(Category = replace(Category, Category == 'F', 'L')) %>%
  mutate(Paper_numb = as.numeric(Paper_numb)) %>%
 # mutate()
  drop_na(Paper_numb) # remove sites without paper indication
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
  filter(!name %in% c("089", "134", "142", "143")) 

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

all_gps2 <- all_gps2 %>% 
  select(c(name, geometry))

nrow(all_gps2)



# Plot XY data collection on the map (do not run) ---------------------------------------------

# get and bind country data
de_sf <- ne_states(country = "germany", 
                   returnclass = "sf")

# Get only bavaria
bav_sf <- de_sf %>% 
  dplyr::filter(name_en == "Bavaria")


# Plot GPS points on the map
#windows()
ggplot() + 
  geom_sf(data = bav_sf, fill = 'grey') +
  geom_sf(data = all_gps2, alpha = 1) +
  theme_classic() +
  theme(legend.position = 'bottom')



# Make unique names for GPS points -----------------------------------------------

# Put the manes of the GPS on the same forst: 3 digits, need to add leading zero!
#sprintf("%08d", as.numeric("14"))


# Add padding zeros on datasets: e.g leading zeros -------------------------------
df_att <- df_att %>%
  filter(GPS.numb != 'NA') %>% 
  mutate(name = sprintf("%03d", as.numeric(GPS.numb)))# %>% 
  #distinct(name)
  
unique(df_att$name)
unique(df_att$GPS.numb)


# adjust names also in the all_gps points:
all_gps2 <- all_gps2 %>%
  mutate(name = sprintf("%03d", as.numeric(name))) 

unique(all_gps2$name)

# 

# Join GPS data with attributes -------------------------------------------
merged_df <- all_gps2  %>%                # first one needs to be sf to keep sf attributes
  #select_if(~!all(is.na(.))) %>%         # Remove all columns that are NA from the GPS data
  right_join(df_att, by = 'name')



nrow(merged_df)

unique(merged_df$name)


# define filters
my_species = c('spruce', 'beech', 'oak', 'pine')



##############################
# Overall table 
##########################


# Make maps:  -------------------------------------------------------------------

# Filter first the data - plots by categories: -------------------------------
cat_sf <- merged_df %>% 
  filter(Category %in% c('C', 'L', "D"))


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
  filter(Species %in% my_species)


p.species <- ggplot() + 
  geom_sf(data = bav_sf, fill = 'grey') +
  geom_sf(data = species_sf, 
          aes(color = Species)) + 
  scale_color_viridis_d() +
  theme_classic() +
  theme(legend.position = 'bottom')


ggarrange(p.categ, p.species)


# Get  stats ---------------------------------------------------------------


# Replace F (forest) by L (Living) as D (dead) is also a forest
# How many triplets by species we have?




# clean up the table
df_out <- merged_df %>% 
  dplyr::select(Date, 
                Paper_numb, 
                Triplet, 
                Forest_district, 
                Contact, 
                Category, 
                GPS.numb, 
                Species, 
                note,
                E.Mail, 
                Phone, 
                geometry) %>% 
  filter(!is.na(Triplet)) #%>% 

# add each row a unique number  
df_out <- df_out %>% 
  mutate(id = 1:nrow(df_out))



# Export the file ------------------------------------------------------------------
st_write(df_out, paste(path, 'fieldData/final', 'all_sites.gpkg', sep = "/"),
         layer = 'all_sites', append=FALSE)



library(data.table)

# fwrite does not work with the correct encoding
# fwrite(df_out, paste(path, 'fieldData/final', 'all_sites.csv', sep = "/")) 
write.csv2(df_out, 
           paste(path, 'fieldData/final', 'all_sites.csv', sep = "/")) 


# Get corrector by the number of supbplots:
# Triplet has 3, pair has 2
# No need if I have group them first by estimated number of teh triplet/pairs


#df_att %>% 
df_out %>% 
  as_tibble() %>% 
  filter(!is.na(Triplet)) %>%  # %in% c('pair', 'emg pair', 'triplet', 'emg triplet')) %>% 
  filter(Species %in% my_species) %>% 
  group_by(Species, Triplet, Paper_numb ) %>% 
  tally() %>%
  ungroup() %>% 
  group_by(Species, Triplet) %>% 
  tally() %>% 
  tidyr::spread(Species, n) #%>% 

































# Filter data by species to share: spruce/beech ---------------------------------------------
# this is just a general overview of triplets!

triplet_sf <- merged_df %>% 
  filter(Triplet == 'triplet' | Triplet == 'emg triplet' |  Triplet == 'pair' |Triplet == 'Triplet') %>% 
  filter(Species %in% c('spruce', 'beech') ) %>% 
  select(name, elevation, geometry, Paper_numb, Triplet, Species, Category,
         Forest_district, Contact)


# Plot final triplets: Beech and spruce
cat_sf <- triplet_sf %>% 
  filter(Category %in% c('C', 'L', "D"))


p.categ <- ggplot() + 
  geom_sf(data = bav_sf, fill = 'grey') +
  geom_sf(data = cat_sf, 
          aes(color = Category), alpha = 0.5) + 
  scale_color_manual(values = c('red', 'orange', 'darkgreen'),
                     labels = c("Salvaged", 'Dead', 'Living'))+
  theme_classic() +
  theme(legend.position = 'bottom')




species_sf <- triplet_sf %>% 
  filter(Species %in% my_species)


p.species <- ggplot() + 
  geom_sf(data = bav_sf, fill = 'grey') +
  geom_sf(data = species_sf, 
          aes(color = Species)) + 
  scale_color_viridis_d() +
  theme_classic() +
  theme(legend.position = 'bottom')


ggarrange(p.categ, p.species)


# Get stats: 
triplet_sf %>%
  #select(-c(geometry)) %>% 
  as_tibble() %>%   # remove the geometry
  filter(Triplet %in% c('triplet', 'emg triplet', 'emg pair')) %>% 
  filter(Species %in% my_species) %>% 
  group_by(Species, Triplet, Paper_numb ) %>% 
  tally() %>%
  ungroup() %>% 
  group_by(Species, Triplet) %>% 
  tally() %>% 
  tidyr::spread(Species, n) #%>% 



# Export the file
st_write(triplet_sf, 'C:/Users/ge45lep/Documents/2021_Franconia_mortality/fieldData/final/filtered.gpkg',
         layer = 'spruce_beech')


library(data.table)
fwrite(triplet_sf, 'C:/Users/ge45lep/Documents/2021_Franconia_mortality/fieldData/final/filtered.csv')




# Plot data on interactive map --------------------------------------------

library(mapview)

# filter to commercial ports and active lighthouses
mapview(triplet_sf, 
        #col.regions="darkblue", 
        cex=5, 
        layer.name="Study sites", 
        pch=22) 
# Plot data by species -----------------------------------------------------------


# Filter firrst the data - plots by categories:
cat_sf <- merged_df %>% 
  filter(Category %in% c('C', 'L', "D"))


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
  filter(Species %in% my_species)


p.species <- ggplot() + 
  geom_sf(data = bav_sf, fill = 'grey') +
  geom_sf(data = species_sf, 
          aes(color = Species)) + 
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
  filter(Triplet %in% c('pair', 'emg pair', 'triplet', 'emg triplet', 'Triplet')) %>% 
  filter(Species %in% my_species) %>% 
  group_by(Species, Triplet, Paper_numb ) %>% 
  tally() %>%
  ungroup() %>% 
  group_by(Species, Triplet) %>% 
  tally() %>% 
  tidyr::spread(Species, n) #%>% 


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
  


          