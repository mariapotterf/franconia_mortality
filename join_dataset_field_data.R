

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
library(readxl)


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
df_att <- read.csv2(paste(path, 'raw/fieldWork/study_sites_2.csv', sep = '/'), 
                    encoding = "UTF-8")

# Select final datasets: the final triplets are indicated by the 'y'
# File made by Rupert on March 25, 2022
df_selected <- read_excel(paste(path, 'raw/fieldWork/all_sites_selected_RS.xlsx', sep = '/'))
# Changed the name of teh 

# Change data for the use --------------------------------------------------------


# Filter only for 'take' == 'y' to have a final paper number for which sites to take
# here I have manually changes 30.1 (double form the Arnds, oak, for #34)
selected_sites_v <- df_selected  %>%    # 37 sites
  #distinct(take) 
  dplyr::filter(take == "y") %>% 
  distinct(Paper_numb) %>% 
  pull()
  

selected_sites_v
# replaced 30.1 by the 34 - manually in the xlsx file from Rupert and in my database! 

# from meinolf Arndt: in total 3 triplets: 
# coordinates (manual): 30 (), 35, 30-1 (this one is a new one!)

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


# Check the projection of the GPS
# st_crs(my_gpx[[2]])

# Process data :

# merge all GPX data in one file
all_gps <- do.call("rbind", my_gpx)


#plot(all_gps)


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



# Remove names from GPX coordinates 
# 089 = Freising
# 134 = Fabrikschleichach

all_gps2 <- all_gps2 %>% 
  filter(!name %in% c("089", "134", "142", "143",
                      '105', '102', # from Hans Stark, duplicated D
                      '099', # Hans Stark, duplicated D
                      '114'  # missplaced from Kielheim ,repplace manually by 50
  )) 





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
  left_join(df_att, by = 'name')


# define filters
my_species = c('spruce', 'beech', 'oak', 'pine')



##############################
# Overall table - filtered
##########################


# Track effort:
#df_att %>% 
 # distinct(Forest_district)  # 45


#df_att %>% 
 # distinct(Contact) # 35 forest partners





# Clean table to export ----------------------------------------------------------

# Replace F (forest) by L (Living) as D (dead) is also a forest
# How many triplets by species we have?  # 37, after selection with Rupert and Jorg


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
  filter(!is.na(Triplet)) %>%
  dplyr::filter(Paper_numb %in% selected_sites_v )


# Order the species by levels
df_out <- df_out %>% 
  mutate(Species = factor(Species, 
                             levels = c('spruce',
                                        'beech',
                                        'oak',
                                        'pine')))

# add each row a unique number  
#df_out <- df_out %>% 
#  mutate(id = 1:nrow(df_out))


# Create a unique identified to add to each row: seems OK
# make sure that all triplets have 3 classes: C, D, L
# order by Paper_numb, check classes
# df_out %>% 
#   arrange(Paper_numb, Category) %>% 
#   select(Paper_numb, Category, Species, GPS.numb) %>% 
#   print(n = 120)
# 
df_out %>%
  group_by(Paper_numb) %>%
  count() %>%
  print(n = 40)


#############################################################
#
#                Site naming convention
#                     
#############################################################



# Make sure the each site has a combination of the LDC - live, dead, cleared;
# accordingly rename the sites: Changed for site n 71, site #
# create a naming convention:
# 65-O-D-T-069 - example:

# need to - arrange the data
# split into categories by species
# generate unique numbers for each triplets spruce(1-20), beech (21-40), oak (41-60), pine (61-80)
# first arrange the data

# get counts  of triplets by species: ----------------------------------------------
(n_spruce = 
  df_out %>% 
  filter(Species == 'spruce') %>% 
  distinct(Paper_numb) %>% 
    count() %>% 
    pull())


(n_beech = 
  df_out %>% 
  filter(Species == 'beech') %>% 
  distinct(Paper_numb) %>% 
  count() %>% 
  pull())


(n_oak = 
  df_out %>% 
  filter(Species == 'oak') %>% 
  distinct(Paper_numb) %>% 
  count() %>% 
  pull())


(n_pine = 
  df_out %>% 
  filter(Species == 'pine') %>% 
  distinct(Paper_numb) %>% 
  count() %>% 
  pull())


# Make individual df by species to generate a unique triplet number  ----------------------
df_spruce <- df_out %>% 
  filter(Species == 'spruce') %>% 
  arrange(Paper_numb) %>%
  mutate(trip_numb = rep(1:n_spruce, each = 3))
  

df_beech <- df_out %>% 
  filter(Species == 'beech') %>% 
  arrange(Paper_numb) %>%
  mutate(trip_numb = rep(1:n_beech, each = 3) + 20)


df_oak <- df_out %>% 
  filter(Species == 'oak') %>% 
  arrange(Paper_numb) %>%
  mutate(trip_numb = rep(1:n_oak, each = 3) + 40)


df_pine <- df_out %>% 
  filter(Species == 'pine') %>% 
  arrange(Paper_numb) %>%
  mutate(trip_numb = rep(1:n_pine, each = 3) + 60)


# Combine the partial tables into one, create a unique ID for each site
df_unique <- bind_rows(df_spruce, df_beech, df_oak, df_pine ) %>% 
   mutate(Triplet = 'T') %>% 
   mutate(roll_numb = 1:nrow(df_out)) %>%   # unique triplet number
   mutate(unique_ID = paste(trip_numb, 
                            Species, 
                            Category, 
                            Triplet, 
                            roll_numb, 
                            sep = '-')) %>% 
   select(-c(Date)) %>%
  rename(pap_num = Paper_numb, 
         district = Forest_district, 
         trip_n = trip_numb, 
         roll_n = roll_numb, 
         uniqID = unique_ID)
  
   #print(n = 130)



# Get  stats ---------------------------------------------------------------

df_unique %>% 
  as_tibble() %>% 
  filter(!is.na(Triplet)) %>%  # %in% c('pair', 'emg pair', 'triplet', 'emg triplet')) %>% 
  filter(Species %in% my_species) %>% 
  group_by(Species, Triplet, pap_num ) %>% 
  tally() %>%
  ungroup() %>% 
  group_by(Species, Triplet) %>% 
  tally() %>% 
  tidyr::spread(Species, n) #%>% 


# Separate geometry columns:  
df_unique_csv <- df_unique %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>% 
  st_drop_geometry()


write.csv2(df_unique_csv, 
           paste(path, 'fieldData/final', 'sites_unique_ID.csv', sep = "/")) 





# Export the file ------------------------------------------------------------------
# GPKG seems not working?
#st_write(df_unique, paste(path, 'fieldData/final', 'sites_unique_ID.gpkg', sep = "/"),
 #        layer = 'sites_unique_ID', append=FALSE)

st_write(df_unique, paste(path, 'fieldData/final', 'sites_unique_ID.shp', sep = "/"),
         append=FALSE)

#library(data.table)

# fwrite does not work with the correct encoding
# fwrite(df_out, paste(path, 'fieldData/final', 'all_sites.csv', sep = "/")) 
# Export at CSV: 


# Get corrector by the number of supbplots:
# Triplet has 3, pair has 2
# No need if I have group them first by estimated number of teh triplet/pairs



# -------------------------------------------
#                  Make maps   
# -------------------------------------------

# Filter first the data - plots by categories: 
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
mapview(df_out, 
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
  


          