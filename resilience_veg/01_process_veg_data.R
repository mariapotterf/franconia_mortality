

# Read vegetation data 
# collected during field work 2022
# by Juri Majerand and Sophia Storbeck

# Process: -----------------------------
# Read the data by weeks
# order the data by the columns by the german names: master name is Week 3 [ENG]
# bin the data by rows
# replace by ENG names to creates groups of data: 
#         inner:
#            - ground cover, regen, advanced regen
#         outer:
#            - distance to trees, tree density 

# Output: ------------------------------
# get statistics per patches (relative to sample area) - sometimes 5 subsites, sometimes 15,...
# recalculate the values to hectare:
# density: regen
#          advanced regen
#          mature trees


# Data structure -----------------------------------
# each record in the Table field has its own columns: ~ 600 columns
# Regeneration: tree regeneration is split across tree species, height classes and damage
# each tree species in height class: has 7 columns
# 6 heights levels: 6*7 = 42 columns per species
# 

# Colnames interpretation --------------------------------------
# if 'env' - surroundings (environment)
# if not env - in the site 
# completed unique names for tree, species, etc.

# Regeneration Tree species (12 species in total):
# 
#Spruce
#Beech
#Rowan
#Fir
#Oak
#Maple
#Birch
#Willow
#Pine
#Ash
#Other hardwood
#Other softwood 

reg_trees <- c(
  'Spruce',    # Picea abies
  'Beech',     # Fagus sylvatica
  'Rowan',     # Sorbus aucuparia ?
  'Fir',       # Abies alba
  'Oak',       # Quercus robur  
  'Maple',     # Acer pseudoplatanus
  'Birch',     # Betula pendula
  'Willow',    # Salix caprea ??
  'Pine',      # Pinus sylvestris
  'Ash',       # Fraxinus excelsior 
  'OtherHardwood',
  'OtherSoftwood'
)


# dominant trees in germany: https://www.forstwirtschaft-in-deutschland.de/index.php?id=52&L=1
# spruce 26
# pine 22.9
# beech 15.8
# oak 10.6
# larch 2.9
# douglas fir 2
# fir 1.7
# other deciduous high life 7.2
# other deciduous lower life: 10.8



# height classes:
reg_trees_heights <- c(
  'HK1', # 0.2-0.4 m 
  'HK2', # 0.4-0.6 m
  'HK3', # 0.6-0.8 m
  'HK4', # 0.8-1.0 m
  'HK5', # 1.0-1.3 m
  'HK6'  # 1.3-2.0 m
)


# 
# Source paths and functions  ----------------------------------------------------------------------

source('myPaths.R')


# Read libraries  -------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)

# Read data 
dat1  <- read_excel(paste(myPath, inFolderFieldVeg, "Data Week 3.xlsx", sep = '/'))
dat2  <- read_excel(paste(myPath, inFolderFieldVeg, "Data_Week_1-2.xlsx", sep = '/'))


# Read New headingsL: in ENG, with unique colnames
# this was done manually in Excell
EN_heading <- read_excel(paste(myPath, 
                               inFolderFieldVeg, 
                                "col_names_ENG.xlsx", sep = '/'), 
                          sheet = "en_name")  # sheet name
# check if teh colnames are equal??
names(dat1) == names(dat2)

# If the names are the same, we can bind them together

# check in excel??
#EN_heading_check <- read_excel(paste(myPath, 
#                               inFolderFieldVeg, 
#                               "col_names_ENG.xlsx", sep = '/'), 
#                         sheet = "check_names_over_weeks")  # sheet name

dat <- rbind(dat1, dat2)


# Check if the columns lenght is the same:
ncol(dat)         # 617
nrow(EN_heading)  # 617

# Remove the spaces from the En_heading, replace some not important characters
EN_col_names <- gsub(' ', '_', EN_heading$ENG)
EN_col_names <- gsub('_in_', '_', EN_col_names)
EN_col_names <- gsub('\\.', '',     EN_col_names) # . means any character, so need to add \\


# Replace the naming to have unique name for each colums 
# and be able to filter throught them
colnames(dat) <- EN_col_names




# Correct mistakes/typos (found during processing): --------------------------------------
dat$triplet_no <- replace(dat$triplet_no, dat$triplet_no == 644, 64) 
dat$Subplot_no <- replace(dat$Subplot_no, dat$Subplot_no == 24, 14) 



# List important variables from raw table  -------------------------------------------


# Get columsn for photos:
photos_id <- c("North_subplot",
               "east_subplot",
               "north_environment",
               "east_environment",
               "south_environment",
               "west_environment")


# Get columss for the site identification
plot_info <- c(#"ObjectID",
               #"GlobalID",
               "triplet_no",
               "triplet_type",
               "surface_type",
               "Subplot_no"
               #"gradient",
               #"exposure"
               )

# get columns for ground cover: in %: marked by 'gc_' in names
# ground_cover <- c("Mature_Trees",
#                   "rejuvenation",
#                   "shrub_layer",
#                   "mosses",
#                   "ferns",
#                   "herb_layer",
#                   "grasses",
#                   "soil/foliage",
#                   "rock",
#                   "deadwood/stumps")


# Get ground cover shares ------------------------------------------------------
df_ground0 <-   
  dat %>% 
  dplyr::select(matches(c(plot_info, "gc_"))) %>% 
  mutate(uniqueID = paste(triplet_no,triplet_type,surface_type,Subplot_no, sep = '_' )) %>% 
  dplyr::select(-c("triplet_no","triplet_type","surface_type",'Subplot_no')) 


df_ground <- 
  df_ground0 %>% 
  pivot_longer(!uniqueID, names_to = 'class', values_to = 'prop') %>% 
  mutate(class = gsub('gc_', '', class)) %>% # replace the name indicator
  separate(uniqueID, c('trip_n', 'dom_sp', 'type', 'sub_n'), '_')



# Get basic statistic -----------------------------------------------------------
# how my triplets?
# type?
# size?
# sample patches by patch??

summary(dat)



# Get Regeneration data  -----------------------------------------------------

# select only columns with regeneration:
# columns are in different class (logi, num, char), some contains NEIN: characters: need only counts!!!
# if the count is not present, than it is 0

# Sebset counts for regeneration datasets
df_regen0 <- dat %>% 
  dplyr::select(matches(paste(c(plot_info, reg_trees), collapse = '|'))) %>% 
  dplyr::select(!matches("Number")) %>% 
  mutate(uniqueID = paste(triplet_no,triplet_type,surface_type,Subplot_no, sep = '_' )) %>% 
  dplyr::select(-c("triplet_no","triplet_type","surface_type",'Subplot_no')) %>%
  dplyr::select_if(function(col) all(col == .$uniqueID) | is.numeric(col)) # select the numeric columns and the siteID (character)


# convert from wide to long to calculate the counts by height classes:
df_regen <- df_regen0 %>% 
  pivot_longer(!uniqueID, names_to = 'type', values_to = 'count') %>% 
  separate(type, c('species', 'height_class'), '_') %>% 
  separate(uniqueID, c('trip_n', 'dom_sp', 'type', 'sub_n'), '_')


# Define sample area per patch - correct the hdensity/ha estimation
subsample_n <- df_regen %>%
  group_by(trip_n, dom_sp, type) %>% 
  distinct(sub_n) %>% 
  tally() 

# Get counts of trees across all heights:
#df_reg_dens <- 
  df_regen %>% 
    left_join(subsample_n) %>% 
  group_by(trip_n, dom_sp, type, n) %>% 
  summarize(reg_count = sum(count, na.rm = T) )  %>%
    mutate(density_ha = reg_count/n/4*10000) %>% 
    ggplot(aes(y = density_ha,
               x = type)) +
    geom_boxplot() + 
    #facet_grid(~dom_sp) +
    ylab('density \n(#trees/ha)')
  
  
  
# Get community weighted means ---------------------------------------------
# https://rpubs.com/CPEL/cwm
  


### get density/ha -----------------------------------
### Join the number of subsamples to regen data:
# Calculate the density from the average counts, or from the sum of the trees/sampled area???
# check both approaches and see the difference?
# for density estimation, ignote the height classes
# keep the grain at the subsite lavel - to keep the variability between sites (!) 
# Get the counts per 4 m2



  
  
  

# Link data: ground cover (deadwood) and density? -------------------------------------------------







# Investigate data by plots --------------------------------------------------------------


# Check density distribution:

df_regen %>% 
  filter(count != 0) %>% # need to filter zero counts first
  group_by(trip_n, dom_sp, type) %>% 
  ggplot(aes(y = density_ha,
             x = type)) +
  geom_boxplot() + 
  facet_grid(~dom_sp) +
  ylab('density \n(#trees/ha)')




# calculate richness:
# count number of species per plot
#richness <- 
  df_regen %>% 
  filter(count != 0) %>% # need to filter zero counts first
  group_by(trip_n,dom_sp, type) %>% 
  distinct(species) %>% 
    count() %>% 
    ggplot(aes(y = n,
               x = type)) +
    geom_boxplot() + 
   # geom_jitter() +
    facet_grid(~dom_sp) +
    ylab('richness \n(# of tree species)')
  
  
# What is regeneration height??
df_regen %>% 
  filter(count != 0)  %>% # need to filter zero counts first
  group_by(trip_n,dom_sp, type) %>% 
#  distinct(species) %>% 
#  count() %>% 
  ggplot(aes(y = count,
               x = height_class,
             fill = type)) +
    geom_boxplot() + 
    # geom_jitter() +
    facet_grid(.~dom_sp) #+
    #ylab('richness \n(# of tree species)')
  
  
  