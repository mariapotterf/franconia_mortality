

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
dat1  <- read_excel(paste(myPath, inFolderFieldVeg, "Data_Week_3.xlsx", sep = '/'))
dat2  <- read_excel(paste(myPath, inFolderFieldVeg, "Data_Week_1-2.xlsx", sep = '/'))
dat3  <- read_excel(paste(myPath, inFolderFieldVeg, "Data_Week_4.xlsx", sep = '/'))


# Read New headingsL: in ENG, with unique colnames
# this was done manually in Excell
EN_heading <- read_excel(paste(myPath, 
                               inFolderFieldVeg, 
                                "col_names_ENG.xlsx", sep = '/'), 
                          sheet = "en_name")  # sheet name
# check if teh colnames are equal??
names(dat1) == names(dat2)
names(dat1) == names(dat3)

# If the names are the same, we can bind them together

# check in excel??
#EN_heading_check <- read_excel(paste(myPath, 
#                               inFolderFieldVeg, 
#                               "col_names_ENG.xlsx", sep = '/'), 
#                         sheet = "check_names_over_weeks")  # sheet name

dat <- rbind(dat1, dat2, dat3)


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
# need for qualit data check! visually in the table, and correct in the script
# from 06/28/2022 -> correct directly in the files
#dat$triplet_no <- replace(dat$triplet_no, dat$triplet_no == 644, 64) 
#dat$triplet_no <- replace(dat$triplet_no, dat$triplet_no == 35 &  dat$triplet_type == 'beech', 32) 
#dat$triplet_no <- replace(dat$triplet_type, dat$triplet_no == 33 &  dat$triplet_type == 'spruce', 33) 
#dat$triplet_no <- replace(dat$triplet_no, dat$triplet_no == 91, 61) 
#dat$triplet_no <- replace(dat$triplet_no, dat$triplet_no == 2 & dat$triplet_type == 'pine', 62) 
#dat$Subplot_no <- replace(dat$Subplot_no, dat$Subplot_no == 24, 14) 


# Check for typos & Get basic statistic -----------------------------------------------------------
# check for triplets numabres, number of subset per site, ...
# sample patches by patch??
# correct then manually in the data themselves
dat %>% 
  group_by(triplet_no, triplet_type) %>% 
  tally() %>% 
  arrange(triplet_type) %>% 
  print(n = 40)





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
               "Subplot_no"               )

plot_geo <- c("gradient",
              "exposure")

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


# get the avergae values per category, not by the subplot:
df_ground_shann <-
  df_ground %>% 
  group_by(trip_n, dom_sp, type, class) %>% 
  summarize(mean_prop = mean(prop)/100) %>%  # for shannon, the scale should be 0-1, not 0-100% !! 
  mutate(shannon_part = replace_na(-(mean_prop)*log(mean_prop),0)) %>% 
    ungroup() %>% 
    group_by(trip_n, dom_sp, type) %>%
    summarize(shannon_ground = sum(shannon_part)) %>% 
  mutate(shannon.entropy = exp(shannon_ground))
    # calculate shannon and replace NA vals by 0
         #shannon = is.na())


df_ground_shann %>% 
  ggplot(aes(x = factor(type),
             y = shannon_ground,
             fill = dom_sp)) +
  geom_boxplot() 



summary(df_ground_shann)
  
# test shannon: ----------------------------------------------------------------
# what is the effective number of species????
  
d <- data.frame(id = c(1,1,1,1),
                spec_pi= c(30,10,5,50)/100)  # roportions ahould be scaled 0-1, not 0-100!! for shannon
  
d$shannon = d$spec_pi*log(d$spec_pi)  

d %>% 
  mutate(pi_ln           = abs(log(spec_pi))) %>% 
  mutate(sp_shannon      = spec_pi*pi_ln ) %>% 
  mutate(shannon_comun   = sum(sp_shannon)) #%>% 
  
    



## Get Regeneration data  -----------------------------------------------------

# The regeneration is combined regeneration and advanced regeneration!

# Extract data in several steps:
# for regen - seedlings
# for advanced regen - samplings??
# steps: 
#     select the columns
#     convert them from wide to long format

### For regeneration: seedlings ---------------------------------------------------
# select only columns with Regeneration:
# columns are in different class (logi, num, char), some contains NEIN: characters: need only counts!!!
# if the count is not present, than it is 0

# Subset counts for regeneration datasets
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


### For Advanced regen: samplings  ---------------------------------------------------

# Extract the columns by the '1_Advanced_' indication and then merge them into one database:
# otherwise,not sure how to do it

# subsets counts for the Advanced regeneration (>2m up to 10 cm dbh)
# for now, max 8 advanced regen trees/subset

get_adv_regen <- function(x, ...) {
  reg_name = paste(x, '_Advanced', sep = '')
  
  # Create a table to subset specific columns
  dat_reg <- dat %>% 
    dplyr::select(matches(paste(c(plot_info, reg_name), collapse = '|'))) %>% 
    dplyr::select(!matches(c('env_','_bin'))) %>% 
    mutate(uniqueID = paste(triplet_no,triplet_type,surface_type,Subplot_no, sep = '_' )) %>% 
    dplyr::select(-c("triplet_no","triplet_type","surface_type",'Subplot_no')) %>%
    mutate(tree_numb = x) %>% 
    filter(complete.cases(.)) # remove the rows that contains NA values
  
  # Rename the columns:
  colnames(dat_reg) <- c('tree_species', 'DBH', 'height', 'uniqueID', 'tree_numb')
  
  # Return the df from teh function
  return(dat_reg)
 
}

##### Create a vectors of values to subset columns form the datatable ----------
x <- c(1:8)
# allply the function to get the whole list of the data
ls_advanced <- lapply(x, get_adv_regen)

lapply(ls_advanced, function(df) unique(df$tree_species))

# Convert dataframes from list into the single dataframe
df_advanced <- do.call(rbind, ls_advanced)


# Replace names of tree species:
df_advanced <- df_advanced %>% 
  mutate(tree_species = case_when(tree_species == "Esche"        ~ "Ash",
                                  tree_species == "Sonstiges NH" ~ "OtherSoftwood",
                                  tree_species == "Sonstiges LH" ~ "OtherHardwood",
                                  tree_species == "Buche"        ~ "Beech" ,
                                  tree_species == "Vogelbeere"   ~ "Rowan",
                                  tree_species == "Bergahorn"    ~ "Maple",
                                  tree_species == "Fichte"       ~ "Spruce",
                                  tree_species == "Eiche"        ~ "Oak",
                                  tree_species == "Kiefer"       ~ "Pine",
                                  tree_species == "Birke"        ~ "Birch",
                                  tree_species == "Weide"        ~ "Willow",
                                  tree_species == "Tanne"        ~ "Fir"
                                                ))



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
  
  
  