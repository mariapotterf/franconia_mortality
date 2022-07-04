

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
#          get the share of teh planted
#          share of damaged and location of damage
# density:
#          mature trees: calculate based on the distance to nearest tree
# 
# 
# photo indication: north subplot
#                   south subplot
#                   north, east, south, west environment
# video indication:  north, east, south, west environment
# export individual dataframes for subsequent processing: shannon, ...


# Data structure -----------------------------------
# each record in the Table field has its own columns: ~ 600 columns
# Regeneration: tree regeneration is split across tree species, height classes and damage
# each tree species in height class: has 7 columns
# 6 heights levels: 6*7 = 42 columns per species
# 

#### Colnames interpretation --------------------------------------
# if 'env' - surroundings (environment)
# if not env - in the site 
# completed unique names for tree, species, etc.


# remove all previous data from R memory
rm(list=ls())


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




# Input data -------------------------------------------------------------------
#### Source paths and functions  -----------------------------------------------

source('myPaths.R')


#### Read libraries  -----------------------------------------------------------
library(readxl)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)



#### Read Regeneration data -----------------------------------------------------
dat1  <- read_excel(paste(myPath, inFolderFieldVeg, "Data_Week_3.xlsx", sep = '/'))
dat2  <- read_excel(paste(myPath, inFolderFieldVeg, "Data_Week_1-2.xlsx", sep = '/'))
dat3  <- read_excel(paste(myPath, inFolderFieldVeg, "Data_Week_4.xlsx", sep = '/'))


#### Get output tables
outRegen  = paste(myPath, outTable, 'df_regen.csv' , sep = '/')
outGround = paste(myPath, outTable, 'df_ground.csv', sep = '/')
outVideo  = paste(myPath, outTable, 'df_video.csv' , sep = '/')
outPhoto  = paste(myPath, outTable, 'df_photo.csv' , sep = '/')
  


## Clean input data -------------------------------------------------------------
# Read New headingsL: in ENG, with unique colnames 
# this was done manually in Excel
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




#### Correct mistakes/typos (found during processing): --------------------------------------
# need for quality data check! visually in the table, and correct in the script
# from 06/28/2022 -> correct directly in the files
#dat$trip_n <- replace(dat$trip_n, dat$trip_n == 644, 64) 
#dat$trip_n <- replace(dat$trip_n, dat$trip_n == 35 &  dat$dom_sp == 'beech', 32) 
#dat$trip_n <- replace(dat$dom_sp, dat$trip_n == 33 &  dat$dom_sp == 'spruce', 33) 
#dat$trip_n <- replace(dat$trip_n, dat$trip_n == 91, 61) 
#dat$trip_n <- replace(dat$trip_n, dat$trip_n == 2 & dat$dom_sp == 'pine', 62) 
#dat$sub_n  <- replace(dat$sub_n,  dat$sub_n == 24, 14) 


## Check for typos & Get basic statistic -----------------------------------------------------------
# check for triplets numabres, number of subset per site, ...
# sample patches by patch??
# correct then manually in the data themselves
dat %>% 
  group_by(trip_n, dom_sp) %>% 
  tally() %>% 
  arrange(dom_sp) %>% 
  print(n = 40)


 
# Get each category size:
dat_size  <- read_excel(paste(myPath, 'fieldData/02_select_sites MP_SK/final/share', 
                              "sites_unique_ID.xlsx", 
                              sep = '/'))

# keep only usefull columns
dat_size <- 
  dat_size %>% 
  select(Name, Area_m2) %>% 
  separate(Name, c('trip_n', 'dom_sp', 'type'), '-') %>% 
  mutate(type = tolower(type),
         trip_n = as.numeric(trip_n)) 



# Join category size with the vegetation data  ----------------------------

dat <- dat %>% 
  left_join(dat_size)

dat_size %>% 
  ggplot(aes(Area_m2/10000), fill = 'white', col = 'black') +
  geom_histogram(bins = 100) +
  theme_bw() +
  facet_grid(.~type)
  


## List important column names from raw table  -------------------------------------------


# Get columsn for photos:
photos_id <- c("north_subplot",
               "east_subplot",
               "north_environment",
               "east_environment",
               "south_environment",
               "west_environment")


# Get columss for the site identification
plot_info <- c(#"ObjectID",
               #"GlobalID",
               "trip_n",
               "dom_sp",
               "type",
               "sub_n"               )

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



#### Get ground cover shares: per category ------------------------------------------------------
df_ground0 <-   
  dat %>% 
  dplyr::select(matches(c(plot_info, "gc_"))) %>% 
  mutate(uniqueID = paste(trip_n,dom_sp,type,sub_n, sep = '_' )) %>% 
  dplyr::select(-c("trip_n","dom_sp","type",'sub_n')) 


df_ground <- 
  df_ground0 %>% 
  pivot_longer(!uniqueID, names_to = 'class', values_to = 'prop') %>% 
  mutate(class = gsub('gc_', '', class)) %>% # replace the name indicator
  separate(uniqueID, c('trip_n', 'dom_sp', 'type', 'sub_n'), '_')


#### Save the table ------------------------------------------------------------------
fwrite(df_ground, outGround)



# get the average values per category, not by the subplot:
df_ground_shann <-
  df_ground %>% 
  group_by(trip_n, dom_sp, type, class) %>% 
  summarize(mean_prop = mean(prop)/100) %>%  # for shannon, the scale should be 0-1, not 0-100% !! 
  mutate(shannon_part = replace_na(-(mean_prop)*log(mean_prop),0)) %>% 
    ungroup() %>% 
    group_by(trip_n, dom_sp, type) %>%
    summarize(shannon_ground = sum(shannon_part)) %>% 
    mutate(effective_n_ground = exp(shannon_ground))
    # calculate shannon and replace NA vals by 0
         #shannon = is.na())


##### check plot: ---------------------------------------------------------------------
windows()
df_ground_shann %>% 
  ggplot(aes(x = factor(type),
             y = effective_n_ground)) +
  geom_boxplot() +
  geom_violin()

windows()
df_ground_shann %>% 
  ggplot(aes(x = factor(type),
             y = shannon_ground,
             fill = dom_sp)) +
  geom_violin() 


# Get the differences between 
# Test: does the management type affects the diversity of th ground cover?
# HO ; the Cleared area will be more homogenous then the Dead ones:
# clearing homogenize teh gropund cover (prevalence of grasses, ...)





# Get Regeneration data  -----------------------------------------------------

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
  mutate(uniqueID = paste(trip_n,dom_sp,type,sub_n, sep = '_' )) %>% 
  dplyr::select(-c("trip_n","dom_sp","type",'sub_n')) %>%
  dplyr::select_if(function(col) all(col == .$uniqueID) | is.numeric(col)) # select the numeric columns and the siteID (character)

# convert from wide to long to calculate the counts by height classes:
df_regen <- df_regen0 %>% 
  pivot_longer(!uniqueID, names_to = 'type', values_to = 'count') %>% 
  separate(type, c('species', 'height_class'), '_') %>% 
  separate(uniqueID, c('trip_n', 'dom_sp', 'type', 'sub_n'), '_')



# Replace the regeneration height classed by estimated heights:

df_regen <- df_regen %>% 
  #rename(height = height_class) %>%   # get heights in cm
 # mutate(height = case_when(height == "HK1" ~ 30,
 #                           height == "HK2" ~ 50,
#                            height == "HK3" ~ 70,
 #                           height == "HK4" ~ 90,
#                            height == "HK5" ~ 115,
 #                           height == "HK6" ~ 175
  #)) %>% 
  mutate(DBH = 0.05) %>% 
  filter(!count == 0) %>% # remove if there is no species present
  mutate(reg_height = 'seedlings')



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
    mutate(uniqueID = paste(trip_n,dom_sp,type,sub_n, sep = '_' )) %>% 
    dplyr::select(-c("trip_n","dom_sp","type",'sub_n')) %>%
    mutate(tree_numb = x) %>% 
    filter(complete.cases(.)) # remove the rows that contains NA values
  
  # Rename the columns:
  colnames(dat_reg) <- c('species', 'DBH', 'height', 'uniqueID', 'tree_numb')
  
  # Return the df from teh function
  return(dat_reg)
 
}

##### Create a vectors of values to subset Advanced regen columns form the datatable ----------
x <- c(1:8)
# allply the function to get the whole list of the data
ls_advanced <- lapply(x, get_adv_regen)

# Convert dataframe from list into the single dataframe
df_advanced <- do.call(rbind, ls_advanced)


# Replace names of tree species:
df_advanced <- df_advanced %>% 
  mutate(species = case_when(species == "Esche"        ~ "Ash",
                             species == "Sonstiges NH" ~ "OtherSoftwood",
                             species == "Sonstiges LH" ~ "OtherHardwood",
                             species == "Buche"        ~ "Beech" ,
                             species == "Vogelbeere"   ~ "Rowan",
                             species == "Bergahorn"    ~ "Maple",
                             species == "Fichte"       ~ "Spruce",
                             species == "Eiche"        ~ "Oak",
                             species == "Kiefer"       ~ "Pine",
                             species == "Birke"        ~ "Birch",
                             species == "Weide"        ~ "Willow",
                             species == "Tanne"        ~ "Fir"
                                                ))

# Replace 'tree number' (originally 1-8) by 1: 
# to corresponds to counts, not to the order of recording :

# check for the low values:
df_advanced %>% filter(height <200)  # change the values to cm in next step


#   species   DBH height uniqueID      tree_numb
#<chr>   <dbl>  <dbl> <chr>             <int>
#  1 Beech       9     10 64_pine_l_1           1
#  2 Beech       8     13 31_beech_c_10         1
#  3 Beech       8     18 32_beech_d_5          1
#  4 Beech       4      6 26_beech_c_1          2


df_advanced2 <- 
  df_advanced %>% 
  # replace the values: some have been recorded in meters instead of cm
  mutate(height = case_when(height< 200 ~ height*100,
                            TRUE ~ height)) %>% 
  mutate(height_class = "HK7") %>% 
  mutate(tree_numb = 1) %>% # # Replace 'tree number' (originally 1-8) by 1: 
                                # to corresponds to counts, not to the order of recording :
  rename(count = tree_numb) %>% 
  separate(uniqueID, c('trip_n', 'dom_sp', 'type', 'sub_n'), '_') %>% 
  mutate(reg_height = 'saplings') %>% 
  dplyr::select(colnames(df_regen))

  

##### Rbind regeneration data into single dataframe: --------------------------------
df_regen_fin <- rbind(df_regen, df_advanced2)

# Save teh table 
fwrite(df_regen_fin, outRegen)



# Calculate Shannon per subsite?? 
# then I can condider the individual patches as random effects in the model
df_reg_fin_by_subsample <- 
  df_regen_fin %>% 
  select(-c(height_class, DBH, reg_height)) %>% 
  arrange(trip_n, type) %>%
  group_by(trip_n, dom_sp, type, sub_n, species) %>% 
  summarize(count_sp        = sum(count))  %>% 
    ungroup() %>% 
    group_by(trip_n, dom_sp, type, sub_n) %>% 
    mutate(dens_tot        = sum(count_sp),
           sp_pi           = count_sp/dens_tot,
           shannon_part_sp = sp_pi*log(sp_pi),
           shannon_sp      = -sum(shannon_part_sp),
           eff_numb_sp     = exp(shannon_sp))# %>%
 #   print(n = 40)
    
    

    
 # Calculate Shannon height diversity ??? -------------
#df_reg_fin_by_subsample_height <- 
  df_regen_fin %>% 
  arrange(trip_n, type) %>%
  group_by(trip_n, dom_sp, type, sub_n) %>% 
  mutate(dens_tot        = sum(count),
         sp_pi           = count/dens_tot,
         shannon_part_sp = sp_pi*log(sp_pi),
         shannon_sp      = -sum(shannon_part_sp),
         eff_numb_sp     = exp(shannon_sp))# %>% 



# evaluate teh dominance of species: dominant: if more then 50% of stems



# How does the height structure looks like?
df_reg_fin_by_subsample %>% 
  ggplot(aes(y = height,
             x = factor(type),
             fill = dom_sp)) +
  geom_violin()




# keep only distinct rows for mixed effects:

dat <- df_reg_fin_by_subsample %>% 
  mutate(uniqueID = factor(paste(trip_n,dom_sp,type,sub_n, sep = '_' ))) %>% 
  dplyr::select(uniqueID, shannon, eff_numb) %>% 
  distinct() #%>% 
  



# How does management affect the dievrsity of species composition?
dat$type <- as.factor(dat$type)
dat$dom_sp <- as.factor(dat$dom_sp)

m1 <- lme(eff_numb~ type, random = ~1|uniqueID, data = dat)

summary(m1)

m2 <- lme(eff_numb~ type + dom_sp, random = ~1|uniqueID, data = dat)

summary(m2)
AIC(m1, m2)

hist(dat$eff_numb)

# use glm and poisson family:
m3 <- glm(eff_numb~ type + dom_sp, family = poisson(link = "log"), # poisson family
          data = dat)

AIC(m3)
summary(m3)

# Define sample area per patch - correct the hdensity/ha estimation----------------
# calculate from the original data table
subsample_n <- 
  dat %>%
  group_by(trip_n , dom_sp , type ) %>% 
  distinct(sub_n ) %>% 
  tally() %>%
  mutate(trip_n = as.character(trip_n))
 

# Get tree densiy/ha across all heights:
df_reg_dens <- 
  df_regen_fin %>% 
  left_join(subsample_n) %>% 
  ungroup() %>% 
  group_by(trip_n, dom_sp, type, n, reg_height, species) %>% 
  summarize(reg_count = sum(count, na.rm = T) )  %>%
    mutate(density_ha = reg_count/n/4*10000) #%>% 


# Calculate shannon for individual subsites
# get the total density
# calculate the share per species - pi
# calculate Shannon: H = -sum(pi*log(pi)) for each species
# values for Shannon have to be in 0-1 range (not 0-100)!
df_reg_dens_shannon <- 
  df_reg_dens %>% 
  group_by(trip_n, dom_sp, type) %>% 
  mutate(dens_tot = sum(density_ha),
         sp_pi = density_ha /dens_tot,
         shannon_part = sp_pi*log(sp_pi)) %>% 
  summarize(shannon = -sum(shannon_part)) %>% 
  mutate(effective_n_sp = exp(shannon))# %>% 
  
df_reg_dens_shannon %>% 
ggplot(aes(y = effective_n_sp,
             x = type)) +
  geom_boxplot() + 
 facet_grid(~dom_sp, scales = 'free') #+
  #ylab('density \n(#trees/ha)') 

 

# Does the management explain the tree regen diversity?? --------------------------------------
m1<-lm(effective_n_sp~1, df_reg_dens_shannon)  # this ignores the management, and cosider the treatmements as independent
summary(m1)

# only predict the mean effective number: 2.6351     
coef(m1)
# (Intercept) 
# 2.63515 

confint(m1)
#               2.5 %   97.5 %
#  (Intercept) 2.237458 3.032842

# Mixed effect model: 
# can I keep in the small patches, and evaluate them as a group within the treatments? e.g. subsites within the patch will
# be more similar that patches further away
library(nlme)
m3 <- lme(effective_n_sp~1, df_reg_dens_shannon)
summary(m3)




    

df_reg_dens %>% 
ggplot(aes(y = density_ha,
               x = type)) +
    geom_boxplot() + 
    facet_grid(reg_height~dom_sp, scales = 'free') +
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







# Investigate data by categories --------------------------------------------------------------


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
  
  
  