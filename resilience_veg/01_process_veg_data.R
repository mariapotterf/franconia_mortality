

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
library(stringr)  # use regex expressions



#### Read Regeneration data -----------------------------------------------------
dat1  <- read_excel(paste(myPath, inFolderFieldVeg, "Data_Week_3.xlsx", sep = '/'))
dat2  <- read_excel(paste(myPath, inFolderFieldVeg, "Data_Week_1-2.xlsx", sep = '/'))
dat3  <- read_excel(paste(myPath, inFolderFieldVeg, "Data_Week_4.xlsx", sep = '/'))


#### Get output tables
outRegen          = paste(myPath, outTable, 'df_regen.csv'          , sep = '/')  # contains infor of plantation& damage
outRegenAdvanced  = paste(myPath, outTable, 'df_regen_advanced.csv' , sep = '/')
outMatureENV         = paste(myPath, outTable, 'df_mature_trees_env.csv'   , sep = '/')

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
EN_col_names <- gsub(' ', '_',    EN_heading$ENG)
EN_col_names <- gsub('_in_', '_', EN_col_names)
EN_col_names <- gsub('\\.', '',   EN_col_names) # . means any character, so need to add \\


# Replace the naming to have unique name for each colums 
# and be able to filter throught them
colnames(dat) <- EN_col_names


# Create unique ID per site:
dat <- dat %>% 
  mutate(uniqueID = paste(trip_n,dom_sp,manag,sub_n, sep = '_' )) #%>%



#### Correct mistakes/typos (found during processing): --------------------------------------
# need for quality data check! visually in the table, and correct in the script
# from 06/28/2022 -> correct directly in the files
#dat$trip_n <- replace(dat$trip_n, dat$trip_n == 644, 64) 
#dat$trip_n <- replace(dat$trip_n, dat$trip_n == 35 &  dat$dom_sp == 'beech', 32) 
#dat$trip_n <- replace(dat$dom_sp, dat$trip_n == 33 &  dat$dom_sp == 'spruce', 33) 
#dat$trip_n <- replace(dat$trip_n, dat$trip_n == 91, 61) 
#dat$trip_n <- replace(dat$trip_n, dat$trip_n == 2 & dat$dom_sp == 'pine', 62) 
#dat$sub_n  <- replace(dat$sub_n,  dat$sub_n == 24, 14) 


## Check for typos & Get basic statistic ---------------------------------------
# check for triplets numbers, number of subset per site, ...
# sample patches by patch??
# correct then manually in the data themselves
dat %>% 
  group_by(trip_n, dom_sp) %>% 
  tally() %>% 
  arrange(dom_sp) %>% 
  print(n = 40)


 
# Get each category size ----------------------------------------------------------------
dat_size  <- read_excel(paste(myPath, 'fieldData/sites_identification/final/share', 
                              "sites_unique_ID.xlsx", 
                              sep = '/'))

# keep only useful columns
dat_size <- 
  dat_size %>% 
  select(Name, Area_m2) %>% 
  separate(Name, c('trip_n', 'dom_sp', 'manag'), '-') %>% 
  mutate(manag = tolower(manag),
         trip_n = as.numeric(trip_n)) 



# Join category size with the vegetation data  -------------------------------------------
dat <- dat %>% 
  left_join(dat_size)

dat_size %>% 
  ggplot(aes(Area_m2/10000), fill = 'white', col = 'black') +
  geom_histogram(bins = 100) +
  theme_bw() +
  facet_grid(.~manag)
  


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
               "manag",
               "sub_n"               )  

# get geographic information
plot_geo <- c("gradient",
              "exposure")



# Export tables for photos & videos  --------------------------------------------------

# Get ID with indication of teh photo number
df_photo <-   
  dat %>% 
  dplyr::select(matches(c(plot_info, photos_id, 'uniqueID'))) %>% 
  dplyr::select(-plot_info)  %>%
  pivot_longer(!uniqueID, names_to = 'class', values_to = 'photo_number')  %>%
  separate(class, c('orientation', 'site'), '_') %>% 
  separate(uniqueID, all_of(plot_info), '_')


#### Save the table 
fwrite(df_photo, outPhoto)


# Get ID with indication of the photo number
df_video <-   
  dat %>% 
  dplyr::select(matches(paste(c(plot_info, 'Video', 'uniqueID'), collapse = '|'))) %>% 
  dplyr::select(-c("trip_n","dom_sp","manag",'sub_n'))  %>%
  dplyr::select_if(function(col) all(col == .$uniqueID) | is.numeric(col)) %>%  # select uniqueID and only numeric columns
  pivot_longer(!uniqueID, names_to = 'class', values_to = 'video_number')  %>%
  filter(complete.cases(.)) %>%  # remove the rows that contains NA values 
  separate(class, c('rec_manag', 'site', 'orientation'), '_') %>% 
  separate(uniqueID, c('trip_n', 'dom_sp', 'manag', 'sub_n'), '_')


#### Save the video df 
fwrite(df_video, outVideo)







# Get ground cover shares: per category ------------------------------------------------------
df_ground <-   
  dat %>% 
  dplyr::select(matches(c(plot_info, "gc_", 'uniqueID'))) %>% 
  dplyr::select(-all_of(plot_info)) %>% 
  pivot_longer(!uniqueID, names_to = 'class', values_to = 'prop') %>% 
  mutate(class = gsub('gc_', '', class)) %>% # replace the name indicator
  separate(uniqueID, all_of(plot_info), '_')


#### Save the ground cover table 
fwrite(df_ground, outGround)







# Get Regeneration data  -----------------------------------------------------

# The regeneration is combined regeneration and advanced regeneration!

# Extract data in several steps:
# for regen - seedlings
# for advanced regen - samplings??
# steps: 
#     select the columns
#     convert them from wide to long format
# planted counts area the counts from the total counts



### For regeneration: seedlings ---------------------------------------------------
# select only columns with Regeneration:
# columns are in different class (logi, num, char), some contains NEIN: characters: need only counts!!!
# if the count is not present, than it is 0

# Subset counts for regeneration dataset
# this number indicates all of teh regeneration: included planted individuals
df_regen <- dat %>% 
  dplyr::select(matches(paste(c(plot_info, reg_trees, 'uniqueID'), collapse = '|'))) %>% 
  dplyr::select(!matches("Number")) %>% 
  dplyr::select(-all_of(plot_info)) %>% 
  dplyr::select_if(function(col) all(col == .$uniqueID) | is.numeric(col)) %>%  # select the numeric columns and the siteID (character)
  pivot_longer(!uniqueID, names_to = 'manag', values_to = 'n_total') %>% 
  separate(manag, c('reg_species', 'height_class'), '_') %>% 
  separate(uniqueID, all_of(plot_info), '_') #%>%
  #filter(n_total !=0)  # keep 0s to have the all oiverview of the total species
 # mutate(origin = 'natural')



## Counts for planted data   -----------------------------------------------------
# counts of planted data is the part of total counts:
# make sure that is true! the number of planted should never be higher than count_tot = total number of regeneration
df_regen_planted <- 
  dat %>% 
  dplyr::select(matches(paste(c(plot_info, 'Planted', 'uniqueID'), collapse = '|'))) %>%
  dplyr::select(-all_of(plot_info)) %>% 
  dplyr::select_if(function(col) all(col == .$uniqueID) | is.numeric(col)) %>% # select the numeric columns and the siteID (character)
  pivot_longer(!uniqueID, names_to = 'manag', values_to = 'n_planted') %>%          # convert to long format
  mutate(manag = gsub('Number_of_planted_individuals', 'planted', manag)) %>%   # replace string pattern to simplify names
  separate(manag, c('reg_species', 'height_class', 'origin'), '_') %>% 
  dplyr::select(-c('origin')) %>% 
  separate(uniqueID, all_of(plot_info), '_') %>% 
  filter(complete.cases(.)) 
  

## Get the damage data & position:
# can I see if teh damage was on planted or on naturally regenerated one?
df_regen_damaged <-
  dat %>% 
  dplyr::select(matches(paste(c(plot_info, 'Damage'), collapse = '|'))) %>%
  mutate(uniqueID = paste(trip_n,dom_sp,manag,sub_n, sep = '_' )) %>% 
    dplyr::select(-all_of(plot_info)) %>% 
    dplyr::select_if(function(col) all(col == .$uniqueID) | is.numeric(col)) %>% # select the numeric columns and the siteID (character)
    pivot_longer(!uniqueID, 
                 names_to = 'dam_manag', 
                 values_to = 'n_damage')  %>%          # convert to long format
   dplyr::mutate(dam_manag = str_replace_all(dam_manag, 
                                      c("leading_drive_damage" = "leader", 
                                        "other_damage"         = "other", 
                                        "combined_damage"      = "combined")))  %>% 
    dplyr::mutate(dam_manag = gsub('Number_of_', '', dam_manag)) %>% 
    separate(uniqueID, c('trip_n', 'dom_sp', 'manag', 'sub_n'), '_') %>% 
    separate(dam_manag, c('reg_species', 'height_class', 'damage_place'), '_')  %>%
    filter(complete.cases(.)) %>% 
    filter(n_damage !=0)
  
  


# Merge the regeneration data to know if damage was on planted/naturally regenerated trees?
# !!! test merge data planted & total: to see if teh 'planted' counts is part of regeneration?
df_reg_full <- df_regen %>% 
  left_join(df_regen_planted)  %>% 
  left_join(df_regen_damaged)  %>% 
  filter(n_total !=0)  # exclude 0 if no regeneration was collected

df_reg_full %>% 
  filter(trip_n == 24)  %>% 
  print(n = 60)

  

# Export df regeneration --------------------------------------------------
fwrite(df_reg_full, outRegen)
 







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
    mutate(uniqueID = paste(trip_n,dom_sp,manag,sub_n, sep = '_' )) %>% 
    dplyr::select(-c("trip_n","dom_sp","manag",'sub_n')) %>%
    mutate(tree_numb = x) %>% 
    filter(complete.cases(.)) # remove the rows that contains NA values
  
  # Rename the columns:
  colnames(dat_reg) <- c('reg_species', 'DBH', 'height', 'uniqueID', 'tree_numb')
  
  # Return the df from teh function
  return(dat_reg)
 
}

##### Create a vectors of values to subset Advanced regen columns form the datatable ----------
x <- c(1:8)
# allply the function to get the whole list of the data
ls_advanced <- lapply(x, get_adv_regen)

# Convert dataframe from list into the single dataframe
df_advanced <- do.call(rbind, ls_advanced)


# Replace names of tree reg_species:
df_advanced <- df_advanced %>% 
  mutate(reg_species = case_when(reg_species == "Esche"        ~ "Ash",
                             reg_species == "Sonstiges NH" ~ "OtherSoftwood",
                             reg_species == "Sonstiges LH" ~ "OtherHardwood",
                             reg_species == "Buche"        ~ "Beech" ,
                             reg_species == "Vogelbeere"   ~ "Rowan",
                             reg_species == "Bergahorn"    ~ "Maple",
                             reg_species == "Fichte"       ~ "Spruce",
                             reg_species == "Eiche"        ~ "Oak",
                             reg_species == "Kiefer"       ~ "Pine",
                             reg_species == "Birke"        ~ "Birch",
                             reg_species == "Weide"        ~ "Willow",
                             reg_species == "Tanne"        ~ "Fir"
                                                ))

# Replace 'tree number' (originally 1-8) by 1: 
# to corresponds to counts, not to the order of recording :

# check for the low values:
df_advanced %>% filter(height <200)  # change the values to cm in next step


#   reg_species   DBH height uniqueID      tree_numb
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
  separate(uniqueID, c('trip_n', 'dom_sp', 'manag', 'sub_n'), '_')# %>% 
  #mutate(reg_height = 'saplings') %>% 
  #dplyr::select(colnames(df_regen))

  

##### Rbind regeneration data into single dataframe: --------------------------------
#df_regen_fin <- rbind(df_regen, df_advanced2)

# Export the advanced regeneration table 
fwrite(df_advanced2, outRegenAdvanced )







# Get pre-disturbance stand density -------------------------------------------------


df_mature_trees_env <-
  dat %>% 
  dplyr::select(matches(paste(c(plot_info, 'env_'), collapse = '|'))) %>%
  dplyr::select(matches(paste(c(plot_info, 'MatureTree'), collapse = '|'))) %>%
  dplyr::select(-c("env_MatureTree_present_the_sectors" )) %>% 
  setnames(c(plot_info, 'sector', 'tree_species', 'DBH', 'distance', 'edge_tree' )) %>% 
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
                                 tree_species == "Tanne"        ~ "Fir"))



#### Save the mature trees dataset -------------------------------------------------
fwrite(df_mature_trees_env, outMatureENV )










# Shannon index -----------------------------------------------------------

## Ground cover: -------------------------------------
# get the average values per category, not by the subplot:
df_ground_shann <-
  df_ground %>% 
  group_by(trip_n, dom_sp, manag, class) %>% 
  summarize(mean_prop = mean(prop)/100) %>%  # for shannon, the scale should be 0-1, not 0-100% !! 
  mutate(shannon_part = replace_na(-(mean_prop)*log(mean_prop),0)) %>% 
  ungroup() %>% 
  group_by(trip_n, dom_sp, manag) %>%
  summarize(shannon_ground = sum(shannon_part)) %>% 
  mutate(effective_n_ground = exp(shannon_ground))
# calculate shannon and replace NA vals by 0
#shannon = is.na())


#### Shannon check plot: ---------------------------------------------------------------------
windows()
df_ground_shann %>% 
  ggplot(aes(x = factor(manag),
             y = effective_n_ground)) +
  geom_boxplot() +
  geom_violin()

windows()
df_ground_shann %>% 
  ggplot(aes(x = factor(manag),
             y = shannon_ground,
             fill = dom_sp)) +
  geom_violin() 


# Get the differences between 
# Test: does the management manag affects the diversity of th ground cover?
# HO ; the Cleared area will be more homogenous then the Dead ones:
# clearing homogenize teh gropund cover (prevalence of grasses, ...)






















# Calculate Shannon per subsite?? 
# then I can condider the individual patches as random effects in the model
df_reg_fin_by_subsample <- 
  df_regen_fin %>% 
  select(-c(height_class, DBH, reg_height)) %>% 
  arrange(trip_n, manag) %>%
  group_by(trip_n, dom_sp, manag, sub_n, reg_species) %>% 
  summarize(count_sp        = sum(count))  %>% 
    ungroup() %>% 
    group_by(trip_n, dom_sp, manag, sub_n) %>% 
    mutate(dens_tot        = sum(count_sp),
           sp_pi           = count_sp/dens_tot,
           shannon_part_sp = sp_pi*log(sp_pi),
           shannon_sp      = -sum(shannon_part_sp),
           eff_numb_sp     = exp(shannon_sp))# %>%
 #   print(n = 40)
    
    

    
 # Calculate Shannon height diversity ??? -------------
#df_reg_fin_by_subsample_height <- 
  df_regen_fin %>% 
  arrange(trip_n, manag) %>%
  group_by(trip_n, dom_sp, manag, sub_n) %>% 
  mutate(dens_tot        = sum(count),
         sp_pi           = count/dens_tot,
         shannon_part_sp = sp_pi*log(sp_pi),
         shannon_sp      = -sum(shannon_part_sp),
         eff_numb_sp     = exp(shannon_sp))# %>% 



# evaluate teh dominance of reg_species: dominant: if more then 50% of stems



# How does the height structure looks like?
df_reg_fin_by_subsample %>% 
  ggplot(aes(y = height,
             x = factor(manag),
             fill = dom_sp)) +
  geom_violin()




# keep only distinct rows for mixed effects:

dat <- df_reg_fin_by_subsample %>% 
  mutate(uniqueID = factor(paste(trip_n,dom_sp,manag,sub_n, sep = '_' ))) %>% 
  dplyr::select(uniqueID, shannon, eff_numb) %>% 
  distinct() #%>% 
  



# How does management affect the dievrsity of reg_species composition?
dat$manag <- as.factor(dat$manag)
dat$dom_sp <- as.factor(dat$dom_sp)

m1 <- lme(eff_numb~ manag, random = ~1|uniqueID, data = dat)

summary(m1)

m2 <- lme(eff_numb~ manag + dom_sp, random = ~1|uniqueID, data = dat)

summary(m2)
AIC(m1, m2)

hist(dat$eff_numb)

# use glm and poisson family:
m3 <- glm(eff_numb~ manag + dom_sp, family = poisson(link = "log"), # poisson family
          data = dat)

AIC(m3)
summary(m3)

# Define sample area per patch - correct the hdensity/ha estimation----------------
# calculate from the original data table
subsample_n <- 
  dat %>%
  group_by(trip_n , dom_sp , manag ) %>% 
  distinct(sub_n ) %>% 
  tally() %>%
  mutate(trip_n = as.character(trip_n))
 

# Get tree densiy/ha across all heights:
df_reg_dens <- 
  df_regen_fin %>% 
  left_join(subsample_n) %>% 
  ungroup() %>% 
  group_by(trip_n, dom_sp, manag, n, reg_height, reg_species) %>% 
  summarize(reg_count = sum(count, na.rm = T) )  %>%
    mutate(density_ha = reg_count/n/4*10000) #%>% 


# Calculate shannon for individual subsites
# get the total density
# calculate the share per reg_species - pi
# calculate Shannon: H = -sum(pi*log(pi)) for each reg_species
# values for Shannon have to be in 0-1 range (not 0-100)!
df_reg_dens_shannon <- 
  df_reg_dens %>% 
  group_by(trip_n, dom_sp, manag) %>% 
  mutate(dens_tot = sum(density_ha),
         sp_pi = density_ha /dens_tot,
         shannon_part = sp_pi*log(sp_pi)) %>% 
  summarize(shannon = -sum(shannon_part)) %>% 
  mutate(effective_n_sp = exp(shannon))# %>% 
  
df_reg_dens_shannon %>% 
ggplot(aes(y = effective_n_sp,
             x = manag)) +
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
               x = manag)) +
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
  group_by(trip_n, dom_sp, manag) %>% 
  ggplot(aes(y = density_ha,
             x = manag)) +
  geom_boxplot() + 
  facet_grid(~dom_sp) +
  ylab('density \n(#trees/ha)')




# calculate richness:
# count number of reg_species per plot
#richness <- 
  df_regen %>% 
  filter(count != 0) %>% # need to filter zero counts first
  group_by(trip_n,dom_sp, manag) %>% 
  distinct(reg_species) %>% 
    count() %>% 
    ggplot(aes(y = n,
               x = manag)) +
    geom_boxplot() + 
   # geom_jitter() +
    facet_grid(~dom_sp) +
    ylab('richness \n(# of tree reg_species)')
  
  
# What is regeneration height??
df_regen %>% 
  filter(count != 0)  %>% # need to filter zero counts first
  group_by(trip_n,dom_sp, manag) %>% 
#  distinct(reg_species) %>% 
#  count() %>% 
  ggplot(aes(y = count,
               x = height_class,
             fill = manag)) +
    geom_boxplot() + 
    # geom_jitter() +
    facet_grid(.~dom_sp) #+
    #ylab('richness \n(# of tree reg_species)')
  
  
  