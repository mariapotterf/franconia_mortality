


# Analyse the data:

# Quantify the propensity of forest reorganization: 

# use all data across plots (4m2), analyze is on the scale of the site (all plots grouped by category and triplet number)

# mature trees: nearest one from plot/surroundings to get the area and density of mature trees 
# nearest advanced regen: 
# advanced regeneration: plot
# new regeneration: plot


# relative frequencies, densities and basal area per plot: 
# for Mature trees: use mature trees metrics for plot: just simple estimation of the number of trees/hectar per the distance radius
# recalculate all to hectares

# analyze several aspects of the composition and structure to see how does the site changes after the disturbances
# remove the planted seedlings!


# 2022/10/06 - get data on plot level, keep nearest metrics for horizontal & vertical structure
# Combine two data:  [plot and nearest tree] in one dataset, having 2 density metrics: from plot, from nearest distance
# plot level: 
#    - mature tree, 
#    - advanced
#    - regen
# for vertical and horizontal structre: 
#   - from nearest distance metrics : mature trees
#   - advanced regen (missing height and DBH!) - skip DBH is missing 
#   - make sure to add +100 cm (distance to the center )
# dbh for advanced regen in ENV:  
#   - skip from IVi calculation
# outcome: get the species importance value per plot! nearest trees add as 
# try simply for site: can be easier (can have all 3 dimensions of species importance value)


# How to get density and basal area?
# ---------------------------------

# two estimations: 
#      on plot and nearest neighbor: those would be for BA, stem density
# simpler: only based on the plot and nearest distances

# density from distance measure: based on nearest individual:
# get the distance to nearest object as radius, calculate how many trees I can have per hectar
# if the tree is too close (in the plot), then cap the value on ha/4m2 = 2500 trees/ha

# get species Importance Value: density, basal area; can't get frequency on plot level;
# ok, just keep the values on [0-100] range





# Slope correction -------------------------------------
# Convert the regeneration counts into density/ha - takes into account the difference in sampling plot!
#     - need to do the slope correction?
# http://wiki.awf.forst.uni-goettingen.de/wiki/index.php/Slope_correction
# our inclinometer Suunto is in degrees: goes 0-90
# slope correction: only needed for slopes > 10% (7 degrees) # https://www.archtoolbox.com/calculating-slope/
# 100% slope = 45 degrees (1:1 gradient)
# NA% slope  = 90 degrees  (1:0 gradient)


rm(list=ls())


# Input data -------------------------------------------------------------------
load(file = "outData/vegData.Rdata")


#### Source paths and functions  -----------------------------------------------
source('myPaths.R')


#### Read libraries  -----------------------------------------------------------
library(readxl)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(stringr)  # use regex expressions
library(ggpubr)




# Get supplementary table ------------------------------------------------------- 
# the number of sub_n per each triplet and category

df_sub_count <- 
  plot_counts_df %>% 
  group_by(trip_n, dom_sp, manag) %>% 
  count() %>% 
  rename(sub_counts = n) %>% 
  mutate(trip_n = as.character(trip_n)) %>% 
  ungroup(.)


# Calculate values on plot level (average per site), including the distance density calculation
# merge all data together on plot level (density, basal area per species)
# then calculate the rIVI = relative species importance value per PLOT (& surroundings)
# cap the densities to exclude the unrealistic ones: for density estimation, e.g. max 2500 trees/ha

# Process: -----------------------------------------------------------------------

# for PLOT: merge all data (regeneration, advanced regen, mature trees) on plot; recalculate density to ha
# need to do slope correction! keep the gradient value
# make slope correction across tree heights

# for ENV: calculate distance dependent density per plot
nrow(df_regen)             # have all columns, all species, height classes, gradient
#nrow(df_regen_all)        # 2406
nrow(df_reg_full)          # 2406, contains infor if planted&damaged
#nrow(df_reg_onlyNatural)   # 2406
nrow(df_mature_trees_plot) # 4976

# Environment
nrow(df_advanced_env)      # 2406
nrow(df_mature_trees_env)  # 1155 


df_regen %>% 
  count(species, height_class) %>% 
  print(n = 100)



#############################################
#                                           #
#              Plot (4m2)                   #
#                                           #
#############################################


# Correct for slope all of teh plots:
# use data for the PLOT (regen, advanced, mature), and for ENV (advanced, mature)
# correct across the different height classes
# use distance density approach for the ENV

my_cols_plot = c('gradient',
                  'trip_n', 
                 # 'dom_sp', 
                  'manag', 
                  'sub_n',
                  'species',   
                  'DBH',  
                  'count', 
                  'height_class') #  'height'


# regen PLOT
head(df_regen)
df_regen2 <- df_regen %>% 
  rename(count = n_total) %>% 
  mutate(height = case_when(height_class == 'HK1' ~ 0.3,
                            height_class == 'HK2' ~ 0.5,
                            height_class == 'HK3' ~ 0.7,
                            height_class == 'HK4' ~ 0.9,
                            height_class == 'HK5' ~ 1.2,
                            height_class == 'HK6' ~ 1.7
  )) %>% 
  mutate(DBH = case_when(height == 1.7 ~ 0.8,  # DBH is ~ 1 cm for the HK6
                         height != 1.7 ~ 0)) %>% 
  dplyr::select(all_of(my_cols_plot)) %>% 
  mutate(species = case_when(species == "OtherHardwood" ~ "O_Hard",
                             species == "OtherSoftwood" ~ "O_Soft",
                             TRUE ~ species))


# advanced PLOT
df_advanced2 <- df_advanced %>% 
  dplyr::select(all_of(my_cols_plot))

# mature PLOT
df_mature_trees_plot2 <- 
  df_mature_trees_plot %>% 
  drop_na() %>% 
  mutate(height_class = 'mature',
         count = 1) %>% 
  ungroup(.) %>% 
  dplyr::select(all_of(my_cols_plot)) 



# merge all PLOT data into single df
df_full_plot = rbind(df_regen2,
                     df_advanced2,
                     df_mature_trees_plot2)


# Slope correct the number of trees per ha: ------------------------------------------
ha     <- 10000
r_side <- 2 # 2m as the lenght of the square

# this table has extimated densities per tree species and per height class
df_full_plot_corr <-
  df_full_plot %>%
  mutate(gradient = as.numeric(gradient)) %>%
  mutate(
    length_corr = r_side * cos(gradient * pi / 180),
    area_corr   = r_side * length_corr,
    correct_factor = ha / area_corr,
    corr_count = count * correct_factor
  )  %>%
  mutate(distance = 50) %>% 
  select(trip_n, manag, sub_n, species, 
         DBH, distance, height_class, count, corr_count) # correctly order the columns



# Histogram of the species counts per ha (by species and height c --------
df_full_plot_corr %>% 
  filter(corr_count > 0) %>% 
  ggplot(aes(corr_count)) + 
  geom_histogram(binwidth = 1000)


# sum up by plot level:
df_full_plot_corr %>% 
  group_by(trip_n, manag, sub_n) %>% 
  summarise(sum_plot = sum(corr_count, na.rm = T )) %>% 
 # filter(corr_count > 0) %>% 
  ggplot(aes(sum_plot)) + 
  geom_histogram(binwidth = 1000)


# maybe cap values? based on 
df_full_plot_corr %>% 
  group_by(trip_n, manag, sub_n) %>% 
  summarise(sum_plot = sum(corr_count, na.rm = T )) %>% 
  ungroup(.) %>% 
  filter(sum_plot > 125000) # 50 

# for 4 m2 real counts: ----------------------------------------- 
# trip_n manag sub_n sum_plot
# <chr>  <chr> <chr>    <dbl>
# 1 12     d     1           65
# 2 12     d     5           72
# 3 15     d     2           61
# 4 42     d     5           54
# 5 43     l     3           51
#   

# corrected counts
# trip_n manag sub_n sum_plot
# <chr>  <chr> <chr>    <dbl>
# 1 12     d     1      165007.
# 2 12     d     5      184021.
# 3 15     d     2      152872.
# 4 42     d     5      135185.
# 5 43     d     3      125019.
# 6 43     l     3      127519.


# ENVL distance based density: correct for slope: ------------------------
## ENV advanced: Get estimated dbh for the individual species and regimes -----------------
df_dbh_mean_advanced <- 
  df_full_plot %>% 
  select(c('trip_n', 'manag', 'species', 'DBH', 'height_class')) %>% 
  filter(height_class %in% c("HK7")) %>% 
  group_by(manag, species) %>%
  summarize(DBH = mean(DBH)) 



# adjust area of teh circle and then calculate the area 
# get the numbers of trees per ha
head(df_advanced_env)

# for advanced
df_advanced_env_corr <- 
  df_advanced_env %>% 
  mutate(count     = 1, 
    length_corr    = distance/100 * cos(gradient * pi / 180),
    area_corr      = pi * length_corr^2, #r_side * length_corr,
    correct_factor = ha / area_corr,
    corr_count     = round(count * correct_factor, 0)
  ) %>%
  mutate(corr_count = case_when(corr_count <= 2500 ~ corr_count,
                                corr_count > 2500 ~ 2500)) %>% 
  mutate(height_class = 'adv_ENV') %>% 
  right_join(df_dbh_mean_advanced, by = c('manag', 'species')) %>%
  select(trip_n, manag, sub_n, species, 
         DBH, distance, height_class, count, corr_count) # correctly order the columns


  
## ENV: mature: -------------------------------------------------------------------
df_mature_env_corr <- 
  df_mature_trees_env %>% 
  mutate(count          = 1, 
         length_corr    = distance/100 * cos(gradient * pi / 180),
         area_corr      = pi * length_corr^2, #r_side * length_corr,
         correct_factor = ha / area_corr,
         corr_count     = round(count * correct_factor)
  ) %>% 
  mutate(corr_count = case_when(corr_count <= 2500 ~ corr_count,
                                 corr_count > 2500 ~ 2500)) %>% 
  mutate(height_class = 'mat_ENV') %>% 
  select(trip_n, manag, sub_n, species, 
         DBH, distance, height_class, count, corr_count) # correctly order the columns


# make distribution plots to share with RS and WR ------------------
# to merge data: select the onces that are closer to site
df_mature_env_corr %>% 
  ggplot(aes(corr_count,
             fill = species)) +
  geom_histogram()


df_advanced_env_corr %>% 
  ggplot(aes(corr_count,
             fill = species)) +
  geom_histogram()





# Merge the densities for PLOT and ENV: ------------------------------------------


# Plot DBH range Mean_se(): ----------------------------------------------
# #p_dbh_dist <- 
# df_full_plot %>% 
#   filter(height_class == 'HK7') %>% 
#   ggplot(aes(y = DBH,
#              x = factor(species), 
#              color = factor(manag))) +
#   stat_summary() +
#   theme_bw() +
#   theme(legend.position = 'bottom') 
# 


# merge all PLOT data into single df !!! continue from here!!
df_full_corr = rbind(df_full_plot_corr,
                     df_advanced_env_corr,
                     df_mature_env_corr)

head(df_full_corr)
nrow(df_full_corr)





# Plot + ENV: rIVI ------------------------------------------------------------------------

# combine the regeneration, advanced regeneration and mature trees per plot & site
# Calculate:
# - relative frequency  - percent of inventory points occupied by species A as a percent of occurence of all species
# - relative density    - the number of individuals per area as a percent tof teh number of individuals of all species
# - relative basal area - the total basal area of species A as a percent of teh total basal area of all species. Basal area = sum of the cross sectional area of all tree species of species A.
# BA - estimated at breast height: eg. remove all regeneration smaller then then 1.3 m: only HK6
#   

## Site: get data for the Mature trees: ------------------------------------------
# merge the mature trees from ones present on plots, 
# and from the ENV: filter which one has a closer tree: on the plot, or nearest one? 

# add distances (in cm) if teh tree is present on the plot:
# measured from the plot center: +100 cm
# -> change the distances for surroundings: should be > 100 cm away
# 
# my_cols_mature = c('trip_n',
#                    'dom_sp',
#                    'manag',
#                    'sub_n',
#                    'species',
#                    'distance',
#                    'DBH',
#                    'orientation')
# 
# # Prepare mature data: from Mature on plot & in ENV: s
# # PLOT:
# df_mature_trees_plot_sub <- df_mature_trees_plot %>%
#   drop_na() %>% 
#   ungroup(.) %>% 
#   mutate(orientation = 'plot',
#          distance = 40)  %>% 
#   dplyr::select(all_of(my_cols_mature)) 
# 
# 
# 
# # ENV:
# df_mature_trees_env_sub <- 
#   df_mature_trees_env %>% 
#   mutate(distance = case_when(distance < 100 ~ distance + 100,
#                               distance >= 100 ~ distance )) %>% 
#   dplyr::select(all_of(my_cols_mature)) %>% 
#   mutate(orientation = 'ENV')
# 
# 
#   
# # rbind the mature tree data fro the plot and from the Environment  
# df_mature_all <- rbind(df_mature_trees_env_sub,
#                        df_mature_trees_plot_sub )
# 
# # select the nearest tree (on the plot, in the surroundings) to calculate tree density and basal area
# # !! not needed! if I am calculating the density estimation from the 
# df_mature_fin <- 
#   df_mature_all %>% 
#   group_by(trip_n, dom_sp, manag, sub_n) %>% 
#   slice_min(order_by = distance)#  %>% # select the nearest tree
# 

# can also check if my density estimation will change if using one or the other approach??







###################################################################
#                                                                 #
# Plot + ENV/ha : Species Importance Value (regen, adv, mature)   #
#                                                                 #
################################################################### -------------------------------

# to get proper densities: need to filter the density between the nearest tree 
# (can be on the PLOT, or in the ENV) and use this value
# if Mature tree and ADV regen are present on plot, use the mean of the corr_counts
# must select either or to get the mature trees density?
# for each plot, I have two densities: plot and distance based;
# how to combine them?
# select the density of the one that is closer? if both present, average them? 
# bt how is they are of different species?

head(df_full_corr)

# check how often I have categories: 
df_full_corr %>% 
  distinct(height_class)

# My classes: 
# 7 HK7       # advanced PLOT  
# 8 mature    # mature PLOT   
# 9 adv_ENV   # advanced ENV  
# 10 mat_ENV  # mature ENV

# cca 110 plots with mature trees in plot
# cca 670 plots with advanced regen in plot
df_full_corr %>% 
  group_by(trip_n, manag, sub_n) %>% 
  dplyr::filter(height_class  %in% c("mature", 'mat_ENV' )) %>% # filter specific classes
  filter(all(c("mature", 'mat_ENV') %in% height_class)) %>% 
  arrange(trip_n, manag, sub_n)


# example:

#  trip_n manag sub_n species   DBH distance height_class count corr_count
# 1 1      d     4     Pine       48       50 mature           1      2500 
# 2 1      d     4     Pine       50      590 mat_ENV          1        91 
# 3 1      l     11    Spruce     39       50 mature           1      2500 
# 4 1      l     11    Pine       35      240 mat_ENV          1       553 
# 5 1      l     14    Spruce     14       50 mature           1      2500.
# 6 1      l     14    Pine       37      200 mat_ENV          1       796 

# question: can I just summ the counts (PLOT & ENV up?) YES

#  filter(trip_n == '24' & manag == 'd' & sub_n == '1') %>% 
#  dplyr::filter(height_class %in% c( "mature" ,"mat_ENV" )) #%>% "adv_ENV","HK7",
#  group_by(trip_n, manag, sub_n) %>% 
  
# dummy example: filter groups that have at least two of values 
# dd <- data.frame(grp = c(1,1,1,2,2,2,2,3,3),
#                  cat = c('a', 'b', 'd',
#                          'a', 'b', 'd', 'e',
#                          'a', 'b'))
# 
# dd <- data.frame(grp = c(1,2,2,2,3), cat = c("a", "a", "b", "c", "c"))
# 
# # filter by two types of filtering:
# dd %>% 
#   group_by(grp) %>% 
#   filter(cat %in% c('a', 'd')) %>% 
#   filter(all(c("a", "d") %in% cat))
# #filter(all(c("a", "b") %in% cat))
# filter(any(cat=="a") & any(cat=="b"))
# 


# Get species rIVI Plot + ENV--------------------------------------------------------------
# # no frequency: not possible on plot level
# # relative density: 
# # the number of individuals per area as a percent of the number of individuals of all species.
df_rel_density <-
  df_full_corr %>%
  group_by(trip_n, manag, sub_n, species) %>%
  summarize(sp_count = sum(corr_count, na.rm = T)) %>%
  ungroup(.) %>%
  group_by(trip_n, manag, sub_n) %>%
  mutate(all_count = sum(sp_count, na.rm = T),
         rel_density = sp_count/all_count*100) #%>% 
#  filter(trip_n == 1 & manag == 'c' & sub_n == 1)
# 
# 
# # Relative basal area.  
# # the total basal area of Species A as a percent of the total basal area of all species.  
df_rel_BA_plot <- 
  df_full_corr %>%
  mutate(r = DBH/2,
         BA = pi*r^2)  %>%
  group_by(trip_n, manag, sub_n, species) %>%
  summarize(sp_BA = sum(BA, na.rm = T)) %>%
  ungroup(.) %>%
  group_by(trip_n, sub_n, manag) %>%
  mutate(all_BA = sum(sp_BA, na.rm = T),
         rel_BA = sp_BA/all_BA*100) %>%
  mutate(rel_BA = replace_na(rel_BA, 0)) #%>%  # replace NA by 0 if BA is missing
  #  filter(trip_n == 1 & manag == 'c' & sub_n == 1)






# SITE: Calculate IVI: species importance value -----------------------------------------------
# merge the rel frequeny, density and basal area ------------------------------------------
plot_IVI <- df_rel_density %>% 
  full_join(df_rel_BA_plot) %>% 
  replace_na(., list(all_BA = 0, rel_BA   = 0)) %>% 
  mutate(rIVI = ( rel_density +rel_BA)/2)  # relative IVI






# Save specific objects: ------------------------------------------------------------
save(plot_IVI,
     df_full_corr,
     df_regen,              # full plot regeneration
     df_ground,             # ground cover
     df_advanced,          # advanced regeneration PLOT, corrected distances
     df_advanced_env,       # advanced regeneration in ENV
     df_mature_trees_env,   # mature trees ENV
     df_mature_trees_plot,  # mature trees PLOT
     plot_counts_df,        # master table having all triplets and subsets structure
     file="outData/dataToPlot.Rdata")


