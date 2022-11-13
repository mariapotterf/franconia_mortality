


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
# keep planted seedlings as well!


# 2022/10/06 - get data on plot level, keep nearest metrics for horizontal & vertical structure
# Combine two data:  [plot and nearest tree] in one dataset, having 2 density metrics: from plot, from nearest distance
# plot level: 
#    - mature tree, 
#    - advanced
#    - regen
# for vertical and horizontal structure: 
#   - from nearest distance metrics : mature trees
#   - advanced regen (missing height and DBH!) - complete DBH from the plot level regen
#   - make sure to add +100 cm (distance to the center, if missing )
# dbh for advanced regen in ENV:  
#   - replace by the dbh from the advanced regen for IVi calculation
# outcome: get the species importance value per plot! nearest trees add as 
# try simply for site: can be easier (can have all 3 dimensions of species importance value)


# Get density and basal area?
# ---------------------------------

# two estimations: 
#      on plot and nearest neighbor: those would be for BA, stem density
# simpler: only based on the plot and nearest distances

# density from distance measure: based on nearest individual:
# get the distance to nearest object as radius, calculate how many trees I can have per hectar
# if the tree is too close (in the plot), then cap the value on ha/4m2 = 2500 trees/ha
# if there in no tree within 15 m: replace distance as 16 m to account for larger gap size
# and for the distance density

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

# vars --------------------------------------------------------------------------

ha <- 10000            # hectar
min_distance <- 50     # cm, minimal distance if the tree is located on the plot 

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
#nrow(df_regen)             # have all columns, all species, height classes, gradient
nrow(df_reg_full)           # 2406, contains replaced rows with specific tree species for the 'Other' species category for the regeneration on PLOT
nrow(df_mature_trees_plot)  # 4976

# Environment
nrow(df_advanced_env)      # 2406
nrow(df_mature_trees_env)  # 1155 


#df_regen %>% 
#  count(species, height_class) %>% 
#  print(n = 100)


# Check for JurI: prevalence of teh fir? ----------------------
# df_mature_trees_env %>% 
#   filter(trip_n== 10 & manag == 'l') %>% 
#   mutate(tree_n_ha = 10000/(pi*(distance/100)^2)) %>% 
#   group_by(species) %>% 
#   summarise(sum_sp = sum(tree_n_ha)) %>% 
#   mutate(all_trees = sum(sum_sp),
#          share = sum_sp/all_trees*100)




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
                  'manag', 
                  'sub_n',
                  'species',   
                  'DBH',  
                  'count', 
                  'height_class') #  'height'


# regen PLOT
head(df_reg_full)
df_reg_full2 <- df_reg_full %>% 
  rename(count = n_total) %>% 
  mutate(height = case_when(height_class == 'HK1' ~ 0.3,
                            height_class == 'HK2' ~ 0.5,
                            height_class == 'HK3' ~ 0.7,
                            height_class == 'HK4' ~ 0.9,
                            height_class == 'HK5' ~ 1.2,
                            height_class == 'HK6' ~ 1.7
  )) %>% 
  mutate(DBH = case_when(height == 1.7 ~ 0.8,  # DBH is ~ 0.8 cm for the HK6
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
df_full_plot = rbind(df_reg_full2,
                     df_advanced2,
                     df_mature_trees_plot2)


# Slope correct the number of trees per ha: ------------------------------------------
#ha     <- 10000
r_side <- 2 # 2m as the lenght of the square

# PLOT: Estimate densities per tree species and per height class; slop corrected
df_full_plot_corr <-
  df_full_plot %>%
  mutate(gradient = as.numeric(gradient)) %>%
  mutate(
    length_corr = r_side * cos(gradient * pi / 180),
    area_corr   = r_side * length_corr,
    correct_factor = ha / area_corr,
    corr_count = count * correct_factor
  )  %>%
  #  filter(corr_count > 100000)
  #  distinct(gradient) %>% 
  #  print(n = 100)
  # summarize(min(corr_count), max(corr_count))
  mutate(distance = 50) %>% 
  dplyr::select(trip_n, manag, sub_n, species, 
         DBH, distance, height_class, count, corr_count) # correctly order the columns

df_full_plot_corr %>% 
  filter(trip_n == '16' & sub_n == '4' & species == 'Spruce' & manag == 'l')

# Histogram of the species counts per ha (by species and height c --------
df_full_plot_corr %>% 
  filter(corr_count > 0) %>% 
  ggplot(aes(corr_count)) + 
  geom_histogram(binwidth = 1000)


# Sum up by plot level:
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
  dplyr::select(c('trip_n', 'manag', 'species', 'DBH', 'height_class')) %>% 
  filter(height_class %in% c("HK7")) %>% 
  group_by(manag, species) %>%
  summarize(DBH = mean(DBH)) 



# adjust area of the circle and then calculate the area 
# get the numbers of trees per ha
head(df_advanced_env)

# complete distance to 16 m if the advanced regen is missing: 
# no meaningfull here, as I am again missing specification for species

# for advanced
df_advanced_env_corr <- df_advanced_env %>% 
  right_join(plot_counts_df) %>% 
  #mutate(distance = case_when(is.na(distance) ~ 16*100, # complete 
   #                           !is.na(distance) ~ distance)) %>% 
  mutate(count     = 1, 
    length_corr    = distance/100 * cos(gradient * pi / 180),
    area_corr      = pi * length_corr^2, #r_side * length_corr,
    correct_factor = ha / area_corr,
    corr_count     = round(count * correct_factor, 0)
  ) %>%
  mutate(corr_count = case_when(corr_count <= 2500 ~ corr_count,
                                corr_count > 2500 ~ 2500)) %>% 
  #   summarize(min(corr_count), max(corr_count)) 
  mutate(height_class = 'adv_ENV') %>% 
  right_join(df_dbh_mean_advanced, by = c('manag', 'species')) %>%
  dplyr::select(trip_n, manag, sub_n, species, 
         DBH, distance, height_class, count, corr_count) # correctly order the columns


range(df_advanced_env_corr$corr_count)
  
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
 # summarize(min(corr_count), max(corr_count)) 
  mutate(height_class = 'mat_ENV') %>% 
  dplyr::select(trip_n, manag, sub_n, species, 
         DBH, distance, height_class, count, corr_count) # correctly order the columns


range(df_mature_env_corr$corr_count)

# make distribution plots to share with RS and WR ------------------
# to merge data: dplyr::select the onces that are closer to site
#p_mature_hist <- 
  df_mature_env_corr %>% 
  ggplot(aes(corr_count,
             fill = species)) +
  geom_histogram() #+
  #ggtitle('Mature tree-Surroundings\n[capped]')


# check the DBH with density?
df_mature_env_corr %>% 
  ggplot(aes(x=corr_count,
             y = DBH)) +
  geom_point()
             

df_mature_env_corr %>% 
  filter(DBH>100) 

#  trip_n manag sub_n species   DBH distance height_class count corr_count
# <chr>  <chr> <chr> <chr>   <dbl>    <dbl> <chr>        <dbl>      <dbl>
# 1 27     l     4     Oak       103      440 mat_ENV          1        166
# 2 27     l     7     Oak       114      230 mat_ENV          1        620
# 3 23     l     14    Beech     145      150 mat_ENV          1       1416
  

p_advanced_hist <- df_advanced_env_corr %>% 
  ggplot(aes(corr_count,
             fill = species)) +
  geom_histogram() +
  ggtitle('Advanced regen-Surroundings\n[capped]')


# Plot DBH range Mean_se(): ----------------------------------------------
p_dbh_dist <-
 df_full_plot %>%
  filter(height_class == 'HK7') %>%
  ggplot(aes(y = DBH,
             x = factor(species),
             color = factor(manag))) +
  stat_summary() +
  theme_bw() +
  xlab('') + 
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, 
                                   vjust = 0.5, 
                                   hjust = 1, size = 5))



# merge all PLOT data into single df !!! continue from here!!
df_full_corr = rbind(df_full_plot_corr,
                     df_advanced_env_corr,
                     df_mature_env_corr)




# test data 
#df_full_plot_corr %>% 
#  filter(trip_n == '16' & sub_n == '4' & species == 'Spruce' & manag == 'l')


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


# 
# Get the list of plots with with having both trees in PLOT and ENV: ---------

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
# cca 670 plots with advanced regen in plot:
# get the list of the trip_n, manag and sub_n to compare density of two groups:
# list of the plots that have mature trees recorded in plot and ENV
df_mature_both <- df_full_corr %>% 
  group_by(trip_n, manag, sub_n) %>% 
  dplyr::filter(height_class  %in% c("mature", 'mat_ENV' )) %>% # filter specific classes
  filter(all(c("mature", 'mat_ENV') %in% height_class)) %>% 
  arrange(trip_n, manag, sub_n) %>% 
  distinct(trip_n, manag, sub_n) %>% 
  mutate(tree_on_plot = 'mat_plot_env')


# density for advanced
df_advanced_both <- df_full_corr %>% 
  group_by(trip_n, manag, sub_n) %>% 
  dplyr::filter(height_class  %in% c("HK7", 'adv_ENV' )) %>% # filter specific classes
  filter(all(c("HK7", 'adv_ENV') %in% height_class)) %>% 
  arrange(trip_n, manag, sub_n) %>% 
  distinct(trip_n, manag, sub_n) %>% 
  mutate(tree_on_plot = 'adv_plot_env')

# try first only with mature trees: 
# how different are densities if both metrics are implemented?
df_full_corr %>% 
  full_join(df_mature_both, by = c("trip_n", "manag", "sub_n")) %>% 
  filter(height_class %in% c('mature', 'mat_ENV')) %>%
  mutate(tree_on_plot = case_when(is.na(tree_on_plot) ~ 'env',
                                  !is.na(tree_on_plot) ~ "mat_plot_env")) %>% 
  #filter(tree_on_plot == 'mat_plot_env')
  group_by(trip_n, manag, sub_n)%>% # tree_on_plot 
  dplyr::filter(tree_on_plot%in% c("mat_plot_env" )) %>% # filter specific classes
  #arrange(trip_n, manag, sub_n)
  filter(all(c("mat_plot_env", 'env' ) %in% tree_on_plot)) %>% 
  
  summarize(sum_corr_count  = sum(corr_count))  %>%
  filter(sum_corr_count > 2500)
  ggplot(aes(x = factor(tree_on_plot),
             y = sum_corr_count,
             color = manag)) + 
  stat_summary()


# !!!!!! strill not sure how to correctly adress this??


df_both = df_mature_both %>% # 100 plots
  bind_rows(df_advanced_both) #263 plots


# trip_n manag sub_n species   DBH distance height_class count corr_count
# <chr>  <chr> <chr> <chr>   <dbl>    <dbl> <chr>        <dbl>      <dbl>
# 1 42     d     1     Oak         0       50 HK1             42    105064.
# 2 42     d     5     Oak         0       50 HK1             54    135185.
# 3 43     l     3     Oak         0       50 HK1             49    122519.
# 4 15     d     2     Spruce      0       50 HK1             56    140342.
# 5 15     d     3     Spruce      0       50 HK1             42    105144.

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



# For density calculation ------------------------------------------------------

# mature trees: select only the ones, that are the closest (eg. on plot), disregarding the species
# Process: in two steps: filter oly mature trees, select the nearest ones
# merge back into original tables

# filter mature trees and select the closest ones
df_full_corr_mature <- df_full_corr %>% 
  filter(height_class %in% c('mature', 'mat_ENV')) %>% 
  group_by(trip_n, manag, sub_n) %>% 
  filter(distance == min(distance)) %>%
  filter(1:n() == 1)


# remove mature trees
df_full_corr_noMature <- df_full_corr %>% 
  filter(!height_class %in% c('mature', 'mat_ENV'))# %

# merge the filtered mature back to the table:
df_full_corr_mrg <-df_full_corr_mature %>% 
  bind_rows(df_full_corr_noMature)


# Get species rIVI Plot + ENV--------------------------------------------------------------
# # no frequency: not possible on plot level
# # relative density: 
# # the number of individuals per area as a percent of the number of individuals of all species.
df_rel_density <-
  df_full_corr_mrg %>%
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
  df_full_corr_mrg %>%
  mutate(r = DBH/2,
         BA = pi*r^2,
         BA_ha = BA*corr_count/10000)  %>%
  group_by(trip_n, manag, sub_n, species) %>%
  summarize(sp_BA = sum(BA_ha, na.rm = T)) %>%
  ungroup(.) %>%
  group_by(trip_n, sub_n, manag) %>%
  mutate(all_BA = sum(sp_BA, na.rm = T),
         rel_BA = sp_BA/all_BA*100) %>%
  mutate(rel_BA = replace_na(rel_BA, 0)) #%>%  # replace NA by 0 if BA is missing
  #  filter(trip_n == 1 & manag == 'c' & sub_n == 1)



# check the extreme value? BA > 2000?
df_rel_BA_plot %>% 
  filter(all_BA> 2000)

# triplet 23_l_14 beech has very gigh bsal area values:
# 23     l     14    Beech 

df_full_corr_mrg %>% 
 filter(trip_n == 23 & manag == 'l')


# if the trees is found on the plot, the density is very height: 2500!!
# how to handle this?

# plot the values first: --------------------------------------------------------
df_full_corr_mrg  %>% 
  filter(height_class %in% c('mature', 'mat_ENV')) %>% 
  ggplot(aes(x = height_class,
             y = corr_count)) +
  geom_boxplot()

nrow(filter(df_full_corr_mrg, height_class == 'mature')) # 100
nrow(filter(df_full_corr_mrg, height_class == 'mat_ENV')) # 1150

# if there is tree per plot ~ max density is 2500 trees/ha ~ minimal distance is 2 m:
# check nearest avg distance for environment: 
df_full_corr_mrg  %>% 
  filter(height_class %in% c('mature', 'mat_ENV')) %>% 
  group_by(manag) %>% #, height_class
  summarise(mean_dist = mean(distance/100, na.rm = T),
            mean_dens = 10000/(pi*mean_dist^2))
# !!!!

# how many trees to have?
my_dist <- seq(2,15, 0.1)
my_densCicr   <- ha/(my_dist^2*pi)
my_densSquare <- ha/(my_dist^2)
my_dist*2

windows()
par(mfrow = c(1, 2))
plot(x = my_dist, y = my_densCicr)
plot(x = my_dist, y = my_densSquare, col = 'red')

#manag mean_dist mean_dens
#<chr>     <dbl>     <dbl>
#  1 c     4.97      129.
#  2 d     3.90      210.
#  3 l     2.47      522.

# check how the density indcrease with distance?



# check basal area between manag
df_rel_BA_plot %>% 
  ggplot(aes(y = all_BA,
             x = manag)) +
  geom_boxplot()


# SITE: Calculate IVI: species importance value -----------------------------------------------
# merge the rel frequeny, density and basal area ------------------------------------------
plot_IVI <- df_rel_density %>% 
  full_join(df_rel_BA_plot) %>% 
  replace_na(., list(all_BA = 0, rel_BA   = 0)) %>% 
  mutate(rIVI = ( rel_density +rel_BA)/2)  # relative IVI





# Get species rIVI Plot + ENV: no DBH for advanced regeneration--------------------------------------------------------------
# how to handle it? now, I have filled in values from the PLOT estimates
# or handle in the the average value (5 cm?)
# # relative density: 
# # the number of individuals per area as a percent of the number of individuals of all species.

# # Relative basal area: DBH for advanced is NA: can't' be true!! then I can't get the relative shares!   
# try with value of 5cm: average values of the advanced regen DBH;
# compare the rIVI (dhb supplemented) and rIVI_5
# # the total basal area of Species A as a percent of the total basal area of all species.  
# df_rel_BA_plot_6 <- 
#   df_full_corr %>%
#   mutate(DBH = case_when(height_class == 'adv_ENV' ~ 6,
#                          height_class != 'adv_ENV' ~ DBH)) %>% 
#   #filter(height_class == 'adv_ENV') %>% 
#   mutate(r = DBH/2,
#          BA = pi*r^2)  %>%
#   group_by(trip_n, manag, sub_n, species) %>%
#   summarize(sp_BA = sum(BA, na.rm = T)) %>%
#   ungroup(.) %>%
#   group_by(trip_n, sub_n, manag) %>%
#   mutate(all_BA = sum(sp_BA, na.rm = T),
#          rel_BA = sp_BA/all_BA*100) %>%
#   mutate(rel_BA = replace_na(rel_BA, 0)) #%>%  # replace NA by 0 if BA is missing
# #  filter(trip_n == 1 & manag == 'c' & sub_n == 1)



# SITE: Calculate IVI: species importance value -----------------------------------------------
# merge the rel frequeny, density and basal area ------------------------------------------
# plot_IVI_6 <-
#   df_rel_density %>% 
#   full_join(df_rel_BA_plot_6) %>% 
#   replace_na(., list(all_BA = 0, rel_BA   = 0)) %>% 
#   mutate(rIVI = ( rel_density +rel_BA)/2)  # relative IVI


# Compare the IVI values between the suplemented DBH, and DBH = 6 for advanced regen?
# nrow(plot_IVI_6)
# nrow(plot_IVI)
# 
# # Compare the two metrics:
# dplyr::select(plot_IVI, c(trip_n, manag, sub_n, species, rIVI))  %>% #sp_BA, rel_BA, 
#   full_join(dplyr::select(plot_IVI_6, c(trip_n, manag, sub_n, species, rIVI)), 
#             by = c('trip_n', 'manag', 'sub_n', 'species')) %>% # sp_BA, rel_BA,
#   rename(rIVI_DBH = rIVI.x,
#          rIVI_DBH6 = rIVI.y) %>% 
#   mutate(diff = rIVI_DBH-rIVI_DBH6) %>%
#   #filter(diff< -10) 

  ggplot(aes(rIVI_DBH)) + 
 geom_density()
  

windows()
p_compare_density_rIVI <-
  dplyr::select(plot_IVI, c(trip_n, manag, sub_n, species, rIVI))  %>% #sp_BA, rel_BA, 
  full_join(dplyr::select(plot_IVI_6, c(trip_n, manag, sub_n, species, rIVI)), 
            by = c('trip_n', 'manag', 'sub_n', 'species')) %>% # sp_BA, rel_BA,
  rename(rIVI_DBH = rIVI.x,
         rIVI_DBH6 = rIVI.y) %>% 
  mutate(diff = rIVI_DBH-rIVI_DBH6) %>%
  pivot_longer(!c(trip_n, manag, sub_n, species, diff),
               names_to = 'type', values_to = 'vals')  %>% 
  ggplot(aes(vals,
             color = type)) + 
  geom_density(alpha = 0.5) + 
  facet_grid(.~type)









# Save specific objects: ------------------------------------------------------------
save(plot_IVI,
     df_full_corr,
     df_full_corr_mrg,      # filtered Mature trees by the nearest distance 
     df_reg_full,           # full plot regeneration
     df_ground,             # ground cover
     df_advanced,           # advanced regeneration PLOT, corrected distances
     df_advanced_env,       # advanced regeneration in ENV
     df_mature_trees_env,   # mature trees ENV
     df_mature_trees_plot,  # mature trees PLOT
     plot_counts_df,        # master table having all triplets and subsets structure
     df_dbh_mean_advanced,  # df - DBH of advanced regeneration on plot
     p_dbh_dist,            # plot of DBH of advanced regeneration
     #p_compare_density_rIVI, # plot: compare density dist. between 2 types of rIVI vals
     p_mature_hist,         # histogram of MATURe trees in ENV
     p_advanced_hist,       # histogram of ADV trees in ENV
     file="outData/dataToPlot.Rdata")


