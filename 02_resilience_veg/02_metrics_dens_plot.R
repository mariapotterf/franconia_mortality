


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





# Slope correction? -------------------------------------
# Convert the regeneration counts into density/ha - takes into account the difference in sampling plot!
#     - need to do the slope correction?
# http://wiki.awf.forst.uni-goettingen.de/wiki/index.php/Slope_correction
# our inclinometer Suunto is in degrees: goes 0-90
# slope correction: only needed for slopes > 10% (7 degrees) # https://www.archtoolbox.com/calculating-slope/
# 100% slope = 45 degrees (1:1 gradient)
# NA% slope = 90 degrees  (1:0 gradient)


rm(list=ls())


# Input data -------------------------------------------------------------------
load(file = "vegData.Rdata")


#### Source paths and functions  -----------------------------------------------
source('myPaths.R')


#### Read libraries  -----------------------------------------------------------
library(readxl)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(stringr)  # use regex expressions
#library(gridExtra)
library(ggpubr)




# Get supplementary table ------------------------------------------------------- 
# the number of sub_n per each triplet and category

df_sub_count <- 
  plot_counts_df %>% 
  group_by(trip_n, dom_sp, manag) %>% 
  count() %>% 
  rename(sub_counts = n) %>% 
  mutate(trip_n = as.character(trip_n))


# Calculate values on plot level (average per site), including the distance density calculation
# merge all data together on plot level (density, frequncy, basal area per species)
# then calculate the rIVI = relative species importance value per PLOT (* surroundings)

#############################################
#                                           #
#              Plot (4m2)                   #
#                                           #
#############################################

# Explore the data: --------------------------------------------------------------
# get density of species for seedlings/samplings
# advanced regeneration is all in height class HK7 
# get counts for teh species in advanced regeneration, and then means per sample to merge it with the 
# seedling data
df_advanced_count <- 
  df_advanced2 %>% 
  group_by(trip_n, dom_sp, manag, sub_n, species) %>% 
  summarize(sum_sapln = sum(count, na.rm = T)) #,



# Remove the planted seedlings:
df_reg_onlyNatural <- df_reg_full %>% 
  mutate(n_natural = n_total - n_planted)


# merge counts of seedlings & saplings: 
df_regen_all <- df_reg_onlyNatural %>% 
  left_join(df_advanced_count, 
            by = c("trip_n", "dom_sp", "manag", "sub_n", "species"))



# Plot: Regeneration counts: ------------------------------------------------------------
# get total density per species: get diversity of the regeneration species???
# need to show as average counts!!!
df_regen_all2 <- df_regen_all %>% 
  group_by(trip_n, dom_sp, manag, sub_n, species) %>% 
  summarize(seedlings = mean(n_natural, na.rm = T),
            sapling = mean(sum_sapln, na.rm = T)) %>%
  mutate_all(~replace(., is.nan(.), 0)) %>% # convert all NaN to zeros
  pivot_longer(!c(trip_n, dom_sp, manag, sub_n, species), 
               names_to = 'veg_class',
               values_to = 'count') # %>%


# get barplot: counts of regeneration by species: -----------------------------------
# p_treeComp_categ <- 
#   df_regen_all2 %>% 
#   ggplot(aes(x = reorder(species, -count), #species,
#              y = count,
#              fill = factor(veg_class))) +
#   stat_summary(fun = mean, 
#                position=position_dodge(0.95), 
#                geom = "bar",
#                color = 'black') + 
#   stat_summary(fun.data = mean_se, 
#                geom = "errorbar", 
#                position=position_dodge(0.95),
#                width = .3) +
#     labs(fill = "Natural\nRegeneration") + 
#  scale_fill_manual(values = c('sapling' = '#E69F00',
#                               'seedling' = 'grey90'),
#                    labels = c('sapling [2-5 m]', 
#                               'seedling [0.2-2m]')) +
#   xlab('') +
#   ylab('Mean counts per tree species') + 
#   theme_bw() +
#   facet_grid(dom_sp~manag, scales = 'free') + # 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
#          legend.position = 'bottom')# + 
#   
#   
#   
# # order by prevelanec of spcies: 
# p_prevalence <- df_regen_all2 %>% 
#     ggplot(aes(x = reorder(species, -count), #species,
#                y = count,
#                fill = factor(veg_class))) +
#     stat_summary(fun = mean, 
#                  position=position_dodge(0.95), 
#                  geom = "bar",
#                  color = 'black') + 
#     stat_summary(fun.data = mean_se, 
#                  geom = "errorbar", 
#                  position=position_dodge(0.95),
#                  width = .3) +
#     labs(fill = "Natural\nRegeneration") + 
#     scale_fill_manual(values = c('sapling' = '#E69F00',
#                                  'seedling' = 'grey90'),
#                       labels = c('sapling [>2-5 m]', 
#                                  'seedling [0.2-2m]')) +
#     xlab('') +
#     ylab('Mean counts\nper tree species') + 
#     theme_bw() +
#     #  facet_grid(dom_sp~manag) + # , scales = 'free'
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
#           legend.position = 'right')# + 
# 
#            
# 
# 
# 







# Plot: rIVI ------------------------------------------------------------------------

# combine the regeneration, advanced regeneration and mature trees per plot & site
# Calculate:
# - relative frequency  - percent of inventory points occupied by species A as a percent of occurence of all species
# - relative density    - the number of individuals per area as a percent tof teh number of individuals of all species
# - relative basal area - the total basal area of species A as a percent of teh total basal area of all species. Basal area = sum of the cross sectional area of all tree species of species A.
# BA - estimated at breast height: eg. remove all regeneration smaller then then 1.3 m: only HK6
#   


# need to calculate Basal area for the tree regeneration:
# for seedlings: only for HK6 (1.3-2 m tall ~ dbh == 1 cm)
# for advanced & mature trees: I have dbh
# mature trees: need to be accounted for teh trees on the plot, trees in nearest distance to calculate the area& basal area

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







#########################################################
#                                                       #
# Plot: Species Importance Value (regen, adv, mature)   #
#                                                       #
######################################################### -------------------------------

# need to calculate values per hectar, and then merge with the values per hectar estimations from the nearest 
# neighbor!!!

# Create full dataset per 4m2 to calculate Importance value: ------------------------------
# data for seedlings & samplings
# get counts, dbh -> basal area to calculate the frequency, deminity and dominance per species:
my_cols_regen = c('trip_n', 
                  'dom_sp', 
                  'manag', 
                  'sub_n',
                  'species',   
                  'DBH',  
                  'count', 
                  'height_class') #  'height'

# filter and rename the values: keep only natural regeneration!
df_reg_onlyNatural2 <- df_reg_onlyNatural %>% 
  rename(count = n_natural) %>% 
  mutate(height = case_when(height_class == 'HK1' ~ 0.3,
                            height_class == 'HK2' ~ 0.5,
                            height_class == 'HK3' ~ 0.7,
                            height_class == 'HK4' ~ 0.9,
                            height_class == 'HK5' ~ 1.2,
                            height_class == 'HK6' ~ 1.7
  )) %>% 
  mutate(DBH = case_when(height == 1.7 ~ 0.8,  # DBH is ~ 1 cm for the HK6
                         height != 1.7 ~ 0)) %>% 
  dplyr::select(all_of(my_cols_regen))



# filter and process seedlings, sampling and mature trees to fit in this database, and then 'rbind' 3 tables:
# calculate BA for each (skip seedlings < 1.3 m = BA is NA)
df_advanced3 <- df_advanced2 %>% 
  dplyr::select(all_of(my_cols_regen))



# mature trees on plot:
df_mature_trees_plot3 <- df_mature_trees_plot %>% 
  mutate(height_class = 'mature',
         count = 1) %>% 
  ungroup(.) %>% 
  dplyr::select(all_of(my_cols_regen))


# check names: correct, can be merged (r bind)
names(df_reg_onlyNatural2)
names(df_advanced3)
names(df_mature_trees_plot3)



# Just calculate frequency, density and basal area from the species present on the plots:
# Merge all tree data on plot together:
df_full_plot = rbind(df_reg_onlyNatural2,
                     df_advanced3,
                     df_mature_trees_plot3)

# remove any NA from teh species (as a part from teh mature trees estimation):
df_full_plot <- df_full_plot %>% 
  drop_na(species)



#Get density from the ENV: advanced regen, ENv: mature trees 
# ENV: Mature trees distance density -> SITE----------------------------------


# Plot: Recalculate the density values per ha ------------------------------------------
# to merge it later with advanced regeneration and Mature trees from surroundings

# ha <- 10000

# get together dataset for plot and nearest distance species ------------------------
# df_dens_plot_ENV <- df_full_plot %>% # plot level
#   group_by(trip_n, manag, species) %>% 
#   summarize(sp_count = sum(count, na.rm = T)) %>%
#   right_join(df_sub_count, by = c("trip_n", "manag")) %>% # add number of plots per site to calculate density for hectar
#   mutate(index = ha/(sub_counts*4),
#          density = index*sp_count) %>%  # "density" represents the value per ha!!!
#   dplyr::select(trip_n, manag, species, density) %>%
#   # add density from ENV mature trees, ENV advanced 
#   bind_rows(select(df_dens_mat_ENV, c('trip_n', 'manag', 'species', 'density'))) %>% 
#   bind_rows(select(df_dens_adv_ENV, c('trip_n', 'manag', 'species', 'density'))) %>% 
#   group_by(trip_n, manag, species) %>% 
#   summarize(dens_sum = sum(density, na.rm = T)) 
#   
# 
# 

# Add ENV data to PLOT data: as new rows  -------------------------------------------------------------

# first, make a subset of the suitable tables, having the same columns as full plot data

df_mature_trees_env_p <- df_mature_trees_env %>% 
  select(trip_n, dom_sp, manag, sub_n, species, DBH) %>%  #  height_class, count
  mutate(height_class = 'mat_ENV',
         count = 1)



# Estimate averager dbh from the advanced regeneration in plots: to have at least some evaluation 
# of teh advanced regeneration dbh in ENV
unique(df_full_plot$height_class)


# Get estimated dbh for the individual species and regimes
df_dbh_mean_advanced <- 
  df_full_plot %>% 
  select(c('trip_n', 'manag', 'species', 'DBH', 'height_class')) %>% 
  filter(height_class %in% c("HK7")) %>% 
  group_by(manag, species) %>%
  summarize(DBH = mean(DBH)) 

  
# Plot DBH range Mean_se(): ----------------------------------------------
#p_dbh_dist <- 
df_full_plot %>% 
  filter(height_class == 'HK7') %>% 
  ggplot(aes(y = DBH,
             x = factor(species), 
             color = factor(manag))) +
  stat_summary() +
  theme_bw() +
  theme(legend.position = 'bottom') 


# Add estimated dbh for advanced regeneration by species:
df_advanced_env_p <-
  df_advanced_env %>%
  right_join(df_dbh_mean_advanced, by = c('manag', 'species')) %>%
  mutate(height_class = 'adv_ENV',
         count = 1) %>%
  select(trip_n, dom_sp, manag, sub_n, species, DBH, height_class, count) # correctly order the columns





# Get species rIVI Plot + ENV--------------------------------------------------------------
# # no frequency: not possible on plot level
# # relative density: 
# # the number of individuals per area as a percent of the number of individuals of all species.
df_rel_density <-
  df_full_plot %>%
  bind_rows(df_mature_trees_env_p) %>% 
  bind_rows(df_advanced_env_p) %>%
  group_by(trip_n, dom_sp, manag, sub_n, species) %>%
  summarize(sp_count = sum(count, na.rm = T)) %>%
  ungroup(.) %>%
  group_by(trip_n, dom_sp, manag, sub_n) %>%
  mutate(all_count = sum(sp_count, na.rm = T),
         rel_density = sp_count/all_count*100)# %>% 
#  filter(trip_n == 1 & manag == 'c' & sub_n == 1)
# 
# 
# # Relative basal area.  
# # the total basal area of Species A as a percent of the total basal area of all species.  
df_rel_BA_plot <- 
  df_full_plot %>%
  bind_rows(df_mature_trees_env_p) %>% 
  bind_rows(df_advanced_env_p) %>%
  mutate(r = DBH/2,
         BA = pi*r^2)  %>%
  group_by(trip_n, dom_sp, manag, sub_n, species) %>%
  summarize(sp_BA = sum(BA, na.rm = T)) %>%
  ungroup(.) %>%
  group_by(trip_n, dom_sp, sub_n, manag) %>%
  mutate(all_BA = sum(sp_BA, na.rm = T),
         rel_BA = sp_BA/all_BA*100) %>%
  mutate(rel_BA = replace_na(rel_BA, 0)) %>%  # replace NA by 0 if BA is missing
    filter(trip_n == 1 & manag == 'c' & sub_n == 1)





# Site: Get relative density -----------------------------------
# site_rel_dens <- #df_dens_plot_ENV %>% 
# df_full_plot %>% 
#   #select(trip_n, manag, sub_n, species) %>% 
#   bind_rows(select(df_mature_trees_env_p, c('trip_n', 'sub_n', 'manag', 'species'))) %>% 
#   bind_rows(select(df_advanced_env_p, c('trip_n', 'sub_n', 'manag', 'species'))) #%>%
#   group_by(trip_n, manag, sub_n, species) %>% 
# 
#   mutate(all_count = sum(dens_sum, na.rm = T),
#          rel_density = dens_sum/all_count*100)

  
# Site: get relative frequency
# add ENV advanced and mature trees to full PLOt dataset, just to see if teh species is present or not
# need it here on the plot level!
# site_rel_freq <- df_full_plot %>% 
#   select(trip_n, manag, sub_n, species) %>% 
#   bind_rows(select(df_mature_trees_env, c('trip_n', 'sub_n', 'manag', 'species'))) %>% 
#   bind_rows(select(df_advanced_env, c('trip_n', 'sub_n', 'manag', 'species'))) %>%
#   distinct() %>% 
#   group_by(trip_n, manag, species) %>%
#   summarise(sites_by_species = n_distinct(sub_n)) %>% # Step 1; count sites by specices
#   left_join(df_sub_count, by = c("trip_n", "manag")) %>%
#   mutate(frequency = 100 * sites_by_species / sub_counts) # on how many plots the species is present??

  
# Get relative basal area: not from the ENV advanced, only from plot and ENV Mature
#site_rel_BA <- 
  # df_full_plot %>%
  #   select(c('trip_n', 'sub_n', 'manag', 'species', 'DBH')) %>% 
  #   bind_rows(select(df_mature_trees_env, c('trip_n', 'sub_n', 'manag', 'species', 'DBH'))) %>% 
  # mutate(r = DBH/2,
  #        BA = pi*r^2)  %>% 
  # group_by(trip_n,manag, species) %>%
  # summarize(sp_BA = sum(BA, na.rm = T)) %>% 
  # ungroup(.) %>% 
  # group_by(trip_n, manag) %>% 
  # mutate(all_BA = sum(sp_BA, na.rm = T),
  #        rel_BA = sp_BA/all_BA*100) %>% 
  # mutate(rel_BA = replace_na(rel_BA, 0))  # replace NA by 0 if BA is missing


# SITE: Calculate IVI: species importance value -----------------------------------------------
# merge the rel frequeny, density and basal area ------------------------------------------
plot_IVI <- 
 # site_rel_freq  %>% 
  df_rel_density %>% 
  full_join(df_rel_BA_plot) %>% 
  replace_na(., list(all_BA = 0, rel_BA   = 0)) %>% 
  #mutate(rIVI = (frequency + rel_density +rel_BA)/3)  # relative IVI
  mutate(rIVI = ( rel_density +rel_BA)/2)  # relative IVI











#  Find novel species -------------------------------------------------------

# Identify winners, losers from the 'site' dataset, not only from the regeneration:
# reason: the mature trees should be simply present as the mature trees ?? so we can still see the
# the appearance of new species?

# Identify novel species in the community: ----------------------------------------
# are there any new species in the regeneration? compare the reference vs disturbed sites:
# just identify which species are new compared to Reference (Living) conditions
# eg ignore height's classes, counts...





# Get winnders and loser species between Ref-Dist:
# df_winners <- site_IVI %>% 
#   filter(manag !='c' ) %>% # & trip_n == 11
#   group_by(trip_n, manag) %>% 
#   distinct(species) %>% 
#   select(trip_n, manag, species) %>% 
#   ungroup() %>% 
#   group_by(trip_n) %>% 
#   filter(manag == 'd' & !species %in% species[manag == 'l'] ) %>%
#   mutate(novelty = 'novel')
# 
# 
# # identify loosers: species that were present at reference; not present after disturbance
# df_loosers <- 
#   site_IVI %>% 
#   filter(manag !='c') %>% 
#   group_by(trip_n, manag) %>% 
#   distinct(species) %>% 
#   select(trip_n, manag, species) %>% 
#   ungroup() %>% 
#   group_by(trip_n) %>% 
#   filter(manag == 'l' & !species %in% species[manag == 'd']) %>% 
#   mutate(novelty = 'lost')


# Merge loosers and winners species together: ----------------------------------------
# df_novelty = 
#   rbind(df_winners, df_loosers) %>% 
#   group_by(species, novelty) %>% 
#   tally() %>% 
#   mutate(n2 = case_when(novelty == 'lost' ~ -n,
#                         novelty == 'novel' ~ n)) #TRUE ~ species
# 
# #windows()
# p_win_loos_sp <- ggplot(df_novelty, aes(x = species, fill = novelty, y = n2)) +
#   geom_col(position = 'identity', col = 'black')  +
#   theme_bw() +
#   ylab('Counts (mean)') + 
#   theme(axis.text = element_text(angle = 90)) 
# #facet_wrap(vars(species), strip.position = "bottom")
# 
# 
# p_winners <- ggplot(df_winners, aes(x = species, fill = species)) + 
#   geom_bar() +
#   ggtitle('Novel species', subtitle= 'Appear after disturbance)') + 
#   theme_bw()
# 
# p_loosers <- ggplot(df_loosers, aes(x = species, fill = species)) + 
#   geom_bar() + 
#   ggtitle('Lost species', subtitle= 'Disappear after disturbance)') + 
#   theme_bw()
# 
# ggarrange(p_winners, p_loosers, nrow = 2)
# 
# 
# 
# head(df_winners)




# vertical distribution: how many trees is in which height category?


# Get tree species traits:  ----------------------------------------------------------

eco_traits <- read_excel(paste(myPath,
                               'notes/litterature/traits_database',  
                               'Niinemets_2006.xls', sep = '/'),
                         skip = 3,
                         sheet = 'Niinemets_2006_appendix',
                         .name_repair = function(x) gsub("\\s+", "_", x)) # replace the spaces in colnames by '_'


# Interpretation: 
# shade tolerance: 
#             higher number = more tolerance (fir), 
#             lower number = less tolarence (more sunny site, pine)
# drought tolerance: 
#             higher  = more tolerance (pine), 
#             lower = less (more drought sensitive, spruce)

# Filter only relevant species:  # how to handle the Other sftwoos and Other harvwood? now just skipped 
# Any way it is likely not dominant
trees_lat <- c(
  'Picea abies',
  'Fagus sylvatica',
  'Sorbus aucuparia',
  'Abies alba',
  'Acer pseudoplatanus',
  'Betula pendula',
  'Pinus sylvestris',
  'Fraxinus excelsior'
) 

quercus_spp <- c('Quercus petraea',  # Quercus will be averaged later
                 'Quercus robur')

salix_spp <- c('Salix caprea',     # Salix will be averaged later
               'Salix alba')

# Get Quercus spp: average the values for the :
# Quercus petraea , Quercus robur
# For salix: Salix alba, Salix caprea  (S. fragilis is not that common)

#'OtherHardwood',
# 'OtherSoftwood'

# Filter eco traits database by species: 
# need to do individuall for Quercis, salix and for other species 
# that we have full name identification
traits_Qc <- 
  eco_traits %>% 
  dplyr::select(c('Species','Shade_tolerance', 'Drought_tolerance')) %>% 
  filter(Species %in% quercus_spp)  %>% 
    mutate(Species = 'Quercus') %>% 
    group_by(Species) %>% 
    summarize(Shade_tolerance = mean(Shade_tolerance),
              Drought_tolerance = mean(Drought_tolerance))

traits_Sx <- 
  eco_traits %>% 
  dplyr::select(c('Species','Shade_tolerance', 'Drought_tolerance')) %>% 
  filter(Species %in% salix_spp)  %>% 
  mutate(Species = 'Salix') %>% 
  group_by(Species) %>% 
  summarize(Shade_tolerance   = mean(Shade_tolerance),
            Drought_tolerance = mean(Drought_tolerance))

# remianing species:
traits_sp <- 
  eco_traits %>% 
  dplyr::select(c('Species','Shade_tolerance', 'Drought_tolerance')) %>% 
  filter(Species %in% trees_lat)

# Merge traits into single df
trait_df <- rbind(traits_Qc,
                  traits_Sx,
                  traits_sp) 

# Change naming to be able to merge them with denity dataset:
trait_df <- trait_df %>%
  mutate(
    Species = case_when(
      Species == "Fraxinus excelsior"    ~ "Ash",
      #Species == "Sonstiges NH"         ~ "OtherSoftwood",
      # Species == "Sonstiges LH"        ~ "OtherHardwood",
      Species == "Fagus sylvatica"       ~ "Beech" ,
      Species == "Sorbus aucuparia"      ~ "Rowan",
      Species == "Acer pseudoplatanus"   ~ "Maple",
      Species == "Picea abies"           ~ "Spruce",
      Species == "Quercus"               ~ "Oak",
      Species == "Pinus sylvestris"      ~ "Pine",
      Species == "Betula pendula"        ~ "Birch",
      Species == "Salix"                 ~ "Willow",
      Species == "Abies alba"            ~ "Fir"
    )
  ) %>% 
  rename(species = Species)


# Slope correction factor:------------------------------------------------------
# correct the measurements in the field to the map plane projection (e.g. 'shrinks the field sampling plot')
# need to calculate the area of the study site on the pane: will change one axis, other stays the same 
# then need to recalculate the correction factor: at plane, the factor is 2500 (4m2 to 10000m2); 
#                                                 at slope it varies

# Example of slope correction calculation of tree density:
ha = 10000
trees_field = 10
gradient = 16.7  #(has to be in degrees!)

# area of the subsite: 2x2 m
r = 2    # m
r1 = r
r2 = r1*cos(gradient*pi/180)   # R works in radians: to get the value in degrees, it has to be in form cos(angle * pi/180) 
# https://r-lang.com/r-cos-function-with-example/
area_field = r^2# m2
area_plane = r1*r2

# Get the correction factor:
ideal_factor   = ha/area_field
correct_factor = ha/area_plane

# Calculate teh tree deisnity based on field, and based on corrected area:
trees_dens_field = trees_field*ideal_factor
trees_dens_plane = trees_field*correct_factor




# Density correction function --------------------------------------------------
# the gradient is subset specific: therefore, first adjust the number of the 
# area per subset, and teh number of densities
# slope_corr <- function(gradient, ...) {
#   
#   # get the dimension of the corrected plane sampling plot
#   r1 = 2
#   r2 = r1*cos(gradient*pi/180) 
#   
#   # calculate the expansion factor
#   correct_factor = ha/r1*r2
#   
#   # correct the number of trees/ha
#   dens_corr = trees_field*correct_factor
#   return(dens_corr)
#   
# }







# Get tree densities ------------------------------------------------------

# Define sample area per patch - correct the density/ha estimation----------------
# calculate from the original data table
subsample_n <- 
  dat %>%
  group_by(trip_n , dom_sp , manag ) %>% 
  distinct(sub_n ) %>% 
  tally() %>%
  mutate(trip_n = as.character(trip_n),
         dom_sp = factor(dom_sp),
         manag = factor(manag)) %>% 
  mutate(manag = case_when(manag == 'c' ~ 'cleared',
                           manag == 'l' ~ 'living',
                           manag == 'd' ~ 'dead'#,
                           )) %>%   #Species == "Fagus sylvatica"       ~ "Beech" ,))
  rename(n_subsites = n) %>% 
  as.data.frame()



# The correction of the counts: need to do it on the level of individual subsites:
# as there is high variation in slopes ('gradient') between the subsites
# So I am correcting the tree density/ha per each subsite: 
# maybe then use teh mean/sum densities per category??? 
df_regen <- df_regen %>% 
  mutate(manag = factor(manag, levels = c('l', 'c', 'd'),
                        labels = c('living','cleared', 'dead'))) %>% 
  mutate(dom_sp = factor(dom_sp))



# Get tree density/ha across all heights (sums across heights),
# correct the density by slope per each subsite
# but it is calculated for each species as a value per hectar. 
# need to account for the different number of subsites: 5-15
# calculate the sums and then divide by number of subsamples
#df_reg_dens <-

# Calculate the sum of tree counts per species, ignore tree heights categories!!
df_reg_dens <- df_regen %>%
  dplyr::select(-c(height_class)) %>%
  dplyr::left_join(subsample_n,
                   by = c('trip_n', 'manag', 'dom_sp')) %>% #, 'dom_sp', 'manag'
  ungroup() %>%
  group_by(trip_n, dom_sp, manag, species,  n_subsites)  %>% #height_class,
  summarize(dens_sum = sum(n_total, na.rm = T)/n_subsites) %>%
  mutate(
    length_corr = 2 * cos(gradient * pi / 180),
    area_corr   = 2 * length_corr,
    correct_factor = ha / area_corr,
    corr_density = dens_sum * correct_factor
  )  %>%
  filter(corr_density != 0) %>%
  ungroup(.) %>% 
  distinct()




# Remove objects: ---------------------------------------------------------
# remove all of teh values:
# https://stackoverflow.com/questions/43626229/how-to-delete-all-values-in-rstudio-environment

rm(list = ls.str(mode = 'numeric'))
rm(list = ls.str(mode = 'character'))
rm(list = lsf.str())

# remove all objects starting with 'a'
# rm(list = ls()[grep("A", ls())])

# https://stackoverflow.com/questions/11761992/how-do-i-clear-only-a-few-specific-objects-from-the-workspace

# save only specific objects instead of the whole image: !!!
#save(list=c("temp","temp2"),file="Test.Rdata") #saves those 2 objects
# save(list=ls(pat="temp")),file="Test2.Rdata") #saves any object with name containing "temp"

# Save all dfs as R object: ------------------------------------------------------------
save.image(file="dataToPlot.Rdata")



