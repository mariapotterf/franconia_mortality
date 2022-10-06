


# Analyse the data:

# Quantify the propensity of forest reorganization: 

# use all data across plots (4m2), analyze is on the scale of the site (all plots grouped by category and triplet number)

# mature trees: nearest one from plot/surroundings to get the area and density of mature trees 
# nearest advanced regen: 
# advanced regeneration: plot
# new regeneration: plot


# relative frequencies, densities and basal area per whole site: 
# for Mature trees: use mature trees metrics for site
# for seedlings & saplings = freq per plot -> this old account for the different plot number per site (5-15)

# analyze several aspects of the composition and structure to see how does the site changes after the disturbances
# skip first the 'managed site' - to see teh pure affect of the disturbance on changing forest structure
# remove the planted seedlings!



# Combine two data:  [plot and nearest tree]
# plot level: 
#    - mature tree, 
#    - advanced
#    - regen
# basal area and density per site: 
#   - from nearest distance metrics : mature trees
#   - advanced regen (missing height and DBH!)
#   - make sure to add +100 cm (distance to the center )
# dbh for advanced regen in ENV:  
#   - skip from IVi calculation




# get species Importance Value: frequency, density, basal area
# identify novel species against reference conditions - do they change the shade tolerance of comunity?



# Get data about community structure :
# 
# ecological traits
# shannon diversity
# gap fraction: how many of the 4m2 plots with no trees <10cm

# test different categories and community resemblance?
# remove all previous data from R memory
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


# Calculate values on plot level (average per site), and on site level (distance density)
# merge all data together on site level (density, freuqncy, basal area per species, per site)
# then calculate the rIVI = relative species importance value

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







# Plot: rIVI ------------------------------------------------------------------------

# combine the regeneration, advanced regeneration and mature trees per plot
# Calculate:
# - relative frequency  - percent of inventory points occupied by species A as a percent of occurence of all species
#    - frequency is not possible on plot level - skip on 2022/10/06
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


# Can I just add the nearest trees (mature, advanced) in the species importance value calculation?
# skip for now

#Get density from the ENV: advanced regen, ENv: mature trees 
# ENV: Mature trees distance density ----------------------------------

# basal area - check from script! !!!!
df_dens_mat_ENV <- 
  df_mature_trees_env %>% 
  group_by(trip_n, manag, species) %>% 
  summarize(mean_dist = mean(distance, na.rm = T)) %>%
  mutate(MA = 2*mean_dist^2,
         density = 1/MA * 10^8)  # 1ha = 1e+8 cm2


#### ENV: advanced --------------------------------------------------------------
df_dens_adv_ENV <- 
  df_advanced_env %>% 
  group_by(trip_n, manag, species) %>% 
  summarize(mean_dist = mean(distance, na.rm = T)) %>%
  mutate(MA = 2*mean_dist^2,
         density = 1/MA * 10^8)  # 1ha = 1e+8 cm2


# Plot: Recalculate the density values per ha ------------------------------------------
# to merge it later with advanced regeneration and Mature trees from surroundings

# Try first with density!!

ha <- 10000

# get together dataset for plot and nearest distance species ------------------------
df_dens_plot_ENV <- df_full_plot %>% 
  group_by(trip_n, manag, species) %>% 
  summarize(sp_count = sum(count, na.rm = T)) %>%
  right_join(df_sub_count, by = c("trip_n", "manag")) %>% # add number of plots per site to calculate density for hectar
  mutate(index = ha/(sub_counts*4),
         density = index*sp_count) %>%  # density represents the value per ha!!!
  dplyr::select(trip_n, manag, species, density) %>%
  bind_rows(select(df_dens_mat_ENV, c('trip_n', 'manag', 'species', 'density'))) %>% 
  bind_rows(select(df_dens_adv_ENV, c('trip_n', 'manag', 'species', 'density'))) %>% 
  group_by(trip_n, manag, species) %>% 
  summarize(dens_sum = sum(density, na.rm = T))
  
  
  
# !!!!!!





unique(df_full_plot$count)


# Calculate relative frequency: if species occurs on 5 plots ot of 8: freq = 62.5% 
# https://stackoverflow.com/questions/73442694/calculate-the-frequency-of-species-occurrence-across-sites/73442974#73442974
df_freq_plot <-
  df_full_plot %>%
  drop_na(species) %>%
  select(trip_n, manag, sub_n, species) %>%
  distinct() %>% # just to check if teh species is present or not
  group_by(trip_n, manag, species) %>%
  summarise(sites_by_species = n_distinct(sub_n)) %>% # Step 1; count sites by specices
  left_join(df_sub_count, by = c("trip_n", "manag")) %>%
  mutate(frequency = 100 * sites_by_species / sub_counts)


# relative density: 
# the number of individuals per area as a percent of the number of individuals of all species.
df_rel_density_plot <- 
  df_full_plot %>%
  group_by(trip_n, dom_sp, manag, species) %>% 
  summarize(sp_count = sum(count, na.rm = T)) %>% 
  ungroup(.) %>% 
  group_by(trip_n, dom_sp, manag) %>% 
  mutate(all_count = sum(sp_count, na.rm = T),
         rel_density = sp_count/all_count*100)


# Relative basal area.  
# the total basal area of Species A as a percent of the total basal area of all species.  
df_rel_BA_plot <- df_full_plot %>%
  mutate(r = DBH/2,
         BA = pi*r^2)  %>% 
  group_by(trip_n, dom_sp, manag, species) %>%
  summarize(sp_BA = sum(BA, na.rm = T)) %>% 
  ungroup(.) %>% 
  group_by(trip_n, dom_sp, manag) %>% 
  mutate(all_BA = sum(sp_BA, na.rm = T),
         rel_BA = sp_BA/all_BA*100) %>% 
  mutate(rel_BA = replace_na(rel_BA, 0))  # replace NA by 0 if BA is missing


#########################################
#                                       #
# Plot: Species Importance Value        #
#                                       #
###################################

# merge the rel frequeny, density and basal area ------------------------------------------
df_IVI_plot <- 
  df_freq_plot %>% 
  left_join(df_rel_density_plot) %>% 
  left_join(df_rel_BA_plot) %>% 
  mutate(sp_IVI = (frequency + rel_density +rel_BA)/3) %>% 
  mutate(scale = 'plot')# new_name = old_name


# Explore teh species importance value: IVI
#windows()
p_jitter_sp_IVI <- df_IVI_plot %>% 
  ggplot(aes(y = sp_IVI,
             x = species,
             col = species)) +
  geom_jitter(alpha = 0.5, width = 0.15) +
  facet_grid(dom_sp~manag) + 
  theme_bw() +
  labs(color = 'Tree species', 
       x = '', 
       y = 'Plot: rel. Importance Value [%]') +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, 
                                   hjust=1, 
                                   size = 8))  




























#####################################################################
#
#   ENV:  Species importance value get counts of teh plots per site: 
#         to calculate frequency, density and areas:
#
#####################################################################---------------------

# should I merge the data from ENV advanced, mature and Plot?
# or calculate them individually, and then calculate their average?
# maybe test on one: how the data will look different?

# Get the Frequency, density, relative basal area
# for mature trees, for advanced regen
# Mature trees basal area: just simple sum, if needed multiply by density??


# Mature trees:
df_dens_mat_ENV     # density estimation 
df_mature_trees_env # individual trees, contain the DBH info-> for basal area



# Calculate relative frequency: if species occurs on 5 plots out of 8: freq = 62.5% 
# https://stackoverflow.com/questions/73442694/calculate-the-frequency-of-species-occurrence-across-sites/73442974#73442974
df_freq_mat_env <-
  df_mature_trees_env %>%
 # drop_na(species   ) %>%
  select(trip_n, manag, sub_n, species   ) %>%
  distinct() %>% # just to check if teh species is present or not
  group_by(trip_n, manag, species   ) %>%
  summarise(sites_by_species = n_distinct(sub_n)) %>% # Step 1; count sites by species
  left_join(df_sub_count, by = c("trip_n", "manag")) %>%
  mutate(frequency = 100 * sites_by_species / sub_counts)


# relative density: 
# the number of individuals per area as a percent of the number of individuals of all species.
df_rel_density_mat_env <- 
 df_dens_mat_ENV %>%
  group_by(trip_n,  manag) %>% 
  mutate(all_dens = sum(density, na.rm = T),
         rel_density = density/all_dens*100)


# Relative basal area.  
# the total basal area of Species A as a percent of the total basal area of all species.  
df_rel_BA_mat_env <- 
  df_mature_trees_env %>%
  mutate(r = DBH/2,
         BA = pi*r^2)  %>% 
  group_by(trip_n, dom_sp, manag, species) %>%
  summarize(sp_BA = sum(BA, na.rm = T)) %>% 
  ungroup(.) %>% 
  group_by(trip_n, dom_sp, manag) %>% 
  mutate(all_BA = sum(sp_BA, na.rm = T),
         rel_BA = sp_BA/all_BA*100) %>% 
  mutate(rel_BA = replace_na(rel_BA, 0))  # replace NA by 0 if BA is missing


# ENV: Mature:  Species Importance Value  --------------------------------------------

# merge the rel frequeny, density and basal area ------------------------------------------
df_IVI_mat_env <- 
  df_freq_mat_env %>% 
  left_join(df_rel_density_mat_env) %>% 
  left_join(df_rel_BA_mat_env) %>% 
  mutate(sp_IVI = (frequency + rel_density +rel_BA)/3) %>%   # scale to [0-100%]
  mutate(scale = 'ENV_mat')



# ENV: advanced regen: rIVI ----------------------------------------


df_dens_adv_ENV %>% 
  ggplot(aes(y = density, x= manag)) + 
  geom_boxplot()


# missing dbh for advanced regen: skip calculation of the Basal area for relative importance values


# ENV Advanced regen:
df_dens_adv_ENV     # density estimation 
df_advanced_env     # individual trees, contain the DBH info-> for basal area



# Calculate relative frequency: if species occurs on 5 plots ot of 8: freq = 62.5% 
# https://stackoverflow.com/questions/73442694/calculate-the-frequency-of-species-occurrence-across-sites/73442974#73442974
df_freq_adv_env <-
  df_advanced_env %>%
  # drop_na(species   ) %>%
  select(trip_n, manag, sub_n, species   ) %>%
  distinct() %>% # just to check if teh species is present or not
  group_by(trip_n, manag, species   ) %>%
  summarise(sites_by_species = n_distinct(sub_n)) %>% # Step 1; count sites by species
  left_join(df_sub_count, by = c("trip_n", "manag")) %>%
  mutate(frequency = 100 * sites_by_species / sub_counts)


# relative density: 
# the number of individuals per area as a percent of the number of individuals of all species.
df_rel_density_adv_env <- 
  df_dens_adv_ENV %>%
  group_by(trip_n,  manag) %>% 
  mutate(all_dens = sum(density, na.rm = T),
         rel_density = density/all_dens*100)



# ENV: Advanced regen: Species Importance Value  ---------------------------------------

# merge the rel frequeny, density and basal area ------------------------------------------
# keep  in mind that IVI for the ENV advanced regen goes only [0-200] 
df_IVI_adv_env <- 
  df_freq_adv_env %>% 
  left_join(df_rel_density_adv_env) %>% 
  mutate(sp_IVI = (frequency + rel_density)/2) %>% 
  mutate(scale = 'ENV_adv')





# -------------------------------------------------------------
#
#    Get species importance value for whole community
#                   3  scales: plot, ENV mature, ENV adv
# 
# -------------------------------------------------------------

# for plot level: sum up the density and basal area per site
# frequency is already on the site level


# test example for different triplet number:
# compares the sites between themselves, not the plots:
# first just compare the distinct species betwee sites

# Seelect columns to subset
my_cols = c('trip_n', 'manag', 'species', 'sp_IVI') # , 'scale'

# merge all scales IVI values: calculate it as means per rIVI per site??
# !!! no! I need to take into account the all 3 dimensions, then calculate the rIVI
# try to merge the frequency, basal area and density for each scale??


df_IVI_3scales <- 
  select(df_IVI_plot, my_cols)  %>%
  bind_rows(select(df_IVI_mat_env, my_cols)) %>%
  bind_rows(select(df_IVI_adv_env, my_cols)) %>%
  #  tail()
  #  ungroup(.) %>% 
  group_by(trip_n, manag, species) %>% 
  summarize(mean_sp_IVI = mean(sp_IVI, na.rm = T))






#  Find novel species -------------------------------------------------------

# Identify winners, losers from the 'site' dataset, not only from the regeneration:
# reason: the mature trees should be simply present as the mature trees ?? so we can still see the
# the appearance of new species?

# Identify novel species in the community: ----------------------------------------
# are there any new species in the regeneration? compare the reference vs disturbed sites:
# just identify which species are new compared to Reference (Living) conditions
# eg ignore height's classes, counts...





# Get winnders and loser species between Ref-Dist:
df_winners <- df_IVI_3scales %>% 
  filter(manag !='c' ) %>% # & trip_n == 11
  group_by(trip_n, manag) %>% 
  distinct(species) %>% 
  select(trip_n, manag, species) %>% 
  ungroup() %>% 
  group_by(trip_n) %>% 
  filter(manag == 'd' & !species %in% species[manag == 'l'] ) %>%
  mutate(novelty = 'novel')


# identify loosers: species that were present at reference; not present after disturbance
df_loosers <- 
  df_IVI_3scales %>% 
  filter(manag !='c') %>% 
  group_by(trip_n, manag) %>% 
  distinct(species) %>% 
  select(trip_n, manag, species) %>% 
  ungroup() %>% 
  group_by(trip_n) %>% 
  filter(manag == 'l' & !species %in% species[manag == 'd']) %>% 
  mutate(novelty = 'lost')


# Merge loosers and winners species together:
df_novelty = 
  rbind(df_winners, df_loosers) %>% 
  group_by(species, novelty) %>% 
  tally() %>% 
  mutate(n2 = case_when(novelty == 'lost' ~ -n,
                        novelty == 'novel' ~ n)) #TRUE ~ species

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



# Reg.density: summary statictics: --------------------------
# how much regeneration is per each site? now it is split among several species on site

df_reg_dens %>% 
  group_by(manag, dom_sp) %>% 
  summarize(sum_dens = sum(corr_density, na.rm = T),
            sd_dens   = sd(corr_density, na.rm = T)) %>% 
  mutate(dens_sum_sd = stringr::str_glue("{round(sum_dens,1)}±{round(sd_dens,1)}")) %>% 
  dplyr::select(manag, dom_sp, dens_sum_sd) %>% 
  pivot_wider(names_from = manag, 
              values_from = dens_sum_sd )

# Reg.density: plot ---------------------------------

p_density <- df_reg_dens %>% 
  ggplot(aes(dens_sum, 
             fill = dom_sp), alpha = 0.5) + 
  geom_density() +
  facet_grid(dom_sp~manag) +
  theme_bw() +
  scale_fill_manual(values=cols, 
                      name="Dominant\nspecies")  +
  ylab('Density') +
  xlab('Stem counts') +
  theme(legend.position = 'none')
  




p_reg_dens <- df_reg_dens %>%
  ggplot(aes(x = dom_sp, 
             y = corr_density/10000)) +
  theme_bw() +
  stat_summary(geom = "errorbar", 
               width = 0.3,
               aes(col = dom_sp)) +
  stat_summary(geom = "pointrange", 
               aes(col = dom_sp),
               size = 0.4) +
  scale_colour_manual(values=cols, 
                      name="Dominant\nspecies") + 
  facet_grid(~manag) +
  labs(y='Regeneration density*10000')

p_reg_dens


# Check is sums are correctly calculated:
df_reg_dens %>% 
  ggplot(aes(x = n_subsites,
             y = corr_density)) + 
  geom_point()


df_reg_dens %>% 
  ggplot(aes(x = n_subsites,
             y = dens_sum)) + 
  geom_point()

# filter just hig values??? if it seems realistic?
df_reg_dens %>% 
  filter(dens_sum >10)

# checks how many regeneration I have per sites and height classes?
df_regen %>% 
  ggplot(aes(n_total)) +
  geom_histogram(binwidth =) +
  facet_grid()


df_regen %>% 
  filter(n_total >20)

#hist($n_total, bin = 1 )

# Get Shannon: diversity -----------------------------------------------------
# - calculate the share per species: pi
# - calculate Shannon: H = -sum(pi*log(pi)) for each species
# - values for Shannon have to be in 0-1 range (not 0-100)!
df_reg_dens_sums <- df_reg_dens %>%
  group_by(trip_n, dom_sp, manag) %>%
  summarize(sum_dens = sum(corr_density, na.rm = T)) # me


# merge the sum of all stems per site 
df_reg_dens_shannon <-
  df_reg_dens %>%
  left_join(df_reg_dens_sums) %>% 
  mutate(
    sp_pi = corr_density  / sum_dens,
    shannon_part = sp_pi * log(sp_pi)
  ) %>%
    group_by(trip_n, dom_sp, manag) %>% 
  summarize(shannon = -sum(shannon_part)) %>%
  mutate(effective_n_sp = exp(shannon))# %>%


# summary table shannon 
df_reg_dens_shannon %>%
  group_by(manag, dom_sp) %>%
  summarise(
    count = n(),
    mean = mean(effective_n_sp, 
                na.rm = TRUE),
    sd = sd(effective_n_sp, 
            na.rm = TRUE)
  )



# get plots Shannon Effective number of species -----------------------------------
p_eff_number <- df_reg_dens_shannon %>% 
  ggplot(aes(y = effective_n_sp,
             x = dom_sp)) +
  theme_bw() +
  stat_summary(geom = "errorbar",
               width=0.5,
               aes(col = dom_sp)) +
  stat_summary(geom = "pointrange", 
               aes(col = dom_sp)) +
  scale_colour_manual(values=cols, 
                      name="Dominant\nspecies") + 
  ylab('Effective # of species') +
  facet_grid(~manag, scales = 'free') 


p_shannon <- df_reg_dens_shannon %>% 
  ggplot(aes(y = shannon ,
             x = dom_sp)) +
  theme_bw() +
  stat_summary(geom = "errorbar",
               width=0.5,
               aes(col = dom_sp)) +
  stat_summary(geom = "pointrange", 
               aes(col = dom_sp)) +
  scale_colour_manual(values=cols, 
                      name="Dominant\nspecies") + 
  ylab('Shannon index') +
  facet_grid(~manag, scales = 'free') 


# Get community weighted means for Reference and Novel species:






# Community weighted means ---------------------------------------------

df_traits_cwm <- 
  df_reg_dens %>%
  #group_by(trip_n, dom_sp, manag, species) %>%
  #summarize(mean_dens = mean(corr_density)) %>%
  left_join(trait_df) %>% 
  ungroup(.) %>% 
  group_by(trip_n, dom_sp, manag) %>% 
  summarize(shade_cwm   = weighted.mean(Shade_tolerance,   corr_density, na.rm = TRUE  ),
            drought_cwm = weighted.mean(Drought_tolerance, corr_density, na.rm = TRUE  )) %>% 
  mutate(manag = factor(manag, 
                        levels = c('living','cleared', 'dead')))

# Make some plots:
# set desired dodge width
pd <- position_dodge(width = 0.4)

p_shade <- 
  df_traits_cwm %>%
  ggplot(aes(x = manag,
             y = shade_cwm))  +
  theme_bw() +
  stat_summary(#geom = "mean", 
    geom = "line", 
    size=0.5, 
    aes(colour = dom_sp, 
        group = dom_sp,
        lty = dom_sp),
    position = pd) +
  stat_summary(geom = "errorbar", 
               width = 0.3,
               aes(col = dom_sp), 
               position = pd) +
  stat_summary(geom = "pointrange", 
               aes(col = dom_sp),
               position = pd) +
  scale_colour_manual(values=cols, 
                      name="Dominant\nspecies") + 
  scale_shape_manual(values=cols, 
                     name="Dominant\nspecies") + 
  #facet_grid(~manag) +
  labs(y='Shade tolerance')

# 

p_drought <- 
  df_traits_cwm %>%
  ggplot(aes(x = manag,
             y = drought_cwm))  +
  theme_bw() +
  stat_summary(#geom = "mean", 
               geom = "line", 
               size=0.5, 
               aes(colour = dom_sp, 
                   group = dom_sp,
                   lty = dom_sp),
               position = pd) +
  stat_summary(#fun.data = "mean_cl_normal",
    geom = "errorbar", 
               width = 0.3,
               aes(col = dom_sp), 
               position = pd) +
  stat_summary(fun = "mean", 
               geom = "point",
               aes(col = dom_sp),
               position = pd) +
  scale_colour_manual(values=cols, 
                      name="Dominant\nspecies") + 
  scale_shape_manual(values=cols, 
                      name="Dominant\nspecies") + 
  labs(y='Drought tolerance')




ggarrange(p_shade,p_drought, ncol = 2, nrow = 1 , 
          common.legend = TRUE  )







# Reorganization: composition ---------------------------------------------
# classify if the dominant species prevails in 
# regeneration? how often they fit the dominant species?
head(df_reg_dens )

# Calculate % of  regeneration species from total density
df_dens_species <- 
  df_reg_dens %>% 
  group_by(trip_n, dom_sp, manag, species) %>% 
  summarize(ds_species = sum(corr_density, na.rm = T)/n_subsites) %>% # divide by number of subsites!!
  distinct() 

df_dens_sum <- df_dens_species %>%  # keep only unique rows
  group_by(trip_n, dom_sp, manag) %>% 
  summarize(ds_sum = sum(ds_species, na.rm = T))

# Merge to original table to calaulctate shares by species:
df_density_change <- df_dens_species %>% 
  left_join(df_dens_sum, by = c("trip_n", "dom_sp", "manag")) %>% 
  mutate(ds_spec_share = ds_species  /ds_sum*100) %>% 
  # filter(species %in% c("Spruce", 'Beech', 'Pine', 'Oak' )) %>% 
  mutate(cl_change = case_when(dom_sp == 'spruce' & species == 'Spruce' & ds_spec_share > 50 ~ 'resilience',
                               dom_sp == 'spruce' & species == 'Spruce' & ds_spec_share <= 50 & ds_spec_share > 25  ~ 'decrease',
                               dom_sp == 'spruce' & species == 'Spruce' & ds_spec_share <= 25 & ds_spec_share > 0  ~ 'reduction',
                               dom_sp == 'beech'  & species == 'Beech'  & ds_spec_share > 50 ~ 'resilience',
                               dom_sp == 'beech'  & species == 'Beech'  & ds_spec_share <= 50 & ds_spec_share > 25  ~ 'decrease',
                               dom_sp == 'beech'  & species == 'Beech'  & ds_spec_share <= 25 & ds_spec_share > 0  ~ 'reduction',
                               dom_sp == 'pine' & species == 'Pine' & ds_spec_share > 50 ~ 'resilience',
                               dom_sp == 'pine' & species == 'Pine' & ds_spec_share <= 50 & ds_spec_share > 25  ~ 'decrease',
                               dom_sp == 'pine' & species == 'Pine' & ds_spec_share <= 25 & ds_spec_share > 0  ~ 'reduction',
                               dom_sp == 'oak'  & species == 'Oak'  & ds_spec_share > 50 ~ 'resilience',
                               dom_sp == 'oak'  & species == 'Oak'  & ds_spec_share <= 50 & ds_spec_share > 25  ~ 'decrease',
                               dom_sp == 'aok'  & species == 'Oak'  & ds_spec_share <= 25 & ds_spec_share > 0  ~ 'reduction',
                               TRUE ~ 'species_change')) %>%
  ungroup(.) #%>% 


# filter data to have only one change group per site & management:
df_dens_flow <- df_density_change %>% 
  group_by(trip_n, dom_sp, manag) %>% 
  top_n(1, ds_spec_share) %>%  # select the highest share per species and category
  ungroup(.) %>% 
  dplyr::select(dom_sp, manag, cl_change) %>%
  group_by(dom_sp, manag, cl_change) %>% 
  count()

#count()


# alternative alluvial diagram: scale_viridis and 3 axis:
# create alternative alluvial diagram
library(ggplot2)
library(ggalluvial)
#p_alluvial <- 
  ggplot(df_dens_flow,
                     aes(axis1 = dom_sp ,
                         axis2 = cl_change ,
                         axis3 = manag,
                         y = n)) +
  geom_alluvium(aes(fill = dom_sp)) +
  geom_stratum() +
  geom_text(stat = "stratum", 
            #label.strata = TRUE
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("dom_sp", "manag", "cl_change"),
                   expand = c(.1, .1)) #+
  #scale_fill_viridis_d() +
  scale_fill_manual(values=cols, 
                    name="Dominant\nspecies") +
  # labs(title = "Titanic data",
  #      subtitle = "stratified by class, sex, and survival",
  #      y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 


# what are the resulting spcies??? -----------------
df_dens_flow_species <- 
  df_density_change %>% 
  group_by(trip_n, dom_sp, manag, species) %>% 
  #top_n(1, ds_spec_share) %>%  # select the highest share per species and category
  ungroup(.) %>% 
  dplyr::select(dom_sp, manag, species) %>%
  group_by(dom_sp, manag, species) %>% 
  count()



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



