# Quantify forest reorganization;

# read data
# each triplet categorize in one of the characteristics:
# as proposed by Rupert
# for inpt data: use the plot + nearest distance tree data to get the dbh, BA, stem density, etc...

# To do:
# how to show sensitivity analysis instead of threshold values? ; eg for novelty??
# run analysis for Disturbed and Cleared? - > make respective lists of novel species fr CC, D

rm(list=ls())

#### Read libraries  -----------------------------------------------------------
library(readxl)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(stringr)  # use regex expressions
library(ggpubr)
library(ggrepel)

# set theme: plotting: 
theme_set(theme_bw())
theme_update(legend.position = 'bottom') 




# Store details about labeling and plotting -------------------------------------
# get labels:
manag.labs <- c("Managed", "Unmanaged", "Reference")
names(manag.labs) <- c("c", "d", "l")


# For density plot:
dens_plot_details <- function() {
  list(
    geom_density(alpha = 0.5),
    geom_vline(xintercept = 0, colour="red", linetype = "dashed"),
    scale_fill_discrete(name = "Management", 
                        breaks = names(manag.labs), #manag_acc,    #c("c", "d"),
                        labels = manag.labs    #c("Managed", "Unmanaged")
  ))
}


# For raw values plotting: 
details_boxpl <- function() {
  list(
    scale_x_discrete(breaks = names(manag.labs),
                     labels = manag.labs),
    facet_grid(.~dom_sp),
    theme(legend.position = 'none',
          axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)
    )  
  )
}




# Input data -------------------------------------------------------------------
source('my_vars_and_functions.R')

getwd()
load(file = paste(getwd(), "outData/dataToPlot.Rdata", sep = '/'))
load(file = paste(getwd(), "outData/eco_traits.Rdata", sep = '/'))

# Identify data to use:
head(df_full_corr_mrg)    # - full PLOT based data: df_full_corr, seedlings, advanced, mature - PLOt & surroundings, mature trees filtered 
head(plot_IVI)            # - df importance value:from plot, env mature, env advanced, merged by density/ha
head(trait_df)            # - trait values for all species: eco_traits
head(df_mature_trees_env) # - trees in the surroundings: mature trees - set distance to 16 m if no tree present
head(df_advanced_env)     # - trees in the surroundings: advanced


# Master plots:
head(plot_counts_df)      # - total count of the plots per triplets & categories: to standardize the densities...


# Get dataframes of the species --------------------------------------------------
trip_species <- plot_counts_df %>% 
  dplyr::select(trip_n, dom_sp) %>% 
  distinct(.)


plot_counts_df <- plot_counts_df %>% 
  dplyr::select(!c(dom_sp))


# Master set for all triplets:
plot_counts_df_sum <- plot_counts_df %>% 
  group_by(trip_n, manag) %>%
  tally()

# get list of triplets
master_tripl <- distinct(dplyr::select(plot_counts_df_sum, trip_n))


# Prepare teh data: get the all available tree species in each site; fill in with 0 is teh species is not present
v_species <- unique(df_full_corr_mrg$species)

# RA: Reassembly: ------------------------------------------------------------------
#
nrow(plot_IVI)  # 15189 - rows with only existing species
# does it contains all species and height classes?



# get the 'total' table : combination of trip_n, sub_n, and unique specie
df_master_species <-   plot_counts_df %>% 
    mutate(species = 'Spruce') %>% 
    group_by(trip_n, manag, sub_n) %>% 
    complete(species = .env$v_species, fill = list(corr_count  = 0))
  


# make full table, Join the master one with the recorded species one
# having all combination of the species and plots (1244*12 = 14928)
plot_IVI_exp <- 
  plot_IVI %>% 
  full_join(df_master_species) %>% 
  ungroup(.) %>% 
  replace_na(list(rIVI = 0))
  


# CHeck species importance value: how often are 'Other species" dominant?? ---------------------
#plot_IVI_exp %>% 
#  ggplot(aes(x = factor(species),
#             y = rIVI)) + 
#  stat_summary(aes(group = manag,
 #                  color = manag))# + 
  #geom_boxplot()



# Get new propensity of forest reorganization: 
# need to get species importance values per plot, not per site!!

## RA1: Dominant species ----------------------------------------
# will the tree species dominating pre-disturbance dominate in post-disturbances?
# Process: in 3 steps: 
# - for REF: 1. average rIVI per species and plot get, get the species with max rIVI 
#            2. get SD: filter this species for each plot to gets its variability (SD)
# - for DIST: 3. compare with DIST: get average value per species for DIST, and compare with REF
 
# Change is indicated by decrease (REF > DIST) within the range of variation (SDref)

# Get first values for the Ref = 'l', get the mean and sd
# find first the average rIVI per species per site; 
# then find the highest average value and corresponding species
# compare what value have the species in D and C?
# need to get the SD: SD origin from the deviation of the dominant species across the plots
RA1_dom_ref <- plot_IVI_exp %>%
  filter(manag == 'l') %>%
  dplyr::select(trip_n, manag, sub_n, species, rIVI) %>%
  group_by(trip_n, manag, species) %>%
  summarize(ref_rIVI_mean = mean(rIVI, na.rm = T)) %>% 
  ungroup(.) %>% 
  group_by(trip_n) %>% 
  filter(ref_rIVI_mean == max(ref_rIVI_mean)) %>%
  dplyr::select(!c(manag))


# Get SD: select the dominant species per trip_n from the REF ('l'); and have the value for each plot!!
# need to add 0 if teh species is not present to get the SD! 
RA1_dom_SD <- 
  plot_IVI_exp %>%
  filter(manag == 'l') %>%
  dplyr::select(trip_n, manag, sub_n, species, rIVI) %>%
  right_join(RA1_dom_ref) %>% # filter only the dominant species in each REF triplet
  full_join(filter(plot_counts_df, manag == 'l'))  %>% # add 0 for missing plots (dominated by different species)
  group_by(trip_n, manag, species) %>% 
    mutate(ref_iIVI_sd = sd(rIVI, na.rm = T)) %>% 
  ungroup(.) %>% 
  dplyr::select(!c(manag)) %>%  
  rename(ref_rIVI = rIVI) %>% 
  dplyr::select(c(trip_n, species, ref_rIVI_mean, ref_iIVI_sd)) %>% 
  distinct()


# Calculate the difference between disturbed and reference conditions: 
df_RA1 <-
  plot_IVI_exp %>%
  filter(manag != 'l') %>%
  dplyr::select(trip_n, manag, species, rIVI) %>%
  group_by(trip_n, manag, species) %>%
  summarize(avg_iIVI_dist = mean(rIVI, na.rm = T)) %>%
  full_join(RA1_dom_SD, by = c("trip_n", "species")) %>%
  drop_na(.) %>% # remove NA values, to keep only species that are REF value (dominant under REF conditions)
  mutate(RA1 = (ref_rIVI_mean - avg_iIVI_dist) / ref_iIVI_sd)

### p_RA1: density Plot the values as density plot --------------------------------------------

p_RA1 <- 
  df_RA1 %>% 
  ggplot(aes(RA1, fill = manag)) + 
  xlim(-5,5) +
  dens_plot_details() +
  ggtitle('Dominant sp.')
  #geom_density(alpha = 0.8)

# 

# plot RA1 raw ---------------------------------------------------------

#p_RA1_pred <-
  plot_IVI_exp %>%
  dplyr::select(trip_n, manag, sub_n, species, rIVI) %>%
  group_by(trip_n, manag, species) %>%
  summarize(ref_rIVI_mean = mean(rIVI, na.rm = T)) %>% 
  ungroup(.) %>% 
  group_by(trip_n, manag) %>% 
  filter(ref_rIVI_mean == max(ref_rIVI_mean)) %>%
  as.data.frame() %>%
  left_join(trip_species, by = "trip_n") %>% 
  ggplot(aes(x = manag, #factor(manag),
             y = ref_rIVI_mean,
             color = dom_sp)) + # , 
  geom_point() + 
  geom_line(aes(group = trip_n), alpha = 0.5) +
  facet_grid(.~dom_sp)
 

#p_RA1_pred <-
plot_IVI_exp %>%
  dplyr::select(trip_n, manag, sub_n, species, rIVI) %>%
  group_by(trip_n, manag, species) %>%
  summarize(ref_rIVI_mean = mean(rIVI, na.rm = T)) %>% 
  ungroup(.) %>% 
  group_by(trip_n, manag) %>% 
  filter(ref_rIVI_mean == max(ref_rIVI_mean)) %>%
  as.data.frame() %>%
  left_join(trip_species, by = "trip_n") %>% 
  ggplot(aes(x = manag, #factor(manag),
             y = ref_rIVI_mean,
             color = dom_sp)) + # , 
  geom_point() + 
  geom_line(aes(group = trip_n), alpha = 0.5) +
  facet_grid(.~dom_sp)














# RA2: Tree species richness ---------------------------------------------------------------------
# compare tree species richness: REF <-> DIST, if richness decrease: indication of change!
# get from counts! 
# steps:
# 1. richness: count number of species average number of species REF, SD same
# 2. D - average number of species post-disturbance
RA2_ref <- 
  plot_IVI_exp %>%
  filter(sp_count  != 0 ) %>% 
  filter(manag == 'l') %>%
  group_by(trip_n, sub_n) %>% 
  summarise(richness = n()) %>% 
  ungroup(.) %>% 
  group_by(trip_n) %>%
  summarize(ref_sd_rich  = sd(richness, na.rm = F),
            ref_avg_rich = mean(richness, na.rm = F)) 


df_RA2 <-   
  plot_IVI_exp %>%
  filter(sp_count  != 0 ) %>% 
  filter(manag != 'l') %>%
  group_by(trip_n, manag, sub_n) %>% 
  summarise(richness = n()) %>% 
  ungroup(.) %>% 
  group_by(trip_n, manag) %>%
  summarize(dist_avg_rich = mean(richness, na.rm = F)) %>% 
  full_join(RA2_ref) %>% 
  mutate(RA2 = (ref_avg_rich - dist_avg_rich)/ref_sd_rich ) %>% 
  mutate(RA2 = case_when(is.infinite(RA2)~ 0,
                         TRUE ~ RA2)) #%>% 


## p_RA2: density: Plot the values as density plot --------------------------------------------
p_RA2 <-df_RA2 %>% 
  ggplot(aes(RA2, fill = manag)) +
  xlim(-3,3) +
  dens_plot_details()+
  ggtitle('Richness')





# RA2 Plot richness raw --------------------------------------------

p_RA2_pred <- plot_IVI_exp %>%
  filter(sp_count  != 0 ) %>% 
  group_by(trip_n, manag, sub_n) %>% 
  summarise(richness = n()) %>% 
  group_by(trip_n, manag) %>%
  summarize(mean_richness = mean(richness, na.rm = T)) %>% 
  as.data.frame() %>%
  left_join(trip_species, by = "trip_n") %>% 
  ggplot(aes(x = manag, #factor(manag),
             y = mean_richness,
             color = trip_n)) + # , 
  geom_point() + 
  geom_line(aes(group = trip_n), alpha = 0.5) +
  facet_grid(.~dom_sp) +
  theme(legend.position = 'none')
 

# test new plotting with function: density plot or histogram
# !!! remove!
#p_RA2_pred <- 
  plot_IVI_exp %>%
  filter(sp_count  != 0 ) %>% 
  group_by(trip_n, manag, sub_n) %>% 
  summarise(richness = n()) %>% 
  group_by(trip_n, manag) %>%
  summarize(mean_richness = mean(richness, na.rm = T)) %>% 
  as.data.frame() %>%
  left_join(trip_species, by = "trip_n") %>% 
  ggplot(aes(x = manag, #factor(manag),
             y = mean_richness,
             color = trip_n)) + # , 
 # geom_boxplot()+
    geom_point() + 
  geom_line(aes(group = trip_n), alpha = 0.5) +
 # facet_grid(.~dom_sp) +
#  theme(legend.position = 'none') + 
    details_boxpl() #+
    

 

# RA3: competition --------------------------------------------------------
# community weighted means of shade tolerance DIST <-> REF
RA3_ref <- 
  plot_IVI_exp %>% 
  left_join(trait_df, by = c('species')) %>% #, by = character()
  filter(manag == 'l') %>% 
  ungroup(.) %>% 
  group_by(trip_n) %>% 
  summarize(ref_mean_shade   = weighted.mean(Shade_tolerance,   
                                        rIVI, na.rm = TRUE),
            ref_sd_shade   = sd(Shade_tolerance, na.rm = TRUE)) #%>%
  

df_RA3 <- 
  plot_IVI_exp %>% 
  left_join(trait_df, by = c('species')) %>% #, by = character()
  filter(manag != 'l') %>% 
  ungroup(.) %>% 
  group_by(trip_n,  manag) %>% 
  summarize(dist_mean_shade   = weighted.mean(Shade_tolerance,   
                                             rIVI, na.rm = TRUE)) %>%
    left_join(RA3_ref, by = 'trip_n') %>% 
    mutate(RA3 = (dist_mean_shade -ref_mean_shade )/ref_sd_shade)



### p_RA3 density Plot the values as density plot --------------------------------------------
p_RA3 <- df_RA3 %>% 
  ggplot(aes(RA3, fill = manag)) +
  xlim(-1.5,1.5) +
  dens_plot_details() +
  ggtitle('Shade tolerance')



### p_RA3_raw  ----------------------------------------------------------------------------
p_RA3_pred <- plot_IVI_exp %>% 
  left_join(trait_df, by = c('species')) %>% #, by = character()
  ungroup(.) %>% 
  group_by(trip_n, manag) %>% 
  summarize(mean_shade   = weighted.mean(Shade_tolerance,   
                                             rIVI, na.rm = TRUE)) %>%
  as.data.frame() %>%
  left_join(trip_species, by = "trip_n") %>% 
  ggplot(aes(x = manag, #factor(manag),
             y = mean_shade,
             color = trip_n)) + # , 
  geom_point() + 
  geom_line(aes(group = trip_n), alpha = 0.5) +
  facet_grid(.~dom_sp) +
  theme(legend.position = 'none')



# RS1: stem density --------------------------------------------------------
RS1_ref <- 
  plot_IVI_exp %>% 
  filter(manag == 'l') %>% 
  group_by(trip_n) %>% 
  summarize(ref_mean_dens   = mean(all_count, na.rm = TRUE),
            ref_sd_dens     = sd(all_count, na.rm = TRUE)) #%>%


df_RS1 <- 
  plot_IVI_exp %>% 
  filter(manag != 'l') %>% 
  group_by(trip_n, manag) %>% 
  summarize(dist_mean_dens   = mean(all_count, na.rm = TRUE)) %>%
  left_join(RS1_ref, by = 'trip_n') %>% 
  mutate(RS1 = (ref_mean_dens  -dist_mean_dens  )/ref_sd_dens)


###  Plot the values as density plot -------------------------------------------
p_RS1 <- df_RS1 %>% 
  ggplot(aes(RS1, fill = manag)) +
  xlim(-15,15) +
  dens_plot_details()+
  ggtitle('Stem density')


### p_RS1 raw  ------------------------------------------------------------------
p_RS1_pred <- plot_IVI_exp %>% 
  group_by(trip_n,manag) %>% 
  summarize(mean_dens   = mean(all_count, na.rm = TRUE)) %>%
  as.data.frame() %>%
  left_join(trip_species, by = "trip_n") %>% 
  ggplot(aes(x = manag, 
             y = mean_dens,
             color = trip_n)) + # , 
  geom_point() + 
  geom_line(aes(group = trip_n), alpha = 0.5) +
  facet_grid(.~dom_sp) +
  theme(legend.position = 'none')




# RS2: Horizontal structure -----------------------------------------------
# define the overall layers: same as for vertical structure
# need to add disance of 16 m if the mature tree is not present
df_full_corr_mrg <- 
  df_full_corr_mrg %>%
  mutate(vert_layer = case_when(height_class %in% c("HK1", "HK2", "HK3", "HK4", "HK5","HK6") ~ 'regen',
                                height_class %in% c("HK7","adv_ENV" ) ~ 'advanced',
                                height_class %in% c("mature","mat_ENV" ) ~ 'mature')) 



# Get stem density by vertical classes:
p_density_vert <- 
  df_full_corr_mrg %>%  
    group_by(trip_n, manag, vert_layer) %>% 
   # summarize(mean_dens = mean(corr_count)) %>% 
    ggplot(aes(x = manag,
               y = corr_count, #mean_dens,
               fill = manag)) +
    geom_boxplot(outlier.size =0.5) +
    facet_wrap(.~vert_layer, scales = 'free')



# 3 layers: 
# - regeneration (<= less then 2 m height)
# - intermediate (> 2 m height & <= 10 cm DBH)
# - mature (> 10 cm dbh )
# if the mean number of layers per DIST > REF -> indication of change

# if there is no tree within 15 m, fill in value 16 m: 

# get the 'total' table : combination of trip_n, sub_n, and height classes
v_height_both = c('advanced', 
                  'mature')


# make master dataframe having both height categories: 
df_master_heights_both <-   
  plot_counts_df %>% 
  mutate(vert_layer = 'mature') %>% 
  group_by(trip_n, manag, sub_n) %>% 
  complete(vert_layer = .env$v_height_both)



# Process: 
# - get average nearest distance
# if mature tree is missing: complete by the distance of 16m*100

# complete by 0 both: advanced and Mature:
RS2_ref <- 
  df_full_corr_mrg %>%
  filter(count  != 0 ) %>% 
  filter(manag == 'l') %>%
  filter(vert_layer != 'regen') %>% 
    dplyr::select(trip_n, manag, sub_n, distance, vert_layer) %>%
     right_join(filter(df_master_heights_both, manag == 'l')) %>%
  mutate(distance = case_when(is.na(distance) ~ 16*100, # complete distances of 16 m if the tree is not present in ENV
                             !is.na(distance) ~ distance)) %>%
    group_by(trip_n, manag, sub_n, vert_layer) %>% 
    slice(which.min(distance)) %>% # filter to have only the shortesdt distance (if several trees were recorded eg on plot)
  ungroup(.) %>% 
    group_by(trip_n, manag, sub_n) %>% # vert_layer 
  summarise(mean_distance = mean(distance, na.rm = T)) %>%
  ungroup(.) %>% 
  group_by(trip_n) %>% #, vert_layer
  summarize(ref_mean_distance   = mean(mean_distance, na.rm = TRUE),
            ref_sd_distance     = sd(mean_distance, na.rm = TRUE)) #%>%

# for DIST
df_RS2 <- 
  df_full_corr_mrg %>% 
  filter(count  != 0 ) %>% 
  filter(manag != 'l') %>%
  filter(vert_layer != 'regen') %>% 
  dplyr::select(trip_n, manag, sub_n, distance, vert_layer) %>%
  right_join(filter(df_master_heights_both, manag != 'l')) %>%
  mutate(distance = case_when(is.na(distance) ~ 16*100, # complete distances of 16 m if the tree is not present in ENV
                              !is.na(distance) ~ distance)) %>%
  group_by(trip_n, manag, sub_n) %>% 
  slice(which.min(distance)) %>% # filter to have only the shortesdt distance (if several trees were recorded eg on plot)
  ungroup(.) %>% 
  group_by(trip_n, manag, sub_n) %>% #, vert_layer
  summarise(mean_distance = mean(distance, na.rm = T)) %>%
  ungroup(.) %>% 
  group_by(trip_n, manag) %>% 
  summarize(dist_mean_distance   = mean(mean_distance, na.rm = TRUE)) %>% 
  left_join(RS2_ref, by  = "trip_n") %>% 
  mutate(RS2 = (dist_mean_distance  - ref_mean_distance )/ref_sd_distance)


# plot the values as density plot
p_RS2 <- df_RS2 %>% 
  ggplot(aes(RS2, fill = manag)) +
  #xlim(-3,3) +
  dens_plot_details()+
  ggtitle('Horizontal str. [adv+mature]')

p_RS2




# get a plot of teh average distances -------------------------------------------
p_dist_16 <- df_full_corr_mrg %>%
  filter(count  != 0 ) %>% 
  filter(vert_layer != 'regen') %>% 
  dplyr::select(trip_n, manag, sub_n, distance, vert_layer) %>%
  right_join(df_master_heights_both) %>%
  mutate(distance = case_when(is.na(distance) ~ 16*100, # complete distances of 16 m if the tree is not present in ENV
                              !is.na(distance) ~ distance)) %>%
  group_by(trip_n, manag, sub_n, vert_layer) %>% 
  slice(which.min(distance)) %>% # filter to have only the shortesdt distance (if several trees were recorded eg on plot)
  ggplot(aes(y = distance/100,
             x = vert_layer,
             color = manag)) +
  stat_summary() +
  coord_cartesian(ylim = c(0, 8)) +
  ggtitle('Added missing trees [set to 16 m]')


# get a plot of teh average distances -------------------------------------------
p_dist_0 <- df_full_corr_mrg %>%
  filter(count  != 0 ) %>% 
  filter(vert_layer != 'regen') %>% 
  dplyr::select(trip_n, manag, sub_n, distance, vert_layer) %>%
  group_by(trip_n, manag, sub_n, vert_layer) %>% 
  slice(which.min(distance)) %>% # filter to have only the shortesdt distance (if several trees were recorded eg on plot)
  ggplot(aes(y = distance/100,
             x = vert_layer,
             color = manag)) +
  stat_summary() +
  ggtitle('Missing trees') +
  coord_cartesian(ylim = c(0, 8))


p_distances <- ggarrange(p_dist_0, 
          p_dist_16,
          nrow = 1, ncol = 2,
          hjust=-0.8)





# [do not run] Compare only horizontal distance to: ----------------------
# - 1 mature trees:
# - to advanced regen:
# Make unique plots for: 
# - all trees (PLOt + ENV), 
# - only ENV 
# - only the nearest PLOT or ENV?
p_avg_distance_nearest <- 
  df_full_corr_mrg %>%
  filter(count  != 0 ) %>%
  filter(vert_layer != 'regen') %>%
  dplyr::select(trip_n, manag, sub_n, distance, height_class) %>%
  mutate(distan_class = case_when(height_class %in% c("mature","mat_ENV") ~ 'mature',
                                height_class %in% c("adv_ENV", "HK7")   ~ 'advanced')) %>%
  group_by(trip_n, manag, sub_n, distan_class) %>% # , height_class

  full_join(plot_counts_df) %>% # add the 0 distances:! how to account if tree is missing??
  mutate(distance = case_when(is.na(distance) ~ 16*100, # complete distances of 16 m if the tree is not present in ENV
                              !is.na(distance) ~ distance)) %>%
  slice(which.min(distance)) %>% # find teh closest mature tree: in plot or in ENV
  ggplot(aes(x = factor(manag),
             y = distance/100,
             color = distan_class)) +
  stat_summary() +
  scale_color_manual(name = "Height class",
                     breaks=c("mature", "advanced"),
                     labels=c("mature", "advanced"),
                     values = c("darkgreen","red")) +
  ylab('Avg Distance [m]') +
  ggtitle('Nearest Tree\nMature/advanced reg [m]') +
  coord_cartesian(ylim = c(0, 8)) +
  theme(legend.position = 'bottom')
# 
# 


# RS3: Vertical structure -------------------------------------------------
# 3 layers: 
# - regeneration (<= less then 2 m height)
# - intermediate (> 2 m height & <= 10 cm DBH)
# - mature (> 10 cm dbh )
# if the mean number of layers per DIST > REF -> indication of change

# Process: 
# - define vertical layers, count them
RS3_ref <- 
  df_full_corr_mrg %>%
  filter(count  != 0 ) %>% 
  filter(manag == 'l') %>%
  dplyr::select(trip_n, manag, sub_n, vert_layer) %>%
  distinct(.) %>%
  group_by(trip_n, manag, sub_n) %>% 
  summarise(vertical_n = n()) %>%
  ungroup(.) %>% 
  group_by(trip_n) %>% 
  summarize(ref_mean_vLayer   = mean(vertical_n, na.rm = TRUE),
            ref_sd_vLayer     = sd(vertical_n, na.rm = TRUE))# %>%
  #mutate(ref_sd_vLayer = case_when(ref_sd_vLayer == 0 ~ ref_sd_vLayer+ 0.001, # change SD by small number to not to have SD  = 0
   #                                ref_sd_vLayer != 0 ~ ref_sd_vLayer) )
#df_RS3

# for DIST
df_RS3 <- 
  df_full_corr_mrg %>% 
  filter(count  != 0 ) %>% 
  filter(manag != 'l') %>%
  dplyr::select(trip_n, manag, sub_n, vert_layer) %>%
  distinct(.) %>%
  #filter(trip_n == '7' & manag == 'l' & sub_n == '1') 
  group_by(trip_n, manag, sub_n) %>% 
  summarise(vertical_n = n()) %>%
  ungroup(.) %>% 
  group_by(trip_n, manag) %>% 
  summarize(dist_mean_vLayer   = mean(vertical_n, na.rm = TRUE)) %>%
  left_join(RS3_ref, by  = "trip_n") %>% 
  mutate(RS3 = (dist_mean_vLayer - ref_mean_vLayer)/ref_sd_vLayer) %>%
  ungroup(.) %>% 
  mutate(RS3 = case_when(is.infinite(RS3)~ 0,
                         TRUE ~ RS3)) %>% 
  mutate(RS3 = replace_na(RS3,0))


# replace Inf value by 0:
df_RS3[as.matrix(df_RS3) == Inf]  <- 0

# Plot distribution of vertical classes: by management and tree species
# !!!??? showhow often which layer is missing? by species, manag, height class? 
# df_full_corr_mrg %>% 
#   filter(count  != 0 ) %>% 
#   dplyr::select(trip_n, manag, sub_n, vert_layer) %>%
#   distinct(.) %>%
#  # filter(trip_n == '7' & manag == 'l' & sub_n == '1') 
#   group_by(trip_n, manag, sub_n, vert_layer) %>% 
#   summarise(vertical_n = n()) #%>%
#   ungroup(.) %>% 
#   group_by(trip_n, manag, vert_layer) %>% 
#   summarize(mean_vLayer   = mean(vertical_n, na.rm = TRUE))# %>%
#   ggplot(aes(x = as.factor(manag),
#              y = mean_vLayer)) +
#   geom_col(identity = )
  
### Plot RS3: raw ---------------------------------------------------------------
p_RS3_pred <- 
  df_full_corr_mrg %>%
  filter(count  != 0 ) %>% 
  dplyr::select(trip_n, manag, sub_n, vert_layer) %>%
  distinct(.) %>%
  group_by(trip_n, manag, sub_n) %>% 
  summarise(vertical_n = n()) %>%
  ungroup(.) %>% 
  group_by(trip_n, manag) %>% 
  summarize(mean_vLayer   = mean(vertical_n, na.rm = TRUE)) %>%
  as.data.frame() %>%
  left_join(trip_species, by = "trip_n") %>% 
  ggplot(aes(x = manag, #factor(manag),
             y = mean_vLayer,
             color = trip_n)) + # , 
  geom_point() + 
  geom_line(aes(group = trip_n), alpha = 0.5) +
  facet_grid(.~dom_sp) +
  theme(legend.position = 'none')
  




# plot the values as density plot
p_RS3 <- df_RS3 %>% 
  ggplot(aes(RS3, fill = manag)) +
  xlim(-4.3,4.3) +
  dens_plot_details() +
  ggtitle('Vertical str.')





# Plot densities together  ------------------------------------------------

p_6vars <- ggarrange(
  p_RA1 + ylim(0,2),
  p_RA2 + ylim(0,2) ,
  p_RA3 + ylim(0,2) ,
  p_RS1 + ylim(0,2) ,
  p_RS2 + ylim(0,2) ,
  p_RS3 + ylim(0,2) ,
  nrow = 2,
  ncol = 3,
  common.legend = TRUE,
  legend = 'bottom'
)

# Join databases into one indicator by triplet -----------------------------
# change is always indicated by the + values, - indicates the no change
# convert - and Inf values to 0
# add reference values

out_reorg <- 
  dplyr::select(df_RA1,           c(trip_n, manag, RA1, ref_rIVI_mean )) %>% 
  full_join(dplyr::select(df_RA2, c(trip_n, manag, RA2, ref_avg_rich    ))) %>%
  full_join(dplyr::select(df_RA3, c(trip_n, manag, RA3, ref_mean_shade ))) %>% 
  full_join(dplyr::select(df_RS1, c(trip_n, manag, RS1, ref_mean_dens ))) %>% #,
  full_join(dplyr::select(df_RS2, c(trip_n, manag, RS2, ref_mean_distance ))) %>%
  full_join(dplyr::select(df_RS3, c(trip_n, manag, RS3, ref_mean_vLayer )))#%>% 
  
 # mutate() # combine indicators together
#
out_reorg_pos <- out_reorg

# Change negative values to 0 (no change)
out_reorg_pos[out_reorg_pos <0]     <- 0
out_reorg_pos[is.na(out_reorg_pos)] <- 0


# merge indicators: -----------------------------------------
out_reorg_pos <- out_reorg_pos %>% 
  mutate(RA_mean = (RA1+RA2+RA3)/3,
         RS_mean = (RS1+RS2+RS3)/3) 



# Get Euclidean distance: scatter points from [0,0] --------------------------
out_reorg_pos <- out_reorg_pos %>% 
  mutate(euclid_dist = euclidean(RA_mean, RS_mean)) #%>%
  # classify the poinst by sector: make as squares, as simpler way
  #mutate(sector =  )


# color scheme testing  ---------------------------------------------------

# The best:nature -------------------------------------------------
# 801 
my_sp_vals = c('spruce'= '#7CBB00', # light green
               'beech' = '#FFBB00', # yellow,
               'oak'   =  '#F65314',  # red
               'pine' = '#3A606E')  #  bluish



# plot Euclidean distance : -----------------------------------------------


# show euclidean distances for each triplet
library(ggrepel)
p_euclid_lollipop <- 
  out_reorg_pos %>% 
  left_join(trip_species) %>% 
    mutate(trip_manag = paste(trip_n, manag, '_')) %>% 
    na.omit() %>% 
  ggplot(aes(x = reorder(trip_manag,-euclid_dist) ,
             y = euclid_dist,
             color = dom_sp,
             group = manag)) +
  geom_point(size = 3) +
  geom_segment( aes(x=reorder(trip_manag, -euclid_dist) , 
                    xend=reorder(trip_manag, -euclid_dist) , 
                    y=0, 
                    yend=euclid_dist)) +
  scale_color_manual(values = my_sp_vals ,
                     name = 'Dominant species') +
    facet_wrap(.~manag, 
               scale = 'free_x', 
               labeller = labeller(manag = manag.labs)) +
    xlab('Triplet number') +
  ylab('Euclidean distance') +
   # ggrepel::geom_text_repel(aes(label = trip_n, color = dom_sp),  size =3.5) +
  theme(axis.text.x= element_blank())  
                 
#windows()
#(p_euclid_lollipop)



# Reorganization: plot averages: ---------------------------------
p_scatter_mean <- 
  out_reorg_pos %>% 
  left_join(trip_species) %>% 
  ggplot(aes(x = RA_mean,
             y = RS_mean,
             color = dom_sp)) +
  geom_abline(intercept = 0, 
              slope = c(0.5, 1.8), 
              size = 0.5, lty = 'dashed', color = 'grey') +
  geom_point(alpha = 0.9, size = 1.4) +
  scale_color_manual(values = my_sp_vals ,
                     name = 'Dominant species') +
  xlim(0,2) +
  ylim(0,2) +
  facet_grid(manag~dom_sp, 
             #scales = 'free',
             labeller = labeller(manag = manag.labs)) +
  theme_update(legend.position = 'bottom',
               aspect.ratio=1) # make plots perfect square


p_scatter_mean



# Classify points by sectors ---------------------------------------
library(tidyverse)
library(ggthemes)
set.seed(123)
dd <- data.frame(x = runif(200, min=0, max=2),
                 y = runif(200, min=0, max=2))

slope = 30 #degrees



# Categorize triplets categories: only b sector: 
# RS, RA, both
res_classes <- 
  out_reorg_pos %>% 
  dplyr::select(RA_mean, RS_mean, euclid_dist) %>% 
  left_join(trip_species) %>% 
  #calculate dfistance from origin
  mutate(orig_dist = sqrt(RA_mean^2 + RS_mean^2)) %>%
  #calculate position (origin, far, etc..)
  mutate(position = case_when(orig_dist < 0.5 ~ "resilience",
                              orig_dist >= 1.5 ~ "-extreme",
                              TRUE ~ "")) %>%
  #calculate XY label
  mutate(labelXY = case_when((180*atan(RA_mean / RS_mean) / pi) < slope ~ "RS",
                             (180*atan(RA_mean / RS_mean) / pi) > (90 - slope) ~ "RA", 
                             TRUE ~ "RA-RS")) #%>%
  # #create group category
  # mutate(group = ifelse(position == "resilience", 
  #                       position, 
  #                       paste0(labelXY, position))) %>%
  # mutate(group = factor(group, levels = c('resilience',
  #                                         'RA',
  #                                         'RA-extreme',
  #                                         'RS',
  #                                         'RS-extreme',
  #                                         "RA-RS",
  #                                         "RA-RS-extreme")))



# Plot by sector colors: --------------------------------------------------------
my_Resilience_class = c('RA' ='gold',  'grey45', # light green
                        'RS' = 'black', # yellow,
                        'RA-RS' =  'red')  #  bluish


p_scatter_mean_col_sect <- 
  res_classes %>% 
  ggplot(aes(x = RA_mean,
             y = RS_mean,
             color = labelXY)) +
  geom_abline(intercept = 0, 
              slope = c(0.6, 1.8), 
              size = 0.5, lty = 'dotted', color = 'grey') +
  geom_point(alpha = 0.9, size = 1.2, shape= 16) +
  scale_color_manual(values = my_Resilience_class ,
                     name = 'Forest reorganization') +
  facet_grid(manag~dom_sp, 
             #scales = 'free',
             labeller = labeller(manag = manag.labs)) +
  labs(x = "Reassembly",
       y = "Restructure") + 
  scale_x_continuous(breaks = seq(0, 2, by = 1), limits = c(0,2)) +
  scale_y_continuous(breaks = seq(0, 2, by = 1), limits = c(0,2)) +
  theme_bw() +
  theme_update(legend.position = 'bottom',
              aspect.ratio=1,
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid  = element_blank()) # make plots perfect squar4
  
p_scatter_mean_col_sect






# Define the slopes of the lines that divide the area into x, y, xy
slope1 <- 0.5
slope2 <- 2

# Define the radii of the circles that define the origin,?, far areas,
# which I've called near, mid, far
r1 <- 0.5
r2 <- 1.5

  
p_res_classes <- res_classes %>%   
  ggplot(aes(x = RA_mean, y = RS_mean, color = dom_sp)) + 
  geom_point(alpha = 0.7) +
  ggthemes::scale_color_colorblind() +
  theme_bw() + theme_update(aspect.ratio=1) +
  geom_abline(intercept = 0, slope = 0.5, size = 0.5, lty = 'dashed', color = 'grey20') +
  geom_abline(intercept = 0, slope = 1.8, size = 0.5, lty = 'dashed', color = 'grey20') +
  annotate("path",
           x = r1*cos(seq(0,2*pi,length.out=100)),
           y = r1*sin(seq(0,2*pi,length.out=100)),
           size = 0.5, lty = 'dashed', color = 'grey20'
  ) +
  annotate("path",
           x = r2*cos(seq(0,2*pi,length.out=100)),
           y = r2*sin(seq(0,2*pi,length.out=100)),
           size = 0.5, lty = 'dashed', color = 'grey20'
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2.3)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2.3)) +
  facet_grid(.~manag, labeller = labeller(manag = manag.labs)) +
  xlab('Reassembly') +
  ylab('Restructure')
  

#windows()
p_res_classes





# Evaluate the drivers: change by indicators ---------------------------------------------------------

# how to show the drivers??? per indicator, per species, per management?
p_segment <- out_reorg_pos %>% 
  dplyr::select(all_of(c('RA1', 'RA2', 'RA3', 'RS1', 'RS2', 'RS3'))) %>% 
  pivot_longer(c(RA1, RA2, RA3, RS1, RS2, RS3),
               names_to = 'indicator',
               values_to = 'vals') %>% 
   left_join(trip_species) %>%
  mutate(ind_manag = paste(indicator, manag)) %>% 
  group_by(dom_sp, manag, indicator) %>% 
  summarize(mean = mean(vals, na.rm = T)) %>% 
  mutate(indicator2 = factor(indicator, 
                            levels = c('RS3', 'RS2', 'RS1', 'RA3', 'RA2', 'RA1'))) %>% 
  ggplot(aes(y = reorder(indicator, mean),#indicator2,
             x = mean,
             color = dom_sp,
             shape = manag,
             group = manag)) +
   geom_point(size = 2) +
  geom_segment( aes(x=0,
                    xend=mean,
                    y=reorder(indicator, mean),
                    yend=reorder(indicator, mean))) +
  scale_color_manual(values = my_sp_vals ,
                     name = 'Dominant species') +
  facet_grid(manag~ dom_sp, 
             scale = 'free_x', 
             labeller = labeller(manag = manag.labs))# +
 


#  Test why grouping does not work?? --------------------------------------

p_drivers <- 
  out_reorg_pos %>% 
  dplyr::select(all_of(c('RA1', 'RA2', 'RA3', 'RS1', 'RS2', 'RS3'))) %>% 
  pivot_longer(c(RA1, RA2, RA3, RS1, RS2, RS3),
               names_to = 'indicator',
               values_to = 'vals') %>% 
  left_join(trip_species) %>%
  mutate(reorg_type= case_when(grepl("RA", indicator) ~ "Reassembly",
                               grepl("RS", indicator) ~ "Restructure")) %>% 
  group_by(dom_sp, manag, indicator) %>% 
  mutate(indicator2 = factor(indicator, 
                             levels = c('RS3', 'RS2', 'RS1', 'RA3', 'RA2', 'RA1'))) %>% 
  ggplot(aes(y = indicator2, #reorder(dom_sp, vals),#indicator2,
             x = vals,
             color = dom_sp)) +
  geom_vline(xintercept = 0, col = 'grey50', linetype = 'dashed') +
  stat_summary(geom = 'point',
               fun = 'mean', #, 
              position =  position_dodge(width = 0.6)
              ) +
  stat_summary(geom = 'errorbar', 
               fun.data = mean_cl_normal, # mean_se, #mean_cl_normal,# mean_sdl, #, 
               #fun.args=list(mult = 3), 
               position =  position_dodge(width = 0.6)
               ) +
  scale_color_manual(values = my_sp_vals ,
                     name = 'Dominant species') +
  scale_x_continuous(breaks = seq(0, 2.9, by = 1)) +
  ylab('') +
  xlab('Z-score') +
  facet_grid(reorg_type~manag, 
             scales="free_y", 
             labeller = labeller(manag = manag.labs)) +
  theme(axis.text.x = element_text(size = 8),
        panel.grid.major = element_line(color="grey90", linetype ='dotted'),#element_blank(),
        panel.grid.minor = element_blank())


p_drivers



# Export classified resilience to the coordinates -------------------------

library(sf)

sites <- st_read('C:/Users/ge45lep/Documents/2021_Franconia_mortality/03_plot_sampling/sites_identification/final/share/sites_final.shp')

# remove sites that were skipped: 45 & 65
sites2 <- 
  sites %>% 
  filter(!Name %in% c('45-oak-D',
                      '45-oak-L',
                      '45-oak-C',
                      '65-pine-D',
                      '65-pine-L',
                      '65-pine-C')) %>% 
    separate(Name, c('trip_n', 'dom_sp', 'manag'), '-') %>%
    mutate(manag = tolower(manag),
           trip_n = as.character(as.numeric(trip_n))) %>% 
    filter(manag != 'l') # %>%

# add resilience category (df) into the sf
sites_out <- sites2 %>% 
  left_join(dplyr::select(res_classes, c('trip_n', 'manag', 'group'))) #%>% 
   # nrow()

st_write(sites_out, 
         'C:/Users/ge45lep/Documents/2021_Franconia_mortality/outSpatial/resilience_class/sites_resilience.shp',
         append=FALSE)

# 
# Export objects -----------------------------------------------------------

save.image(file="outData/dat_restr.Rdata")

