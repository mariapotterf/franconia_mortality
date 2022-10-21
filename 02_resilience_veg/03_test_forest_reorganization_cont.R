# Quantify forest reorganization;

# read data
# each triplet categorize in one of teh characteristics:
# as proposed by Rupert
# for inpt data: use the plot + nearest distance tree data to get the dbh, BA, stem density, etc...

# To do:
# how to show sensitivity analysis instead of thersold values? ; eg for novelty??
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

# make a function to store details about plotting
dens_plot_details <- function() {
  list(
    geom_density(alpha = 0.5),
    geom_vline(xintercept = 0, colour="red", linetype = "dashed"),
    scale_fill_discrete(name = "Management", 
                        breaks=c("c", "d"),
                        labels=c("Clear-cut", "Dead")),
    theme_test()
  )
}


# Input data -------------------------------------------------------------------
getwd()
load(file = paste(getwd(), "outData/dataToPlot.Rdata", sep = '/'))
load(file = paste(getwd(), "outData/eco_traits.Rdata", sep = '/'))

# Identify data to use:
head(df_full_corr)        # - full PLOT based data: df_full_corr, seedlings, advanced, mature 
head(plot_IVI)            # - df importance value:from plot, env mature, env advanced, merged by density/ha
head(trait_df)            # - trait values for all species: eco_traits
head(df_mature_trees_env) # - trees in the surroundings: mature trees
head(df_advanced_env)     # - trees in the surroundings: advanced


# Master plots:
head(plot_counts_df)      # - total count of the plots per triplets & categories: to standardize the densities...


# get dataframe of the species
trip_species <- plot_counts_df %>% 
  select(trip_n, dom_sp) %>% 
  distinct(.)


plot_counts_df <- plot_counts_df %>% 
  select(!c(dom_sp))



# Master set for all triplets:
plot_counts_df_sum <- plot_counts_df %>% 
  group_by(trip_n, manag) %>%
  tally()

# get list of triplets
master_tripl <- distinct(select(plot_counts_df_sum, trip_n))


# Prepare teh data: get the all available tree species in each site; fill in with 0 is teh species is not present
v_species <- unique(df_full_corr$species)

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
  

# CHeck if values are correct? Seems correct
plot_IVI_exp %>% 
  filter(trip_n == 1 & sub_n == 1 & manag == "c") %>% 
  print(n = 40)






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
  select(!c(manag))


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
  select(!c(manag)) %>%  
  rename(ref_rIVI = rIVI) %>% 
  select(c(trip_n, species, ref_rIVI_mean, ref_iIVI_sd)) %>% 
  distinct()


# Calculate the difference betwee disturbed and reference condistions: 
df_RA1 <-
  plot_IVI_exp %>%
  filter(manag != 'l') %>%
  dplyr::select(trip_n, manag, species, rIVI) %>%
  group_by(trip_n, manag, species) %>%
  summarize(avg_iIVI_dist = mean(rIVI, na.rm = T)) %>%
  full_join(RA1_dom_SD, by = c("trip_n", "species")) %>%
  drop_na(.) %>% # remove NA values, to keep only species that are REF value (dominant under REF conditions)
  mutate(RA1 = (ref_rIVI_mean - avg_iIVI_dist) / ref_iIVI_sd)

### Plot the values as density plot --------------------------------------------

p_RA1 <- df_RA1 %>% 
  ggplot(aes(RA1, fill = manag)) + 
  xlim(-5,5) +
  dens_plot_details() +
  ggtitle('Dominant sp.')
  #geom_density(alpha = 0.8)

# 




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


## Plot the values as density plot --------------------------------------------
p_RA2 <-df_RA2 %>% 
  ggplot(aes(RA2, fill = manag)) +
  xlim(-3,3) +
  dens_plot_details()+
  ggtitle('Richness')





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



### Plot the values as density plot --------------------------------------
p_RA3 <- df_RA3 %>% 
  ggplot(aes(RA3, fill = manag)) +
  xlim(-1.5,1.5) +
  dens_plot_details() +
  ggtitle('Shade tolerance')





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




# RS2: Horizontal structure -----------------------------------------------
# define the overall layers: same as for vertical structure
df_full_corr <- 
  df_full_corr %>%
  mutate(vert_layer = case_when(height_class %in% c("HK1", "HK2", "HK3", "HK4", "HK5","HK6") ~ 'regen',
                                height_class %in% c("HK7","adv_ENV" ) ~ 'advanced',
                                height_class %in% c("mature","mat_ENV" ) ~ 'mature')) 

# 3 layers: 
# - regeneration (<= less then 2 m height)
# - internediate (> 2 m height & <= 10 cm DBH)
# - mature (> 10 cm dbh )
# if the mean number of layers per DIST > REF -> indication of change

# Process: 
# - get average nearest distance
RS2_ref <- 
  df_full_corr %>%
  filter(count  != 0 ) %>% 
  filter(manag == 'l') %>%
  filter(vert_layer != 'regen') %>% 
  dplyr::select(trip_n, manag, sub_n, distance, vert_layer) %>%
  group_by(trip_n, manag, sub_n) %>% # vert_layer 
  summarise(mean_distance = mean(distance, na.rm = T)) %>%
  ungroup(.) %>% 
  group_by(trip_n) %>% #, vert_layer
  summarize(ref_mean_distance   = mean(mean_distance, na.rm = TRUE),
            ref_sd_distance     = sd(mean_distance, na.rm = TRUE)) #%>%

# for DIST
df_RS2 <- 
  df_full_corr %>% 
  filter(count  != 0 ) %>% 
  filter(manag != 'l') %>%
  filter(vert_layer != 'regen') %>% 
  dplyr::select(trip_n, manag, sub_n, distance, vert_layer) %>%
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
  xlim(-3,3) +
  dens_plot_details()+
  ggtitle('Horizontal str.')



# Compare only horizontal distance to: ----------------------
# - 1 mature trees:
# - to advanced regen:
# Make unique plots for: 
# - all trees (PLOt + ENV), 
# - only ENV 
# - only the nearest PLOT or ENV?
p_avg_distance_nearest <- df_full_corr %>% 
  filter(count  != 0 ) %>% 
  #filter(manag != 'l') %>%
  filter(vert_layer != 'regen') %>% 
  dplyr::select(trip_n, manag, sub_n, distance, height_class) %>%
  mutate(distan_class = case_when(height_class %in% c("mature","mat_ENV") ~ 'mature',
                                height_class %in% c("adv_ENV", "HK7")   ~ 'advanced')) %>% 
  group_by(trip_n, manag, sub_n, distan_class) %>% # , height_class
  slice(which.min(distance)) %>% # find teh closest mature tree: in plot or in ENV
  full_join(plot_counts_df) %>% # add the 0 distances:! how to account if tree is missing??
  ggplot(aes(x = factor(manag),
             y = distance/100,
             color = distan_class)) +
  stat_summary() + 
  scale_color_manual(name = "Height class", 
                     breaks=c("mature", "advanced"),
                     labels=c("mature", "advanced"),
                     values = c("darkgreen","red")) +
  ylab('Nearest tree\n Mature/advanced reg [m]') +
  ggtitle('Nearest Mature/advanced reg [m]') +
  ylim(0,6) +
  theme(legend.position = 'bottom')


# aveg distance all Mature:
p_avg_distance_mature <- 
  df_full_corr %>% 
  filter(count  != 0 ) %>% 
  filter(vert_layer != 'regen') %>% 
  dplyr::select(trip_n, manag, sub_n, distance, height_class) %>%
    filter(height_class %in% c("mature","mat_ENV")) %>% 
  full_join(plot_counts_df) %>% # add the 0 distances:! how to account if tree is missing??
  ggplot(aes(x = factor(manag),
             y = distance/100,
             color = height_class)) +
  stat_summary(fun = mean) + 
    scale_color_manual(name = "Location", 
                       breaks=c("mat_ENV", "mature"),
                       labels=c("ENV", "Plot"),
                       values = c("red","black")) +
  ylab('Mean distance to\nthe nearest Mature tree\n [m]') +
  ylim(0,6)+
  ggtitle('Mature tree') +
  theme(legend.position = 'bottom') 




# avg distance all advanced:
p_avg_distance_adv <- 
  df_full_corr %>% 
    filter(count  != 0 ) %>% 
    filter(vert_layer != 'regen') %>% 
    dplyr::select(trip_n, manag, sub_n, distance, height_class) %>%
    filter(height_class %in% c("HK7","adv_ENV")) %>%
    full_join(plot_counts_df) %>% # add the missing trees:! how to account if tree is missing??
    ggplot(aes(x = factor(manag),
               y = distance/100,
               color = height_class)) +
    stat_summary(fun = mean) + 
    scale_color_manual(name = "Location", 
                       breaks=c("adv_ENV", "HK7"),
                       labels=c("ENV", "Plot"),
                       values = c("red","black")) +
    ylab('Mean distance to\nthe nearest Advanced tree\n [m]') +
  ylim(0,6) +
  ggtitle('Advanced reg tree') +
  theme(legend.position = 'bottom') 

  

p_distances <- ggarrange(p_avg_distance_nearest, 
                         p_avg_distance_adv,
                         p_avg_distance_mature,
                         nrow = 1, ncol = 3,
                         hjust=-0.8)

(p_distances)
# RS3: Vertical structure -------------------------------------------------
# 3 layers: 
# - regeneration (<= less then 2 m height)
# - internediate (> 2 m height & <= 10 cm DBH)
# - mature (> 10 cm dbh )
# if the mean number of layers per DIST > REF -> indication of change

# Process: 
# - define vertical layers, count them
RS3_ref <- 
  df_full_corr %>%
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
  df_full_corr %>% 
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


# plot the values as density plot
p_RS3 <- df_RS3 %>% 
  ggplot(aes(RS3, fill = manag)) +
  xlim(-4.3,4.3) +
  dens_plot_details() +
  ggtitle('Vertical str.')





# Plot densities together  ------------------------------------------------

p_6vars <- ggarrange(
  p_RA1 + ylim(0,1.5),
  p_RA2 + ylim(0,1.5) ,
  p_RA3 + ylim(0,1.5) ,
  p_RS1 + ylim(0,1.5) ,
  p_RS2 + ylim(0,1.5) ,
  p_RS3 + ylim(0,1.5) ,
  nrow = 2,
  ncol = 3,
  common.legend = TRUE,
  legend = 'bottom'
)

# Join databases into one indicator by triplet -----------------------------
# change is always indicated by the + values, - indicates the no change
# convert - and Inf values to 0

out_reorg <- 
  select(df_RA1, c(trip_n, manag, RA1)) %>% 
  full_join(select(df_RA2, c(trip_n, manag, RA2))) %>%
  full_join(select(df_RA3, c(trip_n, manag, RA3))) %>% 
  full_join(select(df_RS1, c(trip_n, manag, RS1))) %>% #,
  full_join(select(df_RS2, c(trip_n, manag, RS2))) %>%
  full_join(select(df_RS3, c(trip_n, manag, RS3)))#%>% 
  
 # mutate() # combine indicators together
#
out_reorg_pos <- out_reorg

# Change negative values to 0 (no change)
out_reorg_pos[out_reorg_pos <0]     <- 0
out_reorg_pos[is.na(out_reorg_pos)] <- 0


# merge indicators: -----------------------------------------
out_reorg_pos <- out_reorg_pos %>% 
  mutate(RA_mean = (RA1+RA2+RA3)/3,
         RS_mean = (RS1+RS2+RS3)/3,
         RA_sum  = RA1+RA2+RA3,
         RS_sum  = RS1+RS2+RS3) 


# Reorganization plot -----------------------------------------------------
out_reorg_pos %>% 
  left_join(trip_species) %>% 
  ggplot(aes(x = RA_mean,
             y = RS_mean,
             color = dom_sp)) +
  geom_point()




# plot averages: ---------------------------------
p_scatter_mean <- 
  out_reorg_pos %>% 
  left_join(trip_species) %>% 
  ggplot(aes(x = RA_mean,
             y = RS_mean,
             color = manag)) +
  scale_color_manual(name = "Management", 
                     breaks=c("c", "d"),
                     labels=c("Clear-cut", "Dead"),
                     values = c("red","black")) +
  geom_point() +
  xlim(0,2) +
  ylim(0,2) +
  geom_abline(intercept = 0, # add diagnal line
              slope = c(0.5,2),
              col = "grey",
              size = .5,
              lty = 'dashed') +
  facet_grid(manag~dom_sp, scales = 'free') +
  theme_update(legend.position = 'bottom') +
  theme_update(aspect.ratio=1) # make plots perfect square


p_scatter_mean



# plot of sums -----------------------------------------------------
p_reorg_manag_dom_sp <- out_reorg_pos %>% 
  left_join(trip_species) %>% 
  ggplot(aes(x = RA_sum,
             y = RS_sum)) +
  geom_point() +
  xlim(0,7) +
  ylim(0,7) +
  geom_abline(intercept = 0, # add diagonal line
              slope = c(0.5,2),
              col = "grey",
              size = .5,
              lty = 'dashed') +
  facet_grid(manag~dom_sp) +
  theme_update(legend.position = 'bottom') +
  theme_update(aspect.ratio=1) # make plots perfect square


# plot sums: just two colors: ----------------------------------------
windows()
#p_scatter_manag_sum <- 
  out_reorg_pos %>% 
  left_join(trip_species) %>% 
  ggplot(aes(x = RA_sum/3,
             y = RS_sum/3,
             shape = manag,
             color = manag
             )) +
  geom_point() +
  scale_color_manual(name = "Management", 
                     breaks=c("c", "d"),
                     labels=c("Clear-cut", "Dead"),
                     values = c("red","black")) +
  scale_shape_manual(name = "Management", 
                     breaks=c("c", "d"),
                     labels=c("Clear-cut", "Dead"),
                     values = c(17, # triangle
                                16)) + # circle
  xlim(0,2.5) +
  ylim(0,2.5) +
  geom_abline(intercept = 0, # add diagnal line
              slope = c(0.5,2),
              col = "grey",
              size = .5,
              lty = 'dashed') +
  xlab('Reassembly [sum]\n [Z-score]') + 
  ylab('Restructure [sum]\n [Z-score]') +
  theme_update(legend.position = 'bottom') +
  theme_update(aspect.ratio=1) # make plots perfect square



# Add labels to points, and dominant species
  
# add hull polygons
hull_data <- 
    out_reorg_pos %>%
    select(trip_n, manag, RS_mean, RA_mean) %>% 
    left_join(trip_species) %>%
    group_by(dom_sp, manag) %>%
  mutate(dom_sp = factor(dom_sp, # change order of dom_sp
                         level = c('spruce','beech', 'oak', 'pine'))) %>% 
    slice(chull(RA_mean, RS_mean)) 

  
windows()
#p_scatter_manag_sum <- 
  out_reorg_pos %>% 
  left_join(trip_species) %>% 
  mutate(dom_sp = factor(dom_sp, # change order of dom_sp
                         level = c('spruce','beech', 'oak', 'pine'))) %>% 
  ggplot(aes(x = RA_mean,
             y = RS_mean,
             #shape = dom_sp,
             color = dom_sp
  )) +
   # geom_text_repel(aes(label = trip_n)) +
   scale_color_viridis_d(direction = -1) +
    scale_fill_viridis_d(direction = -1) +
    geom_polygon(data = hull_data,
                 aes(fill = dom_sp,
                     color = dom_sp),
                 alpha = 0.3,
                 show.legend = TRUE) +
    geom_point() +
 
  xlim(0,2.5) +
  ylim(0,2.5) +
  geom_abline(intercept = 0, # add diagonal line
              slope = c(0.5,2),
              col = "grey",
              size = .5,
              lty = 'dashed') +
    facet_grid(.~manag) +
  xlab('Reassembly [mean]\n [Z-score]') + 
  ylab('Restructure [mean]\n [Z-score]') +
  theme_update(legend.position = 'bottom') +
  theme_update(aspect.ratio=1) # make plots perfect square









# #windows()
# # Tree plots
# tree_plot <- d2%>%  
#   ggplot(aes(area = n, 
#              fill = dom_sp,
#              label = paste( 
#                            reorganization, 
#                            paste(n/0.4, '%'),
#                            sep = "\n"))) +
#   geom_treemap(color = 'black') +
#   geom_treemap_text(colour = "white") +
#   theme(legend.position = "bottom")
#   
# 
# # Bar plot
# tree_plot_cols<- d2%>%  
#   #arrange(desc(n)) %>%
#   #mutate(comb = factor(comb, 
#   #                     levels = comb)) %>% 
#   mutate(reorganization = factor(reorganization,
#                                  levels = c('resilience',
#                                             'reassembly',
#                                             'restructuring',
#                                             'replacement'))) %>% 
#   ggplot(aes(x = reorganization, 
#              y = n,
#              fill = reorganization)) +
#   geom_col(col = 'black') + 
#   facet_wrap(.~ dom_sp) +
#   theme(legend.position = 'right') + 
#   theme_bw()
# 
# 
# p_stacked_reorg <- d2 %>% 
#   ggplot(aes(y = n,
#              x = dom_sp,
#              fill = reorganization)) + 
#   geom_bar(position="fill", stat="identity", col = 'black') + 
#   theme_bw() +
#   ylab('Share of classes [%]') +
#   xlab('')

# library(waffle)
#   
# out_reorg %>% 
#   mutate(shift = factor(shift, levels = c('resilience',
#                                           'reassembly',
#                                           'restructuring',
#                                           'replacement'))) %>%
#   
#   group_by(shift) %>% 
#   count()  %>%
#   ggplot(aes(values = n, 
#              fill = shift)) +
#   geom_waffle() 
# 
# 
# 
# Export objects -----------------------------------------------------------
#save(list=ls(pat="R"),file="dat_restr.Rdata") 
save.image(file="outData/dat_restr.Rdata")
