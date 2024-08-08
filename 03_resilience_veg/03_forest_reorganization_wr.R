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


source('myPaths.R')

# Store details about labeling and plotting -------------------------------------
# get labels:
# for management
manag.labs        <- c("MAN", "UNM", "REF")
names(manag.labs) <- c("c", "d", "l")
manag.level       <- c('l', 'c', 'd')   # to order the data on x axis: REF, MAN, UNM

# for dominant species
species.labs        <- c("Beech", "Oak", "Pine", "Spruce")
names(species.labs) <- c("beech", "oak", "pine", "spruce")



# Colors ------------------------------------------------------------------

my_sp_vals = c('spruce'= '#7CBB00', # light green
               'beech' = '#FFBB00', # yellow,
               'oak'   =  '#F65314',  # red
               'pine' = '#3A606E')  #  bluish

my_sp_vals2 = c('spruce'= '#85ce00', # light green
                'beech' = '#ff8000',  # orange' 
                'oak'   = '#005dff', # blue,
                'pine' = '#ff0045')  #  red





# Theme set ---------------------------------------------------------------

theme_set(theme_classic())
theme_update(legend.position = 'none',
             panel.background = element_rect(colour = "black", size = 0.5),
             aspect.ratio=1,
             axis.text.x = element_text(angle = 0, #vjust = 1, hjust = 1, 
                                        #face = "italic", 
                                        size = 8))


# For density plot: ----------------------------------------------------------
dens_plot_details <- function() {
  list(
    geom_density(alpha = 0.5),
    geom_vline(xintercept = 0, colour="red", linetype = "dashed"),
    scale_fill_discrete(name = "Management", 
                        breaks = names(manag.labs), #manag_acc,    #c("c", "d"),
                        labels = manag.labs    #c("Managed", "Unmanaged")
  ))
}

# details for violin plot: from the raw data
details_violin <- function() {
  list(
    geom_violin(trim = T, 
                lty = 1, # # remove outer line
                lwd = 0.2,
                alpha = 0.8), 
    stat_summary(fun = "mean", 
                 geom = "point",
                 size = 1,
                 col = 'black'),
    theme_classic(),
    scale_x_discrete(name = '',
                     breaks = names(manag.labs),
                     labels = manag.labs),
    facet_grid(.~dom_sp, 
               labeller = labeller(dom_sp = species.labs)),
    theme(legend.position = 'none',
          panel.background = element_rect(colour = "black", size = 0.5),
          aspect.ratio=1,
          axis.text.x = element_text(angle = 0, #vjust = 1, hjust = 1, 
                                     #face = "italic", 
                                     size = 8)
    )  
  )
}

# details for point plot, merged by the line: use only for site level data!!------
details_pts <- function() {
  list(
    geom_line(aes(group=trip_n), 
              position = position_dodge(0.3),
              alpha = 0.4,
              col = 'lightgrey'), # + , 
  # 
    geom_point(aes(color=dom_sp,
                   group=trip_n), 
               position = position_dodge(0.3),
               size = 1.8,
               alpha = 0.7),
   # theme_classic(),
    scale_x_discrete(name = '',
                     breaks = names(manag.labs),
                     labels = manag.labs),
      scale_color_manual(values = my_sp_vals2 ,
                         name = 'Dominant species'),
    #facet_grid(.~dom_sp, 
    #labeller = labeller(dom_sp = species.labs)),
    theme(legend.position = 'none',
          axis.text.x = element_text(angle = 0, 
                                            #vjust = 1, hjust = 1, 
                                            #face = "italic", 
                                            size = 8)
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
  

# plot RA1 raw ---------------------------------------------------------
# filter the dominant species per plot
# check what is their variability over all plots
# then get this variation to show it on the violin plot
# get the dominant species per triplet REF
df_dom_sp <- dplyr::select(RA1_dom_ref, !c(ref_rIVI_mean)) %>%
  rename(species_ref = species)
  

# filter the whole table with only triplets that have the same species
# complete missing rows by 0
p_RA1_raw <- plot_IVI_exp %>% 
  left_join(df_dom_sp, 
            by=c('trip_n')) %>% 
  filter(species_ref == species) %>% 
  left_join(plot_counts_df) %>% 
  left_join(trip_species, by = "trip_n") %>% 
  ggplot(aes(x = factor(manag, 
                        levels = c('l', 'c', 'd')), #factor(manag),
             y = rIVI,
             fill = manag)) + 
  details_violin() +
  ylab("Dom. species [rIVI, %]")




# on site level, lines between triplets ---------------------

p_RA1_site <- 
  plot_IVI_exp %>%
  dplyr::select(trip_n, manag, sub_n, species, rIVI) %>%
  group_by(trip_n, manag, species) %>%
  summarize(ref_rIVI_mean = mean(rIVI, na.rm = T)) %>% 
  ungroup(.) %>% 
  group_by(trip_n, manag) %>% 
  filter(ref_rIVI_mean == max(ref_rIVI_mean)) %>%
  as.data.frame() %>%
  left_join(trip_species, by = "trip_n") %>% 
  ggplot(aes(x = factor(manag, 
                        levels = c('l', 'c', 'd')),
             y = ref_rIVI_mean,
             color = dom_sp)) + # , 
  details_pts()  +
  ylab("Dom. species [rIVI, %]")


p_RA1_site





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
#windows()
# raw data
p_RA2_raw <- 
  plot_IVI_exp %>%
  filter(sp_count  != 0 ) %>% 
  group_by(trip_n, manag, sub_n) %>% 
  summarise(richness = n()) %>% 
  group_by(trip_n, manag) %>%
  as.data.frame() %>%
  left_join(trip_species, by = "trip_n") %>% 
  ggplot(aes(x = factor(manag, 
                        levels = c('l', 'c', 'd')),
             y = richness,
             fill = manag)) + 
  details_violin() +
  ylab('Sp. richness')
  
 
 
# get plot per site ---------------------------------------------------
p_RA2_site <- 
  plot_IVI_exp %>%
  filter(sp_count  != 0 ) %>% 
  group_by(trip_n, manag, sub_n) %>% 
  summarise(richness = n()) %>% 
  ungroup(.) %>% 
  group_by(trip_n, manag) %>% 
  summarize(avg_rich = mean(richness, na.rm = F)) %>% 
  as.data.frame() %>%
  left_join(trip_species, by = "trip_n") %>% 
  ggplot(aes(x = factor(manag, 
                        levels = c('l', 'c', 'd')),
             y = avg_rich,
             color = dom_sp)) +  
  details_pts()+
  ylab('Sp. richness')





 

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
p_RA3_raw <- 
  plot_IVI_exp %>% 
  left_join(trait_df, by = c('species')) %>% #, by = character()
  ungroup(.) %>% 
  group_by(trip_n, manag) %>% 
  summarize(mean_shade   = weighted.mean(Shade_tolerance,   
                                             rIVI, na.rm = TRUE)) %>%
  as.data.frame() %>%
  left_join(trip_species, by = "trip_n") %>% 
  ggplot(aes(x = factor(manag, 
                        levels = c('l', 'c', 'd')),
             y = mean_shade,
             fill = manag)) + 
  details_violin() +
  ylab('Shade tolerance\n[mean]')



# p_RA2_site --------------------------------------------------------------

p_RA3_site <- plot_IVI_exp %>% 
  left_join(trait_df, by = c('species')) %>% #, by = character()
  ungroup(.) %>% 
  group_by(trip_n,  manag) %>% 
  summarize(mean_shade   = weighted.mean(Shade_tolerance,   
                                              rIVI, na.rm = TRUE)) %>% 
  as.data.frame() %>%
  left_join(trip_species, by = "trip_n") %>% 
  ggplot(aes(x = factor(manag, 
                        levels = c('l', 'c', 'd')),
             y = mean_shade,
             color = dom_sp)) +  
  details_pts()+
  ylab('Shade tolerance')

  


# Define vertical structure -----------------------------------------------
# needed to calculate the stem density per vertical classes as well: RS1 and RS2

# 3 layers: 
# - regeneration (<= less then 2 m height)
# - intermediate (> 2 m height & <= 10 cm DBH)
# - mature (> 10 cm dbh )
# if the mean number of layers per DIST > REF -> indication of change

# classify the data

df_full_corr_mrg <- 
  df_full_corr_mrg %>%
  mutate(vert_layer = case_when(height_class %in% c("HK1", "HK2", "HK3", "HK4", "HK5","HK6") ~ 'regen',
                                height_class %in% c("HK7","adv_ENV" ) ~ 'advanced',
                                height_class %in% c("mature","mat_ENV" ) ~ 'mature')) 



# get the 'total' table : combination of trip_n, sub_n, and height classes
v_height_both = c('advanced', 
                  'mature')


# make master dataframe having both height categories: 
df_master_heights_both <-   
  plot_counts_df %>% 
  mutate(vert_layer = 'mature') %>% 
  group_by(trip_n, manag, sub_n) %>% 
  complete(vert_layer = .env$v_height_both)



# Include plots with 0 stem density to final stem density table ---------

# are there some plots that do not have any regeneration??
# get summary table for regen
df_full_corr_mrg_reg <- 
  df_full_corr_mrg %>% 
  filter(vert_layer == 'regen') %>% 
  group_by(trip_n, manag, sub_n, vert_layer) %>% 
  right_join(plot_counts_df) %>%
  summarize(sum_corr_count = sum(corr_count)) %>% 
  mutate(sum_corr_count = case_when(is.na(sum_corr_count) ~ 0,
                                    !is.na(sum_corr_count) ~ sum_corr_count)) %>% 
  mutate(vert_layer = 'regen') 


# Get table for advanced: here, get avg for advanced regen:
df_full_corr_mrg_advMat <- 
  df_full_corr_mrg %>% 
  filter(vert_layer != 'regen') %>% 
  group_by(trip_n, manag, sub_n, vert_layer) %>% 
  summarize(sum_corr_count = mean(corr_count)) %>%  # mean beacsue I have adv in plot and in ENV!!
  right_join(df_master_heights_both) %>% 
  mutate(sum_corr_count = case_when(is.na(sum_corr_count) ~ 0,
                                    !is.na(sum_corr_count) ~ sum_corr_count)) 

# merge corrected bables for stem density from REg and adv+Matg trees:
df_stem_dens <- rbind(df_full_corr_mrg_reg,
                      df_full_corr_mrg_advMat)

# df_stem_dens %>% 
#   distinct(trip_n, manag, sub_n, vert_layer)
df_stem_dens <- df_stem_dens %>% 
  mutate(vert_layer = factor(vert_layer,
                             levels = c('regen',
                                        'advanced',
                                        'mature')),
         manag = factor(manag,
                             levels = c('l',
                                        'c',
                                        'd')))


# Get output stem table: ----------------------------------------
qntils = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)

out_tab_dens <- df_stem_dens %>% 
  mutate(manag = case_when(manag == 'c' ~ "MAN",
                           manag == 'd' ~ "UNM",
                           manag == 'l' ~ "REF")) %>% 
  mutate(manag = factor(manag, levels = c("REF", "MAN", "UNM"))) %>% 
  group_by(manag, vert_layer)  %>%  # , vert_layer
  summarize(mean = mean(sum_corr_count),
            qs = quantile(sum_corr_count, qntils),
            prob = qntils)  %>%
  pivot_wider(names_from = prob, values_from = qs ) %>% 
  mutate_if(is.numeric, round)


out_tab_dens
outStemDens            = paste(myPath, outTable, 'raw_stem_dens.csv'              , sep = '/')  # contains infor of plantation& damage

#### Save the table 
fwrite(out_tab_dens, outStemDens)





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


# p_RS1_site -------------------------------------------------------------------
p_RS1_site <- plot_IVI_exp %>% 
  group_by(trip_n, manag) %>% 
  summarize(mean_dens   = mean(all_count, na.rm = TRUE)) %>%
  as.data.frame() %>%
  left_join(trip_species, by = "trip_n") %>% 
  ggplot(aes(x = factor(manag, 
                        level = c('l', 'c', 'd')),
             y = mean_dens/1000,
             color = dom_sp)) +  
  details_pts()+
  ylab('Stem dens.(*1000)')



# RAW stem density for the vertical classes: ------------------------------------
# need to go to raw data, as the RS1 is a sum of all of the 
# all vertical classes densities!!


# Get stem density by vertical classes:

# split in two: adv and mature together, regen as separate plot?

library(scales)

# p_RS1_raw <- 
#   df_full_corr_mrg %>%  
#   left_join(trip_species, by = "trip_n") %>% 
#   group_by(trip_n, manag, vert_layer) %>% 
#   ggplot(aes(x = factor(manag, 
#                         level = c('l', 'c', 'd')),
#              y = corr_count/1000,
#              fill = manag)) + 
#   details_violin() +
#   facet_grid(vert_layer~dom_sp, 
#              labeller = labeller(dom_sp = species.labs),
#              scales = 'free') +
#   #  ylim(2.5,10)+
#   ylab('Stem density (*1000)\n')



# Plot mature and advanced

# plot REgen with log y axis
p_RS1_raw_reg <- df_stem_dens %>%  
    left_join(trip_species, by = "trip_n") %>% 
    group_by(trip_n, manag, vert_layer) %>% 
    filter(vert_layer == 'regen') %>% 
    ggplot(aes(x = factor(manag, 
                          level = c('l', 'c', 'd')),
               y = sum_corr_count/1000,
               fill = manag)) + 
    details_violin() +
    facet_grid(vert_layer~dom_sp, 
               labeller = labeller(dom_sp = species.labs),
               scales = 'free') +
    scale_y_log10(breaks = c(0, 10, 100),#trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits = c(-1,101)) +
    ylab('Stem density (*1000/ha)\n')# + 
    #geom_jitter(width = 0.2, alpha = 0.3)  # geom_jitter(position = position_dodge(0.8))+
  

# plot mature and advanced:
p_RS1_raw_MatAdv <- 
  df_stem_dens %>%  
  left_join(trip_species, by = "trip_n") %>% 
   # filter(vert_layer == 'mature' & dom_sp == 'spruce') %>% 
  #  arrange(trip_n, manag, sub_n) %>% 
  #  View() 
  group_by(trip_n, manag, vert_layer) %>% 
  filter(vert_layer != 'regen') %>% 
  ggplot(aes(x = factor(manag, 
                        level = c('l', 'c', 'd')),
             y = sum_corr_count/1000,
             fill = manag)) + 
 #   geom_jitter(alpha = 0.5) +
  details_violin() +
  facet_grid(vert_layer~dom_sp, 
             labeller = labeller(dom_sp = species.labs),
             scales = 'free') +
  scale_y_continuous(breaks = seq(0, 3, by = 1), 
                     limits = c(0,3)) +
  ylab('Stem density (*1000/ha)\n')# + 
#geom_jitter(width = 0.2, alpha = 0.3)  # geom_jitter(position = position_dodge(0.8))+



# RS2: Horizontal structure -----------------------------------------------


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
  ggtitle('Horizontal str.')

p_RS2




# p RS2 raw  --------------------------------------------------------------

p_RS2_raw <- df_full_corr_mrg %>% 
  filter(count  != 0 ) %>% 
  filter(vert_layer != 'regen') %>% 
  dplyr::select(trip_n, manag, sub_n, distance, vert_layer) %>%
  right_join(df_master_heights_both) %>%
  mutate(distance = case_when(is.na(distance) ~ 16*100, # complete distances of 16 m if the tree is not present in ENV
                              !is.na(distance) ~ distance)) %>%
  group_by(trip_n, manag, sub_n) %>% 
  slice(which.min(distance)) %>% # filter to have only the shortest distance (if several trees were recorded eg on plot)
  ungroup(.) %>% 
  group_by(trip_n, manag, sub_n) %>% #, vert_layer
  as.data.frame() %>%
  left_join(trip_species, by = "trip_n") %>% 
  ggplot(aes(x = factor(manag, 
                        level = c('l', 'c', 'd')),
             y = distance/100,
             fill = manag)) + 
  details_violin() +
  ylab('Horizontal distance\n[m]')





# p_RS2_site --------------------------------------------------------------

p_RS2_site <- df_full_corr_mrg %>%
  filter(count  != 0 ) %>% 
  filter(vert_layer != 'regen') %>% 
  dplyr::select(trip_n, manag, sub_n, distance, vert_layer) %>%
  right_join(df_master_heights_both) %>%
  mutate(distance = case_when(is.na(distance) ~ 16*100, # complete distances of 16 m if the tree is not present in ENV
                              !is.na(distance) ~ distance)) %>%
  group_by(trip_n, manag, sub_n, vert_layer) %>% 
  slice(which.min(distance)) %>% # filter to have only the shortesdt distance (if several trees were recorded eg on plot)
  ungroup(.) %>% 
  group_by(trip_n, manag, sub_n) %>% # vert_layer 
  summarise(mean_distance = mean(distance, na.rm = T)) %>%
  ungroup(.) %>% 
  group_by(trip_n, manag) %>% 
  summarize(mean_distance   = mean(mean_distance, na.rm = TRUE)) %>% 
  as.data.frame() %>%
  left_join(trip_species, by = "trip_n") %>% 
  ggplot(aes(x = factor(manag, 
                        level = c('l', 'c', 'd')),
             y = mean_distance/100,
             color = dom_sp)) +  
  details_pts()  +
  ylab('Horiz. dist.[m]')
  







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

 
### Plot RS3: raw ---------------------------------------------------------------
p_RS3_raw <- 
  df_full_corr_mrg %>%
  filter(count  != 0 ) %>% 
  dplyr::select(trip_n, manag, sub_n, vert_layer) %>%
  distinct(.) %>%
  group_by(trip_n, manag, sub_n) %>% 
  summarise(vertical_n = n()) %>%
  ungroup(.) %>% 
  group_by(trip_n, manag) %>% 
 # summarize(mean_vLayer   = mean(vertical_n, na.rm = TRUE)) %>%
  as.data.frame() %>%
  left_join(trip_species, by = "trip_n") %>% 
  ggplot(aes(x = factor(manag, 
                        level = c('l', 'c', 'd')),
             y = vertical_n,
             fill = manag)) + 
  details_violin() +
  ylab('# Vert. layers')
  




# plot the values as density plot
p_RS3 <- df_RS3 %>% 
  ggplot(aes(RS3, fill = manag)) +
  xlim(-4.3,4.3) +
  dens_plot_details() +
  ggtitle('Vertical str.')

### Plot RS3: site ---------------------------------------------------------------
p_RS3_site <- 
  df_full_corr_mrg %>%
  filter(count  != 0 ) %>% 
  dplyr::select(trip_n, manag, sub_n, vert_layer) %>%
  distinct(.) %>%
  group_by(trip_n, manag, sub_n) %>% 
  summarise(vertical_n = n()) %>%
  ungroup(.) %>% 
  group_by(trip_n, manag) %>% 
  summarize(mean_vLayer   = mean(vertical_n, na.rm = TRUE))  %>% 
  as.data.frame() %>%
  left_join(trip_species, by = "trip_n") %>% 
  ggplot(aes(x = factor(manag, 
                        level = c('l', 'c', 'd')),
             y = mean_vLayer,
             color = dom_sp)) +  
  details_pts() +
  ylab('# Vertical layers')






# Plot densities together  ------------------------------------------------

p_6vars <- ggarrange(
  p_RA1 + ylim(0,1.8),
  p_RA2 + ylim(0,1.8) ,
  p_RA3 + ylim(0,1.8) ,
  p_RS1 + ylim(0,1.8) ,
  p_RS2 + ylim(0,1.8) ,
  p_RS3 + ylim(0,1.8) ,
  nrow = 2,
  ncol = 3,
  common.legend = TRUE,
  legend = 'bottom',
  align = c("hv")
)


p_6site <- ggarrange(
  p_RA1_site, #+ ylim(0,1.8),
  p_RA2_site, #+ ylim(0,1.8) ,
  p_RA3_site, #+ ylim(0,1.8) ,
  p_RS1_site, #+ ylim(0,1.8) ,
  p_RS2_site, #+ ylim(0,1.8) ,
  p_RS3_site,# + ylim(0,1.8) ,
  nrow = 2,
  ncol = 3,
  common.legend = TRUE,
  legend = 'bottom',
  align = c("hv")
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
  mutate(euclid_dist = sqrt(RA_mean^2 + RS_mean^2))
#  mutate(euclid_dist = euclidean(RA_mean, RS_mean)) #%>%
  # classify the poinst by sector: make as squares, as simpler way
  #mutate(sector =  )





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
  scale_color_manual(values = my_sp_vals2 ,
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
  scale_color_manual(values = my_sp_vals2 ,
                     name = 'Dominant species') +
  labs(x = "Reassembly",
       y = "Restructure") + 
  xlim(0,2) +
  ylim(0,2) +
  facet_grid(manag~dom_sp, 
             #scales = 'free',
             labeller = labeller(manag = manag.labs)) +
  theme_classic() +
  theme(legend.position = 'none',
        panel.background = element_rect(colour = "black", size = 1),
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
#  mutate(orig_dist = sqrt(RA_mean^2 + RS_mean^2)) %>%
  mutate(euclid_dist = sqrt(RA_mean^2 + RS_mean^2)) %>%
  #calculate position (origin, far, etc..)
  mutate(position = case_when(euclid_dist < 0.5 ~ "resilience",
                              euclid_dist >= 1.5 ~ "-extreme",
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
  scale_color_manual(values = my_sp_vals2 ,
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
  scale_color_manual(values = my_sp_vals2 ,
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

sites <- st_read('C:/Users/ge45lep/Documents/2021_Franconia_mortality/03_plot_sampling/out_fieldData/new_GPS/sites_final_updPassau.shp')

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
  left_join(dplyr::select(res_classes, c('trip_n', 'manag', 'labelXY'))) #%>% 
   # nrow()

# st_write(sites_out, 
#          'C:/Users/ge45lep/Documents/2021_Franconia_mortality/outSpatial/resilience_class/sites_resilience.shp',
#          append=FALSE)

# 
# Export objects -----------------------------------------------------------

#save.image(file="outData/dat_restr.Rdata")


## WR

# copied 02_metrics_density_plot.R

df_full_corr_mrg %>%
  group_by(trip_n, manag, sub_n, species, height_class) %>%
  summarize(sp_count = sum(corr_count, na.rm = T)) %>%
  ungroup(.) %>%
  group_by(trip_n, manag, sub_n) %>%
  mutate(all_count = sum(sp_count, na.rm = T),
         rel_density = sp_count/all_count*100) #%>% 
#  filter(trip_n == 1 & manag == 'c' & sub_n == 1)

#RA1_dom_ref <-  --> skalieren von plots auf sites
 plot_IVI_exp %>%
  filter(manag == 'l') %>%
  dplyr::select(trip_n, manag, sub_n, species, rIVI) %>%
  group_by(trip_n, manag, species) %>%
  summarize(ref_rIVI_mean = mean(rIVI, na.rm = T)) %>% 
  ungroup(.) %>% 
  group_by(trip_n) %>% 
  filter(ref_rIVI_mean == max(ref_rIVI_mean)) %>%
  dplyr::select(!c(manag))

 
# per analogieschluss: 
wr_stand <-  
  df_full_corr_mrg %>%
  group_by(trip_n, manag, sub_n, species, vert_layer) %>%
  summarize(sp_count = sum(corr_count, na.rm = T)) %>% 
  group_by(trip_n, manag, species, vert_layer) %>%
  summarise(sp_count_mean = mean(sp_count), sp_count_sd = sd(sp_count), sp_count_n=n())
 
# aggregate per triplet (without species)

plot_counts_df_sum

wr_stems <-  
  df_full_corr_mrg %>% left_join(plot_counts_df_sum %>% rename(nsubplots = n)) %>%
  group_by(trip_n, manag, sub_n, nsubplots, vert_layer) %>%
  summarize(sp_count = sum(corr_count, na.rm = T)) %>% 
  group_by(trip_n, manag, nsubplots, vert_layer) %>%
  summarise(sp_count_present = mean(sp_count), sp_count_sd = sd(sp_count), sp_count_n=n(), sp_count_mean = sum(sp_count/nsubplots)) %>% 
   left_join(trip_species)

#%>%
#    pivot_wider(id_cols=c(trip_n, manag), names_from=vert_layer, values_from=sp_count_mean:sp_count_n)

wr_stems <-
  wr_stems %>% left_join(trip_species %>% mutate(trip_n=trip_n))

# include species proportions
wr_species_am <-  
  df_full_corr_mrg %>% left_join(plot_counts_df_sum %>% rename(nsubplots = n)) %>%
  mutate(sumtype = case_when(vert_layer %in% c("advanced", "mature") ~ "adv+mat", TRUE ~ "regen" )) %>%
  filter(sumtype == "adv+mat") %>%
  group_by(trip_n, manag, sub_n, species, nsubplots) %>%
  summarize(sp_count = sum(corr_count, na.rm = T)) %>% 
  group_by(trip_n, manag, nsubplots, species) %>%
  summarise(sp_count_present = mean(sp_count), sp_count_sd = sd(sp_count), sp_count_n=n(), sp_count_mean = sum(sp_count/nsubplots)) %>% 
  mutate(rel_count = sp_count_mean / sum(sp_count_mean)) %>%
  left_join(trip_species)
 
wr_species_all <-  
  df_full_corr_mrg %>% left_join(plot_counts_df_sum %>% rename(nsubplots = n)) %>%
  group_by(trip_n, manag, sub_n, species, nsubplots) %>%
  summarize(sp_count = sum(corr_count, na.rm = T)) %>% 
  group_by(trip_n, manag, nsubplots, species) %>%
  summarise(sp_count_present = mean(sp_count), sp_count_sd = sd(sp_count), sp_count_n=n(), sp_count_mean = sum(sp_count/nsubplots)) %>% 
  mutate(rel_count = sp_count_mean / sum(sp_count_mean)) %>%
  left_join(trip_species)%>%
  pivot_wider(id_cols=c(trip_n, manag), names_from=species, values_from=c(sp_count_mean,rel_count))


table( df_full_corr_mrg$vert_layer )


out_reorg_full <- 
  df_RA1 %>% full_join(df_RA2) %>% full_join(df_RA3) %>%
  full_join(df_RS1) %>% full_join(df_RS2) %>% full_join(df_RS3)


  dplyr::select(df_RA1,           c(trip_n, manag, RA1, ref_rIVI_mean )) %>% 
  full_join(dplyr::select(df_RA2, c(trip_n, manag, RA2, ref_avg_rich    ))) %>%
  full_join(dplyr::select(df_RA3, c(trip_n, manag, RA3, ref_mean_shade ))) %>% 
  full_join(dplyr::select(df_RS1, c(trip_n, manag, RS1, ref_mean_dens ))) %>% #,
  full_join(dplyr::select(df_RS2, c(trip_n, manag, RS2, ref_mean_distance ))) %>%
  full_join(dplyr::select(df_RS3, c(trip_n, manag, RS3, ref_mean_vLayer )))#%>% 

  
# noch dazuhängen von triplet  

  out_reorg_full <-
    out_reorg_full %>% left_join(trip_species %>% mutate(trip_n=trip_n))
  
# noch dazuhängen von area
out_reorg_full <-
  out_reorg_full %>% left_join( df %>% select(trip_n, manag, dom_sp, Area_m2) %>% mutate(trip_n=as.integer(trip_n))  )
  
  
write.csv(out_reorg_full, file="outData_share/out_reorg_full.csv", row.names = F)

write.csv(wr_stems, file="outData_share/wr_stems.csv", row.names = F)

write.csv(wr_species_am, file="outData_share/wr_species_am.csv", row.names = F)  

write.csv(wr_species_all, file="outData_share/wr_species_all.csv", row.names = F)  
## /WR

## WR
### look for discrepancy with stem numbers (2023-04-13)

test <- 
  plot_IVI_exp %>% 
  filter(manag != 'l') %>% 
  group_by(trip_n, manag) %>% 
  summarize(dist_mean_dens   = mean(all_count, na.rm = TRUE)) %>%
  left_join(RS1_ref, by = 'trip_n') %>% 
  mutate(RS1 = (ref_mean_dens  -dist_mean_dens  )/ref_sd_dens)

plot_IVI_exp_wr <- 
  plot_IVI %>% 
  full_join(df_master_species) %>% 
  ungroup(.) %>% 
  replace_na(list(rIVI = 0))

test2 <- 
  plot_IVI_exp_wr %>% 
  filter(manag != 'l') %>% 
  group_by(trip_n, manag) %>% 
  summarize(dist_mean_dens   = mean(all_count, na.rm = TRUE)) %>%
  left_join(RS1_ref, by = 'trip_n') %>% 
  mutate(RS1 = (ref_mean_dens  -dist_mean_dens  )/ref_sd_dens)

test == test2 # ok, da kommt das gleiche raus, theorie wiederlegt

# out_reorg_full: dist_mean_dens
# 2c: 10938.6015
# 2d: 11427.2394
# 2ref: 3670.105

# weder alte (mean über plots mit daten), noch neu (inkl. ohne daten) sind die gleichen?
stems %>% group_by(trip_n, manag) %>% summarise(spc_present = sum(sp_count_present), spc_mean=sum(sp_count_mean)) %>% filter(trip_n==2)

### woher kommt das?

df_rel_density_wr <-
  df_full_corr_mrg %>%
  group_by(trip_n, manag, sub_n, species) %>%
  summarize(sp_count = sum(corr_count, na.rm = T)) %>%
  ungroup(.) %>%
  group_by(trip_n, manag, sub_n) %>%
  mutate(all_count = sum(sp_count, na.rm = T),
         rel_density = sp_count/all_count*100)

plot_IVI_wr <- df_rel_density_wr %>% 
  full_join(df_rel_BA_plot) %>% 
  replace_na(., list(all_BA = 0, rel_BA   = 0)) %>% 
  mutate(rIVI = ( rel_density +rel_BA)/2)  # relative IVI
plot_IVI_exp_wr <- 
  plot_IVI_wr %>% 
  full_join(df_master_species) %>% 
  ungroup(.) %>% 
  replace_na(list(rIVI = 0))
df_RS1_wr <- 
  plot_IVI_exp_wr %>% 
  filter(manag != 'l') %>% 
  group_by(trip_n, manag) %>% 
  summarize(dist_mean_dens   = mean(all_count, na.rm = TRUE)) %>%
  left_join(RS1_ref, by = 'trip_n') %>% 
  mutate(RS1 = (ref_mean_dens  -dist_mean_dens  )/ref_sd_dens)

df_RS1_wr %>% filter(trip_n==2) ### cool - werte wie df_RS1 und out_reorg_full



# wr stems:
df_full_corr_mrg %>% left_join(plot_counts_df_sum %>% rename(nsubplots = n)) %>%
  group_by(trip_n, manag, sub_n, nsubplots, vert_layer) %>%
  summarize(sp_count = sum(corr_count, na.rm = T)) %>% 
  group_by(trip_n, manag, nsubplots, vert_layer) %>%
  summarise(sp_count_present = mean(sp_count), sp_count_sd = sd(sp_count), sp_count_n=n(), sp_count_mean = sum(sp_count/nsubplots)) %>% 
  left_join(trip_species) %>% 
  group_by(trip_n, manag) %>% summarise(spc_present=sum(sp_count_present), spc_mean=sum(sp_count_mean))

#                    present  real_mean
#  1      c          31485.   31434.
#2 1      d          22125.   18125.
#3 1      l          20806.   19782.

# df_full_corr_mrg %>% filter(trip_n==2 & manag=="d" && sub_n==2)
# plot_IVI_exp_wr %>% filter(trip_n==2 & sub_n==2 & manag=="d")
wr_species_am %>%  filter(trip_n==2 & manag=="d")

out_reorg_full %>% select(trip_n, manag, dist_mean_dens, ref_mean_dens)
#trip_n manag dist_mean_dens ref_mean_dens
#1       1     c     32085.8024     21185.051
#2       1     d     21204.9731     21185.051

# Ok. The difference is:
# Orig: mean over total trees per subplot, but with *number of species* as weight!!

# how to fix it:
# tree density / SD tree density: should be stems per subplot, then mean/SD

wr_stems_per_subplot <-  
  df_full_corr_mrg %>% left_join(plot_counts_df_sum %>% rename(nsubplots = n)) %>%
  group_by(trip_n, manag, sub_n, nsubplots) %>%
  summarize(all_count = sum(corr_count, na.rm = T)) 

#%>% 
  group_by(trip_n, manag, nsubplots) %>%
  summarise(sp_count_present = mean(sp_count), sp_count_sd = sd(sp_count), sp_count_n=n(), sp_count_mean = sum(sp_count/nsubplots)) %>% 
  left_join(trip_species)

#%>%
#    pivot_wider(id_cols=c(trip_n, manag), names_from=vert_layer, values_from=sp_count_mean:sp_count_n)

wr_stems <-
  wr_stems %>% left_join(trip_species %>% mutate(trip_n=trip_n))

# sum trees per subplot:
wr_stems_total <- wr_stems %>% group_by(trip_n, manag) %>% summarise(all_count = sum(sp_count_mean))

# this shows that here mean() and sum(n)/nsubplots is the same:
RS1_ref_fix <- wr_stems_per_subplot %>% filter(manag=="l") %>%
  group_by(trip_n) %>%
  summarise(sp_count_present = mean(sp_count), sp_count_sd = sd(sp_count), sp_count_n=n(), sp_count_mean = sum(sp_count/nsubplots))

RS1_ref_fix <- 
  wr_stems_per_subplot %>% 
  filter(manag == 'l') %>% 
  group_by(trip_n) %>% 
  summarize(ref_mean_dens   = mean(all_count, na.rm = TRUE),
            ref_sd_dens     = sd(all_count, na.rm = TRUE)) #%>%


df_RS1_fix <- 
  wr_stems_per_subplot %>% 
  filter(manag != 'l') %>% 
  group_by(trip_n, manag) %>% 
  summarize(dist_mean_dens   = mean(all_count, na.rm = TRUE)) %>%
  left_join(RS1_ref, by = 'trip_n') %>% 
  mutate(RS1 = (ref_mean_dens  -dist_mean_dens  )/ref_sd_dens)

# there is a differnce
plot(df_RS1_fix$RS1 ~ df_RS1$RS1, pch=20)
abline(a=0,b=1)

summary(cbind("fix" = df_RS1_fix$RS1,"orig" = df_RS1$RS1))

out_reorg_full_fix <- out_reorg_full %>% 
  select(-dist_mean_dens,  -ref_mean_dens, -ref_sd_dens,  -RS1) %>% left_join(df_RS1_fix %>% mutate(trip_n=trip_n, manag=as.factor(manag)))

save(out_reorg_full_fix, file="outDAta_share/out_reorg_full_fix.RData")

## ok, jetzt noch alternive für RA1 (siehe unten dazuhängen)

out_reorg_full_fix <- out_reorg_full_fix %>% left_join(df_RA1_fix_b %>% mutate(trip_n=as.integer(trip_n)) %>% select( -dom_sp), by=c("trip_n", "manag"))

### RA1

# plot_IVI_exp: hat pro subplot jede Art (1244 * 27 Arten = 33588 Datensätze)
dim(plot_IVI_exp)[1] / length(unique(paste(plot_IVI_exp$trip_n, plot_IVI_exp$manag, plot_IVI_exp$sub_n)))
table(plot_IVI_exp$species)

plot_IVI_exp %>%
  filter(manag == 'l') %>%
  dplyr::select(trip_n, manag, sub_n, species, rIVI) %>%
  group_by(trip_n, manag, species) %>% summarise(n=n()) %>% summary()

# Get first values for the Ref = 'l', get the mean and sd
# find first the average rIVI per species per site; 
# then find the highest average value and corresponding species
# compare what value have the species in D and C?
# need to get the SD: SD origin from the deviation of the dominant species across the plots
RA1_dom_ref_fix 

## IVI and SD per species per trip and management
wr_IVI <-  plot_IVI_exp %>% left_join(trip_species) %>%
  left_join(plot_counts_df_sum %>% rename(nsubplots = n)) %>%
    dplyr::select(trip_n, manag, sub_n, species, rIVI, nsubplots, dom_sp) %>%
  group_by(trip_n, manag, nsubplots, dom_sp, species) %>%
  summarize(ref_rIVI_mean = mean(rIVI), ref_rIVI_corr=mean(sum(rIVI)/nsubplots), ref_rIVI_sd = sd(rIVI)) %>%  
  ungroup(.) 

# now look 


wr_IVI_dom <- wr_IVI %>% 
  group_by(trip_n, manag, dom_sp, nsubplots) %>% 
  mutate(max_ref_rIVI_dom = species[which.max(ref_rIVI_mean)], max_ref_rIVI_sd = ref_rIVI_sd[which.max(ref_rIVI_mean)]) %>%
  filter(ref_rIVI_mean == max(ref_rIVI_mean))

wr_IVI_dom %>% filter(manag!="l") %>% left_join(wr_IVI_dom %>% filter(manag=="l"), by="trip_n")

# variante 1: fokus auf tatsächlich dominante art
wr_IVI_species <- wr_IVI_dom %>% ungroup() %>% filter(manag=="l") %>% select(trip_n, dom_sp, selspecies = species) 

wr_IVI %>% filter(manag!="l") %>% left_join(wr_IVI_dom %>% filter(manag=="l"), by="trip_n") %>% left_join(wr_IVI_species)
  
# variante #1 - tatsächlich dominante Art
wr_IVI_sel <- wr_IVI %>% left_join(wr_IVI_species) %>% filter(species==selspecies)

# now calculate RA1
df_RA1_fix <- wr_IVI_sel %>% filter(manag=="l") %>% 
  left_join(wr_IVI_sel %>% filter(manag != "l"), by="trip_n") %>% 
  select(trip_n, dom_sp=dom_sp.x, species=species.x, manag=manag.y, ref_rIVI_mean = ref_rIVI_mean.x, ref_rIVI_sd = ref_rIVI_sd.x, rIVI_mean=ref_rIVI_mean.y) %>%
  mutate(RA1 = ( ref_rIVI_mean - rIVI_mean) / ref_rIVI_sd )

df_RA1$RA1 == df_RA1_fix$RA1 # identisch wie ergebnisse von Maia

# variante 2: IVI der dominanten art laut triplet
# 
wr_IVI_sel_b <- wr_IVI %>% left_join(wr_IVI_species) %>% filter(tolower(species)==dom_sp)
df_RA1_fix_b <- wr_IVI_sel_b %>% filter(manag=="l") %>% 
  left_join(wr_IVI_sel %>% filter(manag != "l"), by="trip_n") %>% 
  select(trip_n, dom_sp=dom_sp.x, species=species.x, manag=manag.y, ref_rIVI_mean = ref_rIVI_mean.x, ref_rIVI_sd = ref_rIVI_sd.x, rIVI_mean=ref_rIVI_mean.y) %>%
  mutate(RA1 = ( ref_rIVI_mean - rIVI_mean) / ref_rIVI_sd )

plot(df_RA1_fix$RA1~ df_RA1_fix_b$RA1, pch=20)

plot(out_reorg_full_fix$RA1_v2 ~ out_reorg_full_fix$RA1, pch=20)
abline(a=0,b=1)


# rename
df_RA1_fix_b <- df_RA1_fix_b %>% select(trip_n, dom_sp, manag, ref_IVI_mean_v2=ref_rIVI_mean, ref_IVI_sd_v2=ref_rIVI_sd, IVI_mean_v2=rIVI_mean, RA1_v2=RA1)

## IVI and SD per species per trip (reference)
wr_IVI <-  plot_IVI_exp %>% left_join(trip_species) %>%
  left_join(plot_counts_df_sum %>% rename(nsubplots = n)) %>%
  filter(manag == 'l') %>%
  dplyr::select(trip_n, manag, sub_n, species, rIVI, nsubplots, dom_sp) %>%
  group_by(trip_n, manag, nsubplots, dom_sp, species) %>%
  summarize(ref_rIVI_mean = mean(rIVI), ref_rIVI_corr=mean(sum(rIVI)/nsubplots), ref_rIVI_sd = sd(rIVI)) %>%  
  ungroup(.) 

wr_IVI <-  plot_IVI_exp %>%
  left_join(plot_counts_df_sum %>% rename(nsubplots = n)) %>%
  filter(manag == 'l') %>%
  dplyr::select(trip_n, manag, sub_n, species, rIVI, nsubplots) %>%
  group_by(trip_n, manag, species, nsubplots) %>%
  summarize(ref_rIVI_mean = mean(rIVI), ref_rIVI_corr=sum(rIVI)/nsubplots) %>%  
  ungroup(.) %>% 
  group_by(trip_n) %>% mutate(ref_rIVI_dom = species[which.max(ref_rIVI_mean)]) %>%
  filter(ref_rIVI_mean == max(ref_rIVI_mean)) %>%
  dplyr::select(!c(manag))




RA1_dom_ref_fix <- plot_IVI_exp %>%
  filter(manag == 'l') %>%
  dplyr::select(trip_n, manag, sub_n, species, rIVI) %>%
  group_by(trip_n, manag, species) %>%
  summarize(ref_rIVI_mean = mean(rIVI), ref_rIVI_corr = sum) %>%  
  ungroup(.) %>% 
  group_by(trip_n) %>% 
  filter(ref_rIVI_mean == max(ref_rIVI_mean)) %>%
  dplyr::select(!c(manag))


# Get SD: select the dominant species per trip_n from the REF ('l'); and have the value for each plot!!
# need to add 0 if teh species is not present to get the SD! 
RA1_dom_SD <- 
  plot_IVI %>%
  filter(manag == 'l') %>%
  dplyr::select(trip_n, manag, sub_n, species, rIVI) %>%
  right_join(RA1_dom_ref_fix) %>% # filter only the dominant species in each REF triplet
  full_join(filter(plot_counts_df, manag == 'l'))  %>% # add 0 for missing plots (dominated by different species)
  group_by(trip_n, manag, species) %>% 
  mutate(ref_iIVI_sd = sd(rIVI, na.rm = T)) %>% 
  ungroup(.) %>% 
  dplyr::select(!c(manag)) %>%  
  rename(ref_rIVI = rIVI) %>% 
  dplyr::select(c(trip_n, species, ref_rIVI_mean, ref_iIVI_sd)) %>% 
  distinct()


# Calculate the difference between disturbed and reference conditions: 
df_RA1_fix <-
  plot_IVI_exp %>%
  filter(manag != 'l') %>%
  dplyr::select(trip_n, manag, species, rIVI) %>%
  group_by(trip_n, manag, species) %>%
  summarize(avg_iIVI_dist = mean(rIVI, na.rm = T)) %>%
  full_join(RA1_dom_SD, by = c("trip_n", "species")) %>%
  drop_na(.) %>% # remove NA values, to keep only species that are REF value (dominant under REF conditions)
  mutate(RA1 = (ref_rIVI_mean - avg_iIVI_dist) / ref_iIVI_sd)

## calculate species richness on plot level 
wr_species_per_plot <- df_full_corr_mrg %>%
  group_by(trip_n, manag, sub_n) %>% summarise(nspecies = length(unique(species)))

summary(wr_species_per_plot)
wr_species_per_plot %>% group_by(manag) %>% summarise(nmin=min(nspecies), nmean=mean(nspecies), nmed=median(nspecies), nmax=max(nspecies))

wr_species_per_patch <- df_full_corr_mrg %>%
  group_by(trip_n, manag) %>% summarise(nspecies = length(unique(species)))

wr_species_per_patch %>% group_by(manag) %>% summarise(nmin=min(nspecies), nmean=mean(nspecies), nmed=median(nspecies), nmax=max(nspecies))

