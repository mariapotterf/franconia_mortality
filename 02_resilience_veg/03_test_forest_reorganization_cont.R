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
#library(gridExtra)
library(ggpubr)



# Input data -------------------------------------------------------------------
getwd()
load(file = paste(getwd(), "dataToPlot.Rdata", sep = '/'))

# Identify data to use:
head(df_full_plot)        # - full PLOT based data: df_full_plot, seedlings, advanced, mature 
head(site_IVI)            # - df importance value:from plot, env mature, env advanced, merged by density/ha
head(df_winners)          # - novel species:        df_novelty
head(trait_df)            # - trait values for all species: eco_traits
head(df_ground)           # - ground cover, in classes by 5%  
head(df_mature_trees_env) # - trees in the surroundings: mature trees
head(df_advanced_env)     # - trees in the surroundings: advanced

# Master plots:
head(plot_counts_df) # - total count of the plots per triplets & categories: to standardize the densities...


# overview of full triplets:
master_tripl <- distinct(select(plot_counts_df_sum, trip_n))

# Reassembly: ------------------------------------------------------------------
#
site_IVI <- replace_na(site_IVI, list(rel_BA   = 0, rIVI = 0))

# add novelty info to the 'site_IVI':
# inspect the rIVI values per comunity? per site, each species should be max 100?
# can be > 100 because frequency: the occurence of species per plots: each species can have up to 100%
site_IVI %>% 
  filter(trip_n == '1' & manag == 'c') #%>% 
  #select()

# RA1: Novel species: presence ---------------------------------------------------------
# the sum (rIVI) of all species per site  is 100 % 
# in my data, I have different species across 

RA1_plot <- 
  site_IVI %>% 
  filter(manag != 'c') %>% 
  left_join(df_winners) %>% 
    ungroup(.) %>% 
    mutate(novelty = case_when(is.na(novelty) ~ 'present',
                               novelty == 'novel' ~ novelty)) %>% #  ,
           #rIVI = case_when(manag == 'd' ~ rIVI*2, # if disturbed, multiply by *2 (need more trees to re-populate disturbed areas that it was before)
          #                    manag == 'l' ~ rIVI)) %>% 
  #mutate(IVI_sum_living = IVI_sum[manag == 'l']) %>% 
    dplyr::select(trip_n, manag, species, rIVI, novelty) %>%
  #filter(trip_n == '2') %>% 
  #print(n = 20)
  #summarize(max = max(rIVI))
    #filter(manag == 'd') %>% 
    group_by(trip_n, manag) %>% 
    summarize(IVI_sum = sum(rIVI, na.rm = T),
              IVI_sum_novel = sum(rIVI[novelty == 'novel'])) %>%
    mutate(RA1 = case_when(IVI_sum_novel > 50 ~ 1,
                           IVI_sum_novel <= 50 ~ 0)) #%>%

# How to whow the sensitivity of teh threshold value???


# Get RA1 for all triplets, also for missing ones 
RA1 <- RA1_plot %>% 
  filter(manag == 'd') %>% 
  right_join(master_tripl) %>%  # fill in all triplets categories
  mutate(RA1 = replace_na(RA1, 0))  # NA -> 0: if the novel species are not important: no change
  

# RA1 plot only communities: --------------------------------------------------
p_RA1 <- RA1_plot %>% 
  group_by(trip_n) %>% 
  mutate(IVI_sum_living = IVI_sum[manag == 'l']) %>% 
  filter(manag != 'l') %>% 
  pivot_longer(!c(trip_n, manag, RA1), 
               names_to = 'type',
               values_to = 'IVI') %>% 
  filter(type != 'IVI_sum') %>% 
  ggplot(aes(type, y = IVI, fill = type)) +
  geom_boxplot() +
  geom_line(aes(group=trip_n), color = 'grey50', alpha = 0.5, lty = 'solid') +
  geom_point(color = 'grey30', alpha = 0.5)+ 
  theme(legend.position = "none") + 
  ylab('Relative species\nimportance value [sum]') +
  scale_x_discrete(labels = c('Ref', 'Novel')) +
  theme_bw() +
  theme(legend.position = 'bottom')
  


# RA2  Community weighted means: ----------------------------------------------

# Interpretation: 
# shade tolerance: 
#             higher number = more tolerance (fir), 
#             lower number = less tolarence (more sunny site, pine)
# drought tolerance: 
#             higher  = more tolerance (pine), 
#             lower = less (more drought sensitive, spruce)
# NA -> 0: if no novel species => no change (NA == 0)

RA2 <- 
  site_IVI %>% 
  left_join(df_winners, by = c("trip_n", "manag", "species")) %>% 
  left_join(trait_df, by = c('species')) %>% #, by = character()
  filter(manag != 'c') %>% 
  mutate(novelty = case_when(is.na(novelty) ~ 'present',
                             novelty == 'novel' ~ novelty)) %>% 
  ungroup(.) %>% 
  group_by(trip_n,  manag, novelty) %>% 
  summarize(shade_cwm   = weighted.mean(Shade_tolerance,   
                                        rIVI, na.rm = TRUE  ),
            drought_cwm = weighted.mean(Drought_tolerance, 
                                        rIVI, na.rm = TRUE  )) %>%
  dplyr::select(trip_n,  manag, novelty, shade_cwm, drought_cwm)  %>% # how to pass the new value to the new column?
  group_by(trip_n) %>% 
  mutate(shade_novel   = ifelse(any(novelty == "novel"), shade_cwm[novelty == "novel"] ,
                              shade_cwm[novelty == "present"])) %>%
  mutate(drought_novel = ifelse(any(novelty == "novel"), drought_cwm[novelty == "novel"] ,
                              drought_cwm[novelty == "present"])) %>%
  filter(manag != 'd') %>% 
  mutate(RA2 = case_when(shade_novel > shade_cwm  ~ 1,
                         shade_novel <= shade_cwm ~ 0)) %>%  # is there is NA - no change
  mutate(RA2 = replace_na(RA2, 0))


# RA2 plot ----------------------------------------------------------------
p_RA2 <-RA2 %>% 
  pivot_longer(!c(trip_n,  manag, novelty, RA2), 
             names_to = 'type',
             values_to = 'val') %>%
  separate(type, c('trait', 'cond')) %>% 
  filter(trait != 'drought') %>% 
  ggplot(aes(cond, y = val, fill = cond)) +
  geom_boxplot() +
  geom_line(aes(group=trip_n), color = 'grey50', alpha = 0.5, lty = 'solid') +
  geom_point(color = 'grey30', alpha = 0.5)+ 
  facet_grid(.~trait) +
  theme(legend.position = "none") + 
  ylab('Shade tolerance [cwm]') +
  scale_x_discrete(labels = c('Ref', 'Novel')) +
  theme_bw() +
  theme(legend.position = 'bottom')



# competitin: Dominance: Is the species dominating after distrubances the same that dominates under reference conditions?
# identify species with teh highest VI bofore and after disturbance: is it still teh same species?
# NA = interpret as change = 1 -> if teh species is missing, means that it is differenyt from reference
RA3 <-
  site_IVI %>%
  filter(manag != 'c') %>%
  dplyr::select(trip_n, manag, species, rIVI) %>%
  filter(rIVI == max(rIVI)) %>%
  group_by(trip_n) %>%
  mutate(maxIVI_l = species[which(c(manag == 'l'))[1]]) %>%
  filter(manag == "d")  %>%  # to keep only one row per triplet
  mutate(RA3 = case_when(maxIVI_l == species ~ 0,
                         maxIVI_l != species ~ 1)) %>% 
  right_join(master_tripl) %>%  # fill in all triplets categories
  mutate(RA3 = replace_na(RA3, 1))  # if the regeneration is missing, it is a change! so put as 1


# R3: Make barplot of species prevalence (how to do the alluvial plot?) ----------------------------------------

#library(ggalluvial)


p_RA3 <- 
  RA3 %>% 
  pivot_longer(!c(trip_n, manag, rIVI, RA3),
               names_to = 'type',
               values_to = 'species') %>%
  mutate(type = case_when(type == 'species' ~ 'Dist',
                          type == 'maxIVI_l' ~ 'Ref')) %>%
  drop_na() %>% 
  mutate(type = factor(type, levels = c('Ref', 'Dist'))) %>% 
  group_by(species, type) %>% 
  count(species) %>%
  ggplot(aes(x = type,
             y = n, 
             fill = species)) +
  geom_col(col = 'black') +
  ylab('Dominant \ntree species') +
  guides(fill=guide_legend(ncol=2,
                           byrow=TRUE)) + 
  theme_bw()



# RA4: Competition: tree DBH: -----------------------------------------------
# find teh species that has most often the highest DBH 
# exclude surrounding data: ENV mature, ENV adv
# NA = missing values = there is a change  => 1
# tre species are missing in 'dead' category, present in living: triplets: 10,2, 27, 4,61
# considering as change!  = 1
# !!!!!!! Check both here, with and without nearest Mature trees!!! if teh output is different??
# ?? why Ref is less then 40??
# RA4 <- 
#   df_full_plot %>% 
#   select(c('trip_n', 'sub_n', 'manag', 'species', 'DBH', 'height_class')) %>% 
#  # bind_rows(select(df_mature_trees_env, c('trip_n', 'sub_n', 'manag', 'species', 'DBH'))) %>% 
#  # dplyr::select(!height_class) %>% 
#   filter(manag != 'c' & height_class != 'mature') %>% #& height_class != 'mature'
#   group_by(trip_n,  manag, sub_n) %>% #, species for each sub_plot the get prevailing species by plot
#   filter(DBH>0) %>% 
#   group_by(trip_n, manag) %>% 
#   count(species) %>% 
#   slice(which.max(n)) %>% # identify the most frequent species with max dbh per site
#  # print(n = 90)  
#   group_by(trip_n) %>% 
#   mutate(spec_l = species[which(c(manag == 'l'))[1]]) %>% # get prevailing species in ref conditios 
#   filter(manag == "d")  %>%  # to keep only disturbed ones
#   mutate(RA4 = case_when(spec_l == species ~ 0,  # compare with the living ones
#                            spec_l != species ~ 1)) %>% 
#   right_join(master_tripl) %>%  # by = c("trip_n", "manag")) %>%
#   mutate(RA4 = replace_na(RA4, 1))  # if the regeneration is missing, it is a change! so put as 1
#   
# 
#  
#  
# 
# # RA4 plot ----------------------------------------------------------------
# 
# p_RA4 <- RA4 %>% 
#    pivot_longer(!c(trip_n, manag, n, RA4),
#                 names_to = 'type',
#                 values_to = 'species') %>%
#    mutate(type = case_when(type == 'species' ~ 'Dist',
#                            type == 'spec_l' ~ 'Ref')) %>%
#    mutate(type = factor(type, levels = c('Ref', 'Dist'))) %>% 
#   drop_na() %>% 
#    group_by(species, type) %>% 
#    count(species) %>%
#    ggplot(aes(x = type,
#               y = n, 
#               fill = species)) +
#    geom_col(col = 'black') +
#    ylab('Dominant regeneration species (DBH)') +
#   guides(fill=guide_legend(ncol=2,byrow=TRUE)) + theme_bw()
#  
#  


# RA4 Competition: tree height ------------------------------------------
# if NA -> the dominant tree species is different -> evaluate as change (NA -> 1)
# add height for the nearest trees: it has to be 2-5m, HK7
df_advanced_env <-
  df_advanced_env %>% 
  mutate(height_class = 'HK7')


RA4 <- 
  df_full_plot %>% 
  select(., c('trip_n', 'sub_n', 'manag', 'species', 'height_class')) %>% 
  bind_rows(select(df_advanced_env, c('trip_n', 'sub_n', 'manag', 'species', 'height_class'))) %>% 
  # find species most often the tallest tree?
  filter(manag != 'c' & height_class != 'mature') %>% # remove the mature trees
  mutate(height_class_num = as.numeric(gsub('HK', '', height_class))) %>% # change to numeric values
  # group_by(trip_n,  manag, sub_n) %>% #, species for each sub_plot the get prevailing species by plot
  group_by(trip_n, manag) %>% 
  count(species) %>% 
  slice(which.max(n)) %>% # identify the most frequent species with max height per site
  group_by(trip_n) %>% 
  mutate(spec_l = species[which(c(manag == 'l'))[1]]) %>% # get prevailing species in ref conditios 
  filter(manag == "d")  %>%  # to keep only disturbed ones
  mutate(RA4 = case_when(spec_l == species ~ 0,  # compare with the living ones
                         spec_l != species | is.na(spec_l) ~ 1)) %>%  # if the regeneration is absent-> indicates shift?
  right_join(master_tripl) %>%  # by = c("trip_n", "manag")) %>%
  mutate(RA4 = replace_na(RA4, 1))  # if the regeneration is missing, it is a change! so put as 1




# RA4 plot ----------------------------------------------------------------
p_RA4 <- RA4 %>% 
  pivot_longer(!c(trip_n, manag, n, RA4),
               names_to = 'type',
               values_to = 'species') %>%
  mutate(type = case_when(type == 'species' ~ 'Dist',
                          type == 'spec_l' ~ 'Ref')) %>%
  mutate(type = factor(type, levels = c('Ref', 'Dist'))) %>% 
  drop_na() %>% 
  group_by(species, type) %>% 
  count(species) %>%
  ggplot(aes(x = type,
             y = n, 
             fill = species)) +
  geom_col(col = 'black') +
  ylab('Dominant tree species (height)\n[per site]') +
  guides(fill=guide_legend(ncol=2,byrow=TRUE)) + 
  theme_bw()









# RA: Reassambly: -----------------------------------------------------------
# cbind RA tables 
RA = select(RA1, c(trip_n, RA1)) %>%
  full_join(select(RA2, c(trip_n,  RA2))) %>%
  full_join(select(RA3, c(trip_n, RA3))) %>% #,
  full_join(select(RA4, c(trip_n, RA4))) %>%
#  full_join(select(RA5, c(trip_n,  RA5))) %>% 
  mutate(RA = (RA1+ RA2+RA3+RA4)/4) 

RA %>% print(n = 40) 






# RS: Restructuring  ---------------------------------------------------------------------------------------


# RS1: stem density: count stems across all species
# 
# use full dataset: 

# NA -> 1
RS1 <-
  site_IVI %>%    # originally: df_full_plot %>%
  filter(manag != 'c') %>%
  group_by(trip_n, manag) %>%
  summarize(sum_count = sum(dens_sum)) %>% # sum up all of the stems per plot (all species together)
  #left_join(df_sub_count) %>%
  #mutate(rel_count = sum_count / (sub_counts * 4)) %>% # 4 = 4 m2
  mutate(dens_ref = sum_count[which(c(manag == 'l'))[1]]) %>% ## get densities in refrenece condistions
  filter(manag == "d")  %>%  # to keep only disturbed ones; to keep only one row having dist and ref condistions
  mutate(RS1 = case_when(dens_ref / sum_count > 2 ~ 0,  # compare with the living ones
                         dens_ref / sum_count <= 2 ~ 1)) %>% 
  right_join(master_tripl) %>%  # by = c("trip_n", "manag")) %>%
  mutate(RS1 = replace_na(RS1, 1))  # if the regeneration is missing, it is a change! so put as 1



# RS1 plot: compare stem densities: ----------------------------------------------
# NA = no present data = change => 1
p_RS1 <- 
  RS1 %>% 
  group_by(trip_n) %>% 
  pivot_longer(!c(trip_n,  manag, RS1, dom_sp), 
               names_to = 'type',
               values_to = 'val') %>% 
  mutate(type = case_when(type == 'sum_count' ~ 'Dist',
                          type == 'dens_ref' ~ 'Ref')) %>%
  mutate(type = factor(type, levels = c('Ref', 'Dist'))) %>% 
  drop_na() %>% 
  ggplot(aes(type, y = val, fill = type)) +
  geom_boxplot() +
  geom_line(aes(group=trip_n), color = 'grey50', alpha = 0.5, lty = 'solid') +
  geom_point(color = 'grey30', alpha = 0.5)+ 
  theme(legend.position = "none") + 
  ylab('Stem density [n/ha]') +
 # scale_x_discrete(labels = c('Ref', 'Dist')) +
  theme_bw() +
  theme(legend.position = 'none')


# some have values > 50000??? can be, with the regeneration

# RS2: Gap fraction ----------------------------------------------------------------
# share of plots that have no trees > 10 cm dbh, and within the ENV
# get numbers of the plot level
# !!! the threshold is 1.1!!!

RS2 <-
  df_full_plot %>%
  select(c('trip_n', 'manag', 'sub_n', 'species', 'DBH')) %>% 
  #bind_rows(select(df_advanced_env, c('trip_n', 'manag', 'species', 'DBH'))) #%>% # !does not have dbh, and less then 10 cm
  bind_rows(select(df_mature_trees_env, c('trip_n', 'manag','sub_n', 'species', 'DBH'))) %>% 
  filter(DBH > 10) %>%
  select(!c(species, DBH)) %>% 
  distinct() %>% 
 # filter(trip_n == '1') %>% 
  group_by(trip_n, manag) %>%
  count() %>%
  rename(plots_occupied = n) %>%
  right_join(plot_counts_df_sum, by = c("trip_n", "manag")) %>%
    rename(plots_count = n) %>%
  mutate(gaps_share = 1 - plots_occupied / plots_count) %>%   # gap share  = 80%, e.g. occupied sites are 20%
  mutate(gaps_share = replace_na(gaps_share, 1)) %>% # if gap is NA  = 100 % is covered by gaps
  filter(manag != 'c') %>%
  ungroup(.) %>%
  group_by(trip_n) %>%
  mutate(gaps_share_ref = gaps_share[which(c(manag == 'l'))[1]]) %>% ## get densities in referece condistions
  filter(manag == "d")  %>%  # to keep only disturbed ones; to keep only one row having dist and ref condistions
  mutate(RS2 = case_when(
    gaps_share  / gaps_share_ref > 1.1 ~ 1,
    # compare with the living ones: 
    gaps_share  / gaps_share_ref <= 1.1 ~ 0
  )) %>% 
  mutate(RS2 = replace_na(RS2, 0))  # if gap is 0 in the reference and disturbed site -> no change 



# RS2  plot ---------------------------------------------------------------

p_RS2 <- 
  RS2 %>% 
  group_by(trip_n) %>% 
  pivot_longer(!c(trip_n, manag, dom_sp, plots_occupied,  plots_count, RS2), 
               names_to = 'type',
               values_to = 'val') %>% 
  mutate(type = case_when(type == 'gaps_share'     ~ 'Dist',
                          type == 'gaps_share_ref' ~ 'Ref')) %>%
  mutate(type = factor(type, levels = c('Ref', 'Dist'))) %>% 
  ggplot(aes(type, y = val, fill = type)) +
  geom_boxplot() +
  geom_line(aes(group=trip_n), color = 'grey50', alpha = 0.5, lty = 'solid') +
  geom_point(color = 'grey30', alpha = 0.5)+ 
  theme(legend.position = "none") + 
  ylab('Gap share [%]\nabsent trees [>10 cm ]') +
  # scale_x_discrete(labels = c('Ref', 'Dist')) +
  theme_bw() +
  theme(legend.position = 'none')



# RS3: Structure: Infilling
# % of ground not covered by vegetation/regeneration 
# if there is bare soils > 50 % of the soil is uncovered, then it can 'infill' with trees
# compare the two: Ref and Dist, and only disturbance -> as it enters in later correction formula
RS3_both <- 
  df_ground %>% 
    filter(class == 'soil/foliage') %>% 
    group_by(trip_n, manag) %>% 
    summarize(mean_soil = mean(prop, na.rm=T)) %>% 
    mutate(RS3 = case_when(mean_soil > 50 ~ 1,
                           mean_soil <= 50 ~ 0))

# only for disturbed
RS3_dist <- 
  df_ground %>% 
  filter(class == 'soil/foliage'& manag ==  'd') %>% 
  group_by(trip_n, manag) %>% 
  summarize(mean_soil = mean(prop, na.rm=T)) %>% 
  mutate(RS3 = case_when(mean_soil > 50 ~ 1,
                         mean_soil <= 50 ~ 0))


# RS3 plot Infilling ------------------------------------------------------
p_RS3 <- RS3_both %>% 
  filter(manag != 'c') %>% 
  mutate(manag = case_when(manag == 'd'  ~ 'Dist',
                           manag == 'l'  ~ 'Ref')) %>%
  mutate(manag = factor(manag, levels = c('Ref', 'Dist'))) %>% 
  ggplot(aes(x = manag, 
             y = mean_soil,
             fill = manag)) +
  geom_boxplot() +
  geom_line(aes(group=trip_n), color = 'grey50', alpha = 0.5, lty = 'solid') +
  geom_point(color = 'grey30', alpha = 0.5)+ 
  theme(legend.position = "none") + 
  ylab('Bare soil share [%]') +
  # scale_x_discrete(labels = c('Ref', 'Dist')) +
  theme_bw() +
  theme(legend.position = 'none')





# RS4: diameter distribution: ------------------------------------------------------
# is the diameter distribution homogenous or heterogenous?: 
# from seedlings, samplings, mature trees; and from the ENV: Mature
# Ref: (dbhmax-dbhmin)/dbhmean
# from the plot & nearest data!!! as I have trees > 10 cm dbh there

### RS4 exploratory plots: ----------------------------------------------------
# Get diameter distribution of dbh per tree class category:
df_mature_trees_env2 <- df_mature_trees_env %>% 
  mutate(height_class = 'mature_ENV') %>% 
  select(c('trip_n', 'manag', 'species', 'DBH', 'height_class'))


# Get distribution of the dbh across sites ---------------------------------------
# needs to be visible: min, max


# Make plot
df_dbh_distrib <- df_full_plot %>% 
  select(c('trip_n', 'manag', 'species', 'DBH', 'height_class')) %>% 
  bind_rows(df_mature_trees_env2) %>%
  filter(height_class %in% c("HK7", 'mature', 'mature_ENV')) %>% # get only data that contain DBH
  mutate(height_class2 = case_when(height_class == "HK7" ~ 'advanced',
                                   height_class == "mature" ~ 'mature_PLOT',
                                   height_class == "mature_ENV" ~ 'mature_ENV')) #%>% 

# Calculate mean values  
dummy_dbh <- df_dbh_distrib %>%
  group_by(manag, height_class2) %>%
  summarize(mean = mean(DBH),
            min = min(DBH),
            max = max(DBH),
            median = median(DBH)) %>% 
  mutate(norm = (max-min)/mean)

# Get density distribution of DBH:
p_dbh_dist <- df_dbh_distrib %>% 
  ggplot(aes(x = DBH, 
             fill = factor(height_class2), 
             group = factor(height_class2))) +
    geom_density(adjust=1.5, alpha=.8) +
    facet_wrap(manag~factor(height_class2), scales = 'free') +
  theme_bw() +
  scale_fill_discrete(name="") +
  theme(legend.position = 'bottom') +
  geom_vline(data = dummy_dbh, aes(xintercept = mean), color = 'black')
    


# Get density distribution of living trees in Disturbed site:
df_mature_trees_env3 <- df_mature_trees_env %>% 
  mutate(height_class = 'mature_ENV') %>%
  select(c('trip_n', 'manag', 'sub_n', 'species', 'DBH', 'distance', 'height_class'))
         


# Get counst of living trees: take into account the average distances between trees
ha <- 10000         

df_living <- df_full_plot %>% 
  filter(DBH>10) %>% 
  mutate(distance = 100) %>% # get distance for plot in [cm]: tree is located in plot
  select(c('trip_n', 'manag','sub_n', 'species', 'DBH', 'distance', 'height_class')) %>% 
  bind_rows(df_mature_trees_env3) %>%
 # anyNA('sub_n')
    group_by(trip_n, manag, sub_n) %>%
    mutate(mean_distance = mean(distance)/100, # mean distance in meters [not in cm]
          mean_area = pi*mean_distance^2, # [in m2]
          n = n()) %>% 
  select(trip_n, manag, mean_area, n) %>% 
  ungroup(.) %>% 
  distinct() %>% # remove duplicated rows, due to mutate 
  group_by(trip_n, manag) %>% 
  summarize(sum_mean_area = sum(mean_area),
            sum_trees = sum(n)) %>% 
  #rename(n_living_trees = n) %>% 
  right_join(plot_counts_df_sum, 
               by = c("trip_n", 'manag')) %>%
  mutate(index = ha/sum_mean_area,
           n_living_trees = index*sum_trees) # %>%  # "density" represents the value per ha!!!



# # Get summary table
dummy_living_trees <- df_living %>%
  group_by(manag) %>%
  summarize(mean = mean(n_living_trees, na.rm=T),
            min = min(n_living_trees, na.rm=T),
            max = max(n_living_trees,na.rm=T),
            median = median(n_living_trees, na.rm=T)) #%>% 
  #mutate(norm = (max-min)/mean)


# RS4: Get boxplot living
p_n_living <- df_living %>% 
  ggplot(aes(y = n_living_trees, 
             fill = factor(manag), 
             x = factor(manag))) +
  geom_boxplot() +
  #geom_density(adjust=1.5, alpha=.8) +
  #facet_wrap(.~manag, scales = 'free') +
  theme_bw() +
  scale_fill_discrete(name="") +
  theme(legend.position = 'bottom') +
  stat_summary(fun = mean, color = "darkred", position = position_dodge(0.75),
               geom = "point", shape = 18, size = 3,
               show.legend = FALSE)
 # geom_vline(data = dummy, aes(xintercept = mean), color = 'black')



# Get RS4 values -----------------------------------------------------------
RS4_ref <- 
  df_full_plot %>% 
  select(c('trip_n', 'sub_n', 'manag', 'species', 'DBH')) %>% 
  ##
  #filter(height_class != 'mature') %>% 
  filter(manag == 'l') %>% 
  group_by(trip_n, manag) %>%
  filter(DBH != 0) %>% 
  summarize(dbh_min  = min(DBH, na.rm = T),
            dbh_max  = max(DBH, na.rm = T),
            dbh_mean = mean(DBH, na.rm = T)) %>% 
  mutate(RS4_ref = (dbh_max-dbh_min)/dbh_mean) %>%
  mutate(RS4_ref = replace_na(RS4_ref, 0)) %>% 
  select(!manag)



# get count of trees > 10 cm dbh by disturbed sites:
# use the ENV mature trees density estimation

# 
RS4_dist <- df_living %>% 
  select(c('trip_n', 'manag', 'n_living_trees')) %>% 
  filter(manag == 'd')  %>%
  #filter(DBH> 10) %>% 
  #group_by(trip_n, manag) %>%
  #summarize(all_sp = sum(density)) %>% 
  #rename(n_trees_ha = all_sp) %>%  # get count of living trees per site
  right_join(filter(plot_counts_df_sum, manag == 'd'), 
             by = c("trip_n", 'manag')) %>%
  mutate(n_living_trees = replace_na(n_living_trees, 0)) #%>% # if NA = replace by 0


# Merge the RS4 together:
# Investigate different thresholds!
RS4 <- 
  RS4_ref %>% 
  full_join(RS4_dist) %>% 
  mutate(RS4_ref = replace_na(RS4_ref, 0)) %>% # replace NA by 0 - trees are not present
  #  replace_na(0) %>% 
  mutate(RS4 = case_when(RS4_ref >  1 & n_living_trees >  10 ~ 0,
                         RS4_ref <= 1 & n_living_trees <= 10 ~ 0,
                         RS4_ref > 1 & n_living_trees <= 10 ~ 1,
                         RS4_ref <= 1 & n_living_trees > 10 ~ 1))












# RS4 plots ---------------------------------------------------------------
# Diamater distribution in Reference stands:
p_RS4_ref <- RS4_ref %>% 
  mutate(manag = 'Ref') %>% 
  ggplot(aes(x = manag,
            y = RS4_ref)) +
  geom_boxplot() +
  ggtitle('Diameter distribution\nin Reference stands') + 
  theme_bw() + ylab("DBH distribution\n [norm]")

p_RS4_dist <- RS4_dist %>% 
  mutate(manag = 'Dist') %>% 
  ggplot(aes(x = manag,
             y = n_living_trees)) + 
  geom_boxplot() +
  ggtitle('# of legacy trees > 10 cm\nin Disturbed stands') + 
  theme_bw() +
  ylab('# trees/ha')


p_RS4_agg <- ggarrange(p_RS4_ref, p_RS4_dist)

# 
# number of legacies trees: in the plots and in the surroundings?
# take into account matures trees in surroundings and plots:
# df_mature_all %>% 
#   filter(manag == 'd') %>% 
#   group_by(trip_n) %>% 
#   count() %>% 
#   rename(legacy_trees_n = n) %>% 
#   right_join(filter(plot_counts_df_sum, manag == 'd'))




# Compile RS --------------------------------------------------------------

RS <- select(RS1, c(trip_n, RS1)) %>%
  full_join(select(RS2, c(trip_n,  RS2))) %>%
  full_join(select(RS3_dist, c(trip_n, RS3))) %>% #,
  full_join(select(RS4, c(trip_n, RS4)))  %>%
  # Compensate for gaps/infilling
  mutate(RS1_comp = case_when((RS1 == 1 & RS3 == 1) ~ 0.5, #& RS3_dist = 1 
                              TRUE ~ RS1)) %>% 
  mutate(RS2_comp = case_when((RS2 == 1 & RS3 == 1) ~ 0.5, #& RS3_dist = 1 
                              TRUE ~ RS2)) %>%
  mutate(RS = (RS1_comp+RS2_comp+RS4)/3)
  


# Classify Forest reorganization -------------------------------------------
out_reorg <- 
  select(RS, c(trip_n, RS)) %>% 
  left_join(RA) %>% 
  mutate(reorganization = case_when((RS <= 0.5  & RA <= 0.5)  ~ 'resilience',
                           (RS >  0.5  & RA <= 0.5)  ~ 'restructuring',
                           (RS <= 0.5  & RA  > 0.5)  ~ 'reassembly',
                           (RS >  0.5  & RA  > 0.5)  ~ 'replacement'))



# make a mosaic plot of counts!!!  ----------------------------------------------
# having only 4 reorganization categories

library(treemapify)

d <- out_reorg %>% 
  mutate(reorganization = factor(reorganization, levels = c('resilience',
  'reassembly',
  'restructuring',
  'replacement'))) %>%

  group_by(reorganization) %>% 
  count() # %>%
 



# Barplot ---------------------------------
out_reorg %>% 
  mutate(reorganization = factor(reorganization, levels = c('resilience',
                                                            'reassembly',
                                                            'restructuring',
                                                            'replacement'))) %>%
  group_by(reorganization) %>% 
  count()  %>%
  ggplot(aes(x = reorganization, 
             y = n,
             fill = reorganization)) +
  geom_col(color = 'black')


# Tree map with dominant trees species dataset: -----------------------------------
#treemap <- 
 d2 <- out_reorg %>% 
  left_join(master_tripl) %>% 
  group_by(reorganization, dom_sp) %>% 
  mutate(comb = paste( reorganization, sep = ' ')) %>% 
  count(comb) %>% 
  as.data.frame() 

#windows()
# Tree plots
tree_plot <- d2%>%  
  ggplot(aes(area = n, 
             fill = dom_sp,
             label = paste( 
                           reorganization, 
                           paste(n/0.4, '%'),
                           sep = "\n"))) +
  geom_treemap(color = 'black') +
  geom_treemap_text(colour = "white") +
  theme(legend.position = "bottom")
  

# Bar plot
tree_plot_cols<- d2%>%  
  #arrange(desc(n)) %>%
  #mutate(comb = factor(comb, 
  #                     levels = comb)) %>% 
  mutate(reorganization = factor(reorganization,
                                 levels = c('resilience',
                                            'reassembly',
                                            'restructuring',
                                            'replacement'))) %>% 
  ggplot(aes(x = reorganization, 
             y = n,
             fill = reorganization)) +
  geom_col(col = 'black') + 
  facet_wrap(.~ dom_sp) +
  theme(legend.position = 'right') + 
  theme_bw()


p_stacked_reorg <- d2 %>% 
  ggplot(aes(y = n,
             x = dom_sp,
             fill = reorganization)) + 
  geom_bar(position="fill", stat="identity", col = 'black') + 
  theme_bw() +
  ylab('Share of classes [%]') +
  xlab('')

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
save.image(file="dat_restr.Rdata")
