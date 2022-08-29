# Quantify forest reorganization;

# read data
# each triplet categorize in one of teh characteristics:
# as proposed by Rupert


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
head(df_full_plot)   # - full plot based data: df_full_plot
head(df_IVI_out)     # - df importance value:  df_IVI_out
head(df_winners)     # - novel species:        df_novelty
head(trait_df)       # - trait values for all species: eco_traits
head(df_ground)      # - ground cover, in classes by 5%  

# Master plots:
head(plot_counts_df) # - total count of the plots per triplets & categories: to standardize the densities...




# Reassembly: ------------------------------------------------------------------
# add novelty info to the df_IVI_out:

# Example of novel species:
#> df_winners: 
# A tibble: 34 x 4
#trip_n manag reg_species novelty
#  1 24     d     Oak         novel  
#  2 24     d     Ash         novel  


# !!! in working example: 
# Evaluate how does the 300% value changes based on the species occurence, size and prevalence??


# RA1: Novel species: presence ---------------------------------------------------------
RA1 <- 
  df_IVI_out %>% 
  left_join(df_winners) %>% 
    ungroup(.) %>% 
    filter(manag != 'c') %>% 
    mutate(novelty = case_when(is.na(novelty) ~ 'present',
                               novelty == 'novel' ~ novelty),
           sp_IVI = case_when(manag == 'd' ~ sp_IVI*2, # if disturbed, multiply by *2 (need more trees to re-populate disturbed areas that it was before)
                              manag == 'l' ~ sp_IVI)) %>% 
    dplyr::select(trip_n, manag, reg_species, sp_IVI, novelty) %>%
    group_by(trip_n, manag) %>% 
    summarize(IVI_sum = sum(sp_IVI, na.rm = T),
              IVI_sum_novel = sum(sp_IVI[novelty == 'novel'])) %>%
    mutate(RA1 = case_when(IVI_sum_novel > 150 ~ 1,
                           IVI_sum_novel < 150 ~ 0))


# RA1 plot only communities: --------------------------------------------------
RA1 %>% 
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
  ylab('Species importance values [sum]') +
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

RA2 <- 
  df_IVI_out %>% 
  left_join(df_winners, by = c("trip_n", "manag", "reg_species")) %>% 
  left_join(trait_df, by =c('reg_species')) %>% #, by = character()
  filter(manag != 'c') %>% 
  mutate(novelty = case_when(is.na(novelty) ~ 'present',
                             novelty == 'novel' ~ novelty)) %>% 
  ungroup(.) %>% 
  group_by(trip_n, dom_sp, manag, novelty) %>% 
  summarize(shade_cwm   = weighted.mean(Shade_tolerance,   sp_count, na.rm = TRUE  ),
            drought_cwm = weighted.mean(Drought_tolerance, sp_count, na.rm = TRUE  )) %>%
  dplyr::select(trip_n, dom_sp, manag, novelty, shade_cwm, drought_cwm)  %>% # how to pass the new value to the new column?
  group_by(trip_n) %>% 
  mutate(shade_novel   = ifelse(any(novelty == "novel"), shade_cwm[novelty == "novel"] ,
                              shade_cwm[novelty == "present"])) %>%
  mutate(drought_novel = ifelse(any(novelty == "novel"), drought_cwm[novelty == "novel"] ,
                              drought_cwm[novelty == "present"])) %>%
  filter(manag != 'd') %>% 
  mutate(RA2 = case_when(shade_novel > shade_cwm  ~ 1,
                         shade_novel <= shade_cwm ~ 0)) # is there is NA - no change



# RA2 plot ----------------------------------------------------------------
RA2 %>% 
  pivot_longer(!c(trip_n, dom_sp, manag, novelty, RA2), 
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
  ylab('Ecological trait [cwm]') +
  scale_x_discrete(labels = c('Ref', 'Novel')) +
  theme_bw() +
  theme(legend.position = 'bottom')



# competitin: Dominance: Is the species dominating after distrubances the same that dominates under reference conditions?
# identify species with teh highest VI bofore and after disturbance: is it still teh same species?
RA3 <-
  df_IVI_out %>%
  filter(manag != 'c') %>%
  dplyr::select(trip_n, manag, reg_species, sp_IVI) %>%
  filter(sp_IVI == max(sp_IVI)) %>%
  group_by(trip_n) %>%
  mutate(maxIVI_l = reg_species[which(c(manag == 'l'))[1]]) %>%
  filter(manag == "d")  %>%  # to keep only one row per triplet
  mutate(RA3 = case_when(maxIVI_l == reg_species ~ 0,
                         maxIVI_l != reg_species ~ 1)) 

# R3: Make barplot of species prevalence (how to do the alluvial plot?) ----------------------------------------

#library(ggalluvial)


RA3 %>% 
  pivot_longer(!c(trip_n, manag, sp_IVI, RA3),
               names_to = 'type',
               values_to = 'species') %>%
  mutate(type = case_when(type == 'reg_species' ~ 'Dist',
                          type == 'maxIVI_l' ~ 'Ref')) %>%
  mutate(type = factor(type, levels = c('Ref', 'Dist'))) %>% 
  group_by(species, type) %>% 
  count(species) %>%
  ggplot(aes(x = type,
             y = n, 
             fill = species)) +
  geom_col(col = 'black') +
  ylab('Dominant tree species\nbefore and after disturbance [per site]')
  
  # #pivot_wider(names_from = type,
  # #            values_from = n) %>% 
  # ggplot(aes(axis1 = species, 
  #            axis2 = type,
  #            weight = n)) +
  # geom_alluvium(aes(fill = species)) +
  # geom_stratum() + 
  #   geom_text(stat = "stratum", 
  #             aes(label = after_stat(stratum))) +
  #     theme_minimal() +
  # ggtitle("Tree species composition",
  #         "my subtitle")


# RA4: Competition: tree size: -----------------------------------------------
# find teh species that has most often the highest DBH & largest height - split in two columns?
RA4 <- df_full_plot %>% 
  dplyr::select(!height_class) %>% 
  filter(manag != 'c' ) %>% #& height_class != 'mature'
  group_by(trip_n, dom_sp, manag, sub_n) %>% #, reg_species for each sub_plot the get prevailing species by plot
  filter(DBH>0) %>% 
  group_by(trip_n, manag) %>% 
  count(reg_species) %>% 
  slice(which.max(n)) %>% # idetify the most frequent species with max dbh per site
  group_by(trip_n) %>% 
  mutate(spec_l = reg_species[which(c(manag == 'l'))[1]]) %>% # get prevailing species in ref conditios 
  filter(manag == "d")  %>%  # to keep only disturbed ones
  mutate(RA4 = case_when(spec_l == reg_species ~ 0,  # compare with the living ones
                           spec_l != reg_species ~ 1))


 head(RA4)
 
 

# RA4 plot ----------------------------------------------------------------

 RA4 %>% 
   pivot_longer(!c(trip_n, manag, n, RA4),
                names_to = 'type',
                values_to = 'species') %>%
   mutate(type = case_when(type == 'reg_species' ~ 'Dist',
                           type == 'spec_l' ~ 'Ref')) %>%
   mutate(type = factor(type, levels = c('Ref', 'Dist'))) %>% 
   group_by(species, type) %>% 
   count(species) %>%
   ggplot(aes(x = type,
              y = n, 
              fill = species)) +
   geom_col(col = 'black') +
   ylab('Dominant tree species (DBH)\nbefore and after disturbance [per site]')
 
 
  
# RA5 Competition: tree height ------------------------------------------
RA5 <- 
  df_full_plot %>% 
  # find species most often the tallest tree?
  filter(manag != 'c' & height_class != 'mature') %>% # remove the mature trees
  mutate(height_class_num = as.numeric(gsub('HK', '', height_class))) %>% # change to numeric values
   # group_by(trip_n, dom_sp, manag, sub_n) %>% #, reg_species for each sub_plot the get prevailing species by plot
  group_by(trip_n, manag) %>% 
  count(reg_species) %>% 
  slice(which.max(n)) %>% # identify the most frequent species with max dbh per site
  group_by(trip_n) %>% 
  mutate(spec_l = reg_species[which(c(manag == 'l'))[1]]) %>% # get prevailing species in ref conditios 
 filter(manag == "d")  %>%  # to keep only disturbed ones
  mutate(RA5 = case_when(spec_l == reg_species ~ 0,  # compare with the living ones
                                spec_l != reg_species | is.na(spec_l) ~ 1)) # if the regeneration is absent-> indicates shift?

 

# RA5 plot ----------------------------------------------------------------
RA5 %>% 
   pivot_longer(!c(trip_n, manag, n, RA4),
                names_to = 'type',
                values_to = 'species') %>%
   mutate(type = case_when(type == 'reg_species' ~ 'Dist',
                           type == 'spec_l' ~ 'Ref')) %>%
   mutate(type = factor(type, levels = c('Ref', 'Dist'))) %>% 
   group_by(species, type) %>% 
   count(species) %>%
   ggplot(aes(x = type,
              y = n, 
              fill = species)) +
   geom_col(col = 'black') +
   ylab('Dominant tree species (height)\nbefore and after disturbance [per site]')
 
 
 
 
# checK 17-living: missing dominant tallest trees? - because not regeneration was present

# RA: Reassambly: -----------------------------------------------------------
# cbind RA tables 
RA = select(RA1, c(trip_n, RA1)) %>%
  full_join(select(RA2, c(trip_n,  RA2))) %>%
  full_join(select(RA3, c(trip_n, RA3))) %>% #,
  full_join(select(RA4, c(trip_n, RA4))) %>%
  full_join(select(RA5, c(trip_n,  RA5))) %>% 
 # filter(manag != 'l') %>% 
  distinct() %>% 
  mutate(RA = (RA1+ RA2+RA3+RA4+RA5)/5) %>% 
  group_by(trip_n) %>% 
  summarize(RA_mean = mean(RA, na.rm = T))

RA %>% print(n = 40) # need to check why I have some many NAs!!






# RS: Restructuring  ---------------------------------------------------------------------------------------


# RS1: stem density: count stems across all species
# 
head(df_full_plot) 

RS1 <-
  df_full_plot %>%
  filter(manag != 'c') %>%
  group_by(trip_n, manag) %>%
  summarize(sum_count = sum(count)) %>% # sum up all of the stems per plot
  left_join(df_sub_count) %>%
  mutate(rel_count = sum_count / (sub_counts * 4)) %>% # 4 = 4 m2
  mutate(dens_ref = rel_count[which(c(manag == 'l'))[1]]) %>% ## get densities in refrenece condistions
  filter(manag == "d")  %>%  # to keep only disturbed ones; to keep only one row having dist and ref condistions
  mutate(RS1 = case_when(dens_ref / rel_count > 2 ~ 0,  # compare with the living ones
                         dens_ref / rel_count <= 2 ~ 1))


# RS1 plot: compare stem densities: ----------------------------------------------
RS1 %>% 
  group_by(trip_n) %>% 
  pivot_longer(!c(trip_n, dom_sp, manag, sub_counts, sum_count, RS1), 
               names_to = 'type',
               values_to = 'val') %>% 
  mutate(type = case_when(type == 'rel_count' ~ 'Dist',
                          type == 'dens_ref' ~ 'Ref')) %>%
  mutate(type = factor(type, levels = c('Ref', 'Dist'))) %>% 
  
  ggplot(aes(type, y = val, fill = type)) +
  geom_boxplot() +
  geom_line(aes(group=trip_n), color = 'grey50', alpha = 0.5, lty = 'solid') +
  geom_point(color = 'grey30', alpha = 0.5)+ 
  theme(legend.position = "none") + 
  ylab('Stem density [n/m^2]') +
 # scale_x_discrete(labels = c('Ref', 'Dist')) +
  theme_bw() +
  theme(legend.position = 'none')



# RS2: Gap fraction ----------------------------------------------------------------
# share of plots that have no trees > 10 cm dbh
# 08/29/2022->need to checks plot master to have 1244 roas! or why it has 1241 only???
# the master plots needs to have all needed combinations!!!

# df_sub_count or plots_master, needs to have 1244 rows! total number of sites!!!

RS2 <-
  df_full_plot %>%
  filter(DBH > 10) %>%
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
  ))



# RS2  plot ---------------------------------------------------------------

RS2 %>% 
  group_by(trip_n) %>% 
  pivot_longer(!c(trip_n, manag, plots_occupied, dom_sp, plots_count, RS2), 
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
  ylab('Gap share [%]\nmissing trees [<10 cm ]') +
  # scale_x_discrete(labels = c('Ref', 'Dist')) +
  theme_bw() +
  theme(legend.position = 'none')



# RS3: Structure: Infilling
# not clear here? just not covered by vegetation/regeneration?? 
# if there is bare soils > 50 % of the soil is uncovered, then it can 'infill' with trees
# compare the two: Ref and Dist
RS3_both <- 
  df_ground %>% 
    filter(class == 'soil/foliage') %>% 
    group_by(trip_n, manag) %>% 
    summarize(mean_soil = mean(prop, na.rm=T)) %>% 
    mutate(RS3 = case_when(mean_soil > 50 ~ 1,
                           mean_soil <= 50 ~ 0))


RS3_dist <- 
  df_ground %>% 
  filter(class == 'soil/foliage'& manag ==  'd') %>% 
  group_by(trip_n, manag) %>% 
  summarize(mean_soil = mean(prop, na.rm=T)) %>% 
  mutate(RS3 = case_when(mean_soil > 50 ~ 1,
                         mean_soil <= 50 ~ 0))


# RS3 plot Infilling ------------------------------------------------------
RS3 %>% 
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




# plot distribution of all groups:
df_ground %>% 
  #filter(class == 'soil/foliage') %>% 
  group_by(trip_n, manag, class) %>% 
  filter(manag != 'c') %>% 
  summarize(mean_prop = mean(prop, na.rm = T)) %>% 
  ggplot(aes(x = class,
             y = mean_prop,
             fill =manag)) + 
  geom_boxplot()
  


# RS4: diameter distribution: ------------------------------------------------------
# is the diameter distribution homogenous or heterogenous?: only from seedlings, samplings, mature trees:
# Ref: (dbhmax-dbhmin)/dbhmean
dbh_ref <- df_full_plot %>% 
  filter(height_class != 'mature') %>% 
  filter(manag == 'l') %>% 
  group_by(trip_n, manag) %>%
  filter(DBH != 0) %>% 
  summarize(dbh_min  = min(DBH, na.rm = T),
            dbh_max  = max(DBH, na.rm = T),
            dbh_mean = mean(DBH, na.rm = T)) %>% 
  mutate(RS4_ref = (dbh_max-dbh_min)/dbh_mean) %>% 
  mutate(RS4_ref = replace_na(RS4_ref, 0))

# number of legacies trees: in the plots and in the surroundings?
# take into account matures trees in surroundings and plots:
df_mature_all %>% 
  filter(manag == 'd') %>% 
  group_by(trip_n) %>% 
  count() %>% 
  rename(legacy_trees_n = n) %>% 
  right_join(filter(plot_counts_df_sum, manag == 'd'))



# Export objects -----------------------------------------------------------
save(list=ls(pat="R"),file="dat_restr.Rdata") 
