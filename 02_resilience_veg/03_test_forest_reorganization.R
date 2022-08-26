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
head(df_full_plot)  # - full plot based data: df_full_plot
head(df_IVI_out)    # - df importance value:  df_IVI_out
head(df_winners)    # - novel species:        df_novelty
head(trait_df)      # - trait values for all species: eco_traits


# Reassembly:
# add novelty info to the df_IVI_out:

# Example of novel species:
#> df_winners: 
# A tibble: 34 x 4
#trip_n manag reg_species novelty
#  1 24     d     Oak         novel  
#  2 24     d     Ash         novel  


# !!! in working example: 
# Evaluate how does the 300% value changes based on the species occurence, size and prevalence??


# Novel species: presence ---------------------------------------------------------
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
    mutate(RA1 = case_when(IVI_sum_novel > 300 ~ 1,
                           IVI_sum_novel < 300 ~ 0))



# RA2  Community weighted means: ----------------------------------------------

# Interpretation: 
# shade tolerance: 
#             higher number = more tolerance (fir), 
#             lower number = less tolarence (more sunny site, pine)
# drought tolerance: 
#             higher  = more tolerance (pine), 
#             lower = less (more drought sensitive, spruce)

RA2 <- df_IVI_out %>% 
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
  mutate(shade_novel = ifelse(any(novelty == "novel"), shade_cwm[novelty == "novel"] , NA)) %>% 
  mutate(RA2 = case_when(shade_novel > shade_cwm  ~ 1,
                         shade_novel <= shade_cwm ~ 0))


# competitin: Dominance: Is the species dominating after distrbances the same that dominates under reference conditions?
# identify species with teh highest VI bofore and after disturbance: is it still teh same species?
RA3 <-
  df_IVI_out %>%
  filter(manag != 'c') %>%
  dplyr::select(trip_n, manag, reg_species, dom_sp, sp_IVI) %>%
  filter(sp_IVI == max(sp_IVI)) %>%
  group_by(trip_n) %>%
  mutate(maxIVI_l = reg_species[which(c(manag == 'l'))[1]]) %>%
  filter(manag == "d")  %>%  # to keep only one row per triplet
  mutate(RA3 = case_when(maxIVI_l == reg_species ~ 0,
                         maxIVI_l != reg_species ~ 1))

# RA4: Competition: tree size:
# find teh species that has most often the highest DBH & largest height - split in two columns?
d <- data.frame(trip_n = c(1,1,2,4,5),
                sp=c('a', 'b','a','a','b'),
                dbh = c(3,3,2,3,8))

# select the the species with the highest dbh
d %>% 
  #filter(sp_IVI == max(sp_IVI))# %>%
  group_by(trip_n) %>% 
  #mutate(max_sp_dbh = sp[which.max(dbh)[1]])# %>%
  mutate(max_sp_dbh = sp[which.max(dbh)])# %>%


# RA4: DBH

RA4_dbh <- df_full_plot %>% 
  dplyr::select(!height_class) %>% 
  filter(manag != 'c' ) %>% #& height_class != 'mature'
  group_by(trip_n, dom_sp, manag, sub_n) %>% #, reg_species for each sub_plot the get prevailing species by plot
  filter(DBH>0) %>% 
  group_by(trip_n, manag) %>% 
  count(reg_species) %>% 
  slice(which.max(n)) %>% 
  group_by(trip_n) %>% 
  mutate(spec_l = reg_species[which(c(manag == 'l'))[1]]) %>%
  filter(manag == "d")  %>%  # to keep only disturbed ones
  mutate(RA4_1 = case_when(spec_l == reg_species ~ 0,  # compare with the living ones
                           spec_l != reg_species ~ 1))


  #top_n(1, DBH)
  
#RA4 ------------------------------------------
df_full_plot %>% 
  # find species most often the highest dbh, the largest height?
  filter(manag != 'c' & height_class != 'mature') %>% 
  mutate(height_class_num = as.numeric(gsub('HK', '', height_class))) %>% # change to numeric values
  group_by(trip_n, dom_sp, manag) %>% #, reg_species
  select(!sub_n) %>% 
  filter(height_class_num == max(height_class_num)) %>% 
  #mutate(max_dbh = filter(DBH == max(DBH))) %>% 
  arrange(trip_n) 







