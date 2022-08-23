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
head(df_IVI_out)    # - df importance value: df_IVI_out
head(df_winners)    # - novel species: df_novelty
head(trait_df)      # - trait values for all species: eco_traits


# Reassembly:
# add novelty info to the df_IVI_out:

# Example of novel species:
#> df_winners: 
# A tibble: 34 x 4
#trip_n manag reg_species novelty
#  1 24     d     Oak         novel  
#  2 24     d     Ash         novel  

# Some species have importance value NA???? how is this possible????
df_IVI_out %>%
  filter(trip_n == '4' & manag == 'd')


# Novel species: presence
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

#RA2 <-

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
  dplyr::select(trip_n, manag, novelty, shade_cwm, drought_cwm) # %>% # how to pass the new value to the new column?
  mutate(shade_novel = shade_cwm[novelty == 'novel',]) # !!! does not work!! 
  print(n = 50)
  
  group_by(trip_n, manag) %>% 
  summarize(IVI_sum = sum(sp_IVI, na.rm = T),
            IVI_sum_novel = sum(sp_IVI[novelty == 'novel'])) %>%
  mutate(RA2 = case_when(shade_novel > shade_cwm  ~ 1,
                         shade_novel <= shade_cwm ~ 0))







