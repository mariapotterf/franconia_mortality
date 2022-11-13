
# Get drivers of the forest reorganization:

# auxiliary vars:
# - temp
# - precip
# - anomalies?? (compare the 2018-2020 with 1986-2015) - check Cornelius script
# - deadwood volume
# - ground cover:which aspect? eg. bare grund [%]
# - disturbance intensity - % of the basal area removed (after DIST)


#### Read libraries  -----------------------------------------------------------
#library(readxl)
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
head(out_reorg_pos)       # - Euclidean distances for each site


# Master plots:
head(plot_counts_df)      # - total count of the plots per triplets & categories: to standardize the densities...


# Process: 

# Disturbance intensity ---------------------------------------------------------
# Get the BA of removed mature trees:
# = % removed living trees between REF and DIST
# get BA of REF, of DIST
# BA = sum over ha, then divide by the number of sub_n by site
# !!!!

df_full_corr_mrg %>% 
  filter(height_class %in% c('mature', 'mat_ENV')) %>% 
  right_join(plot_counts_df) %>% 
  mutate(DBH = case_when(is.na(DBH) ~ 0, # complete 
                         !is.na(DBH) ~ DBH),
         corr_count  = case_when(is.na(corr_count ) ~ 0, # complete 
                         !is.na(corr_count ) ~ corr_count)) %>%
  #filter(trip_n == 25) %>% 
  mutate(r = DBH/100/2, # r in meters
         BA = pi*r^2, #) %>% #,
         BA_ha = BA*corr_count )  %>% # set on hectar value, is comparable now
  # filter(DBH == 0)
  group_by(trip_n, manag) %>% 
    mutate(avg_BA = mean(BA_ha, na.rm = T)) %>%
  #View()
  filter(trip_n == 66) %>% 
  View()
    #summarize(avg_BA = mean(BA_ha, na.rm = T)) %>%
 mutate(ref_BA = avg_BA[which(c(manag == 'l'))[1]]) %>% 
#filter(trip_n == 1 ) %>% 
  #arrange(BA_ha   ) %>% 
 # print(n = 30)
  mutate(severity_BA = 100 - (avg_BA / ref_BA*100)) %>% # % of removed/died mature trees
  filter(severity_BA < 0)
  #mutate(rel_BA = replace_na(rel_BA, 0)) #%>%  # replace NA by 0 if BA is missing


# weird values:
# 66-d ~ 127%
# 25-d ~ 130%


# Investigate ---------------------------------
df_full_corr_mrg %>% 
  filter(height_class %in% c('mature', 'mat_ENV')) %>% 
  filter(trip_n == 25) %>% 
  group_by(manag) %>% 
  mutate(avg_BA = mean(BA_ha, na.rm = T)) %>%
  View()
