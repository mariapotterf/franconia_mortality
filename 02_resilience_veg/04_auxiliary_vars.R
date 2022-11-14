
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
source('myPaths.R')

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

# Get climatic variables:
df_prec   <- fread(paste(myPath, outTable, 'xy_precip_2000.csv', sep = '/'))
df_temp   <- fread(paste(myPath, outTable, 'xy_temp_2000.csv', sep = '/'))
df_spei   <- fread(paste(myPath, outTable, 'xy_spei.csv', sep = '/'))




# Process: -----------------------------------------------------------------------
# split Name in three columsn, get means per year and site (to have only 40 vals)
# calculate anomalies (1986-2015 vs 2018-2020)
# get only summer temperatures? vegetation period?

# now he climate values are only from 2000 to 2020!!! need to complete the years from 1986!


# Get anomalies:  
# get vector of years as reference
reference_period <- 1986:2015
drought_period   <- 2018:2020

#df_prec_out <- 
  df_prec %>% 
  filter(month %in% 4:10 & year %in% 1986:2020) %>% 
  separate(Name, c('trip_n', 'dom_sp', 'manag'), '-') %>% 
  group_by(trip_n, year) %>% 
    ungroup(.) %>% 
    group_by(trip_n, year) %>% 
  summarize(avg_prec = mean(vals)) %>% 
   # filter(year %in% drought_period)
    mutate(mean_ref   = mean(avg_prec[year %in% reference_period], na.rm = T),
           mean_18_20 = mean(avg_prec[year %in% drought_period], na.rm = T),
           #anomaly    = disturbance_ha / mean(disturbance_ha[year %in% reference_period], na.rm = TRUE) - 1,
           anomaly_18_20  = mean_18_20 / mean_ref - 1) #%>% 


df_temp_out <- 
  df_temp %>% 
  filter(month %in% 4:10) %>% # get only vegetation season
  separate(Name, c('trip_n', 'dom_sp', 'manag'), '-') %>% 
  group_by(trip_n, year) %>% 
  summarize(avg_temp = mean(vals)) %>% 
  mutate(mean_ref   = mean(avg_temp[year %in% reference_period], na.rm = T),
         mean_18_20 = mean(avg_temp[year %in% drought_period], na.rm = T),
         anomaly_18_20  = mean_18_20 / mean_ref - 1) #%>% 






# Calculate anomalies: first remove the grids that have less than 1 ha/year of mortality at average:
# is this valid for my hexa data as well? !
# calculate anomalies for the year 2018-2020 as
# calculate anomalies from year 2019-2020
#out.df2 <-
  out.df %>%
  group_by(gridindex) %>%
  filter(sum(disturbance_ha) > 35) %>% # Exclude areas with less than 1 ha/yr of disturbances on average
  filter(sum(disturbance_ha[year %in% reference_period]) > 30) %>% # Exclude areas with less than 1 ha/yr of disturbances on average
  mutate(mean_ref    = mean(disturbance_ha[year %in% reference_period], na.rm = T),
         sum_18_20   = sum( disturbance_ha[year %in% drought_period], na.rm = T)/3,
         sum_19_20   = sum( disturbance_ha[year %in% c(2019,2020)], na.rm = T)/2,
         anomaly     = disturbance_ha / mean(disturbance_ha[year %in% reference_period], na.rm = TRUE) - 1,
         anomaly_18_20  = sum_18_20 / mean_ref - 1,
         anomaly_19_20  = sum_19_20 / mean_ref - 1) %>% 
  ungroup()








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
