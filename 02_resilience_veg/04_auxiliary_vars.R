
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



# Get patch size data:

dat_size  <- read_excel(paste(myPath, '03_plot_sampling/sites_identification/final/share', 
                              "sites_unique_ID.xlsx", 
                              sep = '/'))

# keep only useful columns
df_patch <- 
  dat_size %>% 
  select(Name, Area_m2) %>% 
  separate(Name, c('trip_n', 'dom_sp', 'manag'), '-') %>% 
  mutate(manag = tolower(manag),
         trip_n = as.character(as.numeric(trip_n))) %>% 
  filter(manag != 'l') %>% 
  filter(!trip_n %in% c(45, 65)) %>% 
  dplyr::select(!dom_sp)
     #    trip_n = as.numeric(trip_n)) 



# Process: -----------------------------------------------------------------------
# split Name in three columsn, get means per year and site (to have only 40 vals)
# calculate anomalies (1986-2015 vs 2018-2020)
# get only summer temperatures? vegetation period?

# !!! now he climate values are only from 2000 to 2020!!! need to complete the years from 1986!


# Get anomalies:  
# get vector of years as reference
reference_period <- 1986:2015
drought_period   <- 2018:2020

# Precipitation
df_prec_out <- 
  df_prec %>% 
  filter(month %in% 4:10 & year %in% 1986:2020) %>% 
  separate(Name, c('trip_n', 'dom_sp', 'manag'), '-') %>% 
  group_by(trip_n, year) %>% 
    ungroup(.) %>% 
    group_by(trip_n, year) %>% 
  summarize(avg_prec = mean(vals)) %>% 
   # filter(year %in% drought_period)
    mutate(prec_ref   = mean(avg_prec[year %in% reference_period], na.rm = T),
           prec_18_20 = mean(avg_prec[year %in% drought_period], na.rm = T),
           #anomaly    = disturbance_ha / mean(disturbance_ha[year %in% reference_period], na.rm = TRUE) - 1,
           anomaly_prec_18_20  = prec_18_20 / prec_ref - 1) %>% 
  dplyr::select(c(prec_ref, prec_18_20, anomaly_prec_18_20)) %>% 
  distinct() 


df_temp_out <- 
  df_temp %>% 
  filter(month %in% 4:10 & year %in% 1986:2020) %>% # get only vegetation season
  separate(Name, c('trip_n', 'dom_sp', 'manag'), '-') %>% 
  group_by(trip_n, year) %>% 
  summarize(avg_temp = mean(vals)) %>% 
  mutate(temp_ref   = mean(avg_temp[year %in% reference_period], na.rm = T),
         temp_18_20 = mean(avg_temp[year %in% drought_period], na.rm = T),
         anomaly_temp_18_20  = temp_18_20 / temp_ref - 1) %>% 
  dplyr::select(c(temp_ref, temp_18_20, anomaly_temp_18_20)) %>% 
  distinct() 



# Merge clim data with Euclidean distances -------------------------------------
df_euc <- out_reorg_pos %>% 
  dplyr::select(c(trip_n, manag, euclid_dist))


df <- 
  df_euc %>% 
    as.data.frame() %>% 
  left_join(df_prec_out) %>% 
  left_join(df_temp_out) %>% 
  left_join(df_patch, by = c("trip_n", "manag")) %>% 
  mutate(manag = as.factor(manag))  


# scatter: euclid vs size:
p_dist_patch <- df %>% 
  ggplot(aes(x = Area_m2/10000 ,
             y = euclid_dist,
             color = manag)) +
  geom_point() +
  facet_grid(.~manag)



# test GAMM:
library(mgcv)
theme_set(theme_bw())
library(tidymv)

model <- gam(
  euclid_dist ~
    manag +
    s(anomaly_temp_18_20),
  data = df
)

summary(model) 

#https://cran.r-project.org/web/packages/tidymv/vignettes/predict-gam.html


model_p <- predict_gam(model)

model_p


p_anom_temp <- model_p %>% 
  ggplot(aes(y = fit  ,
           x = anomaly_temp_18_20)) +
  geom_smooth_ci(manag)


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

 
 
 
save(p_dist_patch, # scatter plot
      p_anom_temp, # test model: temperature
      file="outData/auxData.Rdata")
 