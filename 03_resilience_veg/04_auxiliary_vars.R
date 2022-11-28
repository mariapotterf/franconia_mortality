
# Get drivers of the forest reorganization:

# auxiliary vars:
# - temp
# - precip
# - anomalies?? (compare the 2018-2020 with 1986-2015) - check Cornelius script
# - deadwood volume - from ground cover (%), 
# - from teh ENV data - need to discuss which variables (log, standing, stump, root plate to use?)
# - ground cover:which aspect? eg. bare ground [%]
# - disturbance intensity - % of the basal area removed (after DIST) - not included


#### Read libraries  -----------------------------------------------------------
library(readxl)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(stringr)  # use regex expressions
library(ggpubr)
library(ggrepel)

# Input data -------------------------------------------------------------------
source('my_vars_and_functions.R')
source('myPaths.R')

getwd()
load(file = paste(getwd(), "outData/dataToPlot.Rdata", sep = '/'))
load(file = paste(getwd(), "outData/eco_traits.Rdata", sep = '/'))
load(file = paste(getwd(), "outData/dat_restr.Rdata", sep = '/'))



# Identify data to use:
head(df_full_corr_mrg)     # - full PLOT based data: df_full_corr, seedlings, advanced, mature - PLOt & surroundings, mature trees filtered 
head(plot_IVI)             # - df importance value:from plot, env mature, env advanced, merged by density/ha
head(trait_df)             # - trait values for all species: eco_traits
head(df_mature_trees_env)  # - trees in the surroundings: mature trees - set distance to 16 m if no tree present
head(df_deadwood_env_corr) # - deadwood in ENV, 4 categs: log, root plate, snag, stump  
head(df_advanced_env)      # - trees in the surroundings: advanced
head(out_reorg_pos)        # - Euclidean distances for each site


# Master plots:
head(plot_counts_df)       # - total count of the plots per triplets & categories: to standardize the densities...

# Triplets by dominant species:
trip_species               # dominant species by site design
df_dom_sp                  # - indication of dominant species by site, dominance by rIVI


# Get climatic variables:
df_prec   <- fread(paste(myPath, outTable, 'xy_precip.csv', sep = '/'))
df_temp   <- fread(paste(myPath, outTable, 'xy_temp.csv', sep = '/'))
#df_spei   <- fread(paste(myPath, outTable, 'xy_spei.csv', sep = '/'))

# Get patch size data:
dat_size  <- read_excel(paste(myPath, '02_contact_forest_owners/find_sites_field/final/final_share', 
                              "sites_unique_ID.xlsx", sep = '/'))

# get ground cover: get deadwood
df_ground  <- fread(paste(myPath, outTable, 'df_ground.csv', sep = '/'))


# set theme: plotting ----------------------------------------------------------- 
theme_set(theme_bw())
theme_update(legend.position = 'bottom') 



# Process: ---------------------------------------------------------------------

#$ add padding zeors:
trip_species <-  trip_species %>%  
  mutate(trip_n = stringr::str_pad(trip_n, 2, pad = "0")) 


df_DW_ground <- df_ground %>% 
  mutate(trip_n = as.character(trip_n),
         sub_n = as.character(sub_n)) %>% 
  filter(class == "deadwood/stumps") %>% 
  group_by(trip_n, manag) %>% 
  summarise(prop_DW_gc = mean(prop)) %>% 
  mutate(trip_n = stringr::str_pad(trip_n, 2, pad = "0")) 


# keep only useful columns
df_patch <- 
  dat_size %>% 
  dplyr::select(Name, Area_m2) %>% 
  separate(Name, c('trip_n', 'dom_sp', 'manag'), '-') %>% 
  mutate(manag = tolower(manag),
         trip_n = as.character(as.numeric(trip_n))) %>% 
  filter(manag != 'l') %>% 
  filter(!trip_n %in% c(45, 65)) %>% 
  dplyr::select(!dom_sp) %>% 
  mutate(trip_n = stringr::str_pad(trip_n, 2, pad = "0")) #%>% 
     #    trip_n = as.numeric(trip_n)) 


 

# Get deadwood from surroundings: -----------------------------------------
# need to first sum up across categories (plot level), 
# then calculate the average per site
df_deadwood_site <- df_deadwood_env_corr %>% 
  group_by(trip_n, manag, sub_n) %>% 
  summarize(sum_DW_dens = sum(corr_count)) %>% 
  ungroup(.) %>% 
  group_by(trip_n, manag) %>% 
  summarise(mean_DW = mean(sum_DW_dens)) %>% 
  mutate(trip_n = stringr::str_pad(trip_n, 2, pad = "0"))


# check the values:
df_deadwood_site %>% 
  ggplot(aes(x = manag,
             y = mean_DW)) +
  geom_point() + 
  geom_line(aes(group = trip_n))


# Check the numbers across the DW categories:

manag.labs        <- c("MAN", "UNM", "REF")
names(manag.labs) <- c("c", "d", "l")
manag.level       <- c('l', 'c', 'd')   # to order the data on x axis: REF, MAN, UNM

df_deadwood_env_corr %>% 
  left_join(df_dom_sp, by = c('trip_n')) %>% 
  ggplot(aes(x = manag,
             y = corr_count,
             group = DW_type,
             color = DW_type)) +
  stat_summary(position = position_dodge(0.2)) +
  stat_summary(fun = "mean", geom = "line",
               position = position_dodge(0.2)) +  
  facet_grid(. ~ dom_sp) +
  scale_x_discrete(name = '',
                   breaks = names(manag.labs),
                   labels = manag.labs)



# Process: -----------------------------------------------------------------------
# split Name in three columsn, get means per year and site (to have only 40 vals)
# calculate anomalies (1986-2015 vs 2018-2020)
# get only summer temperatures? vegetation period?

# Get anomalies:  
# get vector of years as reference
reference_period <- 1986:2015
drought_period   <- 2018:2020

# Precipitation
df_prec_out <- 
  df_prec %>% 
  filter(year %in% 1986:2020) %>%  #month %in% 4:10 & 
  separate(Name, c('trip_n', 'dom_sp', 'manag'), '-') %>% 
  group_by(trip_n, year) %>% 
    ungroup(.) %>% 
    group_by(trip_n, year) %>% 
  summarize(avg_prec = mean(vals)) %>% 
   # filter(year %in% drought_period)
    mutate(prec_ref   = mean(avg_prec[year %in% reference_period], na.rm = T),
           prec_18_20 = mean(avg_prec[year %in% drought_period], na.rm = T),
           anomaly_prec_18_20  = prec_18_20 / prec_ref - 1) %>% 
  dplyr::select(c(prec_ref, prec_18_20, anomaly_prec_18_20)) %>% 
  distinct() 


df_temp_out <- 
  df_temp %>% 
  filter( year %in% 1986:2020) %>% # month %in% 4:10 &, get the whole year get only vegetation season
  separate(Name, c('trip_n', 'dom_sp', 'manag'), '-') %>% 
  group_by(trip_n, year) %>% 
  summarize(avg_temp = mean(vals)) %>% 
  mutate(temp_ref   = mean(avg_temp[year %in% reference_period], na.rm = T),
         temp_18_20 = mean(avg_temp[year %in% drought_period], na.rm = T),
         anomaly_temp_18_20  = temp_18_20 / temp_ref - 1) %>% 
  dplyr::select(c(temp_ref, temp_18_20, anomaly_temp_18_20)) %>% 
  distinct() 


# get only Euclidean distances as y ---------------------------------------------
df_euc <- out_reorg_pos %>% 
  dplyr::select(c(trip_n, manag, euclid_dist))


# Merge auxiliary data with Euclidean distances --------------------------------
df <- 
  df_euc %>% 
  mutate(trip_n = stringr::str_pad(trip_n, 2, pad = "0")) %>% 
  as.data.frame() %>% 
  left_join(filter(df_DW_ground, manag != 'l')) %>% 
  left_join(filter(df_deadwood_site, manag != 'l')) %>% 
  left_join(df_prec_out) %>% 
  left_join(df_temp_out) %>% 
  left_join(trip_species) %>% 
  left_join(df_patch, by = c("trip_n", "manag")) %>% 
  mutate(manag = as.factor(manag))  



# add padding zeros

library(stringr)
v <- c('1', '2', '13')






# scatter: euclid vs size:
p_dist_patch <- df %>% 
  ggplot(aes(x = Area_m2/10000 ,
             y = euclid_dist,
             color = manag)) +
  geom_point() +
  facet_grid(.~manag)

# scatter: euclid vs DW:
#p_dist_DW_ground <- 
  df %>% 
  ggplot(aes(x = prop_DW   ,# proportion deadwood
             y = euclid_dist,
             color = manag)) +
  geom_point() +
  facet_grid(.~manag)


# scatter: euclid vs temp
#p_dist_DW_ground <- 
  df %>% 
  ggplot(aes(x = temp_ref ,# proportion deadwood
             y = euclid_dist,
             color = manag)) +
  geom_point() +
  facet_grid(.~manag)
  
  
  
# scatter: euclid vs temp
#p_dist_DW_ground <- 
  df %>% 
    ggplot(aes(x = prec_ref ,# proportion deadwood
               y = euclid_dist,
               color = manag)) +
    geom_point() +
    facet_grid(.~manag)
  


# test GAMM: -----------------------
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



 
 
save(df,        # output file with all predictors for stat testing
     p_dist_patch, # scatter plot
     p_anom_temp, # test model: temperature
     file="outData/auxData.Rdata")
 