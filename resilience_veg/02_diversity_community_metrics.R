# Analyse the data:

# get data about community structure
# ecological traits
# shanon diversity
# test different categories and community resemblance?
# remove all previous data from R memory
# Convert the regeneration counst into density/ha - takes into account the difference in sampling plot!
#  need to do the slope correction?
# http://wiki.awf.forst.uni-goettingen.de/wiki/index.php/Slope_correction
# our inclinometer suunto is in degrees: goes 0-90
# slope correction: only needed for slopes > 10% (7 degrees) # https://www.archtoolbox.com/calculating-slope/
# 100% slope = 45 degrees (1:1 gradient)
# NA% slope = 90 degrees  (1:0 gradient)

rm(list=ls())



# Input data -------------------------------------------------------------------

load(file = "vegData.Rdata")


#### Source paths and functions  -----------------------------------------------

source('myPaths.R')


#### Read libraries  -----------------------------------------------------------
library(readxl)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(stringr)  # use regex expressions
#library(gridExtra)
library(ggpubr)


# Get tree species traits:  ----------------------------------------------------------

eco_traits <- read_excel(paste(myPath,
                               'notes/litterature/traits_database',  
                               'Niinemets_2006.xls', sep = '/'),
                         skip = 3,
                         sheet = 'Niinemets_2006_appendix',
                         .name_repair = function(x) gsub("\\s+", "_", x)) # replace the spaces in colnames by '_'


# Interpretation: 
# shade tolerance: 
#             higher number = more tolerance (fir), 
#             lower number = less tolarence (more sunny site, pine)
# drought tolerance: 
#             higher  = more tolerance (pine), 
#             lower = less (more drought sensitive, spruce)

# Filter only relevant species:  # how to handle the Other sftwoos and Other harvwood? now just skipped 
# Any way it is likely not dominant
trees_lat <- c(
  'Picea abies',
  'Fagus sylvatica',
  'Sorbus aucuparia',
  'Abies alba',
  'Acer pseudoplatanus',
  'Betula pendula',
  'Pinus sylvestris',
  'Fraxinus excelsior'
) 

quercus_spp <- c('Quercus petraea',  # Quercus will be averaged later
                 'Quercus robur')

salix_spp <- c('Salix caprea',     # Salix will be averaged later
               'Salix alba')

# Get Quercus spp: average the values for the :
# Quercus petraea , Quercus robur
# For salix: Salix alba, Salix caprea  (S. fragilis is not that common)

#'OtherHardwood',
# 'OtherSoftwood'

# Filter eco traits database by species: 
# need to do individuall for Quercis, salix and for other species 
# that we have full name identification
traits_Qc <- 
  eco_traits %>% 
  dplyr::select(c('Species','Shade_tolerance', 'Drought_tolerance')) %>% 
  filter(Species %in% quercus_spp)  %>% 
    mutate(Species = 'Quercus') %>% 
    group_by(Species) %>% 
    summarize(Shade_tolerance = mean(Shade_tolerance),
              Drought_tolerance = mean(Drought_tolerance))

traits_Sx <- 
  eco_traits %>% 
  dplyr::select(c('Species','Shade_tolerance', 'Drought_tolerance')) %>% 
  filter(Species %in% salix_spp)  %>% 
  mutate(Species = 'Salix') %>% 
  group_by(Species) %>% 
  summarize(Shade_tolerance   = mean(Shade_tolerance),
            Drought_tolerance = mean(Drought_tolerance))

# remianing species:
traits_sp <- 
  eco_traits %>% 
  dplyr::select(c('Species','Shade_tolerance', 'Drought_tolerance')) %>% 
  filter(Species %in% trees_lat)

# Merge traits into single df
trait_df <- rbind(traits_Qc,
                  traits_Sx,
                  traits_sp) 

# Change naming to be able to merge them with denity dataset:
trait_df <- trait_df %>%
  mutate(
    Species = case_when(
      Species == "Fraxinus excelsior"    ~ "Ash",
      #Species == "Sonstiges NH"         ~ "OtherSoftwood",
      # Species == "Sonstiges LH"        ~ "OtherHardwood",
      Species == "Fagus sylvatica"       ~ "Beech" ,
      Species == "Sorbus aucuparia"      ~ "Rowan",
      Species == "Acer pseudoplatanus"   ~ "Maple",
      Species == "Picea abies"           ~ "Spruce",
      Species == "Quercus"               ~ "Oak",
      Species == "Pinus sylvestris"      ~ "Pine",
      Species == "Betula pendula"        ~ "Birch",
      Species == "Salix"                 ~ "Willow",
      Species == "Abies alba"            ~ "Fir"
    )
  ) %>% 
  rename(reg_species = Species)


# Slope correction factor:------------------------------------------------------
# correct the measurements in the field to the map plane projection (e.g. 'shrinks the field sampling plot')
# need to calculate the area of the study site on the pane: will change one axis, other stays the same 
# then need to recalculate the correction factor: at plane, the factor is 2500 (4m2 to 10000m2)

ha = 10000
trees_field = 10
gradient = 16.7 #(has to be in degrees!)

r = 2    # m
r1 = r
r2 = r1*cos(gradient*pi/180)   # R works in radians: to geth the value in degrees, it has to be in formm cos(angle * pi/180) 
# https://r-lang.com/r-cos-function-with-example/
area_field = r^2# m2
area_plane = r1*r2

# Get the correction factor:
ideal_factor   = ha/area_field
correct_factor = ha/area_plane

# Calculate teh tree deisnity based on field, and based on corrected area:
trees_dens_field = trees_field*ideal_factor
trees_dens_plane = trees_field*correct_factor




# Density correction function --------------------------------------------------
# the gradient is subset specific: therefore, first adjust the number of the 
# area per subset, and teh number of densities
slope_corr <- function(gradient, ...) {
  
  # get the dimension of the corrected plane sampling plot
  r1 = 2
  r2 = r1*cos(gradient*pi/180) 
  
  # calculate the expansion factor
  correct_factor = ha/r1*r2
  
  # correct the number of trees/ha
  dens_corr = trees_field*correct_factor
  return(dens_corr)
  
}




# Get tree densities ------------------------------------------------------

# Define sample area per patch - correct the density/ha estimation----------------
# calculate from the original data table
subsample_n <- 
  dat %>%
  group_by(trip_n , dom_sp , manag ) %>% 
  distinct(sub_n ) %>% 
  tally() %>%
  mutate(trip_n = as.character(trip_n)) %>% 
  rename(n_subsites = n)



# The correction of the counts: need to do it on the level of individual subsites:
# as there is high variation in slopes ('gradient') between the subsites
# So I am correcting the tree density/ha per each subsite: maybe then use teh mean/sum densities per category??? 
df_regen <- df_regen %>% 
  mutate(manag = factor(manag, levels = c('l', 'c', 'd'),
                        labels = c('living','cleared', 'dead')))



df_regen %>% 
ggplot(aes(x = trip_n,
           y = gradient)) + 
  geom_boxplot() + 
  facet_grid(~manag)




# Get tree density/ha across all heights, correct the density by slope
df_reg_dens <- 
  df_regen %>% 
  left_join(subsample_n) %>% 
  ungroup() %>% 
  group_by(trip_n, dom_sp, manag, n_subsites, height_class, reg_species)  %>% 
    mutate(length_corr = 2*cos(gradient*pi/180),
           area_corr   = 2*length_corr,
           correct_factor = ha/area_corr,
           corr_density = n_total*correct_factor)  %>% 
    filter(corr_density != 0) %>% 
  ungroup(.) #%>% 


# Get Shannon: ------------------------------------------------------------
# - get the total density
# - calculate the share per reg_species: pi
# - calculate Shannon: H = -sum(pi*log(pi)) for each reg_species
# - values for Shannon have to be in 0-1 range (not 0-100)!

df_reg_dens_shannon <-
  df_reg_dens %>%
  group_by(trip_n, dom_sp, manag, reg_species) %>%
  summarize(mean_dens = mean(corr_density)) %>%
  mutate(
    dens_tot = sum(mean_dens),
    sp_pi = mean_dens / dens_tot,
    shannon_part = sp_pi * log(sp_pi)
  ) %>%
  summarize(shannon = -sum(shannon_part)) %>%
  mutate(effective_n_sp = exp(shannon))# %>%





df_reg_dens_shannon %>% 
  ggplot(aes(y = effective_n_sp,
             x = manag)) +
  geom_boxplot() + 
  facet_grid(~dom_sp, scales = 'free') #+
#ylab('density \n(#trees/ha)') 






# Get community weighted means ---------------------------------------------
# https://rpubs.com/CPEL/cwm

df_traits_cwm <- 
  df_reg_dens %>%
  group_by(trip_n, dom_sp, manag, reg_species) %>%
  summarize(mean_dens = mean(corr_density)) %>%
  left_join(trait_df) %>% 
  ungroup(.) %>% 
  group_by(trip_n, dom_sp, manag) %>% 
  summarize(shade_cwm = weighted.mean(Shade_tolerance,     mean_dens, na.rm = TRUE  ),
            drought_cwm = weighted.mean(Drought_tolerance, mean_dens, na.rm = TRUE  )) %>% 
  mutate(manag = factor(manag, 
                        #levels = c('l', 'c', 'd'),
                        levels = c('living','cleared', 'dead')))

# Make some plots:
p_shade <- df_traits_cwm %>% 
  ggplot(aes(x = factor(dom_sp),
             y = shade_cwm,
             fill = manag)) +
  geom_boxplot() 

p_drought <- df_traits_cwm %>% 
  ggplot(aes(x = factor(dom_sp),
             y = drought_cwm,
             fill = manag)) +
  geom_boxplot()


ggarrange(p_shade,p_drought, ncol = 2, nrow = 1 , common.legend = TRUE  )






# Shannon index -----------------------------------------------------------

## Ground cover: -------------------------------------
# get the average values per category, not by the subplot:
df_ground_shann <-
  df_ground %>% 
  group_by(trip_n, dom_sp, manag, class) %>% 
  summarize(mean_prop = mean(prop)/100) %>%  # for shannon, the scale should be 0-1, not 0-100% !! 
  mutate(shannon_part = replace_na(-(mean_prop)*log(mean_prop),0)) %>% 
  ungroup() %>% 
  group_by(trip_n, dom_sp, manag) %>%
  summarize(shannon_ground = sum(shannon_part)) %>% 
  mutate(effective_n_ground = exp(shannon_ground))
# calculate shannon and replace NA vals by 0
#shannon = is.na())


#### Shannon check plot: ---------------------------------------------------------------------
windows()
df_ground_shann %>% 
  ggplot(aes(x = factor(manag),
             y = effective_n_ground)) +
  geom_boxplot() +
  geom_violin()

windows()
df_ground_shann %>% 
  ggplot(aes(x = factor(manag),
             y = shannon_ground,
             fill = dom_sp)) +
  geom_violin() 


# Get the differences between 
# Test: does the management manag affects the diversity of th ground cover?
# HO ; the Cleared area will be more homogenous then the Dead ones:
# clearing homogenize teh gropund cover (prevalence of grasses, ...)








# Calculate Shannon per subsite?? 
# then I can condider the individual patches as random effects in the model
df_reg_fin_by_subsample <- 
  df_regen_fin %>% 
  select(-c(height_class, DBH, reg_height)) %>% 
  arrange(trip_n, manag) %>%
  group_by(trip_n, dom_sp, manag, sub_n, reg_species) %>% 
  summarize(count_sp        = sum(count))  %>% 
  ungroup() %>% 
  group_by(trip_n, dom_sp, manag, sub_n) %>% 
  mutate(dens_tot        = sum(count_sp),
         sp_pi           = count_sp/dens_tot,
         shannon_part_sp = sp_pi*log(sp_pi),
         shannon_sp      = -sum(shannon_part_sp),
         eff_numb_sp     = exp(shannon_sp))# %>%
#   print(n = 40)




# Calculate Shannon height diversity ??? -------------
#df_reg_fin_by_subsample_height <- 
df_regen_fin %>% 
  arrange(trip_n, manag) %>%
  group_by(trip_n, dom_sp, manag, sub_n) %>% 
  mutate(dens_tot        = sum(count),
         sp_pi           = count/dens_tot,
         shannon_part_sp = sp_pi*log(sp_pi),
         shannon_sp      = -sum(shannon_part_sp),
         eff_numb_sp     = exp(shannon_sp))# %>% 



# evaluate teh dominance of reg_species: dominant: if more then 50% of stems



# How does the height structure looks like?
df_reg_fin_by_subsample %>% 
  ggplot(aes(y = height,
             x = factor(manag),
             fill = dom_sp)) +
  geom_violin()




# keep only distinct rows for mixed effects:

dat <- df_reg_fin_by_subsample %>% 
  mutate(uniqueID = factor(paste(trip_n,dom_sp,manag,sub_n, sep = '_' ))) %>% 
  dplyr::select(uniqueID, shannon, eff_numb) %>% 
  distinct() #%>% 




# How does management affect the dievrsity of reg_species composition?
dat$manag <- as.factor(dat$manag)
dat$dom_sp <- as.factor(dat$dom_sp)

m1 <- lme(eff_numb~ manag, random = ~1|uniqueID, data = dat)

summary(m1)

m2 <- lme(eff_numb~ manag + dom_sp, random = ~1|uniqueID, data = dat)

summary(m2)
AIC(m1, m2)

hist(dat$eff_numb)

# use glm and poisson family:
m3 <- glm(eff_numb~ manag + dom_sp, family = poisson(link = "log"), # poisson family
          data = dat)

AIC(m3)
summary(m3)




# Does the management explain the tree regen diversity?? --------------------------------------
m1<-lm(effective_n_sp~1, df_reg_dens_shannon)  # this ignores the management, and cosider the treatmements as independent
summary(m1)

# only predict the mean effective number: 2.6351     
coef(m1)
# (Intercept) 
# 2.63515 

confint(m1)
#               2.5 %   97.5 %
#  (Intercept) 2.237458 3.032842

# Mixed effect model: 
# can I keep in the small patches, and evaluate them as a group within the treatments? e.g. subsites within the patch will
# be more similar that patches further away
library(nlme)
m3 <- lme(effective_n_sp~1, df_reg_dens_shannon)
summary(m3)






df_reg_dens %>% 
  ggplot(aes(y = density_ha,
             x = manag)) +
  geom_boxplot() + 
  facet_grid(reg_height~dom_sp, scales = 'free') +
  ylab('density \n(#trees/ha)') 





# Investigate data by categories --------------------------------------------------------------


# Check density distribution:

df_regen %>% 
  filter(count != 0) %>% # need to filter zero counts first
  group_by(trip_n, dom_sp, manag) %>% 
  ggplot(aes(y = density_ha,
             x = manag)) +
  geom_boxplot() + 
  facet_grid(~dom_sp) +
  ylab('density \n(#trees/ha)')




# calculate richness:
# count number of reg_species per plot
#richness <- 
df_regen %>% 
  filter(count != 0) %>% # need to filter zero counts first
  group_by(trip_n,dom_sp, manag) %>% 
  distinct(reg_species) %>% 
  count() %>% 
  ggplot(aes(y = n,
             x = manag)) +
  geom_boxplot() + 
  # geom_jitter() +
  facet_grid(~dom_sp) +
  ylab('richness \n(# of tree reg_species)')


# What is regeneration height??
df_regen %>% 
  filter(count != 0)  %>% # need to filter zero counts first
  group_by(trip_n,dom_sp, manag) %>% 
  #  distinct(reg_species) %>% 
  #  count() %>% 
  ggplot(aes(y = count,
             x = height_class,
             fill = manag)) +
  geom_boxplot() + 
  # geom_jitter() +
  facet_grid(.~dom_sp) #+
#ylab('richness \n(# of tree reg_species)')


