# Analyse the data:

# get data about community structure
# ecological traits
# shanon diversity
# test different categories and community resemblance?
# remove all previous data from R memory
# Convert the regeneration counst into density/ha - takes into account the difference in sampling plot!
#  need to do the slope correction?
# http://wiki.awf.forst.uni-goettingen.de/wiki/index.php/Slope_correction

rm(list=ls())

load(file = "vegData.Rdata")


# Input data -------------------------------------------------------------------
#### Source paths and functions  -----------------------------------------------

source('myPaths.R')


#### Read libraries  -----------------------------------------------------------
library(readxl)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(stringr)  # use regex expressions


# get trait database:
eco_traits <- read_excel(paste(myPath,
                               'notes/litterature/traits_database',  
                               'Niinemets_2006.xls', sep = '/'),
                         skip = 3,
                         sheet = 'Niinemets_2006_appendix',
                         .name_repair = function(x) gsub("\\s+", "_", x)) # replace the spaces in colnames by '_'


# Filter only relevant species:  # how to handle the Other sftwoos and Other harvwood? now just skipped 
# Any way it is likely not dominant
trees_lat <- c('Picea abies',
          'Fagus sylvatica',
          'Sorbus aucuparia',
          'Abies alba',
          'Quercus petraea',  # Quercus will be averaged later
          'Quercus robur',
          'Acer pseudoplatanus',
          'Betula pendula',
          'Salix caprea',     # Salix will be averaged later
          'Salix alba',
          'Pinus sylvestris',
          'Fraxinus excelsior') 

# Get Quercus spp: average the values for the :
# Quercus petraea , Quercus robur
# For salix: Salix alba, Salix caprea  (S. fragilis is not that common)

#'OtherHardwood',
# 'OtherSoftwood'

# Filter eco traits database by species: 
eco_traits %>% 
  dplyr::select(c('Data_set_1', 'Species','Shade_tolerance', 'Drought_tolerance')) %>% 
  filter(Data_set_1 == 'Europe') %>% 
    #print(n = 100)
  filter(Species %in% trees_lat)




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

# Define sample area per patch - correct the hdensity/ha estimation----------------
# calculate from the original data table
subsample_n <- 
  dat %>%
  group_by(trip_n , dom_sp , manag ) %>% 
  distinct(sub_n ) %>% 
  tally() %>%
  mutate(trip_n = as.character(trip_n))


# Get tree densiy/ha across all heights:
df_reg_dens <- 
  df_regen_fin %>% 
  left_join(subsample_n) %>% 
  ungroup() %>% 
  group_by(trip_n, dom_sp, manag, n, reg_height, reg_species) %>% 
  summarize(reg_count = sum(count, na.rm = T) )  %>%
  mutate(density_ha = reg_count/n/4*10000) #%>% 


# Calculate shannon for individual subsites
# get the total density
# calculate the share per reg_species - pi
# calculate Shannon: H = -sum(pi*log(pi)) for each reg_species
# values for Shannon have to be in 0-1 range (not 0-100)!
df_reg_dens_shannon <- 
  df_reg_dens %>% 
  group_by(trip_n, dom_sp, manag) %>% 
  mutate(dens_tot = sum(density_ha),
         sp_pi = density_ha /dens_tot,
         shannon_part = sp_pi*log(sp_pi)) %>% 
  summarize(shannon = -sum(shannon_part)) %>% 
  mutate(effective_n_sp = exp(shannon))# %>% 

df_reg_dens_shannon %>% 
  ggplot(aes(y = effective_n_sp,
             x = manag)) +
  geom_boxplot() + 
  facet_grid(~dom_sp, scales = 'free') #+
#ylab('density \n(#trees/ha)') 



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







# Get community weighted means ---------------------------------------------
# https://rpubs.com/CPEL/cwm



### get density/ha -----------------------------------
### Join the number of subsamples to regen data:
# Calculate the density from the average counts, or from the sum of the trees/sampled area???
# check both approaches and see the difference?
# for density estimation, ignote the height classes
# keep the grain at the subsite lavel - to keep the variability between sites (!) 
# Get the counts per 4 m2







# Link data: ground cover (deadwood) and density? -------------------------------------------------







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


