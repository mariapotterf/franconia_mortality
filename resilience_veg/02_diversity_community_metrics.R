# Analyse the data:

# get data about community structure
# ecological traits
# shanon diversity
# test different categories and community resemblance?
# remove all previous data from R memory
# Convert the regeneration counst into density/ha - takes into account the difference in sampling plot!
#  need to do the slope correction?
# http://wiki.awf.forst.uni-goettingen.de/wiki/index.php/Slope_correction
# our inclinometer Suunto is in degrees: goes 0-90
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

##### Stats
library(MuMIn)
library(vegan)
library(mgcv)
library(gratia) # visualization of mcv


## Colors
cols = c('#0072B2', # strict reserves
         '#E69F00', # buffer 500
         '#F0E442', # buffer 2000
         '#000000') # control

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
# then need to recalculate the correction factor: at plane, the factor is 2500 (4m2 to 10000m2); 
#                                                 at slope it varies

# Example of slope correction calculation of tree density:
ha = 10000
trees_field = 10
gradient = 16.7  #(has to be in degrees!)

# area of the subsite: 2x2 m
r = 2    # m
r1 = r
r2 = r1*cos(gradient*pi/180)   # R works in radians: to get the value in degrees, it has to be in form cos(angle * pi/180) 
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
  mutate(trip_n = as.character(trip_n),
         dom_sp = factor(dom_sp),
         manag = factor(manag)) %>% 
  mutate(manag = case_when(manag == 'c' ~ 'cleared',
                           manag == 'l' ~ 'living',
                           manag == 'd' ~ 'dead'#,
                           )) %>%   #Species == "Fagus sylvatica"       ~ "Beech" ,))
  rename(n_subsites = n) %>% 
  as.data.frame()



# The correction of the counts: need to do it on the level of individual subsites:
# as there is high variation in slopes ('gradient') between the subsites
# So I am correcting the tree density/ha per each subsite: 
# maybe then use teh mean/sum densities per category??? 
df_regen <- df_regen %>% 
  mutate(manag = factor(manag, levels = c('l', 'c', 'd'),
                        labels = c('living','cleared', 'dead'))) %>% 
  mutate(dom_sp = factor(dom_sp))



# Get tree density/ha across all heights, correct the density by slope per each subsite
# but it is calculated for each species as a value per hectar. 
# need to account for the different number of subsites: 5-15
# calculate the sums and then divide by number of subsamples
df_reg_dens <- 
  df_regen %>% 
  dplyr::left_join(subsample_n, by = c('trip_n', 'manag', 'dom_sp')) %>% #, 'dom_sp', 'manag' 
  ungroup() %>% 
  group_by(trip_n, dom_sp, manag, n_subsites, height_class, reg_species)  %>% 
    mutate(length_corr = 2*cos(gradient*pi/180),
           area_corr   = 2*length_corr,
           correct_factor = ha/area_corr,
           corr_density = n_total*correct_factor)  %>% 
    filter(corr_density != 0) %>% 
  ungroup(.) #%>% 

# Get summary statistics: Triplets -------------------------------------------
dat %>% 
  group_by(dom_sp) %>% 
  distinct(trip_n) %>% 
  tally()


# Reg.density: summary statictics: --------------------------
# how much regeneration is per each site? now it is splitted among several species on site

df_reg_dens %>% 
  group_by(manag, dom_sp) %>% 
  summarize(mean_dens = mean(corr_density, na.rm = T),
            sd_dens   = sd(corr_density, na.rm = T)) %>% 
  mutate(dens_mean_sd = stringr::str_glue("{round(mean_dens,1)}±{round(sd_dens,1)}")) %>% 
  dplyr::select(manag, dom_sp, dens_mean_sd) %>% 
  pivot_wider(names_from = manag, 
              values_from = dens_mean_sd )

# Reg.density: plot ---------------------------------

df_reg_dens %>%
  ggplot(aes(x = dom_sp, 
             y = corr_density/10000)) +
  theme_bw() +
  stat_summary(geom = "errorbar", 
               width = 0.3,
               aes(col = dom_sp)) +
  stat_summary(geom = "pointrange", 
               aes(col = dom_sp)) +
  scale_colour_manual(values=cols, 
                      name="Dominant\nspecies") + 
  facet_grid(~manag) +
  labs(y='Regeneration density*10000')



# Get Shannon: ------------------------------------------------------------
# - get the total density
# - calculate the share per reg_species: pi
# - calculate Shannon: H = -sum(pi*log(pi)) for each reg_species
# - values for Shannon have to be in 0-1 range (not 0-100)!

df_reg_dens_shannon <-
  df_reg_dens %>%
  group_by(trip_n, dom_sp, manag, reg_species) %>%
  summarize(mean_dens = mean(corr_density)) %>% # means first I have values for individual species and subsites!
  mutate(
    dens_tot = sum(mean_dens),
    sp_pi = mean_dens / dens_tot,
    shannon_part = sp_pi * log(sp_pi)
  ) %>%
  summarize(shannon = -sum(shannon_part)) %>%
  mutate(effective_n_sp = exp(shannon))# %>%


# summary table shannon 
df_reg_dens_shannon %>%
  group_by(manag, dom_sp) %>%
  summarise(
    count = n(),
    mean = mean(effective_n_sp, 
                na.rm = TRUE),
    sd = sd(effective_n_sp, 
            na.rm = TRUE)
  )



# get plots Shannon Effective number of species -----------------------------------
df_reg_dens_shannon %>% 
  ggplot(aes(y = effective_n_sp,
             x = dom_sp)) +
#  geom_boxplot() + 
  theme_bw() +
  stat_summary(geom = "errorbar",
               width=0.5,
               aes(col = dom_sp)) +
  stat_summary(geom = "pointrange", 
               aes(col = dom_sp)) +
  scale_colour_manual(values=cols, 
                      name="Dominant\nspecies") + 
  
  facet_grid(~manag, scales = 'free') 
#+
#ylab('density \n(#trees/ha)') 
# https://rpubs.com/CPEL/cwm






# Community weighted means ---------------------------------------------

df_traits_cwm <- 
  df_reg_dens %>%
  group_by(trip_n, dom_sp, manag, reg_species) %>%
  summarize(mean_dens = mean(corr_density)) %>%
  left_join(trait_df) %>% 
  ungroup(.) %>% 
  group_by(trip_n, dom_sp, manag) %>% 
  summarize(shade_cwm   = weighted.mean(Shade_tolerance,   mean_dens, na.rm = TRUE  ),
            drought_cwm = weighted.mean(Drought_tolerance, mean_dens, na.rm = TRUE  )) %>% 
  mutate(manag = factor(manag, 
                        levels = c('living','cleared', 'dead')))

# Make some plots:
# set desired dodge width
pd <- position_dodge(width = 0.4)

p_shade <- 
  df_traits_cwm %>%
  ggplot(aes(x = manag,
             y = shade_cwm))  +
  theme_bw() +
  stat_summary(#geom = "mean", 
    geom = "line", 
    size=0.5, 
    aes(colour = dom_sp, 
        group = dom_sp,
        lty = dom_sp),
    position = pd) +
  stat_summary(geom = "errorbar", 
               width = 0.3,
               aes(col = dom_sp), 
               position = pd) +
  stat_summary(geom = "pointrange", 
               aes(col = dom_sp),
               position = pd) +
  scale_colour_manual(values=cols, 
                      name="Dominant\nspecies") + 
  scale_shape_manual(values=cols, 
                     name="Dominant\nspecies") + 
  #facet_grid(~manag) +
  labs(y='Shade tolerance')

# 

p_drought <- 
  df_traits_cwm %>%
  ggplot(aes(x = manag,
             y = drought_cwm))  +
  theme_bw() +
  stat_summary(#geom = "mean", 
               geom = "line", 
               size=0.5, 
               aes(colour = dom_sp, 
                   group = dom_sp,
                   lty = dom_sp),
               position = pd) +
  stat_summary(geom = "errorbar", 
               width = 0.3,
               aes(col = dom_sp), 
               position = pd) +
  stat_summary(geom = "pointrange", 
               aes(col = dom_sp),
               position = pd) +
  scale_colour_manual(values=cols, 
                      name="Dominant\nspecies") + 
  scale_shape_manual(values=cols, 
                      name="Dominant\nspecies") + 
  labs(y='Drought tolerance')




ggarrange(p_shade,p_drought, ncol = 2, nrow = 1 , 
          common.legend = TRUE  )




# Stats: ANOVA: Shannon effective numbers ------------------------------------------

# two way anova, as I have two factors: 
#  - management
#  - dominant tree species

# check frequecy tables:
table(df_reg_dens_shannon$manag, 
      df_reg_dens_shannon$dom_sp)

# Visualize the data:
# boxplot
# two-way interaction plot - can show interactions
ggboxplot(df_reg_dens_shannon, 
          x = "manag", 
          y = "effective_n_sp", 
          color = "dom_sp")

ggline(df_reg_dens_shannon, 
       x = "manag", 
       y = "effective_n_sp", 
       color = "dom_sp",
       add = c("mean_se", 
               "dotplot")#,
       #binwidth = 1/50
       )


## two way anova Effective numbers -----------------------------------------------
# http://www.sthda.com/english/wiki/two-way-anova-test-in-r#what-is-two-way-anova-test
aov1 <- aov(effective_n_sp ~ manag +dom_sp, 
                df_reg_dens_shannon)
summary(aov1)

# on effective number of species,
# there is higher effect of teh dominant species
# then of teh management

# can teh effect be synergic? the use * instead of +
aov2 <- aov(effective_n_sp ~ manag*dom_sp, 
            df_reg_dens_shannon)

# the same as (here it is more explicit)
aov2 <- aov(effective_n_sp ~ manag+dom_sp +
            manag:dom_sp,
            df_reg_dens_shannon)
summary(aov2)
plot(aov2)

AICc(aov1, aov2)





# ANOVA: which groups are different???
# need to do the pairwise comparison
TukeyHSD(aov1)  # if teh interaction is not significant, we
# should use only the additive anova (here aov1)

res.tuk <- TukeyHSD(aov1, which = 'dom_sp') # select only significant factor

# p-value is significant for each combination of spruce-otherSp

plot(res.tuk, las = 1)

# test validity of ANOVA use: ---------------------
# anova assumptions: 
#  - data are normally distributed
#  - variance across groups are homogenous

# 1. Homogeneity of variances
plot(aov1, 1) # numbers indicate outliers: normality is heavily affected by them!

# Use a test of homogeneity
library(car)
leveneTest(effective_n_sp ~ manag*dom_sp, 
           data = df_reg_dens_shannon)

# p-value is >0.5 - we assume the variance across groups is similar


# 2. Cechk for normality assumtions
plot(aov1, 2)

# Run Shapiro-Wilcox test on teh ANOVA residuals
# to check if the normality is violated:
# Extract the residuals
aov_residuals <- residuals(object = aov1)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

# p>0.05 - seems that they are normal


# ANOVA for unbalanced design --------------------------
# there are three types to aaacount for unbalanced design: 
#   when the number of observation is not the same
#   recommended: Type-III sums of squares
# use car::anova()
library(car)
aov2 <- aov(effective_n_sp ~ manag*dom_sp, 
            df_reg_dens_shannon)

car::Anova(aov2, type = "III")
car::Anova(aov2, type = "II")  # in this case, they have teh same results

# Use kruskall wallis -------------------------------
# http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

# kruskall-wallis just test several groups, but having only one factor
# so I can test for each factor individually (does not allow interaction)
# or use the Ordinal logistic regression: can handle factorial structure
# 
# Test GAMS for tree reg densities: --------------------------------------------------
# data: highly skewed
df_reg_dens

str(df_reg_dens)

# claim factors:
df_reg_dens <- df_reg_dens %>%
  mutate(
    dom_sp = factor(dom_sp),
    manag = factor(manag),
    trip_n = factor(trip_n)
  )

# data are not normally distributes and have many outliers

# standardize teh data by z-score: ---------------------------------
# Finding Mean
# standardization/normalization will not change the data distribution!!!



# For categorical data, the smotng is not meanings full: -------------------
# stick with glm ------------------------------
# categorical predictors variables: extend of ANOVA
# # https://anamtk.github.io/GLM_tutorials/tidyTuesday_fall2020/GLM_inR_Tutorial.html#extensions

library(MASS)
library(glmmTMB)
library(DHARMa)
library(effects)

m.sp <- glm(
  corr_density ~ dom_sp,# +   # claim as categorical data; generates random intercept for each level of factor ,
  # claim as random effect
  data = df_reg_dens,
  family = Gamma  # continuous positive data
)

m.sp.manag <- glm(
  corr_density ~ dom_sp + manag,# +   # claim as categorical data; generates random intercept for each level of factor ,
  # claim as random effect
  data = df_reg_dens,
  family = Gamma  # continuous positive data
)

m3 <- glm(
  corr_density ~ dom_sp + manag + trip_n,# +   # claim as categorical data; generates random intercept for each level of factor ,
  # claim as random effect
  data = df_reg_dens,
  family = Gamma  # continuous positive data
)

m.manag <- glm(
  corr_density ~ manag,# +   # claim as categorical data; generates random intercept for each level of factor ,
  # claim as random effect
  data = df_reg_dens,
  family = Gamma  # continuous positive data
)

m.sp.manag2 <- glm(
  corr_density ~ dom_sp*manag,# +   # claim as categorical data; generates random intercept for each level of factor ,
  # claim as random effect
  data = df_reg_dens,
  family = Gamma  # continuous positive data
)



summary(m.manag)
summary(m.sp)

AICc(m.sp, m.manag, m.sp.manag, m.sp.manag2, m3)


simulationOutput <- simulateResiduals(fittedModel = m3, plot = T)
testDispersion(simulationOutput)

plot(m3)


windows()
plot(effects::allEffects(m.sp))
windows()
plot(effects::allEffects(m.manag))
plot(effects::allEffects(m.sp.manag))
plot(effects::allEffects(m.sp.manag2))






# Reorganization: composition ---------------------------------------------
# classify if the dominant species prevails in 
# regeneration? how often they fit the dominant species?
head(df_reg_dens )

# Calculate % of  regeneration species from total density
df_dens_species <- 
  df_reg_dens %>% 
  group_by(trip_n, dom_sp, manag, reg_species) %>% 
  summarize(ds_species = sum(corr_density, na.rm = T)/n_subsites) %>% # divide by number of subsites!!
    distinct() 

df_dens_sum <- df_dens_species %>%  # keep only unique rows
  group_by(trip_n, dom_sp, manag) %>% 
  summarize(ds_sum = sum(ds_species, na.rm = T))

# Merge to original table to calaulctate shares by reg_species:
df_density_change <- df_dens_species %>% 
  left_join(df_dens_sum) %>% 
  mutate(ds_spec_share = ds_species  /ds_sum*100) %>% 
 # filter(reg_species %in% c("Spruce", 'Beech', 'Pine', 'Oak' )) %>% 
  mutate(cl_change = case_when(dom_sp == 'spruce' & reg_species == 'Spruce' & ds_spec_share > 50 ~ 'resilience',
                               dom_sp == 'spruce' & reg_species == 'Spruce' & ds_spec_share <= 50 & ds_spec_share > 25  ~ 'decrease',
                               dom_sp == 'spruce' & reg_species == 'Spruce' & ds_spec_share <= 25 & ds_spec_share > 0  ~ 'reduction',
                               dom_sp == 'beech'  & reg_species == 'Beech'  & ds_spec_share > 50 ~ 'resilience',
                               dom_sp == 'beech'  & reg_species == 'Beech'  & ds_spec_share <= 50 & ds_spec_share > 25  ~ 'decrease',
                               dom_sp == 'beech'  & reg_species == 'Beech'  & ds_spec_share <= 25 & ds_spec_share > 0  ~ 'reduction',
                               dom_sp == 'pine' & reg_species == 'Pine' & ds_spec_share > 50 ~ 'resilience',
                               dom_sp == 'pine' & reg_species == 'Pine' & ds_spec_share <= 50 & ds_spec_share > 25  ~ 'decrease',
                               dom_sp == 'pine' & reg_species == 'Pine' & ds_spec_share <= 25 & ds_spec_share > 0  ~ 'reduction',
                               dom_sp == 'oak'  & reg_species == 'Oak'  & ds_spec_share > 50 ~ 'resilience',
                               dom_sp == 'oak'  & reg_species == 'Oak'  & ds_spec_share <= 50 & ds_spec_share > 25  ~ 'decrease',
                               dom_sp == 'aok'  & reg_species == 'Oak'  & ds_spec_share <= 25 & ds_spec_share > 0  ~ 'reduction',
                               TRUE ~ 'species_change')) %>%
  ungroup(.) #%>% 
  

# filter data to have only one change group per site & management:
df_dens_flow <- df_density_change %>% 
  group_by(trip_n, dom_sp, manag) %>% 
  top_n(1, ds_spec_share) %>%  # select the highest share per species and category
  ungroup(.) %>% 
  dplyr::select(dom_sp, manag, cl_change) %>%
  group_by(dom_sp, manag, cl_change) %>% 
  count()
  
#count()


# alternative alluvial diagram: scale_viridis and 3 axis:
# create alternative alluvial diagram
library(ggplot2)
library(ggalluvial)
ggplot(df_dens_flow,
       aes(axis1 = dom_sp ,
           axis2 = manag,
           axis3 = cl_change,
           y = n)) +
  geom_alluvium(aes(fill = dom_sp)) +
  geom_stratum() +
  geom_text(stat = "stratum", 
            #label.strata = TRUE
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("dom_sp", "manag", "cl_change"),
                   expand = c(.1, .1)) +
  scale_fill_viridis_d() +
 # labs(title = "Titanic data",
 #      subtitle = "stratified by class, sex, and survival",
 #      y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 










# Make alluvial/Sanky plot ------------------------------------------------

# illustrate the flow between categorical categories
# https://rkabacoff.github.io/datavis/Other.html#

# input data
library(readr)
titanic <- read_csv("titanic.csv")

# summarize data
library(dplyr)
titanic_table <- titanic %>%
  group_by(Class, Sex, Survived) %>%
  count()

titanic_table$Survived <- factor(titanic_table$Survived, 
                                 levels = c("Yes", "No"))

head(titanic_table)





# create alluvial diagram
library(ggplot2)
library(ggalluvial)



ggplot(titanic_table,
       aes(axis1 = Class,
           axis2 = Survived,
           y = n)) +
  geom_alluvium(aes(fill = Sex)) +
  geom_stratum() +
  geom_text(stat = "stratum", 
            label.strata = TRUE) +
  scale_x_discrete(limits = c("Class", "Survived"),
                   expand = c(.1, .1)) +
  labs(title = "Titanic data",
       subtitle = "stratified by class, sex, and survival",
       y = "Frequency") +
  theme_minimal()



# alternative alluvial diagram: scale_viridis and 3 axis:
# create alternative alluvial diagram
library(ggplot2)
library(ggalluvial)
ggplot(titanic_table,
       aes(axis1 = Class,
           axis2 = Sex,
           axis3 = Survived,
           y = n)) +
  geom_alluvium(aes(fill = Class)) +
  geom_stratum() +
  geom_text(stat = "stratum", 
            label.strata = TRUE) +
  scale_x_discrete(limits = c("Class", "Sex", "Survived"),
                   expand = c(.1, .1)) +
  scale_fill_viridis_d() +
  labs(title = "Titanic data",
       subtitle = "stratified by class, sex, and survival",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 











# test for z score!
# generate random data from poisson distrib
set.seed(2)
a_norm <- rnorm(4000)
a_pois <- rpois(4000, lambda = 2)
hist(a_norm)
hist(a_pois)

a_scale <- scale(a_pois)



# conversion from z score
m<-mean(a_pois)

# Finding Standard Deviation
s<-sd(a_pois)
a_t <- (a_pois-m)/s


# test for normality
shapiro.test(a_norm)
shapiro.test(a_pois)
shapiro.test(a_scale)


windows()
hist(a_t)
hist(a_scale)



m<-mean(df_reg_dens$corr_density)

# Finding Standard Deviation
s<-sd(df_reg_dens$corr_density)


df_reg_dens <- df_reg_dens %>% 
  mutate(dens_z = (corr_density-m)/s)

ggplot(df_reg_dens) + 
  geom_density(aes(dens_z))

hist(log10(df_reg_dens$corr_density))
hist(df_reg_dens$corr_density)
plot(density(df_reg_dens$corr_density))
plot(density(df_reg_dens$dens_z))


# check frequecy tables:
table(df_reg_dens$manag, 
      df_reg_dens$dom_sp)

# Visualize the data:
# boxplot
# two-way interaction plot - can show interactions
ggboxplot(df_reg_dens, 
          x = "manag", 
          y = "corr_density", 
          color = "dom_sp")

ggline(df_reg_dens, 
       x = "manag", 
       y = "corr_density", 
       color = "dom_sp",
       add = c("mean_se", 
               "jitter")#,
       #binwidth = 1/50
)


# Run anova on data:--------------------------------------------------
# two way anova:
# http://www.sthda.com/english/wiki/two-way-anova-test-in-r#what-is-two-way-anova-test
aov1 <- aov(effective_n_sp ~ manag +dom_sp, 
            df_reg_dens_shannon)
summary(aov1)

# on effective number of species,
# there is higher effect of teh dominant species
# then of teh management

# can teh effect be synergic? the use * instead of +
aov2 <- aov(effective_n_sp ~ manag*dom_sp, 
            df_reg_dens_shannon)

# the same as (here it is more explicit)
aov2 <- aov(effective_n_sp ~ manag+dom_sp +
              manag:dom_sp,
            df_reg_dens_shannon)
summary(aov2)
plot(aov2)

AICc(aov1, aov2)


# get summary statitics:
model.tables(aov2, 
             type="means", se = TRUE)

# Get summary statistics:
df_reg_dens_shannon %>%
  group_by(manag, dom_sp) %>%
  summarise(
    count = n(),
    mean = mean(effective_n_sp, 
                na.rm = TRUE),
    sd = sd(effective_n_sp, 
            na.rm = TRUE)
  )


# ANOVA: which groups are different???
# need to do the pairwise comparison
TukeyHSD(aov1)  # if teh interaction is not significant, we
# should use only the additive anova (here aov1)

res.tuk <- TukeyHSD(aov1, which = 'dom_sp') # select only significant factor

# p-value is significant for each combination of spruce-otherSp

plot(res.tuk, las = 1)

# test validity of ANOVA use: ---------------------
# anova assumptions: 
#  - data are normally distributed
#  - variance across groups are homogenous

# 1. Homogeneity of variances
plot(aov1, 1) # numbers indicate outliers: normality is heavily affected by them!

# Use a test of homogeneity
library(car)
leveneTest(effective_n_sp ~ manag*dom_sp, 
           data = df_reg_dens_shannon)

# p-value is >0.5 - we assume the variance across groups is similar


# 2. Cechk for normality assumtions
plot(aov1, 2)

# Run Shapiro-Wilcox test on teh ANOVA residuals
# to check if the normality is violated:
# Extract the residuals
aov_residuals <- residuals(object = aov1)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

# p>0.05 - seems that they are normal






# Compare if teh groups are differet?
lm(drought_cwm~manag, dat = df_traits_cwm)

hist(df_traits_cwm$drought_cwm)

windows()
ggplot(df_traits_cwm, aes(drought_cwm, 
                          fill= manag)) +
  geom_density(alpha = 0.5) +
  facet_grid(~dom_sp)




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


#### Ground Shannon check plot: ---------------------------------------------------------------------
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


