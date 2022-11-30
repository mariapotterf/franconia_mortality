
# Analyze the drivers of forest reorganization

# euclid distance ~ temp + precip + anomTemp + anomPrec + DW + gap_size

#### Read libraries  -----------------------------------------------------------

# https://anamtk.github.io/GLM_tutorials/tidyTuesday_fall2020/GLM_inR_Tutorial.html#Packages

# Input data -------------------------------------------------------------------
source('my_vars_and_functions.R')
source('myPaths.R')


#### Read libraries  -----------------------------------------------------------
# Learn GAMs
library('here')
library('mgcv')
library('gratia')
library('gamair')
library('ggplot2')
library('purrr')
library('mvnfast')
library("tibble")
library('gganimate')
library('cowplot')
library('tidyr')
library("knitr")
library("viridis")
library('readr')
library('dplyr')
library('gganimate')

library('readr')
library('dplyr')




# Get data:
source('my_vars_and_functions.R')
source('myPaths.R')

# Input data -------------------------------------------------------------------

getwd()
#load(file = paste(getwd(), "outData/auxData.Rdata", sep = '/'))

#Read data
#C:/Users/ge45lep/Documents/2021_Franconia_mortality/outTables/
df <- read.csv(paste(myPath, outTable, 'df_gam.csv', sep = '/'))  

# Describe data:

# "trip_n"             - site number
# "manag"              - management: c = managed, d = unmanaged (standing dead)
# "euclid_dist"        - euclidean distance  (dependent var.)    
# "prop_DW_gc"         - deadwood % from ground_cover [0-100%]
# "mean_DW"            - avg deadwood from the ENV per site
# "prec_ref"           - avg precipitation 1986-2015 
# "prec_18_20"         - avg precipitation 2018-2020
# "anomaly_prec_18_20" - anomaly precipitation 
# "temp_ref"           - avg temp 1986-2015
# "temp_18_20"         - avg temp 2018-2020
# "anomaly_temp_18_20" - anomaly temp
# "dom_sp"             - dominant species: spruce, beech, oak, pine
# "Area_m2"            - patch size


# See data distribution:
ggplot(df, aes(euclid_dist,
               fill = manag)) +
  geom_density(alpha = 0.5)


# declare factors:
df$manag <- factor(df$manag)
df$dom_sp <- factor(df$dom_sp)


# add small constant to all distances, to get rid of teh 0 values 
# occur in 2 cases
#if euclidean distances == 0 to be able to use Gamma distribution
df2 <- df %>% 
  mutate(euclid_dist = euclid_dist + 0.0001)

# likely distribution: continuous positive data: gamma distribution

m.gam <- gam(euclid_dist ~ manag + dom_sp + s(Area_m2) +
               s(prop_DW_gc) + s(mean_DW) + s(prec_ref) +
               #s(trip_n) +
               s(temp_ref), 
             family = tw, #nb, #Gamma(link = "log"),
             data = df, method = 'REML')
summary(m.gam)
plot(m, pages = 1)


# test with added constant and gamma distribution:
m.gam2 <- gam(euclid_dist ~ 
                #manag + 
                dom_sp, #+ 
               # s(Area_m2) +
                #s(prop_DW_gc) + 
               # s(mean_DW) + 
               # s(prec_ref) +
               # s(anomaly_prec_18_20) +
               # s(anomaly_temp_18_20) +
              #trip_n +
              # s(temp_ref), 
            # family = Gamma(link = 'log'), #'log'"log", inverse, identity
             data = df2, 
             method = 'REML')
summary(m.gam2)
#plot(m.gam2, pages = 1)
#gam.check(m.gam2)
draw(m.gam2)





#model_p <- predict_gam(m.gam2)

model_p


p_anom_temp <- model_p %>% 
  ggplot(aes(y = fit  ,
             x = anomaly_temp_18_20)) +
  geom_smooth_ci(manag)



plot_smooth(m.gam2, 
            view="YEAR", 
            rm.ranef =TRUE, 
            transform=function(x) 100*inv.logit(x),  
            plot_all="DISTANCE")$fv


# try gamma in GLM:
# chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://pj.freefaculty.org/guides/stat/Regression-GLM/Gamma/GammaGLM-01.pdf

library(glmmTMB)

hist(df$euclid_dist)

library(MASS) #glm.nb for negative binomial models
library(glmmTMB) #Salamanders dataset and lots of families
library(lme4) #pseudo-R2 and and mixed model functionality
library(MuMIn) #dredge function for comparing models, AIC, marginal R^2 for mixed models
library(sjmisc) #pseudo R^2 - but I think gets loaded as a dependency
library(DHARMa) #model diagnostics
library(effects) #what do my marginal effects look like?
library(performance) #binomial model diagnostics
library(emmeans) #post hoc for categorical predictors


m <- glm(euclid_dist ~ anomaly_temp_18_20 + 
           anomaly_prec_18_20 + dom_sp + manag,
         data = df) # family=Gamma(link="log")
summary(m)
