
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
# "Area_m2"            - patch size (m2)
# "RA1"                - dominant species by rIVI         
# "ref_rIVI_mean"      - avg REF IVI by dominant species
# "RA2"                - richness
# "ref_avg_rich"       - avg REF richness
# "RA3"                - shade tolerance
# "ref_mean_shade"     - avg REF shade tolerance
# "RS1"                - density
# "ref_mean_dens"      - avg REF stem density
# "RS2"                - horizontal distance
# "ref_mean_distance"  - avg REF horizontal distance
# "RS3"                - number of vertical layers 
# "ref_mean_vLayer"    - avg REF number of vertical layers  
# "RA_mean"            
# "RS_mean"

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

# plot some data:
# melt the columns first:
#library(reshape2)
plot(df$euclid_dist, df$ref_mean_shade)
plot(df$euclid_dist, df$ref_rIVI_mean)
plot(df$euclid_dist, df$ref_avg_rich )
plot(df$euclid_dist, df$ref_mean_distance )
plot(df$euclid_dist, df$ref_mean_vLayer )


# df: pivot to long format:
melt(df, id.vars = c("trip_n", "manag", "euclid_dist"))




# test scatter plot over rotating axis:

A = structure(list(Gene1 = c(0.02, 0, 0, 0.06, 0, 0, 0, 0, 0), 
                   Gene2 = c(0.038798682,
                             0.059227225, 0.052116384, 0.199264618, 0.123521916, 0.128767634, 
                             0.080097356, 0.017421323, 2.10281137), 
                   Gene3 = c(0.1423662, 0.208765213,
                             0.230437735, 0.261100548, 0.273330986, 0.263491811, 0.234511372, 
                             0.247775683, 0.401582013), 
                   Gene4 = c(2.778587067, 0.818810739,
                             2.535040249, 2.516963635, 2.751309388, 2.882878373, 3.568192768,
                             5.109428797, 8.202902242), 
                   Gene5 = c(0.471403939, 0.353671882, 
                             0.504061015, 0.63659138, 0.623572499, 0.359322715, 0.386217698, 
                             0.068760572, 0.140596724), 
                   Gene6 = c(18.93687655, 1.379027685, 
                              9.773089223, 11.01441624, 34.0563519, 13.02402045, 9.068928569, 
                             15.7490551, 60.25989178)), 
              class = "data.frame", 
              row.names = c("Sample1",
                            "Sample2", "Sample3", 
                            "Sample4", "Sample5", 
                            "Sample6", "Sample7", 
                            "Sample8", "Sample9")) 



for (gene in paste0("Gene",1:4)){
  a= A %>% 
    gather(key = variable, value = values, Gene5:Gene6) %>% 
    ggplot(aes(get(gene), values)) + 
    geom_point() + 
    facet_grid(. ~ variable, scales = "free_x") + 
    geom_smooth(method = "lm", se = FALSE) + 
    scale_y_continuous(trans = "log2", labels = NULL, breaks = NULL) + 
    scale_x_continuous(trans = "log2", labels = NULL, breaks = NULL)
  
  print(a)
  
}



# Loop over all predictors:

cols <- colnames(df)
cols <- cols[!cols %in% c("euclid_dist")]

for(i in cols){
  name <- paste(i, ".pdf", sep = "")
  id <- which(colnames(df) == i)
  # add a new column - this is the one accepting the "rotating" gene input
  df$Gene <- df[,id]
  
  p <- df %>% 
    select(Gene, euclid_dist) %>%
    gather(variable, values, Gene5:Gene6) %>% 
    ggplot(aes(Gene, values)) + 
    geom_point() + 
    facet_grid(. ~ variable, scales = "free_x") + 
    geom_smooth(method = "lm", se = FALSE) + 
    scale_y_continuous(trans = "log2", labels = NULL, breaks = NULL) + 
    scale_x_continuous(i, trans = "log2", labels = NULL, breaks = NULL)
  
  pdf(name)
  print(p)
  
  dev.off()
}








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
