
# Analyze the drivers of forest reorganization

# euclid distance ~ temp + precip + anomTemp + anomPrec + DW + gap_size

#### Read libraries  -----------------------------------------------------------
#library(readxl)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(stringr)  # use regex expressions
library(ggpubr)
#library(ggrepel)

# https://anamtk.github.io/GLM_tutorials/tidyTuesday_fall2020/GLM_inR_Tutorial.html#Packages

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
df_dom_sp                  # - indication of dominant species by site



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
df_dom_sp                  # - indication of dominant species by site

