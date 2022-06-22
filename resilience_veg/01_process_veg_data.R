

# Read vegetation data 
# collected during field work 2022
# by Juri and Sophia

# To do:

# Read the data
# ttranslate german names???
#   to know what is what

source('myPaths.R')

# Loading
library("readxl")

# xlsx files
dat <- read_excel(paste(myPath, inFolderFieldVeg, "Data Week 3.xlsx", sep = '/'))

names(dat)

# Translate:

# each record in the Table field has its own columns: ~ 600 columns
# Regeneration: tree regeneration is split across tree species, height classes and damage
# each tree species in height class: has 7 columns
# 6 heights levels: 6*7 = 42 columns per species
# 

# get organized by the columns names:
# to get some reasonable data: 


# How to read human readable format spread across columns?
 
