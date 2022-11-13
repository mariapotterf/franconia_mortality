# Get climate data

# Get SPEI data

# at daily resolution
# for each study site

# SPEI data monitor the drought worldwide:

# Calculation of the Standardised Precipitation-Evapotranspiration Index
# Global SPEI database


# The Global SPEI database, SPEIbase, offers long-time, 
# robust information about drought conditions at the global scale, 
# with a 0.5 degrees spatial resolution and a monthly time resolution. 
# It has a multi-scale character, providing SPEI time-scales between 1 and 48 months. 
# Currently it covers the period between January 1901 and December 2020.

# Beguería S. (2017) SPEIbase: R code used in generating the SPEI global database, doi:10.5281/zenodo.834462.


rm(list = ls())


# Read my paths -----------------------------------------------------------
source('myPaths.R')


# Read libs  --------------------------------------------------------------

library(SPEI)
library(sf)
library(dplyr)
library(data.table)
library(tidyr)
library(rgdal)
library(raster)
library(tidyverse)
library(lubridate)
library(patchwork)
library(fasterize)
library(ggpubr)
library(terra)
library(R.utils)

# Get spatial data for each site:
# maybe first group them by the site number, and calculate centroid?
library(sf)

#xy  <- st_read('C:/Users/ge45lep/Documents/2021_Franconia_mortality/03_plot_sampling/sites_identification/final/share/sites_final.shp')

xy        <- vect('C:/Users/ge45lep/Documents/2021_Franconia_mortality/03_plot_sampling/sites_identification/final/share/sites_final.shp') # read trap location
# Convert data to DWD coordinate system:
xy2 <- terra::project(xy, "EPSG:31467")  # coordinate system from the DWD data: Germany


# filter through years: >1970 ------------------------
# 

myPath = 'C:/Users/ge45lep/Documents/2022_BarkBeetles_Bavaria'

# List files: from 2000 onwards:
i = 'temp'
file_ls <- list.files(paste(myPath, 'rawData/DeutschWetter', i, sep = "/"),
                      #pattern = "^20.*\\.gz$",
                      pattern = "*\\.gz$",
                      recursive=TRUE)

s <- c("jan/193501asc.gz", "feb/188209asc.gz", "mar/197501asc.gz", "apr/202107asc.gz")

f_1970 <- function(x, y = 1970) {
  first4 <- substr(x, 8, 11)
  print(first4)
  #year <- as.numeric(first4)
  # year >= y
}

file_ls[f_1970(file_ls)]


# Get vector of folders by climate variable
vars <- c('temp', 'precip')


for (i in vars){
  print(i)
  
  # List files: from 2000 onwards:
  file_ls <- list.files(paste(myPath, "rawData/DeutschWetter", i, sep = "/"),
                        #pattern = "^20.*\\.gz$",
                        pattern = "^19.*\\.gz$",
                        recursive=TRUE)
  
  # read in rasters 
  ras_ls <- lapply(file_ls, function(file, ...) {
    # set raster file
    #file = '11_Nov'
    ras_path = paste(myPath, 'rawData/DeutschWetter',  i, file , sep = "/")
    
    #print(file)
    # unzip file
    R.utils::gunzip(ras_path, remove = FALSE, overwrite = T)
    
    # read file
    ff <- gsub(".gz$", "", ras_path)
    # read raster
    z <- rast(ff)
    # set the reference system:
    crs(z) <- "EPSG:31467"
    return(z)
    
  })
  
  #plot(ras_ls[[50]])
  
  # extract raster values to a vector:
  ext_ls <- lapply(ras_ls, function(ras) {
    df <- terra::extract(ras, xy2)
    return(df)
  })
  
  
  # merge data by columns:
  df <- do.call(cbind, ext_ls)
  
  # add the globalID XY data 
  out.df <- cbind(df, Name = xy$Name )
  # names(out.df)
  
  # remove IDs: every raster column now has each ID column: duplicated
  out.df <- out.df %>% 
    dplyr::select(-c(ID))
  
  
  # organize the data in time: 
  # codng for the names XXXXYY - XXXX - year, YY - month
  # data represent the monthly mean values at the grid of 1km2
  # 
  # Mean of the monthly averaged mean daily air temperature in 2 m height above ground, 
  # given in 1/10 °C.
  names(out.df) <- gsub('asc', '', names(out.df))
  # names(out.df)
  
  
  # Convert to long format
  long.df <- 
    out.df %>% 
    pivot_longer(!Name, names_to = "time", values_to = 'vals') %>% 
    # split year and months
    mutate(month = as.numeric(str_sub(time, -2, -1)),  # extract last two characters (month)
           year = as.numeric(str_sub(time, 1,4))) %>%      # extract first 4 characters (year)  
    dplyr::select(-c(time))
  
  
  # Export file
  outName = paste0('xy_', i, '.csv')
  #print(paste(myPath, outTable, outName, sep = '/'))
  fwrite(long.df, paste("C:/Users/ge45lep/Documents/2021_Franconia_mortality/outTables", outName, sep = '/'))
  
}


