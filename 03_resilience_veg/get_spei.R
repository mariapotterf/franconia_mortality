# Calculate SPEI data

# at monthly resolution
# for each trap

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
library(zoo)

xy        <- vect('C:/Users/ge45lep/Documents/2021_Franconia_mortality/03_plot_sampling/sites_identification/final/share/sites_final.shp') # read trap location
# Convert data to DWD coordinate system:
xy2 <- terra::project(xy, "EPSG:31467")  # coordinate system from the DWD data: Germany


# Get precip and temp data:
df_prec   <- fread(paste(myPath, outTable, 'xy_precip.csv', sep = '/'))
df_temp   <- fread(paste(myPath, outTable, 'xy_temp.csv', sep = '/'))

head(df_prec)
head(df_temp)

# rename column names to join the datasets:
df_prec <- df_prec %>% 
  rename(PRCP = vals)

df_temp <- df_temp %>% 
  rename(TMED = vals) %>% 
  mutate(TMED = TMED/10)  # as in the description

# join data
df <- df_prec %>% 
  full_join(df_temp, by = c("Name", "month", "year"))


# data check
par(mfrow=c(1,1)) 
plot(df$PRCP, df$TMED)

plot(df$month, df$TMED, pch=".")
plot(df$month, df$PRCP, pch=".")
plot(df$year, df$TMED, pch=".")
abline(lm(df$TMED~df$year), col=2) #  



# Calculate SPEI: -----------------------------------------------------------
# Given a time series of the climatic water balance (precipitation minus potential evapotranspiration), 
# gives a time series of the Standardized Precipitation-Evapotranspiration Index (SPEI).
# SPEI - input: 


# Variables needed:
# YEAR monthly precipitation totals, in mm. 
# MONTH monthlyprecipitation totals, in mm. 
# PRCP monthly precipitation totals, in mm. 
# TMED monthly mean temperature, in ºC. 
# other data are missing: use Thornthwaite transformation for middle Bavaria

# Compute potential evapotranspiration (PET) and climatic water balance (BAL) 
df$PET <- thornthwaite(df$TMED, lat = 48.777500 ) # 48.777500 # Ingolstadt
df$BAL <- df$PRCP-df$PET 

# convert df to time series
df.ts <- df %>% 
  ts(df, start = c(2000, 01), end=c(2021,12), frequency=12) 

spei1 <- spei(df[,'BAL'], scale = 1) # calculate SPEI for current month


# Convert to a ts (time series) object for convenience
# convert to time series data ----------------------------------------

# calculate SPEI for each XY location:
df_ls <- df %>%
  group_split(Name)


# Calculate the SPEI for each location:
get_SPEI <- function(df, ...){
  
  # get XY name
  id = unique(df$globalid)
  
  # convert df to time series
  df.ts <- df %>% 
    ts(df, start = c(2000, 01), end=c(2021,12), frequency=12) 
  
  # Calculate spei or different time intervals:
  my_scales = c(1,3,6,12)
  spei_ls <- lapply(my_scales, function(s) {
    
    # extract just values from SPEI object:
    dd = spei(df.ts[,'BAL'], scale = s)$fitted
    
    # covert to dataframe, convert date to format
    df.out <- data.frame(spei=as.matrix(dd), 
                         date=zoo::as.Date(time(dd)))
    
    # add scale indication
    df.out <-df.out %>% 
      mutate(scale = rep(s, nrow(df.out)))
    return(df.out)
  })
  # merge scaes tables
  out_scales = do.call('rbind', spei_ls)
  
  # add location indication
  out_scales <-out_scales %>% 
    mutate(Name = rep(id, nrow(out_scales)))
  
  return(out_scales)
  
}

# apply over the list of df (locations):

df_ls2<- lapply(df_ls, get_SPEI)

# merge into one file:
out.df <- do.call('rbind', df_ls2)

# export file:
fwrite(out.df, paste(myPath, outTable, 'xy_spei.csv', sep = "/"))
