
# ------------------------------------------------
# Extract geoinformation from photos in R
# -------------------------------------------------

# https://www.r-bloggers.com/2016/11/extracting-exif-data-from-photos-using-r/

# list photos
# get coordinates
# extract as point in a gdb

#library(exiftoolr)
library(exifr)            # get exif info from photos
library(dplyr)            # data managemnet
library(leaflet)          # make maps
library(sf)
library(tidyr)

#path = 'C:/Users/ge45lep/Documents/2021_Franconia_mortality/photo'
path = 'C:/Users/ge45lep/Downloads/iCloud Photos/iCloud Photos'

setwd(path)


# Get files 
files <- list.files(pattern = "*.JPEG") # 
dat <- read_exif(files)

# test if it works:
# Yes, need to change the 'exifr' to 'read_exif'
#image_files <- list.files(system.file("images", package = "exifr"), full.names = TRUE)

dat2 <- dat %>%
  #filter NA values
  filter(!is.na(GPSLongitude)) %>% 
  filter(!is.na(GPSLatitude)) %>% 
  dplyr::select(SourceFile, DateTimeOriginal,
               GPSLongitude, GPSLatitude)


# Which time and date to take?
# FileCreateDate = when copied
# DateTimeOriginal - seems like creation date and time 

class(dat2)

# Check the data
dat2[203:260,]

# Convert df to spatial data frame
out_shp <- st_as_sf(dat2, coords = c("GPSLongitude", "GPSLatitude"))

# Export shapefile
st_write(out_shp, "photo_loc.shp", append=FALSE)


# Write output file as csv
write.csv(dat2, 'Exifdata.csv',
          row.names = F)


# Quickj overview of data
windows()
plot(dat$GPSLongitude, dat$GPSLatitude)

# Make an interactive map
leaflet(dat2) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addMarkers(~ GPSLongitude, ~ GPSLatitude)  
