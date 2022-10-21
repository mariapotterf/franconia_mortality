
# Common variables:

triplets_exclude = c(45, # no permission, no sampled
                     65, # no sampled: no owner permission
                     47 # oak, recently harvested
                       # spruce - harvested dead Passau
                    )


# Get euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))


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
# slope_corr <- function(gradient, ...) {
#   
#   # get the dimension of the corrected plane sampling plot
#   r1 = 2
#   r2 = r1*cos(gradient*pi/180) 
#   
#   # calculate the expansion factor
#   correct_factor = ha/r1*r2
#   
#   # correct the number of trees/ha
#   dens_corr = trees_field*correct_factor
#   return(dens_corr)
#   
# }



