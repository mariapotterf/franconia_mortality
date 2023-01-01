# Franconia_mortality

Process:

1. identify mortality hotspots based on Cornelius data
    - need for localization of the field work sites (triplets)
2. collect forest regeneration data
    - evaluate resilience of sites: along the two axes: 
        -> forest reassembly (change in composition)
        -> forest restructuring (change in structure)

-----------------------------------------------------------


### 1. Hotspots of tree mortality in Franconia 2018-2019

- identify input data: disturbances absolute and disturbance anomalies from Cornelius
- get forest cover Europe - for final map
- Bavaria shp - to overlay the raster data

Process:

- create hexagonal grid over Bavaria at different grid sizes to identify the changing spatal pattern
- overlap with disturbance map (Cornelius Senf), keep only years 2018-2019, 2020
- intersect with individual grids and forest cover to understand how much forest have been disturbed by years
- identify the > %% quantile as hotspots
- get some basic characteristics of disturbances: shape, area, perimeter, complexity?? -> not further used


Goal: 

- identify mortality hotspots to lead field sampling in summer 2022
- hexagons with >300% of disturbance anomaly over 2018-2020 will be used to select affected forest owners, and localize field work 


### 2. Identify resilience of Bavarian forests

- input data: vegetations data collected by Juri and Sophia, summer 2022
- on plot level, and capturing the plot surrounding: up to 15 m radius
- triplet = 3 sites:
  Cleared (C)   = Managed (clear cut trees died during 2018-2020 drought)
  Dead    (D)   = Unmanaged (left standing dead trees, died during 2018-2020 drought)
  Living  (L)   = Reference (living standing trees, unaffected by drought) 


Process:
- clean data from the raw datasets (from ArcGIS Survey123: 600 columns.. ) to extract the regeneration, advanced regeneration and mature trees for plot level, 
ENV data for mature trees and advanced regeneration in surroundings
- recalculate data on the plot level: but counts are on hectares! 
- apply slope correction on each of them

Analyze data:
- follow by the 'Quantifying reorganization.docx' protocol to identify the changes in vegetation patterns 



