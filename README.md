# Franconia_mortality

Hotspots of tree mortality in Franconia 2018-2019

- identify input data: disturbances absolute and disturbance anomalies (??) from Cornelius
- get forest cover Europe
- Bavaria shp

Process:

- create hexagonal grid over Bavaria; overlap with disturbances years
- keep only years 2018-2019, 2020
- convert to polygons
- intersect with individual grids and forest cover to understand how much forest have been disturbed by years
- identify the > %% quantile as hotspots
- get some basic characteristics of disturbances: shape, area, perimeter, complexity??


Goal: 

- identify mortality hotspots to lead field sampling in summer 2022
- generate random points for field sampling: stratified random sampling for disturbed, undisturbed forestrs
- disturbance forests contains salvaged/unsalvaged sites: can I see them from aerial pcs for example??
