# Plots for Leipzig GfO conference

# read from Werners data - need to go back to my data,
# as therse data are already relative REF vs DIST, and organized in teh wide format



# get barplots with IQR for each indicator:

load('C:/Users/ge45lep/Documents/2021_Franconia_mortality/code_werner/out_reorg_full_v3/out_reorg_full.RData')

# show median and IQR for data (25 and 75th percentile)

library(ggplot2)
library(dplyr)

ggplot(data = out_reorg_full) + 
  stat_summary(
    mapping = aes(x = manag, 
                  y = RS1),
  fun.min = function(z) { quantile(z,0.25) },
  fun.max = function(z) { quantile(z,0.75) },
  fun = median)



ggplot(data = out_reorg_full,
       aes(x = manag, 
           y = RS1)) +
  geom_bar(#aes(x = manag, 
           #    y = median(RS1)),
               stat = "identity") +
  geom_pointrange(#mapping = ,
                  stat = "summary",
                  fun.min = function(z) {quantile(z,0.25)},
                  fun.max = function(z) {quantile(z,0.75)},
                  fun = median)





# Example: https://stackoverflow.com/questions/41077199/how-to-use-r-ggplot-stat-summary-to-plot-median-and-quartiles

ggplot(data = diamonds,
       aes(x = cut, y = depth)) + 
 # Add bars as medians
  stat_summary(fun = "median", 
               geom = "bar", 
               alpha = .7) +
  stat_summary(
    data = diamonds,
    mapping = aes(x = cut, y = depth),
    fun.min = function(z) { quantile(z,0.25) },
    fun.max = function(z) { quantile(z,0.75) },
    fun = median,
    geom  = 'errorbar',
    width = .2) +
  coord_cartesian(ylim=c(60, 67)) # ylim=c(59,66)

             

# understand the (mean(dist) - mean(ref))/sd(ref)

(dist <- c(5,4,3,4,5))
mean(dist)
(ref <- c(3,3,3,3,2.8))
mean(ref)
sd(ref)
(mean(dist)-mean(ref))/sd(ref)
