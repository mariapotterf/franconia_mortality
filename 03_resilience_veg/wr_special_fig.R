# WR Plots
#install.packages("ggforce")
library(tidyverse)
library(ggforce)

load("out_reorg_full_v3.Rdata") # this includes updates of indicators 21.4.2023

# 
plot(out_reorg_full$RS_max ~ out_reorg_full$RA_max, type="p", pch=20)
abline(a=0, b=1)
abline(a=0, b=0.5)
abline(a=0, b=1.5)

# Frage: was ist der richtige/logische faktor?
reorg_ds <- out_reorg_full %>% select(trip_n, manag, dom_sp, RA_max, RS_max, R_strength, R_direction) %>%
  mutate(R_dir = RS_max / RA_max) %>% mutate(R_dirs = case_when( R_dir < tan(30*pi/180) ~ "Reassembly",
                                                                 R_dir > tan(60*pi/180) ~ "Restructuring",
                                                                 T ~ "Replacement")) %>%
                                               mutate(R_dirs = if_else(R_strength<1, "Resilience", R_dirs))

ggplot(reorg_ds, aes(x=RA_max, y=RS_max))  +
    theme_classic() + # scale_color_manual(values=c("#666666", "#888888", "#AAAAAA", "#888888")) + 
  coord_fixed() + lims(x=c(0,5.5), y=c(0,5.5)) + 
  geom_arc_bar( aes(x0=0, y0=0, r0=r0, r=r, start=start, end=end,  fill=arcid), arcs, inherit.aes = FALSE, show.legend = F, color="grey" ) +
    geom_point(aes(pch=dom_sp), fill="#666666", color="#cccccc", size=3) + 
  #geom_abline(slope=tan(30*pi/180), intercept=0, linetype=2) +
  #geom_abline(slope=tan(60*pi/180), intercept=0, linetype=2) + 
  scale_fill_manual(values = arcs$cols) + 
  xlab("Change in composition (units sd)") + ylab("Change in structure (units sd)") + labs(pch="Forest type") +
  scale_shape_manual(values=c(22,23,25,24), labels=c("Beech", "Oak", "Pine", "Spruce"))  # c(22,23,25,24)

ggsave("test.pdf")

arcs <- data.frame(
  start = seq(0, 2 * pi, length.out = 11)[-11],
  end = seq(0, 2 * pi, length.out = 11)[-1],
  r = rep(1:2, 5)
)

arcs <- data.frame(start = c(0, 0, 30, 60,0, 30, 60)*pi/180,  end = c(90, 30, 60, 90, 30, 60, 90)*pi/180, r=c(1,2,2,2, 5.5, 5.5, 5.5), 
                   cols=c("#ffffff80", "#d9dea630", "#cccccc30", "#c7dbed30", "#bec47c30", "#aaaaaa30", "#759bbd30"), r0=c(0,1,1,1,2,2,2), arcid=paste("a", 1:7))

arcs <- data.frame(start = c(0, 0, 30, 60,0, 30, 60)*pi/180,  end = c(90, 30, 60, 90, 30, 60, 90)*pi/180, r=c(1,2,2,2, 5, 5, 5), 
                   cols=c("#aaaaaa80","#bec47c30", "#dddddd30", "#759bbd30",  "#d9dea630", "#cccccc30", "#c7dbed30"), r0=c(0,1,1,1,2,2,2), arcid=paste("a", 1:7))


ggplot(arcs) + geom_arc_bar(aes(x0=0, y0=0, r0=r0, r=r, start=start, end=end, fill=type)) + 
  coord_fixed() +
  theme_no_axes() + scale_fill_manual(values=c("#666666", "#888888", "#AAAAAA", "#888888"))


