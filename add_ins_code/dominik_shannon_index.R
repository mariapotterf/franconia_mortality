biomass_sp$shannon=(-biomass_sp$perc)*log(biomass_sp$perc)
biomass_sp$shannon[is.na(biomass_sp$shannon)]<-0 # 0 if there is nothing

alpha.shannon<-aggregate(biomass_sp$shannon, by=list(stand=biomass_sp$stand), FUN=sum, na.rm=TRUE)
colnames(alpha.shannon)[2] = "shannon"
alpha.shannon$shannon=exp(alpha.shannon$shannon)		
summary(alpha.shannon)
alpha.shannon