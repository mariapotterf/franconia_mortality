

# Stats for diversity metrics 
load(file = "vegData.Rdata")
load(file = "dataToPlot.Rdata")





# Stats: ANOVA: Shannon effective numbers ------------------------------------------

# two way anova, as I have two factors: 
#  - management
#  - dominant tree species

# check frequecy tables:
table(df_reg_dens_shannon$manag, 
      df_reg_dens_shannon$dom_sp)

# Visualize the data:
# boxplot
# two-way interaction plot - can show interactions
ggboxplot(df_reg_dens_shannon, 
          x = "manag", 
          y = "effective_n_sp", 
          color = "dom_sp")

ggline(df_reg_dens_shannon, 
       x = "manag", 
       y = "effective_n_sp", 
       color = "dom_sp",
       add = c("mean_se", 
               "dotplot")#,
       #binwidth = 1/50
)


## two way anova Effective numbers -----------------------------------------------
# http://www.sthda.com/english/wiki/two-way-anova-test-in-r#what-is-two-way-anova-test
aov1 <- aov(effective_n_sp ~ manag +dom_sp, 
            df_reg_dens_shannon)
summary(aov1)

# on effective number of species,
# there is higher effect of teh dominant species
# then of teh management

# can teh effect be synergic? the use * instead of +
aov2 <- aov(effective_n_sp ~ manag*dom_sp, 
            df_reg_dens_shannon)

# the same as (here it is more explicit)
aov2 <- aov(effective_n_sp ~ manag+dom_sp +
              manag:dom_sp,
            df_reg_dens_shannon)
summary(aov2)
plot(aov2)

AICc(aov1, aov2)





# ANOVA: which groups are different???
# need to do the pairwise comparison
TukeyHSD(aov1)  # if teh interaction is not significant, we
# should use only the additive anova (here aov1)

res.tuk <- TukeyHSD(aov1, which = 'dom_sp') # select only significant factor

# p-value is significant for each combination of spruce-otherSp

plot(res.tuk, las = 1)

# test validity of ANOVA use: ---------------------
# anova assumptions: 
#  - data are normally distributed
#  - variance across groups are homogenous

# 1. Homogeneity of variances
plot(aov1, 1) # numbers indicate outliers: normality is heavily affected by them!

# Use a test of homogeneity
library(car)
leveneTest(effective_n_sp ~ manag*dom_sp, 
           data = df_reg_dens_shannon)

# p-value is >0.5 - we assume the variance across groups is similar


# 2. Cechk for normality assumtions
plot(aov1, 2)

# Run Shapiro-Wilcox test on teh ANOVA residuals
# to check if the normality is violated:
# Extract the residuals
aov_residuals <- residuals(object = aov1)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

# p>0.05 - seems that they are normal


# ANOVA for unbalanced design --------------------------
# there are three types to aaacount for unbalanced design: 
#   when the number of observation is not the same
#   recommended: Type-III sums of squares
# use car::anova()
library(car)
aov2 <- aov(effective_n_sp ~ manag*dom_sp, 
            df_reg_dens_shannon)

car::Anova(aov2, type = "III")
car::Anova(aov2, type = "II")  # in this case, they have teh same results

# Use kruskall wallis -------------------------------
# http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

# kruskall-wallis just test several groups, but having only one factor
# so I can test for each factor individually (does not allow interaction)
# or use the Ordinal logistic regression: can handle factorial structure
# 
# Test GAMS for tree reg densities: --------------------------------------------------
# data: highly skewed
df_reg_dens

str(df_reg_dens)

# claim factors:
df_reg_dens <- df_reg_dens %>%
  mutate(
    dom_sp = factor(dom_sp),
    manag = factor(manag),
    trip_n = factor(trip_n)
  )

# data are not normally distributes and have many outliers

# standardize teh data by z-score: ---------------------------------
# Finding Mean
# standardization/normalization will not change the data distribution!!!



# For categorical data, the smotng is not meanings full: -------------------
# stick with glm ------------------------------
# categorical predictors variables: extend of ANOVA
# # https://anamtk.github.io/GLM_tutorials/tidyTuesday_fall2020/GLM_inR_Tutorial.html#extensions

library(MASS)
library(glmmTMB)
library(DHARMa)
library(effects)

m.sp <- glm(
  corr_density ~ dom_sp,# +   # claim as categorical data; generates random intercept for each level of factor ,
  # claim as random effect
  data = df_reg_dens,
  family = Gamma  # continuous positive data
)

m.sp.manag <- glm(
  corr_density ~ dom_sp + manag,# +   # claim as categorical data; generates random intercept for each level of factor ,
  # claim as random effect
  data = df_reg_dens,
  family = Gamma  # continuous positive data
)

m3 <- glm(
  corr_density ~ dom_sp + manag + trip_n,# +   # claim as categorical data; generates random intercept for each level of factor ,
  # claim as random effect
  data = df_reg_dens,
  family = Gamma  # continuous positive data
)

m.manag <- glm(
  corr_density ~ manag,# +   # claim as categorical data; generates random intercept for each level of factor ,
  # claim as random effect
  data = df_reg_dens,
  family = Gamma  # continuous positive data
)

m.sp.manag2 <- glm(
  corr_density ~ dom_sp*manag,# +   # claim as categorical data; generates random intercept for each level of factor ,
  # claim as random effect
  data = df_reg_dens,
  family = Gamma  # continuous positive data
)



summary(m.manag)
summary(m.sp)

AICc(m.sp, m.manag, m.sp.manag, m.sp.manag2, m3)


simulationOutput <- simulateResiduals(fittedModel = m3, plot = T)
testDispersion(simulationOutput)

plot(m3)


windows()
plot(effects::allEffects(m.sp))
windows()
plot(effects::allEffects(m.manag))
plot(effects::allEffects(m.sp.manag))
plot(effects::allEffects(m.sp.manag2))








# test for z score!
# generate random data from poisson distrib
set.seed(2)
a_norm <- rnorm(4000)
a_pois <- rpois(4000, lambda = 2)
hist(a_norm)
hist(a_pois)

a_scale <- scale(a_pois)



# conversion from z score
m<-mean(a_pois)

# Finding Standard Deviation
s<-sd(a_pois)
a_t <- (a_pois-m)/s


# test for normality
shapiro.test(a_norm)
shapiro.test(a_pois)
shapiro.test(a_scale)


windows()
hist(a_t)
hist(a_scale)



m<-mean(df_reg_dens$corr_density)

# Finding Standard Deviation
s<-sd(df_reg_dens$corr_density)


df_reg_dens <- df_reg_dens %>% 
  mutate(dens_z = (corr_density-m)/s)

ggplot(df_reg_dens) + 
  geom_density(aes(dens_z))

hist(log10(df_reg_dens$corr_density))
hist(df_reg_dens$corr_density)
plot(density(df_reg_dens$corr_density))
plot(density(df_reg_dens$dens_z))


# check frequecy tables:
table(df_reg_dens$manag, 
      df_reg_dens$dom_sp)

# Visualize the data:
# boxplot
# two-way interaction plot - can show interactions
ggboxplot(df_reg_dens, 
          x = "manag", 
          y = "corr_density", 
          color = "dom_sp")

ggline(df_reg_dens, 
       x = "manag", 
       y = "corr_density", 
       color = "dom_sp",
       add = c("mean_se", 
               "jitter")#,
       #binwidth = 1/50
)


# Run anova on data:--------------------------------------------------
# two way anova:
# http://www.sthda.com/english/wiki/two-way-anova-test-in-r#what-is-two-way-anova-test
aov1 <- aov(effective_n_sp ~ manag +dom_sp, 
            df_reg_dens_shannon)
summary(aov1)

# on effective number of species,
# there is higher effect of teh dominant species
# then of teh management

# can teh effect be synergic? the use * instead of +
aov2 <- aov(effective_n_sp ~ manag*dom_sp, 
            df_reg_dens_shannon)

# the same as (here it is more explicit)
aov2 <- aov(effective_n_sp ~ manag+dom_sp +
              manag:dom_sp,
            df_reg_dens_shannon)
summary(aov2)
plot(aov2)

AICc(aov1, aov2)


# get summary statitics:
model.tables(aov2, 
             type="means", se = TRUE)

# Get summary statistics:
df_reg_dens_shannon %>%
  group_by(manag, dom_sp) %>%
  summarise(
    count = n(),
    mean = mean(effective_n_sp, 
                na.rm = TRUE),
    sd = sd(effective_n_sp, 
            na.rm = TRUE)
  )


# ANOVA: which groups are different???
# need to do the pairwise comparison
TukeyHSD(aov1)  # if teh interaction is not significant, we
# should use only the additive anova (here aov1)

res.tuk <- TukeyHSD(aov1, which = 'dom_sp') # select only significant factor

# p-value is significant for each combination of spruce-otherSp

plot(res.tuk, las = 1)

# test validity of ANOVA use: ---------------------
# anova assumptions: 
#  - data are normally distributed
#  - variance across groups are homogenous

# 1. Homogeneity of variances
plot(aov1, 1) # numbers indicate outliers: normality is heavily affected by them!

# Use a test of homogeneity
library(car)
leveneTest(effective_n_sp ~ manag*dom_sp, 
           data = df_reg_dens_shannon)

# p-value is >0.5 - we assume the variance across groups is similar


# 2. Cechk for normality assumtions
plot(aov1, 2)

# Run Shapiro-Wilcox test on teh ANOVA residuals
# to check if the normality is violated:
# Extract the residuals
aov_residuals <- residuals(object = aov1)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

# p>0.05 - seems that they are normal






# Compare if teh groups are differet?
lm(drought_cwm~manag, dat = df_traits_cwm)

hist(df_traits_cwm$drought_cwm)

windows()
ggplot(df_traits_cwm, aes(drought_cwm, 
                          fill= manag)) +
  geom_density(alpha = 0.5) +
  facet_grid(~dom_sp)




# Shannon index -----------------------------------------------------------

## Ground cover: -------------------------------------
# get the average values per category, not by the subplot:
df_ground_shann <-
  df_ground %>% 
  group_by(trip_n, dom_sp, manag, class) %>% 
  summarize(mean_prop = mean(prop)/100) %>%  # for shannon, the scale should be 0-1, not 0-100% !! 
  mutate(shannon_part = replace_na(-(mean_prop)*log(mean_prop),0)) %>% 
  ungroup() %>% 
  group_by(trip_n, dom_sp, manag) %>%
  summarize(shannon_ground = sum(shannon_part)) %>% 
  mutate(effective_n_ground = exp(shannon_ground))
# calculate shannon and replace NA vals by 0
#shannon = is.na())


#### Ground Shannon check plot: ---------------------------------------------------------------------
windows()
df_ground_shann %>% 
  ggplot(aes(x = factor(manag),
             y = effective_n_ground)) +
  geom_boxplot() +
  geom_violin()

windows()
df_ground_shann %>% 
  ggplot(aes(x = factor(manag),
             y = shannon_ground,
             fill = dom_sp)) +
  geom_violin() 


# Get the differences between 
# Test: does the management manag affects the diversity of th ground cover?
# HO ; the Cleared area will be more homogenous then the Dead ones:
# clearing homogenize teh gropund cover (prevalence of grasses, ...)








# Calculate Shannon per subsite?? 
# then I can condider the individual patches as random effects in the model
df_reg_fin_by_subsample <- 
  df_regen_fin %>% 
  select(-c(height_class, DBH, reg_height)) %>% 
  arrange(trip_n, manag) %>%
  group_by(trip_n, dom_sp, manag, sub_n, reg_species) %>% 
  summarize(count_sp        = sum(count))  %>% 
  ungroup() %>% 
  group_by(trip_n, dom_sp, manag, sub_n) %>% 
  mutate(dens_tot        = sum(count_sp),
         sp_pi           = count_sp/dens_tot,
         shannon_part_sp = sp_pi*log(sp_pi),
         shannon_sp      = -sum(shannon_part_sp),
         eff_numb_sp     = exp(shannon_sp))# %>%
#   print(n = 40)




# Calculate Shannon height diversity ??? -------------
#df_reg_fin_by_subsample_height <- 
df_regen_fin %>% 
  arrange(trip_n, manag) %>%
  group_by(trip_n, dom_sp, manag, sub_n) %>% 
  mutate(dens_tot        = sum(count),
         sp_pi           = count/dens_tot,
         shannon_part_sp = sp_pi*log(sp_pi),
         shannon_sp      = -sum(shannon_part_sp),
         eff_numb_sp     = exp(shannon_sp))# %>% 



# evaluate teh dominance of reg_species: dominant: if more then 50% of stems



# How does the height structure looks like?
df_reg_fin_by_subsample %>% 
  ggplot(aes(y = height,
             x = factor(manag),
             fill = dom_sp)) +
  geom_violin()




# keep only distinct rows for mixed effects:

dat <- df_reg_fin_by_subsample %>% 
  mutate(uniqueID = factor(paste(trip_n,dom_sp,manag,sub_n, sep = '_' ))) %>% 
  dplyr::select(uniqueID, shannon, eff_numb) %>% 
  distinct() #%>% 




# How does management affect the dievrsity of reg_species composition?
dat$manag <- as.factor(dat$manag)
dat$dom_sp <- as.factor(dat$dom_sp)

m1 <- lme(eff_numb~ manag, random = ~1|uniqueID, data = dat)

summary(m1)

m2 <- lme(eff_numb~ manag + dom_sp, random = ~1|uniqueID, data = dat)

summary(m2)
AIC(m1, m2)

hist(dat$eff_numb)

# use glm and poisson family:
m3 <- glm(eff_numb~ manag + dom_sp, family = poisson(link = "log"), # poisson family
          data = dat)

AIC(m3)
summary(m3)




# Does the management explain the tree regen diversity?? --------------------------------------
m1<-lm(effective_n_sp~1, df_reg_dens_shannon)  # this ignores the management, and cosider the treatmements as independent
summary(m1)

# only predict the mean effective number: 2.6351     
coef(m1)
# (Intercept) 
# 2.63515 

confint(m1)
#               2.5 %   97.5 %
#  (Intercept) 2.237458 3.032842

# Mixed effect model: 
# can I keep in the small patches, and evaluate them as a group within the treatments? e.g. subsites within the patch will
# be more similar that patches further away
library(nlme)
m3 <- lme(effective_n_sp~1, df_reg_dens_shannon)
summary(m3)






df_reg_dens %>% 
  ggplot(aes(y = density_ha,
             x = manag)) +
  geom_boxplot() + 
  facet_grid(reg_height~dom_sp, scales = 'free') +
  ylab('density \n(#trees/ha)') 





# Investigate data by categories --------------------------------------------------------------


# Check density distribution:

df_regen %>% 
  filter(count != 0) %>% # need to filter zero counts first
  group_by(trip_n, dom_sp, manag) %>% 
  ggplot(aes(y = density_ha,
             x = manag)) +
  geom_boxplot() + 
  facet_grid(~dom_sp) +
  ylab('density \n(#trees/ha)')




# calculate richness:
# count number of reg_species per plot
#richness <- 
df_regen %>% 
  filter(count != 0) %>% # need to filter zero counts first
  group_by(trip_n,dom_sp, manag) %>% 
  distinct(reg_species) %>% 
  count() %>% 
  ggplot(aes(y = n,
             x = manag)) +
  geom_boxplot() + 
  # geom_jitter() +
  facet_grid(~dom_sp) +
  ylab('richness \n(# of tree reg_species)')


# What is regeneration height??
df_regen %>% 
  filter(count != 0)  %>% # need to filter zero counts first
  group_by(trip_n,dom_sp, manag) %>% 
  #  distinct(reg_species) %>% 
  #  count() %>% 
  ggplot(aes(y = count,
             x = height_class,
             fill = manag)) +
  geom_boxplot() + 
  # geom_jitter() +
  facet_grid(.~dom_sp) #+
#ylab('richness \n(# of tree reg_species)')

