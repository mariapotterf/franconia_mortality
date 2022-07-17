# Sanky/ alluvial plot


# Make alluvial/Sanky plot ------------------------------------------------

# illustrate the flow between categorical categories
# https://rkabacoff.github.io/datavis/Other.html#

# input data
library(readr)
titanic <- read_csv("titanic.csv")

# summarize data
library(dplyr)
titanic_table <- titanic %>%
  group_by(Class, Sex, Survived) %>%
  count()

titanic_table$Survived <- factor(titanic_table$Survived, 
                                 levels = c("Yes", "No"))

head(titanic_table)





# create alluvial diagram
library(ggplot2)
library(ggalluvial)



ggplot(titanic_table,
       aes(axis1 = Class,
           axis2 = Survived,
           y = n)) +
  geom_alluvium(aes(fill = Sex)) +
  geom_stratum() +
  geom_text(stat = "stratum", 
            label.strata = TRUE) +
  scale_x_discrete(limits = c("Class", "Survived"),
                   expand = c(.1, .1)) +
  labs(title = "Titanic data",
       subtitle = "stratified by class, sex, and survival",
       y = "Frequency") +
  theme_minimal()



# alternative alluvial diagram: scale_viridis and 3 axis:
# create alternative alluvial diagram
library(ggplot2)
library(ggalluvial)
ggplot(titanic_table,
       aes(axis1 = Class,
           axis2 = Sex,
           axis3 = Survived,
           y = n)) +
  geom_alluvium(aes(fill = Class)) +
  geom_stratum() +
  geom_text(stat = "stratum", 
            label.strata = TRUE) +
  scale_x_discrete(limits = c("Class", "Sex", "Survived"),
                   expand = c(.1, .1)) +
  scale_fill_viridis_d() +
  labs(title = "Titanic data",
       subtitle = "stratified by class, sex, and survival",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 




