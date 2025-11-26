library(tidyverse)
source('R-scripts/factors.R')

sum_ab<- cbind(density = rowSums(ab), factors)#%>%
  # filter(habitat != "bch")%>%
  # filter(transect != "1", transect != "2", transect!= "7", transect!= "8")
sum_mass<- cbind(mass = rowSums(mass), factors)#%>%
  # filter(habitat != "bch")%>%
  # filter(transect != "1", transect != "2", transect!= "7", transect!= "8")

theme_set(theme_bw())
  
ggplot(sum_ab, aes(sum_ab$area, density, fill = habitat))+
  geom_col(position = "dodge")
ggsave("figs/ab_density.jpg")
ggplot(sum_mass, aes(interaction(sum_ab$season, sum_ab$area), mass, fill = habitat))+
  geom_col(position = "dodge")
ggsave("figs/mass_density.jpg")

