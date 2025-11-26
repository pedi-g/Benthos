# Clustering for species and functional composition
# Pedram Ghahramani
# pedram.ghrm@gmail.com
# last edit: 6-24-2024

##### Libraries ----
library(tidyverse)
library(ggthemes)
library(vegan)
library(FD)
library(ggfortify)
library(ggpubr)
library(patchwork)
library(phytools)

##### Input ----
# Species-trait distance 
sp.gow <- read.csv("output/dist.spxtrait(gowdis).csv", row.names = 1) # Gower
sp.bray <- read.csv("output/dist.spxtrait(bray).csv", row.names = 1) # Bray Curtis
# Community-trait matrix and distance
com.trait <- read.csv("output/comxtrait(LOGab).csv", row.names = 1)
com.gow <- read.csv("output/dist.comXtrait(gowdis).csv", row.names = 1) # Gower
com.bray <- read.csv("output/dist.comxtrait(bray).csv", row.names = 1) # Bray Curtis
# Community abundance matrix
ab_raw <- read.csv("raw data/ab.csv", row.names = 1)
# Community biomass matrix
mass <- read.csv("raw data/mass.csv", row.names = 1)
# Community Weighted Mean
cwm <- read.csv("output/CWM.csv", row.names = 1)

ab <- ab_raw %>%
  select(-habitat, -area, -station, -season)

# Defining factor variables --------
source('R-scripts/factors.R')
ab_raw<- cbind(ab, factors)

# Calculate group means for each 'seas.ar.hab' group
group_means <- ab_raw %>%
 # filter(season == "s")%>%
  group_by(season.area) %>%
  # select(where)%>%
  summarise(across(where(is.numeric), mean))%>%
  select(-1)

# Perform hierarchical clustering on the group means
group_dist <- vegdist(group_means)
group_tree <- hclust(group_dist)

# Convert to phylo object and make ultrametric
ult_group_tree <- force.ultrametric(as.phylo(group_tree), method = c("nnls", "extend"))

# Set the tip labels to the group names
ult_group_tree$tip.label <- levels(factors$area.habitat)

# Plot the ultrametric tree, labeling with factor names
plot(ult_group_tree, cex = 0.8, main = "Dendrogram of season.hab Groups")

plot(1:10,1:10)

