

### libraries
library(vegan)
library(ggrepel)

#### input and initial preparations
source('R-scripts/1.factors.R') #recalling factors
ab<- read.csv("input/abundance.csv", row.names = 1)
ab[is.na(ab)]<- 0.0
ab<- log(1+ ab)

ab1<- cbind(ab,factors)

######## filtration
 #filter 1:
# ab1<- ab1[ ab1$Habitat != 'beach'& ab1$Proj != 'Parima' & ab1$Mud != 'far',] 
 #filter 2:
ab1<- ab1[ ab1$Habitat == 'creek'& ab1$Proj == 'Parn' ,] # for parnian

### applying filtered rows to other data frames
ab<- ab[row.names(ab) %in% row.names(ab1),]
factors<- factors[row.names(factors) %in% row.names(ab1),]

#### NMDS computations
sp.dis <- vegdist(ab)
nmds<- metaMDS(sp.dis)
mds<- cbind(as.data.frame(nmds$points), factors)
stress<- round(nmds$stress,3)


########### the plot
## change setting according to the filters
(sp<- ggplot(mds, aes(MDS1,MDS2))+
    geom_point(aes(#shape = Site,
                   color = Mud), size = 2)+
    scale_color_manual(values = c(
      "#0000f0", "#00cF00"))+
    scale_shape_manual(values = c(15:24))+
    # geom_text_repel(aes(label = rownames(mds)), size = 3) + # for readable labels
    # geom_text(aes(label = rownames(mds)), vjust = -0.5, size = 3) + # simple labels
    geom_text(x= Inf, y = Inf,vjust="inward",hjust="inward",label = paste("stress:",stress))+
    labs(x= "nMDS1", y= "nMDS2")#+
   # my_theme
)

# ____________________________________________________________________
'in nmds of filter1 D213_3, C322_w, L311_w are singular points '
# ____________________________________________________________________

#############################################################################.
################## Cluatering ###############################################

# Load packages
library(vegan)
library(ggplot2)
library(dplyr)
library(dendextend)

#### input and initial preparations
source('R-scripts/1.factors.R') #recalling factors
ab<- read.csv("input/abundance.csv", row.names = 1)
ab[is.na(ab)]<- 0.0
ab<- log(1+ ab)

ab1<- cbind(ab,factors)

######## filtration
#filter 1:
ab1<- ab1[ ab1$Habitat != 'beach'& ab1$Proj != 'Parima' & ab1$Mud != 'far',]
#filter 2:
# ab1<- ab1[ ab1$Habitat == 'creek'& ab1$Proj == 'Parn' ,] # for parnian

### applying filtered rows to other data frames
ab<- ab[row.names(ab) %in% row.names(ab1),]
factors<- factors[row.names(factors) %in% row.names(ab1),]



library(vegan)
library(ggplot2)

# --- Step 1: PCoA (Bray–Curtis) ---
sp_dis <- vegdist(ab, method = "bray")
pcoa_ab <- capscale(sp_dis ~ 1)

# Extract site scores
scores_sites <- scores(pcoa_ab, display = "sites") %>% as.data.frame()
scores_sites$Station <- rownames(ab)

# --- Step 2: Use only first 2 axes for clustering ---
coords <- scores_sites[, c("MDS1", "MDS2")]

# Distance between stations in ordination space
dist_coords <- dist(coords)

# Complete linkage clustering
hc_complete <- hclust(dist_coords, method = "complete")

# Plot dendrogram with groups
k <- 5   # number of groups you want
plot(hc_complete, labels = scores_sites$Station,
     main = "Complete linkage on PCoA (axes 1 & 2)")
rect.hclust(hc_complete, k = k, border = 2:6)   # highlight clusters

# Assign groups
groups <- cutree(hc_complete, k = k)
scores_sites$cluster <- factor(groups)

# --- Step 3: Plot PCoA with groups ---
ggplot(scores_sites, aes(x = MDS1, y = MDS2, color = cluster, shape = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = Station), vjust = -0.8, size = 3) +
  theme_minimal() +
  labs(title = "PCoA (Bray–Curtis) with complete linkage groups",
       x = "Axis 1", y = "Axis 2")



