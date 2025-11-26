library(picante)
ab_raw<- read.csv("raw data/ab.csv", header = T, row.names = 1)
metadata<- read.csv("raw data/env.csv", header = T, row.names = 1)
# metadata <- ab_raw[(ncol(ab_raw)-3):ncol(ab_raw)]
comm<- ab_raw[1:(ncol(ab_raw)-4)]
apply(comm, 1, sum)
# Turn percent cover to relative abundance by dividing each value by sample
# total abundance
comm <- decostand(comm, method = "total")
# check total abundance in each sample
apply(comm, 1, sum)
comm[1:5, 1:5]


######### Trait data ###########################
# replace filename with file.choose() to open interactive window
traits <- read.csv("raw data/spxtraits.csv", header = TRUE, row.names = 1)
traits
# take a peek at the data
head(traits)
# plot the data
pairs(traits)

plot(specaccum(comm), xlab = "# of samples", ylab = "# of species")

############### Cleaning and matching data sets #################
ls()
all.equal(rownames(comm), rownames(metadata))

# they all match - if they didn't we could sort them to the same order sort
# metadata rows to be in the same order as community rows
metadata <- metadata[rownames(comm), ]

###################### Visualizing and summarizing biodiversity data ###########
# compare species richness between fescue and mixedgrass habitats
boxplot(specnumber(comm) ~ metadata$area, ylab = "# of species", xlab = "area")

# statistical test of difference
t.test(specnumber(comm) ~ metadata$area)

# plot species accumulion curve across samples
plot(specaccum(comm), xlab = "# of samples", ylab = "# of species")

######################## Multivariate community analysis #####################
# calculate Bray-Curtis distance among samples
comm.bc.dist <- vegdist(comm, method = "bray")
# cluster communities using average-linkage algorithm
comm.bc.clust <- hclust(comm.bc.dist, method = "average")
# plot cluster diagram
plot(comm.bc.clust, ylab = "Bray-Curtis dissimilarity")

######################### Ordination #########################################
# The metaMDS function automatically transforms data and checks solution
# robustness
comm.bc.mds <- metaMDS(comm, dist = "bray")
# Assess goodness of ordination fit (stress plot)
stressplot(comm.bc.mds)

# plot site scores as text
ordiplot(comm.bc.mds, display = "sites", type = "text")

# automated plotting of results - tries to eliminate overlapping labels
ordipointlabel(comm.bc.mds)

# ordination plots are highly customizable set up the plotting area but
# don't plot anything yet
mds.fig <- ordiplot(comm.bc.mds, type = "none")
# plot just the samples, colour by habitat, pch=19 means plot a circle
points(mds.fig, "sites", pch = 19, col = "green", select = metadata$area == 
         "Tis")
points(mds.fig, "sites", pch = 19, col = "blue", select = metadata$area == 
         "Gw")
# add confidence ellipses around habitat types
ordiellipse(comm.bc.mds, metadata$area, conf = 0.95, label = TRUE)
# overlay the cluster results we calculated earlier
ordicluster(comm.bc.mds, comm.bc.clust, col = "gray")

# plot Sphaeralcea abundance. cex increases the size of bubbles.
ordisurf(comm.bc.mds, comm[, "Assiminea.sp.1"], bubble = TRUE, main = "Assiminea.sp.1 abundance", 
         cex = 3)

################# Adding environmental and trait data to ordinations##############
ordiplot(comm.bc.mds)
# calculate and plot environmental variable correlations with the axes use
# the subset of metadata that are environmental data
plot(envfit(comm.bc.mds, metadata[, 1:8], na.rm = T))





