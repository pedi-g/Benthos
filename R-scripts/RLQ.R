
library(ade4)


source('R-scripts/factors.R') #recalling factors 

ab<- read.csv("raw data/ab(for traits).csv", row.names = 1)
env<- read.csv("raw data/env2.csv", row.names = 1)%>%
  select(-DO, -sand)
traits<- read.csv("raw data/spxtraits(new).csv", row.names = 1)%>%
  select(starts_with(c("s.",'FD','Mv','BT','T','FL','RT','LS')))

#selecting habitats
factors<- factors%>% filter(habitat != "bch")
ab<- ab[row.names(ab) %in% row.names(factors),]
env<- env[rownames(env) %in% rownames(factors),]


# for water vars
env_water<- as.data.frame(scale(na.omit(env[1:3])))
ab_water<- ab[rownames(ab) %in% rownames(env_water),]



# for sediments
env_sed<- as.data.frame(scale(na.omit(env[factors$season == "w",])))
ab_sed<- ab[rownames(ab) %in% rownames(env_sed),]


# for TOM
env_TOM<- as.data.frame(scale(na.omit(env[6])))
ab_TOM<- ab[rownames(ab) %in% rownames(env_TOM),]


# Example: Assuming you have the three matrices
# R: Environmental data (sites x environmental variables)
# L: Species trait data (species x traits)
# Q: Species abundance data (sites x species)

R<- env_sed
Q<- ab_sed
L<- traits


# Perform individual ordinations on the R, Q, and L matrices
pcaR <- dudi.pca(R, scannf = FALSE)
coaQ <- dudi.coa(Q, scannf = FALSE)
pcaL <- dudi.pca(L, scannf = FALSE)

# RLQ analysis combines the three ordinations
rlq_result <- rlq(pcaR, coaQ, pcaL, scannf = FALSE)

# Print and summarize the RLQ result
summary(rlq_result)

# Plot the RLQ analysis results
plot(rlq_result)




# Step 1: Perform ordination on Q (species abundance data) using Correspondence Analysis (CA)
coaQ <- dudi.coa(Q, scannf = FALSE, nf = 2)

# Step 2: Perform PCA on the R matrix (environmental data) using row weights from Q
# Use the row weights from the Correspondence Analysis (CA) of Q (coaQ$lw)
pcaR <- dudi.pca(R, row.w = coaQ$lw, scannf = FALSE, nf = 2)

# Step 3: Perform PCA on the L matrix (species traits) using column weights from Q
# Use the column weights from the CA of Q (coaQ$cw)
pcaL <- dudi.pca(L, row.w = coaQ$cw, scannf = FALSE, nf = 2)

# Step 4: Perform RLQ analysis with the properly aligned weights
rlq_result <- rlq(pcaR, coaQ, pcaL, scannf = FALSE)

# Step 5: Summarize and plot the RLQ results
summary(rlq_result)
plot(rlq_result)

