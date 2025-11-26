# calculating the species diversity, functional diversity, and ecological quality indices

#Pedram Ghahramani pedram.ghrm@gmail.com
#last update: 5/11/2025

#libraries
library(dplyr) #for filtering at the end
library(vegan)  # species diversity
# library(picante)  # functional diversity

  ##importing raw data ----
source("R-scripts/1.factors.r")

ab<- read.csv("input/abundance.csv", row.names = 1)
ab[is.na(ab)]<- 0.0
mass<- read.csv("input/biomass.csv", row.names = 1)
mass[is.na(mass)]<- 0.0

# ecoQ<- read.csv("raw data/EcoQab.csv", header = T, row.names = 1) # abundance of each ecoQ group
# M_AMBI<- read.csv("raw data/M-AMBI.csv", header = T, row.names = 1) # M-AMBI index and grouping (computed in "Azti.exe" software)

  ##Species diversity ----

#without transformation of the abundance
S<- specnumber(ab) # Species richness (S) 
N<- rowSums(ab) #total abundance
Smg=(S-1)/log(N) #Margalef’s diversity index
H=diversity(ab) #Shannon-Wiener’s diversity index
D=diversity(ab,"simpson") #Simpson’s diversity index
# H<- replace(H, H == 0,  NA)
J=H/log(S) #Pielou’s evenness index
abun <- rowSums(ab)



# ## ecological quality  ----
# 
# EG=ecoQ[1:4] #AMBI groups
# G=ecoQ[c(5,6)] #BENTIX groups
# AMBI<- (0*EG[,1])+(1.5*EG[,2])+(3*EG[,3])+(4.5*EG[,4])
# BENTIX<- 6*G[,1]+2*G[,2]

# AMBI[specnumber(ab) == 1]<- NA

#export  ----

indices0<- data.frame( # AMBI, BENTIX, M_AMBI,
  abun,
  S,  H , D, J ,Dm= Smg
                      )
ab <- ab[indices0$S>1,]
indices <- indices0[indices0$S>1,]
factors <- factors[indices0$S>1,]

############ beta ##################

## Whittaker’s multiplicative (overall turnover):
ab_tot <- colSums(ab) #total abundance of each species
gammaS <- specnumber(ab_tot) # total species richness
gammaH <- diversity(ab_tot, "shannon") # total Shannon diversity
gammaD <- diversity(ab_tot, "simpson") # total Simpson diversity
gammaJ <- gammaH / log(gammaS) # total Pielou's
gammaDM <- (gammaS-1)/log(sum(ab_tot)) # total Margalef's

gamma<- cbind(S= gammaS, H = gammaH, D = gammaD, J = gammaJ, Dm = gammaDM)

Alpha_bar <- sapply((indices), mean) # average alpha diversity indices

#### final Whittaker's beta diversity ----
# it is the overall turover of the rigion
betaW <- gamma/Alpha_bar[-1]



beta_jaccard <-as.matrix(vegdist(ab, method = "jaccard"))  # Jaccard index
beta_bray <- vegdist(ab, method = "bray")  # Bray-Curtis dissimilarity

beta_dispersion <-  betadisper(beta_bray, factors$Site)

write.csv(indices0, file = "output/indices.CSV")
# write.csv(indices.L, file = "output/indices(Log).CSV")


