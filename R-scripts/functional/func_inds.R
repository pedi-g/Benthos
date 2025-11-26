

# library(gawdis)   # gower dissimilarity
library(picante)  # functional diversity 
#library(ade4)    # functional diverity "divc" function for Rao
#library(data.table) # functional diverity
library(FD)         # functional diverity
library(adiv)   # for Rao and Redundancy

##importing raw data ----
source("R-scripts/1.factors.r")
dist<-as.dist(read.csv("output/dist.spxtrait(gowdis).csv", row.names = 1))
ab_t<- read.csv("raw data/ab(for traits).csv", header = T, row.names = 1)# abundance with exclusion of rare species
ab_t.L <- log(1+ ab_t)

#without transformation of the abundance
# mpd<- mpd (ab_t, as.matrix(dist), abundance.weighted = F)#Mean pairwise distance
# mntd<- mntd (ab_t, as.matrix(dist), abundance.weighted = F)#Mean nearest taxon distance
FD<- dbFD(dist,ab_t, message = F, 
          calc.CWM = FALSE, stand.FRic = TRUE) #sp.num, FRich, FEve, FDiv, FDis, Rao

#with transformation 
# mpd.L<- mpd (ab_t.L, as.matrix(dist), abundance.weighted = F)#Mean pairwise distance
# mntd.L<- mntd (ab_t.L, as.matrix(dist), abundance.weighted = F)#Mean nearest taxon distance
FD.L<- dbFD(dist, ab_t.L, message = F, 
            calc.CWM = FALSE, stand.FRic = TRUE)

####### functional redundancy #############
##indices with adive package
adiv<- uniqueness(ab_t, dist)[[3]]
#log transformed
adiv.L<- uniqueness(ab_t.L, dist)[[3]]


##### station with less that 2 species should be eliminated 
adiv$Q[specnumber(ab_t) == 1]<- NA
adiv.L$Q[specnumber(ab_t) == 1]<- NA

#export  ----

indices<- data.frame(
                      # mpd, mntd, 
                      FRic=FD$FRic, FEve= FD$FEve,
                      Rao = adiv$Q, redun = adiv$R
                      )

indices.L <- data.frame(
                        # mpd.L,mntd.L,
                        FRic = FD.L$FRic, FEve = FD.L$FEve,
                        Rao = adiv.L$Q, redun = adiv.L$R 
                        )

# indices[indices$S <3,]$AMBI <- 7
# indices.L[indices.L$S <3,]$AMBI <- 7


write.csv(indices, file = "output/func_inds.CSV")
write.csv(indices.L, file = "output/func_inds(Log).CSV")



