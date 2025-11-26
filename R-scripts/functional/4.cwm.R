#pedram Ghahramani
# pedram.ghrm@gmail.com
#last edit; 8-3-2025

'
Computation of Community Weighed mean
For the functional composition
in mangrove macrobenthic communities
'

# libraries and source codes
library(FD)
library(tidyverse)
source('R-scripts/1.factors.R') #factors

#data input and preperation
traits<- read.csv("input/traits.csv", row.names = 1)

ab_raw <- read.csv("input/abundance.csv",row.names = 1)
tot_ab_species <- colSums(ab_raw)
ab <- ab_raw[,tot_ab_species > 4]

traits <- cbind( totAb = tot_ab_species[tot_ab_species > 4], traits)

write.csv(traits, 'input/traits1.csv' )






#transform for highly skewed numerical traits
# hist(traits$Weight)
traits$Weight <- log(1+traits$Weight)
# hist(traits$Weight)
#cumputation of cwm
cwm <- functcomp(traits, log(1+as.matrix(ab)), CWM.type = "all")

#scaling all traits to be between 0 and 1
# cwm[18] <- cwm[18]/max(cwm[18])
# cwm[19] <- cwm[19]/max(cwm[19])
# cwm[2:17] <- cwm[2:17]/5
# cwm[27:30] <- cwm[27:30]/4
# 
# cwm<- round(cwm,4)
# 
# cwm$Weight<- com.trait$Weight
# 
# cwm[1] <- cwm[1]/max(cwm[1])
cwm1<- cbind(cwm, factors)
boxplot(Weight ~ season.area.habitat, data = cwm1)

higher_outliers <- rownames(cwm[cwm[1]>0.24,])
lower_outliers <- rownames(cwm[cwm[1]<0.004,])

ncwm <- cwm
ncwm[rownames(cwm) %in% higher_outliers | rownames(cwm) %in% lower_outliers,1]<- NA
w<- cbind( wAll = cwm$Weight, wNew = ncwm$Weight, factors)

w<- w %>%
  filter(habitat != "bch")

cwm1<- cbind(cwm, factors)
par(mfrow = c(1,2))
boxplot(wAll ~ season.area.habitat, data = w)
boxplot(wNew ~ season.area.habitat, data = w)


cwm$Weight<- ncwm$Weight
#output
write.csv(cwm, "output/CWM.csv")


