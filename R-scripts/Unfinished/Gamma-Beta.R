#pedram Ghahramani
#pedram.ghrm@gmail.com
#jan-2024
#speces Gamma and Betha diverity
library(vegan)
library(tidyverse)
source("R-scripts/factors.R")
ab_raw<- read.csv("raw data/ab.csv", header = T, row.names = 1)

abG <- dplyr::filter(ab_raw, area=="Gw")
abT <- dplyr::filter(ab_raw, area=="Tis")
abG <- abG [-c(74,75,76,77)]
abT <- abT [-c(74,75,76,77)]

gammaG <- ncol(abG[,colSums(abG)!=0])
gammaT <- ncol(abT[,colSums(abT)!=0])

bethaG= gammaG/mean(specnumber(abG))
bethaT= gammaT/mean(specnumber(abT))



###################################### beta ################################
a<- ab_raw%>%
  group_by(season, area, habitat)%>%
  summarise(across(everything(),sum))

b<- betadiver(a , method = "-2"); nmds<- metaMDS(b);mds<- cbind(as.data.frame(nmds$points),
            season.area = interaction(a$season, a$area),
            habitat = a$habitat); ggplot(mds, aes(MDS1,MDS2))+
  geom_point(aes(shape = season.area , color = habitat), size = 1.5)+
  scale_shape_manual(values = c(1, 16, 2, 17))


betadiver(help=TRUE)
