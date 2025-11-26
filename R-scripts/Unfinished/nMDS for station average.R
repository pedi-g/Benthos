#nMDS ordination plots for average of stations
#Pedram Ghahramani
#pedram.ghrm@gmail.com
#1-27-2024
library(vegan)
library(gawdis)
library(tidyverse)

#input###########################
#input the trait-species and species-community matrices
sp.trait<- read.csv("raw data/spXtraits.csv", row.names = 1) 
#first trait is numinal, 6th and 7th are ordinal, the rest are fuzzy
sp.trait1<- read.csv("raw data/spXtraits(new).csv", row.names = 1)
#fist ,numinal, 2nd to 5th area fuzzy, 6th and 7th are ordinal, 8th, 9th and 10th are categorical
ab <- read.csv("raw data/ab(for traits).csv",row.names = 1)
#ab row just for the factor at the end of it
ab_raw <- read.csv("raw data/ab.csv",row.names = 1)

#### factors ####
source("R-scripts/factors.R")

##### Plot function ######
plot<- function(){
  ggplot(mds, aes(MDS1,MDS2))+
    geom_point(aes(shape = season.area , color = habitat),stroke = 1.5, size = 2)+
    theme_classic()+
    labs(x= "nMDS1", y= "nMDS2")+
    scale_color_manual( values=c("#264653", "#2a9d8f", "#e9c46a"))+
    theme( legend.title = element_text(size=7) ,legend.position = "none",
           legend.text = element_text(size = 6),
           axis.title = element_text(size= 14),
           axis.text = element_text(size = 12))+
    scale_shape_manual(values = c(1,2, 16, 17))+
    geom_text(x= Inf, y = Inf,vjust="inward",hjust="inward",label = paste("stress:",stress))
  #geom_text(x= max(mds$MDS1)-0.25,y = max(mds$MDS2) ,label = paste("stress:",stress))
}

##### preparing the community-trait data ####
com.t <- as.data.frame( log(1+as.matrix(ab)) %*% as.matrix(sp.trait))
com.t<- cbind(com.t, factors)
str(com.t)

com.trait<- com.t %>% group_by(station)%>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE), 
            across(where(is.factor), ~first(.)))%>%
  ungroup()
com.trait<- cbind(com.trait[,2:ncol(com.trait)], com.trait[,1])

########### gower distance ###########
com.gow<- (gowdis(com.trait[1])+ #wieght
                   gowdis(com.trait[2:6])/max(gowdis(com.trait[2:6]))+ #feeding
                   gowdis(com.trait[7:10])/max(gowdis(com.trait[7:10])) + # movement
                   gowdis(com.trait[11:13])/max(gowdis(com.trait[11:13])) + #living habitat
                   gowdis(com.trait[14:17])/max(gowdis(com.trait[14:17])) + #bioturbation
                   gowdis(com.trait[18]) + # shell toughness
                   gowdis(com.trait[19]) + # flexibility
                   gowdis(com.trait[20:22])/max(gowdis(com.trait[20:22])) + # larval developement
                   gowdis(com.trait[23:25])/max(gowdis(com.trait[23:25])) + # reproductive technique
                   gowdis(com.trait[26:29])/max(gowdis(com.trait[26:29])) # form
)/10

################# nmds 
nmds<- metaMDS(com.gow)
mds<- cbind(as.data.frame(nmds$points), com.trait[c("station","area","habitat","season", "season.area")])
stress<- round(nmds$stress,3)

########ploting 
(p1 <- ggplot(mds, aes(MDS1, MDS2, color = area, shape = season))+
  geom_point())
plot()


############### bray curtis distance############################
S1 <- vegdist(com.trait[1])
FD1 <- vegdist(com.trait[2:6])
M1 <- vegdist(com.trait[7:10])
H1 <- vegdist(com.trait[11:13])
B1 <- vegdist(com.trait[14:17])
t1 <- vegdist(com.trait[18])
FL1 <- vegdist(com.trait[19])
LD1 <- vegdist(com.trait[20:22])
RT1 <- vegdist(com.trait[23:25])
#SD1 <- vegdist(com.trait[35:36])
FR1 <- vegdist(com.trait[26:29])
#RS1 <- vegdist(com.trait[42:43])

dis.comXt.list<- list(as.matrix(S1), as.matrix(FD1), as.matrix(M1), as.matrix(H1), as.matrix(B1), 
                      as.matrix(t1), as.matrix(FL1), as.matrix(LD1), as.matrix(RT1), as.matrix(FR1))
com.bray <- as.data.frame(apply(simplify2array(dis.comXt.list), c(1, 2),
                                       mean, na.rm = T), 2)
rownames(com.bray)<- colnames(com.bray)    

#############nmds
nmds<- metaMDS(com.bray)
mds<- cbind(as.data.frame(nmds$points), com.trait[c("station","area","habitat","season")])
stress<- round(nmds$stress,3)

#########plotting
(p1 <- ggplot(mds, aes(MDS1, MDS2, color = area, shape = season))+
    geom_point())

