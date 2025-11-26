library(dplyr) #for filtering at the end
library(vegan)  # species diversity
library(gawdis)   # gower dissimilarity
library(picante)  # functional diversity 
library(ade4)    # functional diverity "divc" function for Rao
#library(data.table) # functional diverity
library(FD) 
library(adiv)
library(psych)

######## input ###################
traits<- read.csv("raw data/spXtraits.csv", header = T, row.names = 1)#sp-trait matrix
ab_t<- read.csv("raw data/ab(for traits).csv", header = T, row.names = 1)# abundance with exclusion of rare species

## previously calculated indices
inds <- read.csv("output/indices.csv", row.names = 1)
inds<- inds[inds$habitat!="bch",]
indsL <- read.csv("output/indices(log).csv", row.names = 1)
indsL<- indsL[indsL$habitat!="bch",]
# inds<- indsL

inds1<- inds%>%
  select(-Status,-habitat, -area,- station, -season)
indsL1<- indsL%>%
  select(-Status,-habitat, -area,- station, -season)
#defining factor variables --------
source('R-scripts/factors.R')
factors<- factors[factors$habitat!="bch",]
inds<- cbind(inds1,factors)%>%
  filter(S !=1)
inds1<- inds1%>%
  filter(S !=1)
indsL<- cbind(indsL1,factors)%>%
  filter(S != 1)
indsL1<- indsL1%>%
  filter(S !=1)
factors<- factors[rownames(factors) %in% rownames(inds1),]


# ab_t<- ab_t[rownames(ab_t) %in% rownames(inds),]

############ Distance ###################
dist<- (gowdis(traits[1]) + gowdis(traits[2:6])/max(gowdis(traits[2:6]))+ 
          gowdis(traits[7:10])/max(gowdis(traits[7:10])) + 
          gowdis(traits[11:13])/max(gowdis(traits[11:13])) + 
          gowdis(traits[14:17])/max(gowdis(traits[14:17])) + 
          gowdis(traits[18]) + gowdis(traits[19]) + 
          gowdis(traits[20:22])/max(gowdis(traits[20:22])) + 
          gowdis(traits[23:25])/max(gowdis(traits[23:25])) + 
          gowdis(traits[26:29])/max(gowdis(traits[26:29])) )/10

ab_t.L <- log(1+ ab_t)

##indices with adive package
adiv<- uniqueness(ab_t, dist)[[3]]
#log transformed
adiv.L<- uniqueness(ab_t.L, dist)[[3]]

rao<- data.frame(adiv$N, adiv$D, adiv.L$D, adiv$R, adiv.L$R,
                 adiv$Q, adiv.L$Q, FD$RaoQ, FD.L$RaoQ, FD1$RaoQ,
                 FD1.L$RaoQ)
rao<- rao[rownames(rao) %in% rownames(inds),]

rao2<- data.frame(N = rao$adiv.N, S =  inds$S, adiv.D = rao$adiv.D,
                   adivl.D = rao$adiv.L.D, D = inds$D, D.L = indsL$D,
             adiv.R= rao$adiv.R, adivR.l = rao$adiv.L.R,
             R = rao$adiv.Q, R.l = rao$adiv.L.Q, row.names = rownames(inds))

rao2<- rao2%>%
  filter(N > 1)
factors<- factors[ row.names(factors) %in% row.names(rao2),]
ind<- cbind(rao2, factors)

############### bar plot ###########################
meanSAH<-list()
for (i in seq(ncol(rao2))){
  dum<-data.frame(index = rao2[,i],season = factors$season,
                  area = factors$area, habitat = factors$habitat)
  
  SAH<- dum %>%
    group_by(season, area, habitat)%>%
    summarise(mean = mean(index),SD = sd(index))
  meanSAH[[length(meanSAH)+1]]<- SAH
  names(meanSAH)[length(meanSAH)]<-names(ind[i])
  
}

plotSAH<- function(index= "inds.D"){
  colind <- which(colnames(ind) == index)
  
  ins<-as.data.frame(meanSAH[[as.character(index)]])
  
    factor<- interaction(ins$area,ins$season)
    fill<- ins$habitat
    name<- "season.area"

  ins<- data.frame(fill,factor, ins[4:5])
  
  bar<- ggplot(ins, aes(factor, mean,
                        fill = fill))+
    geom_col(position = "dodge",col = "black")+ #by dodge the fill factor will be side to side instead of stacked
    labs(x = name, y = as.character(index))+
    geom_errorbar(aes(ymin = mean-SD, ymax = mean+SD),
                  position = position_dodge2(width = 0.5, padding = 0.5)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_fill_manual(values = c("#E7298A", "#66A61E", "#E6AB02"))
}


ALL<-list()
for (i in names(rao2)){
  ALL[[length(ALL)+1]]<- plotSAH(i)
  names(ALL)[length(ALL)]<-names(rao2[i])
}

ALL[[1]]
ggarrange( ALL[[3]], ALL[[4]], ALL[[5]], 
          ALL[[6]], ALL[[7]], ALL[[8]], ALL[[9]], ALL[[10]])
