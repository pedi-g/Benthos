#nMDS ordination diagrams
#Pedram Ghahramani
#pedram.ghrm@gmail.com
#last mod: 6/11/2025

#####libraries ----
library(tidyverse)
library(ggthemes)
library(vegan)
library(FD)
library(ggfortify)
library(ggpubr)
library(patchwork)

#####input and data preparation----

source('R-scripts/1.factors.R') #recalling factors 

ab<- read.csv("input/abundance.csv", row.names = 1)
ab[is.na(ab)]<- 0.0
ab<- log(1+ ab)

mass<- read.csv("input/biomass.csv", row.names = 1)
mass[is.na(mass)]<- 0.0
mass<- log(1+ mass)


for (i in c("R432_Su","D213_w", "L412_w", "C322_w", "C132_w")){
  factors<- factors[row.names(ab) != i,]
  mass<- mass[row.names(ab) != i,]
  ab<- ab[row.names(ab) != i,]
}
rm(i)

#species-trait distance 
# sp.gow<- read.csv("output/dist.spxtrait(gowdis).csv", row.names = 1) #gower
# sp.bray<- read.csv("output/dist.spxtrait(bray).csv", row.names = 1) #bray curtis
#community-trait matrix and distance
# com.trait<- read.csv("output/comxtrait(LOGab).csv", row.names = 1)
# com.gow<- read.csv("output/dist.comXtrait(gowdis).csv", row.names = 1) #gower
# com.bray<- read.csv("output/dist.comxtrait(bray).csv", row.names = 1) #bray curtis
#community abundance matrix
# ab_raw<- read.csv("raw data/ab.csv", row.names = 1)
# ab<- ab_raw[,-((ncol(ab_raw)-3):ncol(ab_raw))]
ab_raw<- cbind(ab, factors)
#func group abundance matrix
# abg<- read.csv("output/ab_grouped.csv", row.names = 1)

#environmental data
# env<- read.csv("raw data/env.csv", row.names = 1)
#community Weighted Mean
# cwm <- read.csv("output/CWM.csv", row.names = 1)
#biomass
# mass<- read.csv("raw data/mass.csv", row.names = 1)


#selecting Habitats
# factors<- factors%>% filter(Habitat != "bch")
# ab_raw<- ab_raw[row.names(ab_raw) %in% row.names(factors),]
# abg<- abg[row.names(abg) %in% row.names(factors),]
# ab<- ab_raw%>%
  # select(-names(factors))
# sp.dis <- vegdist(log(1+ab))
sp.dis <- vegdist(ab)
# mass<- mass[row.names(mass) %in% row.names(factors),]
# mass_raw<- cbind(mass, factors)


# com.bray<- com.bray[rownames(com.bray) %in% rownames(factors),
                    # colnames(com.bray) %in% rownames(factors)]
# com.gow<- com.gow[rownames(com.gow) %in% rownames(factors),
#                     colnames(com.gow) %in% rownames(factors)]
# com.trait<- com.trait[rownames(com.trait) %in% rownames(factors),]
# env<- env[rownames(env) %in% rownames(ab_raw),]
# cwm<- cwm[rownames(cwm) %in% rownames(ab_raw),]


############### we define plot functions to prevent repetition#####
  # a plot theme for all three factors in use

palette1 <- c(
  "#9400D3", "#4B0082", "#0000FF", "#0080FF", "#00FFFF", "#00FF80", 
  "#00FF00", "#80FF00", "#FFFF00", "#FFBF00", "#FF8000", "#FF4000", "#FF0000"
)

my_theme <-
  theme_classic()+#base_size = 12)+ #another way to detemine the text size
  theme(panel.grid.major = element_blank(), # remove the major lines
          panel.grid.minor = element_blank())+ # remove the minor lines
  theme( legend.title = element_text(size=8), legend.position = "right",
           legend.text = element_text(size = 9),
           axis.title = element_text(size= 12),
           axis.text = element_text(size = 10))+
  theme(legend.title = element_text(face = "bold"))+
  theme(axis.title.y = element_text(vjust = 1))+ # increase distance from the y-axis
  theme(axis.title.x = element_text(vjust = -1)) # increase distance from the x-axis
  # theme(legend.position = c(0.93, 0.3))

plot1<- function(){
  ggplot(mds, aes(MDS1,MDS2))+
    geom_point(aes(shape = Season.Region , color = Habitat),stroke = 1.7, alpha = 0.8, size = 2)+
    scale_color_manual( values=c("#264653", "#2a9d8f", "#e9c46a"))+
#                        labels = c("Creek","Beach","Mangrove"))+
    scale_shape_manual(values = c(17, 16, 2, 1))+
    geom_text(x= Inf, y = Inf,vjust="inward",hjust="inward",label = paste("stress:",stress))+
    labs(x= "MDS1", y= "MDS2", shape = "", colour = "")+
    my_theme
    #geom_text(x= max(mds$MDS1)-0.25,y = max(mds$MDS2) ,label = paste("stress:",stress))
  #stat_ellipse(aes(shape = Season.area, color = Season.area), color= "gray50",type = "t", linetype= 1)+
  #stat_ellipse(aes(shape = Season.area, color = Season.area), geom="polygon",alpha= 0.08 , color= 'gray',type = "t")
}

  # a plot theme for two factors of Season and Habitat (use for areas separately)
plot2<- function(){
  ggplot(mds, aes(MDS1,MDS2))+
    geom_point(aes(shape = Season , color = Habitat), size = 1.5)+
    scale_color_manual( values=c("#264653", "#2a9d8f", "#e9c46a"))+
    scale_shape_manual(values = c(16, 17))+
    geom_text(x= Inf, y = Inf,vjust="inward",hjust="inward",label = paste("stress:",stress))+
    labs(x= "nMDS1", y= "nMDS2")+
    my_theme
  #geom_text(x= max(mds$MDS1)-0.25,y = max(mds$MDS2) ,label = paste("stress:",stress))
}


###############################  NMDS ########################################

############## nmds of community-trait composition ########


        ###### based on gower distance (gowdis function) #
  # nmds<- metaMDS(com.gow)
  # mds<- cbind(as.data.frame(nmds$points), factors)
  # stress<- round(nmds$stress,3)
  # 
  # # g1 <- ggplot(mds, aes(MDS1, MDS2, color = Season))+
  # #   geom_point()
  # # g2 <- ggplot(mds, aes(MDS1, MDS2, color = area))+
  # #   geom_point()
  # # g3 <- ggplot(mds, aes(MDS1, MDS2, color = Habitat))+
  # #   geom_point()
  # # 
  # (g4<- plot1()+
  #     stat_ellipse(aes(fill = area), color = "gray0", alpha= 0.2, type = "t", level = 0.9))
  
  # (gower<- ggarrange(g1, g2, g3, g4))
  
############## nmds of community-species composition ########

# nmds <- metaMDS(ab, distance = "bray")
# mds<- cbind(as.data.frame(nmds$points), factors)
# stress<- round(nmds$stress,3)
# (sp2 <- plot1()+
#     stat_ellipse(aes(fill = area), color = "gray0", alpha= 0.2, type = "t", level = 0.9))


#another way
nmds<- metaMDS(sp.dis)
mds<- cbind(as.data.frame(nmds$points), factors)
#scores(nmds , "species")
stress<- round(nmds$stress,3)
# (sp <- plot1()+
#     stat_ellipse(aes(fill = area), color = "gray0", alpha= 0.2, type = "t", level = 0.9))


(sp<- ggplot(mds, aes(MDS1,MDS2))+
  geom_point(aes(shape = Habitat , color = Region), size = 2)+
  scale_color_manual(values = c(
    "#0000f0", "#00cF00",
    "#999999"
  ))+
  # scale_color_viridis_d()+
  
  # scale_color_manual( values=c("#264653", "#2a9d8f", "#e9c46a"))+
  # scale_shape_manual(values = c(16, 17))+
  geom_text(x= Inf, y = Inf,vjust="inward",hjust="inward",label = paste("stress:",stress))+
  labs(x= "nMDS1", y= "nMDS2")+
  my_theme
)
  #ggtitle("species composition")
 
  sp + stat_ellipse(aes(shape = Habitat,
                        color = Region), #color= "gray50",
                    type = "t", alpha = 0.3, linetype= 1)
  # stat_ellipse(aes(shape = Region, color = Habitat), geom="polygon",alpha= 0.01 , color= 'gray',type = "t")


(sp_Season<- ggplot(mds, aes(MDS1,MDS2))+
  geom_point(aes( color = Season), size = 2)+
  scale_color_manual(values = c(
    "#0000FF", 
    "#FF8000"
  ))+
  # scale_color_viridis_d()+
  geom_text(x= Inf, y = Inf,vjust="inward",hjust="inward",label = paste("stress:",stress))+
  labs(x= "nMDS1", y= "nMDS2")+
  my_theme)

(sp_hab<- ggplot(mds, aes(MDS1,MDS2))+
    geom_point(aes( color = Habitat), size = 2)+
    scale_color_manual(values = c(
      "#0000f0", "#00cF00",
      "#999999"
    ))+
    # scale_color_viridis_d()+
    geom_text(x= Inf, y = Inf,vjust="inward",hjust="inward",label = paste("stress:",stress))+
    labs(x= "nMDS1", y= "nMDS2")+
    my_theme)


################### nmds of biomass composition ##############
mass_dis <- vegdist(log(1+mass))
nmds<- metaMDS(mass_dis)
mds<- cbind(as.data.frame(nmds$points), factors)
#scores(nmds , "species")
stress<- round(nmds$stress,3)
(p_mass <- ggplot(mds, aes(MDS1,MDS2))+
    geom_point(aes(shape = Habitat , color = Region), size = 2)+
    scale_color_manual(values = c(
      "#9400D3", "#4B0082", "#0000FF", "#0080FF", "#00FFFF", "#00FF80", 
      "#00FF00", "#80FF00", "#FFFF00", "#FFBF00", "#FF8000", "#FF4000", "#FF0000"
    ))+
    # scale_color_viridis_d()+
  
    # scale_color_manual( values=c("#264653", "#2a9d8f", "#e9c46a"))+
    # scale_shape_manual(values = c(16, 17))+
    geom_text(x= Inf, y = Inf,vjust="inward",hjust="inward",label = paste("stress:",stress))+
    labs(x= "nMDS1", y= "nMDS2")+
    my_theme)




###############sp and trait compositon together#############
# (two<- g4+labs(tag = "a")| sp+ labs(tag = "b"))
# space<- ggplot()+theme_tufte() # a third, blanck plot to increase the space
# (two<- ggarrange(sp,space,g4, common.legend = T, labels = c('a','','b'),
#                   ncol = 3,
#                   nrow = 1,
#                   label.x = 0.12,
#                   label.y = 1,
#                   hjust = -1.5,
#                   vjust = 1.5,
#                   font.label = list(size = 15, color = "gray30",
#                                     face = "bold", family = NULL),
#                   align = c("none", "h", "v", "hv"),
#                   widths = c(1,0.09,1),
#                   heights = 1,
#                   legend = "right",
#                   legend.grob = NULL))

################# sp and mass together #############

space<- ggplot()+theme_tufte() # a third, blanck plot to increase the space
(two1<- ggarrange(sp,space,p_mass, common.legend = T, labels = c('abundance','','biomass'),
                  ncol = 3,
                  nrow = 1,
                  label.x = -0.1,
                  label.y = 1,
                  hjust = -1.5,
                  vjust = 1.5,
                  font.label = list(size = 15, color = "gray30",
                                    face = "bold", family = NULL),
                  align = c("none", "h", "v", "hv"),
                  widths = c(1,0.09,1),
                  heights = 1,
                  legend = "right",
                  legend.grob = NULL))



#-------------------------nmds of species-trait distance ---------------------

#gower (gowdis)
# nmds<- metaMDS(sp.gow)
# mds<- cbind(as.data.frame(nmds$points))
# stress<- round(nmds$stress,3)

# ggplot(mds, aes(-MDS1,MDS2))+
#   theme_classic()+
#   labs(x= "nMDS1", y= "nMDS2")+
#   geom_point(size = 2)+
#   ggtitle("sp gowdis")+
#   theme( legend.title = element_text(size=7) ,legend.position = "none",
#          #legend.text = element_text(size = 6),
#          axis.title = element_text(size= 14),
#          axis.text = element_text(size = 12))+
#   geom_text(x= Inf, y = Inf,vjust="inward",hjust="inward",label = paste("stress:",stress))



################################ PCoA ######################################### 
 #  pco <- dudi.pco(as.dist(com.gow), scannf = F, nf = 4)
 # scatter(pco) ### same info as nMDS --> no need for this
 # sum(pco$eig[1:2]) / sum(pco$eig)
 # 
 # cor(pco$li[, 1:2], com.trait)
 # 
 # scatter(dudi.pca(com.trait, scannf = F, nf = 4))
 
 ######################## envfit ###########################

 # env1 <- na.omit(env[!rownames(env)=="C611_w",])
 # 
 # env1 <- scale(dplyr::select(env1, -DO))
 # 
 # # function for the envfit plot
 # envfitplot<-function(df, size = 1){ 
 #   com1 <- df %>% filter( rownames(df) %in% rownames(env1))
 #   
 #   nmds1 <- metaMDS(com1)
 #   stress1<- round(nmds1$stress,3)
 #   fit <-  envfit(nmds1, env1, perm = 9999) #correlation with env data
 # 
 # 
 #   spp.scrs1 <- size*as.data.frame(scores(fit, display = "vectors"))
 #   scrs1 <- as.data.frame(scores(nmds1, display = "sites"))
 #   
 #   mds1<- cbind(as.data.frame(nmds1$points),
 #               factors[row.names(factors) %in% row.names(env1),])
 #   
 #   ggplot(mds1, aes(MDS1,MDS2))+
 #     geom_point(aes(shape = Season.area , color = Habitat),stroke = 1.7, alpha = 0.8, size = 2)+
 #     scale_color_manual( values=c("#264653", "#2a9d8f", "#e9c46a"))+
 #     #                        labels = c("Creek","Beach","Mangrove"))+
 #     scale_shape_manual(values = c(17, 16, 2, 1))+
 #     geom_text(x= Inf, y = Inf,vjust="inward",hjust="inward",label = paste("stress:",stress1))+
 #     labs(x= "MDS1", y= "MDS2", shape = "", colour = "")+
 #     my_theme+
 #   stat_ellipse(aes(fill = area), color = "gray0", alpha= 0.2, type = "t", level = 0.9)+
 #   # Add environmental vectors
 #     geom_segment(data = spp.scrs1, aes(x = 0, y = 0, 
 #                                     xend = NMDS1, yend = NMDS2), 
 #                arrow = arrow(length = unit(0.2, "cm")), 
 #                color = "gray10",size = 0.6) +
 #   # Highlight significant vectors (assuming p.max = 0.05 means significant vectors)
 #   geom_segment(data = subset(spp.scrs1, fit$vectors$pvals <= 0.05), 
 #                aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
 #                arrow = arrow(length = unit(0.2, "cm")), 
 #                color = "red2", size = 0.6) +
 #   # Add variable names at a fixed distance from the tip of the arrows
 #   geom_text(data = spp.scrs1,
 #             aes(x = NMDS1 * 1.1, y = NMDS2 * 1.1, label = rownames(spp.scrs1)),
 #             vjust = 0.5, hjust = 0.5, color = "gray20")
 #  }
 # 
 # envfitplot(com.gow, 0.7)
 # 
 # envfitplot(as.data.frame(as.matrix(sp.dis)), 0.2)
 ############################## RDA #############################################
 # env1 <- na.omit(env[!rownames(env)=="C611_w",])%>%
 #   select(-DO)
 # env1 <- scale(env1)
 # abg2 <- abg[rownames(env1),]%>%
 #   select(starts_with("G"))
 # env1 <- as.data.frame(env1)
 # rda1 <- rda(abg2 ~ ., data = env1, scale = TRUE)
 # plot(rda1, display = c("bp", "sp"))
 # 
 # #RDA for cwm relation with env data
 # 
 # cwm2<- filter(cwm, rownames(cwm) %in% rownames(env1))%>%
 #   select(starts_with(c('S.','FD','MV','BT','TN','FL','RT','LS')))
 # traits2 <- filter(com.trait, rownames(com.trait) %in% rownames(env1))
 # ab2 <- filter(ab, rownames(ab) %in% rownames(env1))
 # 
 # par(mfrow = c(1, 1))
 # rda.cwm <- rda(cwm2 ~ ., data = env1)
 # 
 # png("figs/RDA.png", width = 1000, height = 900,pointsize = 25)
 # plot(rda.cwm, type = "n", scaling = "sites")
 # text(rda.cwm, dis = "cn", scaling = "sites")
 # text(rda.cwm, display =  "sp", scaling = "sites", col = "darkblue", cex = 1 ) #modalities are shown as 
 # dev.off()
 
 
 ##output####
 # ggsave("figs/nMDS.triat(gower)&sp.png", plot = two, width = 12, height = 5) 
 # ggsave("figs/nMDS.triat.bray.4.png", plot = bray , width = 12, height = 8)  
 # ggsave("figs/nMDS.triat.bray.png", plot = b4 , width = 8, height = 5) 
 # ggsave("figs/nMDS.trait.gower.4.png", plot = gower , width = 12, height = 8)  
 # ggsave("figs/nMDS.trait.gower.png", plot = g4, width = 8, height = 5)
 # ggsave("figs/nMDS.sp(nolog).png", plot = sp2, width = 8, height = 5)
 ggsave("figs/nMDS.spLog.png", plot = sp, width = 8, height = 5)
ggsave("figs/nMDS.sp_Season.png", plot = sp_Season, width = 8, height = 5)
ggsave("figs/nMDS.sp_Habitat.png", plot = sp_hab, width = 8, height = 5)
ggsave("figs/nMDS.massLog.png", plot = p_mass, width = 8, height = 5)
ggsave("figs/nMDS.sp&massLog.png", plot = two1, width = 12, height = 5)
 #ggsave("figs/sp.nMDS.area.png", plot = p1, width = 4.5, height = 3)
 #ggsave("figs/trait.nMDS.area.png", plot = p2, width = 4.5, height = 3)
 
 #ggsave("penguin_plot.pdf", dpi = 600, width = 100, height = 60, unit = "mm")
 
 #pdf("ggplot-cmyk.pdf", width = 12 / 2.54, height = 8 / 2.54, colormodel = "cmyk")
 
 #print(penguin_plot)
 
 #dev.off()

ggplot(mds, aes(MDS1, MDS2)) +
  geom_point(aes(shape = Habitat, color = Region), size = 1.5) +
  scale_color_manual(values = c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", 
    "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#a55194", "#66c2a5", "#e41a1c"
  ))

