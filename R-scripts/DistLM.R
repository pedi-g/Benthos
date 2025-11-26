# DisLM analysis for the relationship of env variables
# with sp composition and trait composition

# Libraries
library(tidyverse)
library(vegan)
library(ggthemes)
library(ggpubr)

#####input and data prepration----

source('R-scripts/factors.R') #recalling factors 
com_gow<- read.csv("output/dist.comXtrait(gowdis).csv", row.names = 1) #gower
ab_raw<- read.csv("raw data/ab.csv", row.names = 1)
ab<- ab_raw[,-((ncol(ab_raw)-3):ncol(ab_raw))]
env<- read.csv("raw data/env2.csv", row.names = 1)%>%
  select(-DO, -sand)

#selecting habitats
factors<- factors%>% filter(habitat != "bch")
ab<- ab[row.names(ab) %in% row.names(factors),]
com_gow<- com_gow[rownames(com_gow) %in% rownames(factors),
                  colnames(com_gow) %in% rownames(factors)]
env<- env[rownames(env) %in% rownames(factors),]

sp_dis <- vegdist(log(1+ab))


# for water vars
env_water<- as.data.frame(scale(na.omit(env[1:3])))
ab_water<- ab[rownames(ab) %in% rownames(env_water),]
com_gow_water<- com_gow[rownames(com_gow) %in% rownames(env_water),]%>%
  select(row.names(env_water))
sp_dis_water<- vegdist(log(1+ab_water))

# for sediments
env_sed<- as.data.frame(scale(na.omit(env[factors$season == "w",4:5])))
ab_sed<- ab[rownames(ab) %in% rownames(env_sed),]
com_gow_sed<- com_gow[rownames(com_gow) %in% rownames(env_sed),]%>%
  select(row.names(env_sed))
sp_dis_sed<- vegdist(log(1+ab_sed))

# for TOM
env_TOM<- as.data.frame(scale(na.omit(env[6])))
ab_TOM<- ab[rownames(ab) %in% rownames(env_TOM),]
com_gow_TOM<- com_gow[rownames(com_gow) %in% rownames(env_TOM),]%>%
  select(row.names(env_TOM))
sp_dis_TOM<- vegdist(log(1+ab_TOM))



########## DistLM #########################

#### sp
DLM_sp_water<-capscale(log(1+ab_water) ~ ., data = env_water, distance = "bray")
DLM_sp_sed<-capscale(log(1+ab_sed) ~ ., data = env_sed, distance = "bray")
DLM_sp_TOM<-capscale(log(1+ab_TOM) ~ ., data = env_TOM, distance = "bray")

DLM_trait_water<-capscale(log(1+com_gow_water) ~ ., data = env_water, distance = "bray")
DLM_trait_sed<-capscale(log(1+com_gow_sed) ~ ., data = env_sed, distance = "bray")
DLM_trait_TOM<-capscale(log(1+com_gow_TOM) ~ ., data = env_TOM, distance = "bray")
# Perform permutation tests for significance of the model
anova_sp_water <- anova(DLM_sp_water, by = "margin", permutations = 999)
anova_sp_sed <- anova(DLM_sp_sed, by = "margin", permutations = 999)
anova_sp_TOM <- anova(DLM_sp_TOM, by = "margin", permutations = 999)

anova_Trait_water <- anova(DLM_trait_water, by = "margin", permutations = 999)
anova_Trait_sed <- anova(DLM_trait_sed, by = "margin", permutations = 999)
anova_Trait_TOM <- anova(DLM_trait_TOM, by = "margin", permutations = 999)

################ nmds env fit ###################

######### MDS plot ##

plot1<- function(){
  ggplot(mds, aes(MDS1,MDS2))+
    geom_point(aes(shape = season.area , color = habitat),stroke = 1.7, alpha = 0.8, size = 2)+
    scale_color_manual( values=c("#264653", "#2a9d8f", "#e9c46a"))+
    #                        labels = c("Creek","Beach","Mangrove"))+
    scale_shape_manual(values = c(17, 16, 2, 1))+
    geom_text(x= Inf, y = Inf,vjust="inward",hjust="inward",label = paste("stress:",stress))+
    labs(x= "MDS1", y= "MDS2", shape = "", colour = "")+
    theme_classic()+#base_size = 12)+ #another way to detemine the text size
    theme(panel.grid.major = element_blank(), # remove the major lines
          panel.grid.minor = element_blank())+ # remove the minor lines
    theme( legend.title = element_text(size=8), legend.position = "right",
           legend.text = element_text(size = 9),
           axis.title = element_text(size= 12),
           axis.text = element_text(size = 10))+
    theme(legend.title = element_text(face = "bold"))+
    theme(axis.title.y = element_text(vjust = 1))+ # increase distance from the y-axis
    theme(axis.title.x = element_text(vjust = -1))+ # increase distance from the x-axis
    # stat_ellipse(aes(fill = area.habitat), color = "gray0", alpha= 0.4, type = "t", level = 0.8)
    stat_ellipse(aes(fill = area), color = "gray20", alpha= 0.4, type = "t", level = 0.9)
  # theme(legend.position = c(0.93, 0.3))
  #geom_text(x= max(mds$MDS1)-0.25,y = max(mds$MDS2) ,label = paste("stress:",stress))
  #stat_ellipse(aes(shape = season.area, color = season.area), color= "gray50",type = "t", linetype= 1)+
  #stat_ellipse(aes(shape = season.area, color = season.area), geom="polygon",alpha= 0.08 , color= 'gray',type = "t")
}

nmds<- metaMDS(sp_dis)
mds<- cbind(as.data.frame(nmds$points), factors)
stress<- round(nmds$stress,3)

nmds_sp_water <- metaMDS(sp_dis_water, distance = "bray") # for sp lines
nmds_sp_sed <- metaMDS(sp_dis_sed, distance = "bray")
nmds_sp_TOM <- metaMDS(sp_dis_TOM, distance = "bray")

fit_sp_water <- envfit(nmds_sp_water ~ ., data = env_water)
fit_sp_sed <- envfit(nmds_sp_sed ~ ., data = env_sed)
fit_sp_TOM <- envfit(nmds_sp_TOM ~ ., data = env_TOM)

fit<- rbind(scores(fit_sp_water, display = "vectors"),scores(fit_sp_sed, display = "vectors"),scores(fit_sp_TOM, display = "vectors"))
spp.scrs1 <- as.data.frame(2*fit)

pvals<- cbind(fit_sp_water$vectors$pvals, fit_sp_sed$vectors$pvals, fit_sp_TOM$vectors$pvals)

p_sp<- plot1() +
  geom_segment(data = spp.scrs1, aes(x = 0, y = 0, 
                                     xend = NMDS1, yend = NMDS2), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "gray10",size = 0.6) +
  # Highlight significant vectors (assuming p.max = 0.05 means significant vectors)
  geom_segment(data = subset(spp.scrs1, pvals <= 0.05), 
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "red2", size = 0.6) +
  # Add variable names at a fixed distance from the tip of the arrows
  geom_text(data = spp.scrs1,
            aes(x = NMDS1 * 1.1, y = NMDS2 * 1.1, label = rownames(spp.scrs1)),
            vjust = 0.5, hjust = 0.5, color = "gray20")


nmds<- metaMDS(com_gow) # for trait dots
mds<- cbind(as.data.frame(nmds$points), factors)
stress<- round(nmds$stress,3)

nmds_trait_water <- metaMDS(com_gow_water, distance = "bray") # for tirat lines
nmds_trait_sed <- metaMDS(com_gow_sed, distance = "bray")
nmds_trait_TOM <- metaMDS(com_gow_TOM, distance = "bray")

fit_trait_water <- envfit(nmds_trait_water ~ ., data = env_water)
fit_trait_sed <- envfit(nmds_trait_sed ~ ., data = env_sed)
fit_trait_TOM <- envfit(nmds_trait_TOM ~ ., data = env_TOM)

fit<- rbind(scores(fit_trait_water, display = "vectors"),scores(fit_sp_sed, display = "vectors"),scores(fit_sp_TOM, display = "vectors"))
spp.scrs1 <- as.data.frame(0.3*fit)

pvals<- cbind(fit_trait_water$vectors$pvals, fit_trait_sed$vectors$pvals, fit_trait_TOM$vectors$pvals)


scrs1 <- as.data.frame(scores(nmds, display = "sites"))



p_trait <- plot1() +
  geom_segment(data = spp.scrs1, aes(x = 0, y = 0, 
                                     xend = NMDS1, yend = NMDS2), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "gray10",size = 0.6) +
  # Highlight significant vectors (assuming p.max = 0.05 means significant vectors)
  geom_segment(data = subset(spp.scrs1, pvals <= 0.05), 
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "red2", size = 0.6) +
  # Add variable names at a fixed distance from the tip of the arrows
  geom_text(data = spp.scrs1,
            aes(x = NMDS1 * 1.2, y = NMDS2 * 1.2, label = rownames(spp.scrs1)),
            vjust = 0.5, hjust = 0.5, color = "gray20")

###############sp and trait compositon together#############
# (two<- g4+labs(tag = "a")| sp+ labs(tag = "b"))
space<- ggplot()+theme_tufte() # a third, blanck plot to increase the space
(two<- ggarrange(p_sp,space, p_trait, common.legend = T, labels = c('a','','b'),
                 ncol = 3,
                 nrow = 1,
                 label.x = 0.12,
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
  
ggsave("figs/nmds_envfit_sp.png", plot = p_sp, width = 8, height = 5)
ggsave("figs/nmds_envfit_trait.png", plot = p_trait, width = 8, height = 5)
ggsave("figs/nMDS_envfit_both.png", plot = two, width = 12, height = 5)

############# dbRDA ####################
# Perform db-RDA
dbrda_result <- capscale(log(1+ab_water) ~ ., data = env_water)

# View and plot results
summary(dbrda_result)
plot(dbrda_result)

############# PERMANOVA ###########
# Perform PERMANOVA
permanova_result <- adonis2(sp_dis_water ~ ., data = env_water)

# View results
permanova_result

permanova_result <- adonis2(com_gow_water ~ ., data = env_water)

# View results
permanova_result





