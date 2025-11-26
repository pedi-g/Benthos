#Pedram Ghahramani
#pedram.ghrm@gmail.com
#visualization of indices
#5/11/2025
#libraries----------
library(tidyverse)
library(ggthemes)
library(ggfortify)
library(ggpubr)
library(psych)

theme_set(theme_bw())
#input-----------
source('R-scripts/1.factors.R')

inds1 <- read.csv("output/indices.csv", row.names = 1)
inds<- cbind(inds1,factors)

inds<- inds[inds$S>1 & inds$Habitat == 'mangrove',]

inds1<- inds1[row.names(inds1) %in% row.names(inds),]

indsL <- log(1+ inds1)

# Pezh filter
# inds <- inds %>%
#   filter(Region %in% c('Sorgalm', 'Jask', 'Khalasi', 'Gabrik'), Habitat == 'mangrove')


########################## histograms #########################################

# #### no transformation
# # png('figs/ind_hist_s2.png',width=900, height=700) 
# par(mfrow = c(2,3))
# for(i in c(1:ncol(inds1))){
#   hist(inds1[,i], ylab = '',
#        main = names(inds1)[i])
# }
# # dev.off()
# 
# #### log transformed
# for(i in c(1:ncol(indsL))){
#   hist(indsL[,i], ylab = '',
#        main = names(inds1)[i])
# }


################################# bar and box #################################

################## a function for bar and box plots #
plotSAH<- function(index= "H", f1 = "Season", f2 = "Region", f3 = "Habitat"){
  colind <- which(colnames(inds) == index)
  
  ############### barplot #####.
  ind<-as.data.frame(meanSAH[[as.character(index)]])
  
  if(f1 == "Season" & f2 == "Region"){
    factor<- interaction(ind$Region,ind$Season)
    fill<- ind$Habitat
    name<- "Region.Season"
  }
  if(f1 == "Season" & f2 == "Habitat"){
    factor<- interaction(ind$Season,ind$Habitat)
    fill<- ind$Region
    name<- "Season.Habitat"
  }
  if(f1 == "Region" & f2 == "Habitat"){
    factor<- interaction(ind$Region,ind$Habitat)
    fill<- ind$Season
    name<- "Region.Habitat"
  }
  ind<- data.frame(fill,factor, ind[4:5])
  
  bar<- ggplot(ind, aes(factor, mean,
                        fill = fill))+
    geom_col(position = "dodge",col = "black")+ #by dodge the fill factor will be side to side instead of stacked
    labs(x = name, y = as.character(index))+
    geom_errorbar(aes(ymin = mean-SD, ymax = mean+SD),
                  position = position_dodge2(width = 0.5, padding = 0.5)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_fill_manual(values = c("#E7298A", "#66A61E", "#E6AB02"))
  # scale_fill_brewer(palette = "Dark2")
  #scale_color_manual(values = c("darkblue", "#E6AB02"))
  #geom_line(aes(group = Region))
  
  ################ Boxplot #####.
  if(f1 == "Season" & f2 == "Region"){
    factor<- interaction(inds$Season,inds$Region)
    fill<- inds$Habitat
  }
  if(f1 == "Season" & f2 == "Habitat"){
    factor<- interaction(inds$Season,inds$Habitat)
    fill<- inds$Region
  }
  if(f1 == "Region" & f2 == "Habitat"){
    factor<- interaction(inds$Region,inds$Habitat)
    fill<- inds$Season
  }
  
  data<- data.frame(fill,factor, value = df[,colind])
  box<- ggplot(data, aes(factor, value, fill = fill))+
    geom_boxplot(position = "dodge")+
    labs(x = name, y = as.character(index))+
    scale_fill_manual(values = c("#7570b3", "#1b9e77", "#d95f02"))+
    # scale_fill_brewer(palette = "Dark2", direction = -1)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(list(bar,box))
  
}

################ mean and standard error of all indices #######################

df<- inds #or indsL

meanSAH<-list()
for (i in seq(ncol(inds1))){
  dum<-data.frame(index = df[,i],Season = inds$Season,
                  Region = inds$Region, Habitat = inds$Habitat)

  SAH<- dum %>%
    group_by(Season, Region, Habitat)%>%
    summarise(mean = mean(index, na.rm =T),SD = sd(index, na.rm =T))
meanSAH[[length(meanSAH)+1]]<- SAH
names(meanSAH)[length(meanSAH)]<- names(inds1[i])
}

################ bar plots and box plots for all 3 factors ##################

AllSAH<-list()
for (i in names(inds1)){
  AllSAH[[length(AllSAH)+1]]<- plotSAH(i)
  names(AllSAH)[length(AllSAH)]<-names(inds1[i])
}

######## editing x axis lable 
for (i in 1:ncol(inds1)) {
  AllSAH[[i]][[2]] <- AllSAH[[i]][[2]] + theme(axis.title.x = element_blank())
  AllSAH[[i]][[1]] <- AllSAH[[i]][[1]] + theme(axis.title.x = element_blank())
}

bar<- list()
for( i in c(1:ncol(inds1))){
  bar[[length(bar)+1]]<-  AllSAH[[i]][[1]]
}

box<- list()
for( i in c(1:ncol(inds1))){
 box[[length(box)+1]]<-  AllSAH[[i]][[2]]
}


 p1<- ggarrange(bar[[1]],bar[[2]],bar[[4]],bar[[5]],bar[[6]],bar[[3]],
          common.legend = T,
          labels = c("a",'b','c','d','e','f','g'),
          font.label = list(size = 14, color = "black", face = "plain", family = NULL),hjust = -1.4)

 p1
 p2<-
  ggarrange(box[[1]],box[[2]],box[[4]],box[[5]],box[[6]],box[[3]],
            common.legend = T,
               labels = c('a','b','c','d','e',' f'),
               font.label = list(size = 13, color = "black", face = "plain", family = NULL),
               hjust = -5.8, vjust = 1.8)
 p2
# SAHplots<- ggarrange(AllSAH[["S"]][[1]], AllSAH[["J"]][[1]], AllSAH[["H"]][[1]], 
#           AllSAH[["FRic"]][[1]], AllSAH[["FEve"]][[1]], AllSAH[["Rao"]][[1]],
#           common.legend = TRUE)
ggsave("figs/indices_bar.png", plot = p1, width = 10, height = 10)
ggsave("figs/indices_box.png", plot = p2, width = 10, height = 7)
 

###################### EcoQS#########################################

# EcoQplot<- ggarrange(AllSAH[["AMBI"]][[2]], AllSAH[["BENTIX"]][[2]], AllSAH[["M.AMBI"]][[2]], 
#                      common.legend = TRUE)


############################### correlation#############################

# #png("figs/corr.inds.png", width = 800, height = 1000)
# pairs.panels(inds1[names(inds1) != "S2"], gap=0,pch=20,method="spearman", 
#              ellipses = F, lm= T, density = F, hist.col ="#236573", 
#              jiggle = F,rug=F,smoother=F,stars=F,ci=F,alpha=.05,)
# #dev.off()



