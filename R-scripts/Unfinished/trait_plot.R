#libraries
library(tidyverse)
library(vegan)
library(ggpubr)

traits <- read.csv("output/comXtrait(LOGab).csv",row.names = 1)
# cwm<- traits
###### factors and data filtering
source('R-scripts/factors.R')

traits1 <- cbind(traits,factors)#%>%
#filter(habitat != "bch")
traits<- traits[row.names(traits) %in% row.names(traits1),]
factors<- factors[row.names(factors) %in% row.names(traits1),]

# color palettes ----
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# The palette with grey:
cbPalette <- c(  "#7570b3", "#1b9e77", "#d95f02", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

theme_set(theme_bw())

################bar charts for CWM ##########################

######### non fuzzy traits ##########

nonfuzz<- function(trait,a){
  t<- traits1%>% 
    group_by(season, area, habitat)%>%
    select(a = starts_with(trait))%>%
    summarize(mean = mean(a, na.rm =T),SD = sd(a, na.rm =T))
  
  t$factors <- interaction(t$habitat, t$area, t$season)
  t$season.area <- interaction(t$area, t$season)
  
  ggplot(t, aes(x = season.area, y = t[[4]], fill = habitat ))+
    geom_bar(position = "dodge",stat = "identity", col = "gray20")+
    geom_errorbar(aes(ymin = mean-SD, ymax = mean+SD),
                  position = position_dodge2(width = 0.5, padding = 0.5)) +
    scale_fill_manual(values=cbPalette)+
    labs(x = "", y = trait)+
    theme_bw(base_size = 15)+
    theme(legend.text = element_text(size = 8),
          legend.title = element_text(size = 9),
          legend.key.size = unit(0.4, "cm"))
}

########### fuzzy traits ##############

# function for selecting each function and summarizing them according to selected factors
fdbar<- function(trait){

  fd <- traits1 %>% group_by(season.area.habitat)
  fd<- fd %>% select(starts_with(trait))

 
  
  fd<- fd%>%
    summarise(across(everything(), list(sum), .names = "{col}"))
  
   fd[2:ncol(fd)]<- fd[2:ncol(fd)]/rowSums(fd[2:ncol(fd)])
  
  if (nrow(fd) != 1){
    names(fd)[1] <- "factor" 
    
  }
  
  if(ncol(fd) == 1){
    names(fd)[1] <- "factor"
    fd_l <- fd
    fd_l$n <- 1
    plot <- ggplot(fd_l, aes( x = 1, y = factor)) +
      geom_bar(stat = "identity", col = "gray20") +
      theme_bw(base_size = 16)+
      labs(x = "", y = "", fill = "") +
      theme_update(legend.position = "top")+
      theme(
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 10)
      )+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else{
    fd_l<- fd %>%
      pivot_longer(cols = starts_with(trait), names_to = "traits", values_to = "value")%>%
      mutate(traits = factor(traits))
    fd_l$value<- fd_l$value/5
    
    if(nrow(fd) == 1 ) {
      plot<- ggplot(fd_l, aes(x = "all", y = value, fill = traits)) +
        geom_bar(stat = "identity", col = "gray20") +
        theme_bw(base_size = 16)+
        theme(
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 10)
        )+
        labs(x = "", y = "", fill = "") +
        theme_update(legend.position = "top") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        #scale_fill_manual(values=cbPalette)
        scale_fill_brewer(palette="Spectral")
      
    } else {
      plot<- ggplot(fd_l, aes(x = factor, y = value, fill = traits)) +
        geom_bar(stat = "identity", col = "gray20") +
        theme_bw(base_size = 13)+
        labs(x = "", y = "", fill = "") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8), legend.position = "top")+
        #scale_fill_manual(values=cbPalette)
        scale_fill_brewer(palette="Spectral")+
        theme(
          legend.text = element_text(size = 6.5),
          legend.title = element_text(size = 8),
          legend.key.size = unit(0.3, "cm") 
        )
    }
  }
  
  return(list(fd_l, plot))
}

############ output ###############################################
w<- nonfuzz("w")
fl<- nonfuzz("FL")
tn<- nonfuzz("TN")

FD<- fdbar("FD")[[2]]
HB<- fdbar("HB")[[2]]
BT<- fdbar("BT")[[2]]
LD<- fdbar("LD")[[2]]
RT<- fdbar("RT")[[2]]
FR<- fdbar("FR")[[2]]
MV<- fdbar("MV")[[2]]
S<- fdbar("s.")[[2]]
LS<- fdbar("ls")[[2]]
# SD<- fdbar("SD")[[2]]

g1<- ggarrange( w,tn,
                # ggplot() + theme_void(),
                nrow = 1, common.legend = T)
# widths = c(1,2,2,1))
g2<- ggarrange(MV,BT,RT,LS, nrow = 2, ncol = 2)

g3<- ggarrange( ggplot() + theme_void(),
                FD,
                ggplot() + theme_void(),
                nrow = 1, common.legend = T,
                widths = c(1,2,1))
ggarrange(g1,g2,g3,
          # LD,RS,SD,
          ncol = 1, nrow = 3, heights = c(1,2.5,1.2))

ggsave("figs/traits_new.png", width = 11, height = 13)
