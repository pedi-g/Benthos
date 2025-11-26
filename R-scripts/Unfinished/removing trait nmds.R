
#comparing each trait removed in nmds
#Pedram Ghahramani
#pedram.ghrm@gmail.com
#2-27-2024

#libraries----
library(tidyverse)
library(FD)

#input----
ab_raw<- read.csv("raw data/ab.csv", row.names = 1) # for factors
ab <- read.csv("raw data/ab(for traits).csv",row.names = 1) # abundance
sp.trait<- read.csv("raw data/spXtraits.csv", row.names = 1) # species-trait matirx

#manipulation---------
#factors
fact <- ab_raw[,(ncol(ab_raw)-3):ncol(ab_raw)]
ab_raw<- ab_raw[,-((ncol(ab_raw)-3):ncol(ab_raw))]
season<-fact[2]
area <- fact[3]
habitat <- fact[4]
season[season== "s"] <- "Summer"
season[season== "w"] <- "Winter"
area[area == "Gw"] <- "Gwatr"
habitat[habitat == "crk"]<- "Creek"
habitat[habitat == "mng"]<- "Mangrove"
habitat[habitat == "bch"]<- "Beach"
#making a combined factor of season and area
fact2<- NULL
for (i in 1:89) {
  fact2<- rbind(fact2, paste(season[i,1],area[i,1], sep = "." ))
  print(i)
}
fact1<- cbind(season, area, habitat, season.area = fact2)
fact1$habitat<- factor(fact1$habitat, level = c("Creek", "Mangrove","Beach"))


#-------------------gower distance for community-trait matrix----

# input and data prepration

com.trait <- as.data.frame( log(1+as.matrix(ab)) %*% as.matrix(sp.trait))


#computing distance
dis.com.trait<- (gowdis(com.trait[1])+#wieght
                   gowdis(com.trait[2:6])/max(gowdis(com.trait[2:6])) + #feeding
                   gowdis(com.trait[7:10])/max(gowdis(com.trait[7:10])) + # movement
                   gowdis(com.trait[11:13])/max(gowdis(com.trait[11:13])) + #living habitat
                   gowdis(com.trait[14:17])/max(gowdis(com.trait[14:17])) + #bioturbation
                   gowdis(com.trait[18]) + # shell toughness
                   gowdis(com.trait[19]) +# flexibility
                   gowdis(com.trait[20:22])/max(gowdis(com.trait[20:22])) + # larval developement
                   gowdis(com.trait[23:25])/max(gowdis(com.trait[23:25])) + # reproductive technique
                   gowdis(com.trait[26:29])/max(gowdis(com.trait[26:29])) # form
                 )/10

com.gow <- as.matrix(dis.com.trait)
#nmds
nmds<- metaMDS(com.gow)
mds<- cbind(as.data.frame(nmds$points), fact1)
stress<- round(nmds$stress,3)
# ploting
p<- ggplot(mds, aes(MDS1,MDS2))+
  theme_classic()+
  labs(x= "nMDS1", y= "nMDS2")+
  scale_color_manual( values=c("#264653", "#2a9d8f", "#e9c46a"))+
  theme( legend.title = element_text(size=7) ,legend.position = "none",
         legend.text = element_text(size = 6),
         axis.title = element_text(size= 14),
         axis.text = element_text(size = 12))+
  geom_point(aes(shape = season.area, color = habitat),stroke = 1, size = 2)+
  scale_shape_manual(values = c(1,2, 16, 17))+
  geom_text(x= Inf, y = Inf,vjust="inward",hjust="inward",label = paste("stress:",stress))+
  ggtitle("all");
p1 <- ggplot(mds, aes(MDS1, MDS2, color = season))+
    geom_point();
p2 <- ggplot(mds, aes(MDS1, MDS2, color = area))+
    geom_point();
p3 <- ggplot(mds, aes(MDS1, MDS2, color = habitat))+
    geom_point();

(bray<- ggarrange(p1, p2, p3, p))





ggsave("figs/traitmds/first5.png", plot = p, width = 8, height = 5)
