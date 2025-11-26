#Ped
#jan-2024
#visualization practice and ideas
library(psych)
library(tidyverse)

ind_l<- read.csv("output/indices(log).CSV", row.names = 1)
ind<- read.csv("output/indices.CSV", row.names = 1)

ind$habitat<- factor(ind$habitat, levels = c("crk", "mng", "bch"))
#boxplots ----
boxplot( H ~ habitat:season:area, data = ind)

par(mfrow = c(1,1))
boxplot( H ~ habitat:season, data = ind, subset = area == "Tis", sep = "-",
        col = "gray90", boxwex = 0.2, at= 1:6, ylim = c(0,2.5),xaxt = "n", xlab = NULL)
boxplot(H ~ habitat:season, data = ind, subset = area == "Gw", add= T,sep = "-",
        col = "gray50", boxwex = 0.2, at= 1:6-0.22 , xaxt = "n")
matplot( rep((par("usr")[2]+par("usr")[1])/2,15), seq(par("usr")[3],par("usr")[4],length.out= 15), 
         type = "c", add = T)
#legend(3.5,2.5, c("Tis", "Gwatr"), pch= "-_")
legend(5.3,2.5, c("Tis", "Gwatr"), fill= c("gray90","gray50"), cex = 0.7)
axis(1, at = seq(1,6, 1), labels= F)
text(1:6, par("usr")[3],labels = c("creek","mangrove", "beach"),srt = 0, pos = 1, xpd = T,cex = 0.7)
text(c(2,5), par("usr")[3]- 0.2,labels = c("Summer", "Winter"),srt = 0, pos = 1, xpd = T,cex = 1.1)



#scatter plots----
pairs(ind_l[1:5], pch = 20)
pairs(ind[1:5], pch = 20)

pairs.panels(ind_l[,c(1,2,4,5,8,9,12,13,14,15)],gap=0,
             pch=20,method="spearman", ellipses = F, lm= T,density=T, hist.col = "lightblue", jiggle = F,rug=F,
             smoother=F,stars=T,ci=T,alpha=.1,)

png("figs/ind.cor.pairs.png", width = 800, height = 1000)
pairs.panels(ind_l[,c(1,2,4,5,8,9,12)],gap=0,pch=20,method="spearman", 
             ellipses = F, lm= T, density = F, hist.col ="#408085", 
             jiggle = F,rug=F,smoother=F,stars=F,ci=F,alpha=.05,)
dev.off()


summary(lm(H~ J, ind))

plot(ind$Rao, ind$mpd)

mpdl<- ind_l$Rao/ ind_l$D
mpd<- ind$Rao/ ind$D
plot(ind$mpd,mpd)
plot(ind_l$mpd.L,mpd)
plot(mpd,mpdl)
plot(ind_l$mpd.L, ind_l$Rao)
plot(mpd, ind$Rao)
plot(ind$Rao, ind_l$Rao)
plot(ind_l$mpd.L,ind$mpd)


