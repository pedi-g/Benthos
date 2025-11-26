# Pedram.ghrm@gmail.com
# 8-17-2024
#sum of Phylumes of species

library(tidyverse)

#input
ab<- read.csv('raw data/ab_class.csv')[-2]
ab[-1]<- as.data.frame(sapply(ab[-1], as.numeric))
# ab[-1]<- log(1+ ab[-1])
source('R-scripts/factors.R')

### deleting bch
factors<- factors%>% filter(habitat != "bch")

#### deleting only winter stations
# factors<- factors%>%
#     filter(transect != "1", transect != "2", transect!= "7", transect!= "8")

ab<- ab[,colnames(ab) %in% row.names(factors)|colnames(ab) == "Phylum" ]

ab$Phylum <- factor(ab$Phylum)

# sum of abundances of each phylum
ab1<- ab%>%
  group_by(Phylum)%>%
  summarise(across(everything(),sum))

# transposing the abundance matrix
ab2<- as.data.frame(t(ab1[-1]))

names(ab2)<- as.character(ab1$Phylum)


 ab2<- cbind(ab2, season.area.habitat = factors$season.area.habitat)#%>%
#   filter(!grepl("bch", season.area.habitat)) # deleting bch

# sum of abudance for each group 
ab3<- ab2%>% 
  group_by(season.area.habitat)%>%
  summarise(across(everything(), sum))

# relative abundance to make every group total equal to one
a<- ab3[-1]
for (i in 1:nrow(ab3)){
  for( j in 1:ncol(a)){
    a[i,j]<- ab3[i,j+1]/sum(ab3[i,-1])
  }
}
ab4<- cbind(ab3[1], a)

# turning the data frame into a long table for ploting
long<- ab4 %>%
  pivot_longer(cols = names(ab4)[-1], names_to = "Phylum", values_to = "value")%>%
  mutate(Phylum = factor(Phylum))

long<- ab3 %>%
  pivot_longer(cols = names(ab4)[-1], names_to = "Phylum", values_to = "value")%>%
  mutate(Phylum = factor(Phylum))

# ggplot(long, aes(x =season.area.habitat , y = value, fill = Phylum)) +
#   geom_col( position ="dodge", col = "gray20")+
#   coord_polar("y", start=0)
# 
# ggplot(long, aes(x =season.area.habitat , y = value, fill = Phylum)) +
#   geom_bar( stat ="identity", col = "gray20")+
#   theme_bw(base_size = 13)+
#   labs(x = "", y = "", fill = "") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")+
#   #scale_fill_manual(values=cbPalette)
#   scale_fill_brewer(palette="15")+
#   theme(
#     legend.text = element_text(size = 8),
#     legend.title = element_text(size = 10))

plot<- ggplot(long, aes(x =season.area.habitat , y = value, fill = Phylum)) +
  geom_bar( stat ="identity", col = "gray20", width = 0.7) +
  theme_bw(base_size = 13)+
  labs(x = "", y = "", fill = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")+
  #scale_fill_manual(values=cbPalette)
  scale_fill_brewer(palette=16)+
  theme(
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10))

# plot

long$value<- long$value+1
p2<- ggplot(long, aes(x =season.area.habitat , y = value, fill = Phylum)) +
  geom_col(position = "dodge",col = "black") +
  theme_bw(base_size = 13)+
  labs(x = "", y = "", fill = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")+
  #scale_fill_manual(values=cbPalette)
  scale_fill_brewer(palette=16)+
  theme(
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10))+
scale_y_continuous(trans = 'log10')

p2

# ggsave("figs/phylum(-bch)log.png", plot = plot,width = 6, height = 5)
