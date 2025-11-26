# plotting the relative abundance of functional groups of species
#pedram.ghrm@gmail.com
#9/11/2024

library(tidyverse)
library(RColorBrewer)
library(paletteer)


source("R-scripts/factors.R")
ab_raw<- log(1+read.csv("raw data/ab(for traits).csv", row.names = 1))
groups<- read.csv('output/grouped.csv', row.names = 1)$group6

########## turning sp to f groups in abundance data ####
abT<- cbind(as.data.frame(t(ab_raw)), group =factor(groups))
                                                    
# sum of groups 
ab_sum<- abT%>%
  group_by(group)%>%
  summarise(across(everything(), sum))
ab_sum<- as.data.frame(ab_sum)

# changing the group names

for (i in 1:nrow(ab_sum)){
  rownames(ab_sum)[i]<- paste('G',as.roman(i))
}

# trasposing and binding  with factors
ab<- cbind(as.data.frame(t(ab_sum[-1])), factors) 


############# ploting ###################

#deleting beach
ab <- ab %>%
  filter(habitat != "bch")

# # ######### seasonal filter
ab<- ab%>%
  filter(transect != "1", transect != "2", transect!= "7", transect!= "8")


theme_set(theme_bw())

ab1<- ab%>%
  group_by(season.area.habitat)%>% # group by factor
  summarise(across(starts_with("G") ,sum))

ab2<-ab1 %>% # the mean
  mutate(sum = rowSums(ab1[2:(max(groups)+1)])) # sum 

g2<- ab%>%
  group_by(season.area.habitat)%>% # group by factor
  summarise(across(starts_with("G") ,list(sum = sum, sd = sd)))

  sum_l<- g2%>%
    pivot_longer(cols = ends_with("sum"), names_to = "group", values_to = "sums")
  
  sd_l<- g2%>%pivot_longer(cols = ends_with("sd"), names_to = "group", values_to = "sd")%>%
    mutate(group = factor(group))
  
  g_l<- as.data.frame(cbind(sums = sum_l$sums,sd =  sd_l$sd))
  g_l$season.area.habitat <- sum_l$season.area.habitat
  g_l$group<- sum_l$group
    

ab_l<- ab2 %>%
  pivot_longer(cols = starts_with("G"), names_to = "group", values_to = "value")%>%
  mutate(group = factor(group))
                        # ,levels = c('G II', 'G III', 'G IV', 'G V', 'G VI', 'G I') ))
ab_l$value<- ab_l$value/ab_l$sum


ab_l1<- ab_l%>%
  arrange(desc(value))
####### colors
display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE,
                   colorblindFriendly=F)
display.brewer.pal(n = 11, name =
                     "Reds"
                     # "PRGn"
                     # "RdYlBu"
                   # "Spectral"
                   )
brewer<-brewer.pal(n = 8, name = 
                     # "PRGn"
                     # "Set2"
                     # "RdYlBu"
                   # "Dark2"
                     # 'Reds'
                   'YlGnBu'
                   # "Spectral"
                   )
a<- brewer[c(5,6,4,3,2,1)]
a<- brewer[1:max(groups)]
perccols<- c('gray10','gray10','gray10','gray100','gray100','gray100')

my_colors <-
#   c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# c(  "#7570b3", "#1b9e77", "#d95f02", "#0072B2", "#CC79A7")
# c("#3CB371", "#FFD700", "#FFA500", "#FF69B4", "#1E90FF")
# c("#1f77b4", "#aec7e8", "#ff7f0e", "#2ca02c", "#9467bd")
#  c("#0072B2", "#E69F00", "#56B4E9", "#009E73", "#F0E442")
#  c("#0072B2", "#E69F00", "#D55E00","#56B4E9", "#009E73", "#F0E442")
 a
ggplot(ab_l, aes(x = season.area.habitat, y = value, fill = group)) +
  geom_bar(stat = "identity", col = "gray20",width = 0.8) +
  theme_bw(base_size = 13)+
  labs(x = "", y = "", fill = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8), legend.position = "top")+
  
  # scale_color_paletteer_d("nationalparkcolors::Acadia") +
  scale_fill_manual(values = my_colors)+
  # scale_fill_brewer(palette="Spectral", direction = -1)+
  # scale_fill_brewer(palette="Dark2")+
  theme(
    legend.position = "right",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.3, "cm") )+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label = scales::percent(value, accuracy = 1)),
            position = position_stack(vjust = 0.5), 
            size = 4, color = rep(perccols,8))
  


# ggsave("figs/Fgoups6.png", width = 7, height = 7)
 ggsave("figs/Fgoups6_-bch.png", width = 7, height = 9)

 
 ab3<- ab2[2:(ncol(ab2)-1)]
 
for(i in 1:nrow(ab3)){
  for(j in 1:ncol(ab3)){
    ab3[i,j]<- ab3[i,j]/ab2$sum[i]
  }
}

 # write.csv(ab, "output/ab_grouped.csv")
 # write.csv(ab3, "output/ab_relative_grouped_factored.csv") 
 
write.csv(ab, "output/ab_grouped_-bch.csv")
write.csv(ab3, "output/ab_relative_grouped_factored_-bch.csv")
 