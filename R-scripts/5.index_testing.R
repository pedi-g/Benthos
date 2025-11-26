

#### input and initial preparations
source('R-scripts/1.factors.R')
inds1 <- read.csv("output/indices.csv", row.names = 1)
inds<- cbind(inds1,factors)


######## filtration
#filter 1:
# inds<- inds[inds$S>1 & inds$Habitat != 'beach'& inds$Proj != 'Parima' & inds$Mud != 'far',]
#filter 2:
inds<- inds[inds$S>1 & inds$Habitat == 'creek'& inds$Proj == 'Parn',] # for parnian

inds1<- inds[,!colnames(inds) %in% colnames(factors)]
indsL <- log(1+ inds1)

#------------------------ Box plot ---------------------------------------
### chose factor and fill
factor <- inds$Station
fill <- inds$Mud
name<- "station"
###choose index
index <-  'H' 
### chose which data to use (logarithmic or not)
df <- inds1


colind <- which(colnames(inds) == index)
data<- data.frame(fill,factor, value = df[,colind])
box<- ggplot(data, aes(factor, value, fill = fill))+
  geom_boxplot(position = "dodge")+
  labs(x = name, y = as.character(index))+
  scale_fill_manual(values = c("#7570b3", "#1b9e77", "#d95f02"))+
  #scale_fill_brewer(palette = "Dark2", direction = -1)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



box
















