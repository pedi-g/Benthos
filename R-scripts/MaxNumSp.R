library(dplyr)
library(vegan)
ab_raw<- read.csv('raw data/ab.csv', row.names = 1)
ab1<- ab_raw
# ab2<- ab_raw[ab_raw$habitat !='bch',]
 
a<- colSums(ab1[2:73])
a<- as.data.frame(a)

ab <- ab1[-c(74:77)]%>%
   group_by( ab1$area)%>%
  summarise(across(everything(), sum, .names = "sum_{.col}")) %>%
  rowwise() %>%
  mutate(
    max_column = colnames(select(., starts_with("sum_")))[which.max(c_across(starts_with("sum_")))[1]],
    # the + numer at the end of the last line should change as the grouping changes
    max_value = max(c_across(starts_with("sum_")))  # Find the maximum value itself
  ) %>%
  ungroup()

# species with Maximum abundance
ab[(ncol(ab)-1):ncol(ab)]


ab<- as.data.frame(ab)
# # number of total specimens and species
# ab <- as.data.frame(ab1[-c(74:77)]%>%     # grouping 
#    group_by(ab1$habitat)%>%
#   summarise(across(everything(), sum, .names = "sum_{.col}")))
rownames(ab)<-paste(ab[[1]])#,'.',ab[[2]])
ab<- ab[-c(1)]

names(ab[,ab[1,]>0])
ncol(ab[,ab[3,]>0])

ncol(ab[,ab[1,]== 0 & ab[3,] == 0])
# number of speciments
sum(ab1[1:(ncol(ab1)-4)]) #with bch
# sum(ab2[1:(ncol(ab2)-4)]) # without bch

# number of species in each group
specnumber(ab)


sumrow<- rowSums(ab[1:73])
ab2<- ab[1:73]/rowSums(ab[1:73])

ab2<- t(ab2)
names(ab2[,ab2>0.2])
