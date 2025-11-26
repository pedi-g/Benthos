
library(vegan)
# inputdata
source('R-scripts/factors.R') #factors

####### station selection #########

######### seasonal filter
# # factors<- factors%>% filter(transect != "1", transect != "2", transect!= "7", transect!= "8")
# 
# ######## selecting habitats
# factors<- factors%>% filter(habitat != "bch")
# 
# ab_raw<- ab_raw[row.names(ab_raw) %in% row.names(factors),]
# ab<- ab_raw%>%
#   select(-names(factors))

ab<- ab[rowSums(ab)>0,]
mass<- mass[rowSums(mass)>0,]
factors<- factors[rownames(factors) %in% rownames(ab),]


dis_ab<- vegdist(ab)
dis_mass<- vegdist(mass)

mantel_result <- mantel(dis_mass, dis_ab, method = "spearman")

# Display the result
mantel_result

