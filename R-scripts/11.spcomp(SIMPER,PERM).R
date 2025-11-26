#species and trait composition analysis
#SIMPER and PERMANOVA
#Pedram.ghrm@gmail.com
#6/11/2025

#libraries
library(vegan)
library(tidyverse)
library(pairwiseAdonis)

# inputdata
source('R-scripts/1.factors.R') #factors
ab<- read.csv("input/abundance.csv", row.names = 1)
ab[is.na(ab)]<- 0.0
ab<- log(1+ ab)


#gower distance for community-trait composition
# dist.t <- read.csv("output/dist.comXtrait(gowdis).csv", row.names = 1)
#community-trait matrix
# trait<- read.csv('output/comXtrait(LoGab).csv', row.names = 1)%>%
  # select(starts_with(c("s.",'FD','Mv','BT','T','FL','RT','LS')))
#community-species matrix


####### station selection #########

######### seasonal filter
# factors<- factors%>% filter(transect != "1", transect != "2", transect!= "7", transect!= "8")

######## selecting habitats
# factors<- factors%>% filter(habitat != "bch")

 # ab_raw<- ab_raw[row.names(ab_raw) %in% row.names(factors),]
# ab<- ab_raw%>%
#   select(-names(factors))
# dist.t<- as.dist(dist.t[rownames(dist.t) %in% rownames(factors),
#                     colnames(dist.t) %in% rownames(factors)])
# trait<- trait[rownames(trait) %in% rownames(factors),]



simp_sp<- simper(ab, factors$area)
perm_sp<- adonis2(ab ~ area*season, data = factors,
           method = "bray", permutations = 9999)
pairwise_sp <- pairwise.adonis(ab, ab_raw$season.area.habitat, p.adjust.m = "fdr", perm = 9999)

simp_trait<- simper(trait, factors$season.area.habitat)
perm_trait<- adonis2(dist.t ~ season * area * habitat, data = factors, permutations = 9999)
pairwise_t <- pairwise.adonis(dist.t, ab_raw$season.area.habitat, p.adjust.m = "fdr", perm = 9999)


simp_sp$crk.Tis.w_crk.Tis.s$cusum
simp_sp$mng.Tis.w_mng.Tis.s$cusum
# simp_sp$bch.Tis.w_bch.Tis.s$cusum
simp_sp$crk.Gw.w_crk.Gw.s$cusum
simp_sp$mng.Gw.w_mng.Gw.s$cusum
# simp_sp$bch.Gw.w_bch.Gw.s$cusum
simp_sp$crk.Tis.w_mng.Tis.w$cusum
# simp_sp$crk.Tis.w_bch.Tis.w$cusum
# simp_sp$mng.Tis.w_bch.Tis.w$cusum
simp_sp$crk.Tis.s_mng.Tis.s$cusum
# simp_sp$crk.Tis.s_bch.Tis.s$cusum
# simp_sp$mng.Tis.s_bch.Tis.s$cusum
simp_sp$crk.Gw.w_mng.Gw.w$cusum
# simp_sp$crk.Gw.w_bch.Gw.w$cusum
# simp_sp$mng.Gw.w_bch.Gw.w$cusum
simp_sp$crk.Gw.s_mng.Gw.s$cusum
# simp_sp$crk.Gw.s_bch.Gw.s$cusum
# simp_sp$mng.Gw.s_bch.Gw.s$cusum
simp_sp$crk.Tis.w_crk.Gw.w$cusum
simp_sp$mng.Tis.w_mng.Gw.w$cusum
# simp_sp$bch.Tis.w_bch.Gw.w$cusum
simp_sp$crk.Tis.s_crk.Gw.s$cusum
simp_sp$mng.Tis.s_mng.Gw.s$cusum
# simp_sp$bch.Tis.s_bch.Gw.s$cusum

simp_trait$crk.Tis.w_crk.Tis.s$cusum
simp_trait$mng.Tis.w_mng.Tis.s$cusum
# simp_trait$bch.Tis.w_bch.Tis.s$cusum
simp_trait$crk.Gw.w_crk.Gw.s$cusum
simp_trait$mng.Gw.w_mng.Gw.s$cusum
# simp_trait$bch.Gw.w_bch.Gw.s$cusum
simp_trait$crk.Tis.w_mng.Tis.w$cusum
# simp_trait$crk.Tis.w_bch.Tis.w$cusum
# simp_trait$mng.Tis.w_bch.Tis.w$cusum
simp_trait$crk.Tis.s_mng.Tis.s$cusum
# simp_trait$crk.Tis.s_bch.Tis.s$cusum
# simp_trait$mng.Tis.s_bch.Tis.s$cusum
simp_trait$crk.Gw.w_mng.Gw.w$cusum
# simp_trait$crk.Gw.w_bch.Gw.w$cusum
# simp_trait$mng.Gw.w_bch.Gw.w$cusum
simp_trait$crk.Gw.s_mng.Gw.s$cusum
# simp_trait$crk.Gw.s_bch.Gw.s$cusum
# simp_trait$mng.Gw.s_bch.Gw.s$cusum
simp_trait$crk.Tis.w_crk.Gw.w$cusum
simp_trait$mng.Tis.w_mng.Gw.w$cusum
# simp_trait$bch.Tis.w_bch.Gw.w$cusum
simp_trait$crk.Tis.s_crk.Gw.s$cusum
simp_trait$mng.Tis.s_mng.Gw.s$cusum
# simp_trait$bch.Tis.s_bch.Gw.s$cusum


testsL1<- list()
testsL1$perm_sp<- perm_sp
testsL1$perma_t<- perm_trait

pair_ord<- c(
  'crk.Tis.w vs crk.Tis.s','mng.Tis.w vs mng.Tis.s','bch.Tis.w vs bch.Tis.s', #seasonal Tis
  'crk.Gw.w vs crk.Gw.s', 'mng.Gw.w vs mng.Gw.s','bch.Gw.w vs bch.Gw.s',   #seasonal G
  'crk.Tis.w vs mng.Tis.w', 'crk.Tis.w vs bch.Tis.w','mng.Tis.w vs bch.Tis.w', #habitat T win
  'crk.Tis.s vs mng.Tis.s', 'crk.Tis.s vs bch.Tis.s','mng.Tis.s vs bch.Tis.s', #habitat T sum
  'crk.Gw.w vs mng.Gw.w','crk.Gw.w vs bch.Gw.w', 'mng.Gw.w vs bch.Gw.w', # hab G win
  'crk.Gw.s vs mng.Gw.s', 'crk.Gw.s vs bch.Gw.s', 'mng.Gw.s vs bch.Gw.s', #hab G sum
  'crk.Tis.w vs crk.Gw.w', 'mng.Tis.w vs mng.Gw.w', 'bch.Tis.w vs bch.Gw.w', # area win
  'crk.Tis.s vs crk.Gw.s', 'mng.Tis.s vs mng.Gw.s','bch.Tis.s vs bch.Gw.s' #area sum
)

  filtered_rows <- pairwise_sp[
    pairwise_sp[,1] %in% pair_ord,]
  testsL1$pairwise_sp <- filtered_rows[match(pair_ord, filtered_rows[, 1]), ]

filtered_rows <- pairwise_t[
  pairwise_t[,1] %in% pair_ord,]
testsL1$pairwise_t <- filtered_rows[match(pair_ord, filtered_rows[, 1]), ]

testsL1
