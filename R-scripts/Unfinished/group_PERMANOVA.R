# libraries
library(vegan)
library(tidyverse)
library(pairwiseAdonis)

source("R-scripts/factors.R")
ab_raw<- read.csv("raw data/ab(grouped).csv", row.names = 1)
ab<- cbind(log(1+ab_raw), factors)

############ data filtering ######
# ######### seasonal filter
ab<- ab%>%
  filter(transect != "1", transect != "2", transect!= "7", transect!= "8")
# 
# # ######## filter bch out
# ab<- ab%>%
#   filter(habitat != "bch") #removing beach habitat

ab_raw<- ab_raw[row.names(ab_raw) %in% row.names(ab),]
factors<- factors[row.names(factors) %in% row.names(ab),]


############################ PERMANOVA & Pairwise #########################
tests <- function(df){

  ##################################### permanova
  perm<- list()
  for (i in names(select_if(df, is.numeric))){
    
    var<- df[c(i, 'season', 'area', 'habitat')]
    names(var)[1]<- "a"
    var<- na.omit(var)
    a<- as.matrix(var[1])
    # a <- as.matrix(var[1])
    per <- adonis2(a ~ season * area * habitat, data = var,
                   method = "euclidean", permutations = 9999)
    
    perm[[length(perm)+1]]<- per
    names(perm)[length(perm)]<- names(df[i])
    
  }
  
  ################################### pair wise test 
  ## solution two (more data in the function)
  pairwise<- list()
  for (i in names(select_if(df, is.numeric))){
    var<- na.omit(df[c(i,"season.area.habitat")])
    names(var)[1]<- "a"
    dis<- dist(var$a)
    pair <- pairwise.adonis(dis, var$season.area.habitat, p.adjust.m='fdr',
                            sim.method = "euclidean", perm = 9999)
    
    pairwise[[length(pairwise)+1]]<- pair
    names(pairwise)[length(pairwise)]<- names(df[i])
  }
  
  return(list(#anova = anov, Tukey = tuk, t_test = ttest,
    permanova = perm, pairwise = pairwise))
}

test0<- tests(ab)


############ selecting desired pairs and in order#################

tests0<- list()
tests0$permanova<- test0$permanova

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
for (ind in 1:length(test0$pairwise)){
  filtered_rows <- test0$pairwise[[ind]][
    test0$pairwise[[ind]][,1] %in% pair_ord,]
  
  tests0$pairwise[[ind]] <- filtered_rows[match(pair_ord, filtered_rows[, 1]), ]
}

names(tests0$pairwise)<- names(test0$pairwise)

############### excel output ############################################
library(openxlsx)

# Assuming test0 is the output list from the function tests
output <- tests0
# Create a new workbook
wb <- createWorkbook()

# Add PERMANOVA results to the workbook
addWorksheet(wb, "PERMANOVA")
permanova_data <- do.call(rbind, lapply(names(output$permanova), function(name) {
  df <- as.data.frame(output$permanova[[name]])
  df$Variable <- name
  return(df)
}))
writeData(wb, sheet = "PERMANOVA", permanova_data)


# Add pairwise test results to the workbook
addWorksheet(wb, "Pairwise")

pairwise_data <- do.call(rbind, lapply(names(output$pairwise), function(name) {
  df <- as.data.frame(output$pairwise[[name]])
  df$Variable <- name
  return(df)
}))
writeData(wb, sheet = "Pairwise", pairwise_data)

# Save the workbook to a file
## all
# saveWorkbook(wb, "output/group_PERMANOVA_-bch.xlsx", overwrite = TRUE)
# saveWorkbook(wb, "output/group_PERMANOVA.xlsx", overwrite = TRUE)
#   ## seasonal 
# saveWorkbook(wb, "output/group_PERMANOVA_S_-bch.xlsx", overwrite = TRUE)
saveWorkbook(wb, "output/group_PERMANOVA_S.xlsx", overwrite = TRUE)


