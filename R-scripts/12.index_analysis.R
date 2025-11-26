# pedram.ghrm@gmail.com
# analysing significancy for fucntional and species indices
# last edit 8-22-2024

library(vegan)
library(tidyverse)
library(pairwiseAdonis)
##### input #######
inds <- read.csv("output/indices.csv", row.names = 1)#%>%
  #select(-Dm, -D ,-AMBI, - BENTIX, -M.AMBI, -Status)
# indsL <- read.csv("output/indices(log).csv", row.names = 1)#%>%
  #select(-Dm, -D ,-AMBI, - BENTIX, -M.AMBI, -Status)

len<- ncol(inds)

source('R-scripts/1.factors.R') # factors
# inds<- inds%>% select(-Habitat, -Site,- station, -Season)
# indsL<- indsL%>% select(-Habitat, -Site,- station, -Season)
inds<- cbind(inds,factors)
# indsL<- cbind(indsL,factors)

inds <- inds %>%
  filter(Site %in% c('Sorgalm', 'Jask', 'Khalasi', 'Gabrik'), Habitat != 'beach')

######## manipulation ###########

######### seasonal filter
# indsL<- indsL%>%
#   filter(transect != "1", transect != "2", transect!= "7", transect!= "8")

# ######## filter bch out
# inds<- inds%>%
#   filter(Habitat != "bch") #removing beach Habitat


# eliminating stations with richness of one (all the species) for species diversity
inds<- inds[inds$S>1,]
# indsL<- indsL[indsL$S>1,]

############### outliers ##################################
## Function to detect outliars using Z-scores
# out <- function(df, factor = "Season.Site.Habitat", threshold = 2) {
#   groups<- unique(df[[factor]])
#   
#   outliars<- list()
#   for (i in groups){
#     outliar <- df[df[[factor]] == i,]%>%
#       select(1:13)%>%
#       mutate(across(everything(), ~ ifelse(abs(scale(.)) > threshold, ., NA)))
#     
#     outliars[[as.character(i)]] <- outliar
# 
#     # Get non-NA values and their indices
#     non_na_indices <- which(!is.na(outliar), arr.ind = TRUE)
#     non_na_values <- data.frame(
#       row = rownames(outliar)[non_na_indices[, 1]],
#       column = colnames(outliar)[non_na_indices[, 2]],
#       value = outliar[non_na_indices])
#     
#     outliars[[paste(as.character(i),"1")]] <- non_na_values
#     
#   }
#   
#   # all of the variables across all stations
#   all<- df%>%
#     select(1:13)%>%
#     mutate(across(everything(), ~ ifelse(abs(scale(.)) > threshold, ., NA)))
#   
#   outliars[["all"]] <- all
#   
#   non_na_indices <- which(!is.na(all), arr.ind = TRUE)
#   non_na_values <- data.frame(
#     row = rownames(all)[non_na_indices[, 1]],
#     column = colnames(all)[non_na_indices[, 2]],
#     value = all[non_na_indices])
#   
#   outliars[["all1"]] <- non_na_values
#   
#   #plotting all of the variables across all stations
#   f<- df%>%
#     select(1:13)
#   data_long <- f %>%
#     pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
#   
#   outliars[["plot"]]<- ggplot(data_long, aes(x = Variable, y = Value)) +
#     geom_boxplot() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
#   
#   return(outliars)
# }
# 
# out(inds, threshold = 2)$all1
# out(inds, threshold = 2.5)$all1
#   
# indsL[row.names(indsL) == "C313_s",]$FEve <- NA
# # indsL[row.names(indsL) == "C513_s",]$S <- NA
# # indsL[row.names(indsL) == "C312_w",]$J <- NA
# # indsL[row.names(indsL) == "C423_w",]$J <- NA
# indsL[row.names(indsL) == "C311_w",]$FEve <- NA
# 
# # 
# # indsL[row.names(indsL) == "C222_w",]$FEve <- NA
# # 
# # indL

########### normality ###########################
###### a function that gives the groups that are not normally distributed

notnorm<- function(df){    
  all<- list()
  for(var in c(1:len)){# the outer loop for chosing each variable
    
    l<- data.frame()
    for(group in unique(df$Season.Site.Habitat)){ # the inner loop chooses each of the groups
        if (nrow(na.omit(df[df$Season.Site.Habitat == group,][var]))>2){
      
      data<- df[df$Season.Site.Habitat == group,var]
      a <- shapiro.test(data)
      l[nrow(l)+1,1]<- a[[1]]
      l[nrow(l),2]<- a[[2]]
      rownames(l)[nrow(l)] <- as.character(group) 
      
      #     l[[length(l)+1]]<- as.data.frame(w = a[[1]], p = a[[2]])
      # names(l)[length(l)]<- names(group)
        }
    }
    names(l) <- c("w", "p-value")
    all[[length(all)+1]]<- l
    names(all)[length(all)]<- names(df[var])
  }
  
#printing out the non normal groups
  nons<- data.frame()
for( n in 1:length(all)){
  for (x in 1:nrow(all[[n]])){
    if (all[[n]]$`p-value`[x]<0.05){
      nons<- rbind(nons,cbind(index = names(all)[n],all[[n]][x,]))
      # print(names(all)[n])
      # print(all[[n]][x,])
    }
  }
}
  return(list(all,nons))
}  

notnorm(inds)[[2]]
notnorm(cbind(log(1+inds[sapply(inds, is.numeric)]),inds[!sapply(inds, is.numeric)]))[[2]] # not transformed #res: s,J,AMBI,BENTIX
# notnorm(indsL)[[2]] # log transformed #res: s,H, Dm, Rao, AMBI, BENTIX,


# hist(indsL[indsL$Season.Site.Habitat == "mng.Gw.w",]$J)
# shapiro.test(indsL[indsL$Season.Site.Habitat == "mng.Gw.w",]$J)

################### tests of significancy #################################
tests <- function(df){
  ######################### ANOVA and Tukey's 
  anov<- list()
  tuk<- list()
  ttest<- list()
  for (i in names(df[1:len])){
    var<- df[c(i, 'Season', 'Site', 'Habitat', 'Season.Site.Habitat')]
    names(var)[1]<- "a"
    
    an<- aov(a ~ Season * Site * Habitat, data = var)
    tu<- TukeyHSD(an)
    t<- pairwise.t.test(var$a, var$Season.Site.Habitat, p.adjust.method = "none")
    
    anov[[length(anov)+1]]<- an
    tuk[[length(tuk)+1]]<- tu
    ttest[[length(ttest)+1]]<- t
    
    names(anov)[length(anov)]<- names(df[i])
    names(tuk)[length(tuk)]<- names(df[i])
    names(ttest)[length(ttest)]<- names(df[i])
}



############ post hoc for normal distribution
# 1. tukey's Honest Significant Difference:
# Ideal for comparing all possible pairs of groups.

# 2.Bonferroni Correlation
# A more conservative approach that adjusts p-values to control the family-wise error rate.
# pairwise.t.test(df$H, df$Season.Site.Habitat, p.adjust.method = "bonferroni")
# print(pairwise_result)

# Scheffé's Method:
# Useful when you have unequal sample sizes or are making many comparisons.
# library(DescTools)
# scheffe_result <- ScheffeTest(aov_result)
# print(scheffe_result)

##################################### permanova
perm<- list()
for (i in names(df[1:len])){
# 
#   df<- indsL
#   i<- "FEve"
  var<- df[c(i, 'Season', 'Site', 'Habitat')]
  
  names(var)[1]<- "a"
  var<- na.omit(var)
  a<- as.matrix(var[1])

  per <- adonis2(a ~ Season * Site * Habitat, data = var,
                 method = "euclidean", permutations = 9999)

  perm[[length(perm)+1]]<- per
  names(perm)[length(perm)]<- names(df[i])

}

################################### pairwise test 
## solution two (more data in the function)
pairwise<- list()
for (i in names(df[1:len])){
  var<- na.omit(df[c(i,"Season.Site.Habitat")])
  names(var)[1]<- "a"
  dis<- dist(var$a)
  pair <- pairwise.adonis(dis, var$Season.Site.Habitat, p.adjust.m='none',
                          sim.method = "euclidean", perm = 9999)

  pairwise[[length(pairwise)+1]]<- pair
  names(pairwise)[length(pairwise)]<- names(df[i])
}

return(list(anova = anov, Tukey = tuk, t_test = ttest,permanova = perm, pairwise = pairwise))
}

# tests0<- tests(inds)
testsL<- tests(inds)

############ selecting desired pairs and in order#################

testsL1<- list()
testsL1$anova<- testsL$anova
testsL1$permanova<- testsL$permanova

pair_ord<- c(
  # 'crk.Tis.w vs crk.Tis.s','mng.Tis.w vs mng.Tis.s','bch.Tis.w vs bch.Tis.s', #Seasonal Tis
  # 'crk.Gw.w vs crk.Gw.s', 'mng.Gw.w vs mng.Gw.s','bch.Gw.w vs bch.Gw.s',   #Seasonal G
  # 'crk.Tis.w vs mng.Tis.w', 'crk.Tis.w vs bch.Tis.w','mng.Tis.w vs bch.Tis.w', #Habitat T win
  # 'crk.Tis.s vs mng.Tis.s', 'crk.Tis.s vs bch.Tis.s','mng.Tis.s vs bch.Tis.s', #Habitat T sum
  # 'crk.Gw.w vs mng.Gw.w','crk.Gw.w vs bch.Gw.w', 'mng.Gw.w vs bch.Gw.w', # hab G win
  # 'crk.Gw.s vs mng.Gw.s', 'crk.Gw.s vs bch.Gw.s', 'mng.Gw.s vs bch.Gw.s', #hab G sum
  # 'crk.Tis.w vs crk.Gw.w', 'mng.Tis.w vs mng.Gw.w', 'bch.Tis.w vs bch.Gw.w', # Site win
  # 'crk.Tis.s vs crk.Gw.s', 'mng.Tis.s vs mng.Gw.s','bch.Tis.s vs bch.Gw.s' #Site sum
)

  for (ind in 1:len){
    filtered_rows <- testsL$pairwise[[ind]][
      testsL$pairwise[[ind]][,1] %in% pair_ord,]
    
    testsL1$pairwise[[ind]] <- filtered_rows[match(pair_ord, filtered_rows[, 1]), ]
  }
names(testsL1$pairwise)<- names(testsL$pairwise)

tuk_ord <- c(
  # 's:Tis:crk-w:Tis:crk', 's:Tis:mng-w:Tis:mng', 's:Tis:bch-w:Tis:bch', #Seasonal Tis
  # 's:Gw:crk-w:Gw:crk', 's:Gw:mng-w:Gw:mng', 's:Gw:bch-w:Gw:bch',#Seasonal G
  # 'w:Tis:mng-w:Tis:crk', 'w:Tis:bch-w:Tis:crk', 'w:Tis:bch-w:Tis:mng',#Habitat T win
  # 's:Tis:mng-s:Tis:crk', 's:Tis:bch-s:Tis:crk', 's:Tis:bch-s:Tis:mng',#Habitat T sum
  # 'w:Gw:mng-w:Gw:crk', 'w:Gw:bch-w:Gw:crk', 'w:Gw:bch-w:Gw:mng',# hab G win
  # 's:Gw:mng-s:Gw:crk', 's:Gw:bch-s:Gw:crk', 's:Gw:bch-s:Gw:mng', #hab G sum
  # 'w:Gw:crk-w:Tis:crk', 'w:Gw:mng-w:Tis:mng', 'w:Gw:bch-w:Tis:bch', # Site win
  # 's:Gw:crk-s:Tis:crk', 's:Gw:mng-s:Tis:mng', 's:Gw:bch-s:Tis:bch'#Site sum
)

for (ind in names(inds[1:len])){
  
  filtered_rows <- testsL$Tukey[[ind]][[7]][
    row.names(testsL$Tukey[[ind]][[7]]) %in% tuk_ord,]
  testsL1$Tukey[[ind]]<- filtered_rows[match(tuk_ord, row.names(filtered_rows)), ]
}

testsL1$permanova$FEve
testsL1$pairwise$FEve

############### excel output ############################################
library(openxlsx)

# Assuming testsL is the output list from the function tests
output <- testsL1

# Create a new workbook
wb <- createWorkbook()

# Add ANOVA results to the workbook
addWorksheet(wb, "ANOVA")
anova_data <- do.call(rbind, lapply(names(output$anova), function(name) {
  anova_summary <- summary(output$anova[[name]])
  df <- as.data.frame(anova_summary[[1]])
  df$Variable <- name
  df$RowName <- rownames(df)
  return(df)
}))
writeData(wb, sheet = "ANOVA", anova_data)

# Add Tukey test results to the workbook
addWorksheet(wb, "Tukey")
tukey_data <- do.call(rbind, lapply(names(output$Tukey), function(name) {
  tukey_summary <- output$Tukey[[name]]
  df <- as.data.frame(tukey_summary)
  df$Variable <- name
  df$RowName <- rownames(df)
  return(df)
}))
writeData(wb, sheet = "Tukey", tukey_data)

# Add PERMANOVA results to the workbook
addWorksheet(wb, "PERMANOVA")
permanova_data <- do.call(rbind, lapply(names(output$permanova), function(name) {
  df <- as.data.frame(output$permanova[[name]])
  df$Variable <- name
  df$RowName <- rownames(df)
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

# Save the workbook to a file ####

# saveWorkbook(wb, "output/ind_anal.xlsx", overwrite = TRUE) # all
# saveWorkbook(wb, "output/ind_anal_s.xlsx", overwrite = TRUE) # Seasonal

saveWorkbook(wb, "output/ind_anal_-bch.xlsx", overwrite = TRUE) # all -beach
# saveWorkbook(wb, "output/ind_anal_s_-bch.xlsx", overwrite = TRUE) #Seasonal -beach

