# R-code for Kappa analysis
# Jian-Yu DONG
# 
# Fisheries College, Guangdong Ocean University
# No. 1, Haida Road, Mazhang District, Zhanjiang, Guangdong, CHINA
# 
# Email: dongjianyu@gdou.edu.cn
# ResearchGate
# modified by pedram ghahramani
# 7-10-2024
#pedram.ghrm@gmail.com

rm(list=ls())
setwd("C:/Users/pedi/docs/[01]Academia/[01]research papers/[1]Mangrove papers/my paper/R-analysis/output")
#libraries ----
library(tidyverse)
library(openxlsx)
# install.packages('irr')
library(irr)



# input ----
# library(openxlsx) # Dong data 
# ## level_5
# df <- read.xlsx('0.indices.xlsx',3, rowNames = TRUE)

df <- read.csv('indices.csv', row.names = 1) # Ped's data
 
# data preperation ----
#index <- subset(df, select=c('AMBI', 'MAMBI', 'BENTIX', 'BO2A', 'jFD', 'FinalEcoQs')) # Dong data
index <- subset(df, select = c('AMBI', 'M.AMBI', 'BENTIX'))
index %>% head()
index <- na.omit(index)
# index2 <- df %>% dplyr::select(AMBI, MAMBI, BENTIX, BO2A, jFD, FinalEcoQs)
# index2 %>% head()

#################### clasication of each index #######################
######################## by predetermined boundaries##

##################### a function to make a vector of grouping for each trait.
classification<- function(ind, boundaries){
  # ind: a character value. it is the name of one of the index in the index data frame
  # boundaries: as a vector of values according to the selected index
  # eg. for AMBI : ind = "AMBI", boundaries = c(1.2, 3.3, 4.3, 5.5, 7)
  # boundaries for BENTIX: c(2, 2.5, 3.5, 4.5, 6)
  # boundaries for M.AMBI: c(0.2, 0.38, 0.53, 0.77, 1) 

  
  a<- index[colnames(index)==ind]
  ind_class<- c(0)
  
  for(i in 1:nrow(index)){
    
    if(a[i,]< boundaries[1]){
      if (ind == "AMBI"){
        ind_class[i] <- "high"
        }else{
      ind_class[i] <- "bad"}
      
      }else if(a[i,]< boundaries[2]){
        if (ind == "AMBI"){
          ind_class[i] <- "good"
        }else{
          ind_class[i] <- "poor"}
  
        }else if(a[i,]< boundaries[3]){
          if (ind == "AMBI"){
            ind_class[i] <- "moderate"
          }else{
            ind_class[i] <- "moderate"}
  
        }else if(a[i,]< boundaries[4]){
          if (ind == "AMBI"){
            ind_class[i] <- "poor"
          }else{
            ind_class[i] <- "good"}
  
        }else{
          if (ind == "AMBI"){
            ind_class[i] <- "bad"
          }else{
            ind_class[i] <- "high"}
        }
    }
    return(ind_class)
    }

AMBI_class<- classification("AMBI", c(1.2, 3.3, 4.3, 5.5))
M.AMBI_class<- classification("M.AMBI", c( 0.2, 0.38, 0.53, 0.77) )
BENTIX_class<- classification("BENTIX", c( 2, 2.5, 3.5, 4.5))
  
# }

index$AMBI<- AMBI_class
index$M.AMBI<- M.AMBI_class
index$BENTIX<- BENTIX_class

# index<- cbind(index, AMBI_class, M.AMBI_class, BENTIX_class)


# kappa ---------------

  ## Convert each column of index into a factor
k<- index
for(i in 1:ncol(index)){
  k[,i]<- factor(index[,i], levels = c('bad', 'poor', 'moderate', 'good', 'high'), ord = T)
}

k[,1]
k[,2]
k[,3]
# k <- mutate_if(index, is.character, as.factor)
k %>% head()
class(k$AMBI)
(AMBI_vs_MAMBI <- kappa2(k[,c(1:2)],"squared"))
# Loop calculation kappa value ----------
(index_name <- colnames(index))
index_name[1]
##
for(i in 1:(length(index)-1)) {
  j = i + 1
  for(j in j:length(index)) {

    # 2/5
  
    assign(paste0(index_name[i], '.vs.', index_name[j]), kappa2(index[,c(i,j)], "squared"))
  }
}
############################# ATENTION   POSIBILITY OF ERROR ########################################.
############### Check the names. they change according to the input file and the selected indices########-
############################# Check line 29 for the names ########################################################.

kappa <- as.data.frame(t(cbind(AMBI.vs.M.AMBI,
                               AMBI.vs.BENTIX,
                               # AMBI.vs.BO2A,
                               # AMBI.vs.jFD,
                               # AMBI.vs.FinalEcoQs,
                               M.AMBI.vs.BENTIX
                               # MAMBI.vs.BO2A,
                               # MAMBI.vs.jFD, 
                               # MAMBI.vs.FinalEcoQs,
                               # BENTIX.vs.BO2A,
                               # BENTIX.vs.jFD,
                               # BENTIX.vs.FinalEcoQs,
                               # BO2A.vs.jFD,
                               # BO2A.vs.FinalEcoQs,
                               # jFD.vs.FinalEcoQs
                               )))

kappa$value <- as.numeric(kappa$value)
kappa$p.value <- as.numeric(kappa$p.value)
kappa %>% head()
write.xlsx(kappa, file='000kappa.values20221206-2.xlsx', rowNames = TRUE, overwrite = TRUE)

############ Alteriantive way of saving as a csv file
# kappa$value <- as.numeric(kappa$value)
# kappa$p.value <- as.numeric(kappa$p.value)
# kappa$statistic <- as.numeric(kappa$statistic)
# kappa$raters <- as.numeric(kappa$raters)
# kappa$subjects <- as.numeric(kappa$subjects)
# kappa$method<- as.character(kappa$method)
# kappa$irr.name<- as.character(kappa$irr.name)
# kappa$stat.name<- as.character(kappa$stat.name)
# 
# write.table(as.data.frame(kappa), file="kappa.CSV", row.names = T)

# kappa classification levels -------------
for(i in 1:nrow(kappa)){
  if(kappa$value[i] > 0.99) {kappa$level[i] = 'Perfect'}
  else if(kappa$value[i] > 0.85) {kappa$level[i] = 'Excellent'}
  else if(kappa$value[i] > 0.70) {kappa$level[i] = 'Very good'}
  else if(kappa$value[i] > 0.55) {kappa$level[i] = 'good'}
  else if(kappa$value[i] > 0.40) {kappa$level[i] = 'Fair'}
  else if(kappa$value[i] > 0.20) {kappa$level[i] = 'Low'}
  else if(kappa$value[i] > 0.05) {kappa$level[i] = 'Very low'}
  else {kappa$level[i] = 'Null'}
}

# indices agreement ----------------
##
for(i in 1:(length(index)-1)) {
  j = i + 1
  for(j in j:length(index)) {
    assign(paste0(index_name[i], '.vs.', index_name[j]), agree(index[, c(i,j)]))
  }
}


####################### ATTENTION #### Same error potential as above (line 60) ########.
agree <- as.data.frame(t(cbind(AMBI.vs.M.AMBI,
                               AMBI.vs.BENTIX,
                               # AMBI.vs.BO2A,
                               # AMBI.vs.jFD,
                            # AMBI.vs.FinalEcoQs,
                           # All rights reserved!
                           #   Personal study use only, other use infringement will be prosecuted!
                           #   Copyright © Jian-Yu DONG
                           # 3/5
                               M.AMBI.vs.BENTIX
                               # MAMBI.vs.BO2A, 
                               # MAMBI.vs.jFD,
                               # MAMBI.vs.FinalEcoQs,
                               # BENTIX.vs.BO2A,
                               # BENTIX.vs.jFD, 
                               # BENTIX.vs.FinalEcoQs,
                               # BO2A.vs.jFD,
                               # BO2A.vs.FinalEcoQs,
                               # jFD.vs.FinalEcoQs
                           )))


agree$value <- as.numeric(agree$value)
agree %>% head()
# write.xlsx(agree, file='kappa.agree.xlsx', rowNames = TRUE, overwrite = TRUE)
kappa_agree <- data.frame(indices = row.names(kappa), kappa.value = kappa$value,
                          kappa_p.value=kappa$p.value, Level=kappa$level, agree.match = agree$value)
kappa_agree
write.xlsx(kappa_agree, file='000kappa.agree20221206.xlsx', overwrite = TRUE) #rowNames = TRUE,
#####---------------------------------------------------------------------#####
# two categories kappa analysis --------------
index2 <- data.frame()
for(i in 1:nrow(index)){
  for(j in 1:ncol(index)){
    if(index[i,j] == c('high')) {index2[i,j]=1}
    else if(index[i,j] == c('good')) {index2[i,j]=1}
    else index2[i,j] =0
  }
}
colnames(index2) <- colnames(index)
index2 %>% head()

## kappa -----
##
(index_name <- colnames(index2))
index_name[1]
for(i in 1:(length(index)-1)) {
  j = i + 1
  for(j in j:length(index2)) {
    assign(paste0(index_name[i], '.vs.', index_name[j]), kappa2(index2[,c(i,j)], "squared"))
  }
}

 ################################## POSIBLE ERROR ####################################.
kappa <- as.data.frame(t(cbind(AMBI.vs.M.AMBI,
                               AMBI.vs.BENTIX,
                               # AMBI.vs.BO2A, 
                               # AMBI.vs.jFD,
                                       # All rights reserved!
                                       #   
                                       #   Personal study use only, other use infringement will be prosecuted!
                                       #   
                                       #   Copyright © Jian-Yu DONG
                                       # 
                                       # 4/5
                               
                               # AMBI.vs.FinalEcoQs,
                               M.AMBI.vs.BENTIX
                               # MAMBI.vs.BO2A,
                               # MAMBI.vs.jFD,
                               # MAMBI.vs.FinalEcoQs,
                               # BENTIX.vs.BO2A,
                               # BENTIX.vs.jFD, 
                               # BENTIX.vs.FinalEcoQs,
                               # BO2A.vs.jFD,
                               # BO2A.vs.FinalEcoQs,
                               # jFD.vs.FinalEcoQs
                               )))

kappa$value <- as.numeric(kappa$value)
kappa$p.value <- as.numeric(kappa$p.value)
kappa %>% head()
# write.xlsx(kappa, file='kappa.values.xlsx', rowNames = TRUE, overwrite = TRUE)
# kappa classification levels -------------
for(i in 1:nrow(kappa)){
  if(kappa$value[i] > 0.99) {kappa$level[i] = 'Perfect'}
  else if(kappa$value[i] > 0.85) {kappa$level[i] = 'Excellent'}
  else if(kappa$value[i] > 0.70) {kappa$level[i] = 'Very good'}
  else if(kappa$value[i] > 0.55) {kappa$level[i] = 'good'}
  else if(kappa$value[i] > 0.40) {kappa$level[i] = 'Fair'}
  else if(kappa$value[i] > 0.20) {kappa$level[i] = 'Low'}
  else if(kappa$value[i] > 0.05) {kappa$level[i] = 'Very low'}
  else {kappa$level[i] = 'Null'}
}

# indices agreement ----------------
##
for(i in 1:(length(index)-1)) {
  j = i + 1
  for(j in j:length(index)) {
    assign(paste0(index_name[i], '.vs.', index_name[j]), agree(index2[, c(i,j)]))
  }
}
agree <- as.data.frame(t(cbind(AMBI.vs.M.AMBI, 
                               AMBI.vs.BENTIX,
                               # AMBI.vs.BO2A,
                               # AMBI.vs.jFD,
                               # AMBI.vs.FinalEcoQs,
                               M.AMBI.vs.BENTIX
                               # MAMBI.vs.BO2A,
                               # MAMBI.vs.jFD,
                               # MAMBI.vs.FinalEcoQs,
                               # BENTIX.vs.BO2A,
                               # BENTIX.vs.jFD, 
                               # BENTIX.vs.FinalEcoQs,
                               # BO2A.vs.jFD,
                               # BO2A.vs.FinalEcoQs,
                               # jFD.vs.FinalEcoQs
                               )))

# All rights reserved!
#   
#   Personal study use only, other use infringement will be prosecuted!
#   
#   Copyright © Jian-Yu DONG
# 
# 5/5

agree$value <- as.numeric(agree$value)
agree %>% head()
# write.xlsx(agree, file='kappa.agree.xlsx', rowNames = TRUE, overwrite = TRUE)
kappa_agree <- data.frame(indices = row.names(kappa), kappa.value = kappa$value,
                          kappa_p.value=kappa$p.value, agree.match = agree$value)
kappa_agree
# write.xlsx(kappa_agree, file='kappa.agree_2_class.xlsx', overwrite = TRUE) #rowNames = TRUE,
## End


