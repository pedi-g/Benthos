
rm(list=ls())
# library(here)
# here::here()
library(openxlsx)
## level_5
df <- read.xlsx('0.indices.xlsx',3, rowNames = TRUE)
library(tidyverse)
index <- subset(df, select=c('AMBI', 'MAMBI', 'BENTIX', 'BO2A', 'jFD', 'FinalEcoQs'))
index %>% head()
# index2 <- df %>% dplyr::select(AMBI, MAMBI, BENTIX, BO2A, jFD, FinalEcoQs)
# index2 %>% head()
# install.packages('irr')
# kappa ---------------
library(irr)
## 把 index 每一列都转化为因子 (Convert each column of index into a factor)
k <- dplyr::mutate_if(index, is.character, as.factor)
k %>% head()
class(k$AMBI)
(AMBI_vs_MAMBI <- kappa2(k[,c(1:2)],"squared"))
# 循环计算 kappa value (Loop to calculate kappa value)----------
(index_name <- colnames(index))
index_name[1]
##
for(i in 1:(length(index)-1)) {
  j = i + 1
  for(j in j:length(index)) {
    # All rights reserved!
    #   Personal study use only, other use infringement will be prosecuted!
    #   Copyright © Jian-Yu DONG
    # 2/5
    assign(paste0(index_name[i], '.vs.', index_name[j]), kappa2(index[,c(i,j)], "squared"))
  }
}
kappa <- as.data.frame(t(cbind(AMBI.vs.MAMBI, AMBI.vs.BENTIX, AMBI.vs.BO2A, AMBI.vs.jFD,
                               AMBI.vs.FinalEcoQs,
                               MAMBI.vs.BENTIX, MAMBI.vs.BO2A, MAMBI.vs.jFD, MAMBI.vs.FinalEcoQs,
                               BENTIX.vs.BO2A, BENTIX.vs.jFD, BENTIX.vs.FinalEcoQs,
                               BO2A.vs.jFD, BO2A.vs.FinalEcoQs,
                               jFD.vs.FinalEcoQs)))
kappa$value <- as.numeric(kappa$value)
kappa$p.value <- as.numeric(kappa$p.value)
kappa %>% head()
write.xlsx(kappa, file='000kappa.values20221206-2.xlsx', rowNames = TRUE, overwrite = TRUE)
# kappa classification levels -------------
for(i in 1:nrow(kappa)){
  if(kappa$value[i] > 0.99) {kappa$level[i] = 'Perfect'}
  else if(kappa$value[i] > 0.85) {kappa$level[i] = 'Excellent'}
  else if(kappa$value[i] > 0.70) {kappa$level[i] = 'Very good'}
  else if(kappa$value[i] > 0.55) {kappa$level[i] = 'Good'}
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
agree <- as.data.frame(t(cbind(AMBI.vs.MAMBI, AMBI.vs.BENTIX, AMBI.vs.BO2A, AMBI.vs.jFD,
                               # AMBI.vs.FinalEcoQs,
                               # All rights reserved!
                               #   Personal study use only, other use infringement will be prosecuted!
                               #   Copyright © Jian-Yu DONG
                               # 3/5
                               MAMBI.vs.BENTIX, MAMBI.vs.BO2A, MAMBI.vs.jFD,
                               MAMBI.vs.FinalEcoQs,
                               BENTIX.vs.BO2A, BENTIX.vs.jFD, BENTIX.vs.FinalEcoQs,
                               BO2A.vs.jFD, BO2A.vs.FinalEcoQs,
                               jFD.vs.FinalEcoQs)))
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
kappa <- as.data.frame(t(cbind(AMBI.vs.MAMBI, AMBI.vs.BENTIX, AMBI.vs.BO2A, AMBI.vs.jFD,
                               # All rights reserved!
                               #   Personal study use only, other use infringement will be prosecuted!
                               #   Copyright © Jian-Yu DONG
                               # 4/5
                               AMBI.vs.FinalEcoQs,
                               MAMBI.vs.BENTIX, MAMBI.vs.BO2A, MAMBI.vs.jFD,
                               MAMBI.vs.FinalEcoQs,
                               BENTIX.vs.BO2A, BENTIX.vs.jFD, BENTIX.vs.FinalEcoQs,
                               BO2A.vs.jFD, BO2A.vs.FinalEcoQs,
                               jFD.vs.FinalEcoQs)))
kappa$value <- as.numeric(kappa$value)
kappa$p.value <- as.numeric(kappa$p.value)
kappa %>% head()
# write.xlsx(kappa, file='kappa.values.xlsx', rowNames = TRUE, overwrite = TRUE)
# kappa classification levels -------------
for(i in 1:nrow(kappa)){
  if(kappa$value[i] > 0.99) {kappa$level[i] = 'Perfect'}
  else if(kappa$value[i] > 0.85) {kappa$level[i] = 'Excellent'}
  else if(kappa$value[i] > 0.70) {kappa$level[i] = 'Very good'}
  else if(kappa$value[i] > 0.55) {kappa$level[i] = 'Good'}
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
agree <- as.data.frame(t(cbind(AMBI.vs.MAMBI, AMBI.vs.BENTIX, AMBI.vs.BO2A, AMBI.vs.jFD,
                               AMBI.vs.FinalEcoQs,
                               MAMBI.vs.BENTIX, MAMBI.vs.BO2A, MAMBI.vs.jFD,
                               MAMBI.vs.FinalEcoQs,
                               BENTIX.vs.BO2A, BENTIX.vs.jFD, BENTIX.vs.FinalEcoQs,
                               BO2A.vs.jFD, BO2A.vs.FinalEcoQs,
                               jFD.vs.FinalEcoQs)))
# All rights reserved!
#   Personal study use only, other use infringement will be prosecuted!
#   Copyright © Jian-Yu DONG
# 5/5
agree$value <- as.numeric(agree$value)
agree %>% head()
# write.xlsx(agree, file='kappa.agree.xlsx', rowNames = TRUE, overwrite = TRUE)
kappa_agree <- data.frame(indices = row.names(kappa), kappa.value = kappa$value,
                          kappa_p.value=kappa$p.value, agree.match = agree$value)
kappa_agree
# write.xlsx(kappa_agree, file='kappa.agree_2_class.xlsx', overwrite = TRUE) #rowNames = TRUE,
## End
# All rights reserved!
#   Personal study use only, other use infringement will be prosecuted!