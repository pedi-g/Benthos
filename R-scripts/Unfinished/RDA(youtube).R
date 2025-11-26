#RDA 
#pedram gahramani
#pedram.ghrm@gmail.com
#6-7-2024

#new script for RDA
#source: just one bird's opinion (youtube)

library(vegan)
library(ggplot2)
library(tidyverse)
#--------------------- data entry and prep -----------------------------------
env <- read.csv("raw data/env.csv", row.names = 1)
ab <- read.csv("raw data/ab.csv", row.names = 1)
trait<- read.csv("output/comxtrait(LOGab).csv", row.names = 1)
facts <- select(env, station, season, area, habitat)
env <- select(env ,-station, -habitat, - area, - season)
ab <- select(ab, -station, -habitat, - area, - season)

trait <- decostand(trait, method = "hellinger")
ab <- decostand(ab, method = "hellinger")
env<- env[rownames(ab),]
env<- na.omit(env)
ab<- ab[rownames(env),]
all.equal(rownames(ab), rownames(env))
all.equal(rownames(trait), rownames(env))
trait<- trait[rownames(env),]
#env.st<- decostand(env, method = "log")
env.st<- decostand(env, method = "standardize")

################### RDA ####################
spec.rda <- rda(trait ~ ., env.st)
summary(spec.rda)

############### ploting
model <- ordiplot(spec.rda, type = "none", scaling = 2, cex=10, 
                  xlab = "RDA1 (26.2%)", ylab = "RDA2 (9.8%)", cex.lab=1.25)
points(spec.rda, col="darkgrey", cex=1)
points(spec.rda, dis="sp", col="blue")
text(spec.rda, dis="sp", col="blue")
text(spec.rda, dis="bp", col="black")


#WAIT - We want to make sure we are using the best model with important terms
#A step function chooses the best variables to simplify the model
spec.rda1 <- step(spec.rda, scope=formula(spec.rda), test="perm")
summary(spec.rda1)

#Inspect for collinearity to create a simpler model
#I am omitting anything with a VIF greater than 20 (This is the usual value used, but adjust to your needs)
vif.cca(spec.rda1)
#Let's get rid of meadow percent 

spec.rda2 <- rda(trait ~ Temp + Sal + pH + clay+
                  + TOM, env.st)
summary(spec.rda2)

#check VIF again - OK!
vif.cca(spec.rda2)

#You can calculate the adjusted r-squared here
RsquareAdj(spec.rda2)

#ANOVA
anova(spec.rda2, perm.max=1000) #tells you if entire model is significant
anova(spec.rda2, by="axis", perm.max=1000) #tells you which axes are significant
anova(spec.rda2, by="terms", perm.max=1000) #tells you which terns are significant
anova(spec.rda2, by="margin", perm.max=1000) #tells you if the order of the terms is significant

summary(spec.rda2)

#Let's plot our final, simplified RDA! 
simplified_model <- ordiplot(spec.rda2, type = "none", scaling = 2, cex=10, xlab = "RDA1 (22.8%)", ylab = "RDA2 (7.2%)", cex.lab=1.25)
points(spec.rda2, col="darkgrey", cex=1)
points(spec.rda2, dis="sp", col="blue")
#text(spec.rda2, dis="sp", col="blue")
text(spec.rda2, dis="bp", col="black")

#Good luck and happy coding! 
#Check out my channel 'Just One Bird's Opinion' on YouTube if you have any questions! 





