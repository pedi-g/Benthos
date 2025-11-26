# data preperation

# input
factors <- read.csv("input/factors.csv", row.names = 1)
ab<- read.csv("input/abundance.csv", row.names = 1)


# factor preparation
factors <- data.frame(row.names = rownames(factors),
                      Proj = factor(factors$project),
                      Season = factor(factors$season2, levels = c("w", "s")),
                      Site = factor(factors$area, levels = c("Gwatr","Tis", 
                                                               "Gabrik", "Sorgalm", "Khalasi","Jask",
                                                               "Laft", "Gavarzin", "Soheili",
                                                               "Tabl", "Dorbani",
                                                               "Lashteghan", "Khamir")),
                      Habitat = factor(factors$hab 
                                       ,levels = c("creek", "mangrove", "beach")
                                       ),
                      Station = factor(factors$station),
                      Mud = factor(factors$mudstat),
                      Region = factor(factors$Subregion, levels = c('EO', 'WO','SH'))
                      
                      )

# combined factors
factors$Season.Site <- interaction(factors$Site, factors$Season)
factors$Season.Habitat <- interaction(factors$Season, factors$Habitat)
factors$Site.Habitat <- interaction(factors$Site, factors$Habitat)
factors$Season.Site.Habitat <- interaction(factors$Habitat, factors$Site, factors$Season)

factors$Season.Region <- interaction(factors$Region, factors$Season)
factors$Region.Habitat <- interaction(factors$Region, factors$Habitat)
factors$Season.Region.Habitat <- interaction(factors$Habitat, factors$Region, factors$Season)



# s <- as.character(row.names(factors))

# for(i in 1:length(s)){
#   if(s[i] == "13"){
#     s[i] <- "33"
#   }
#   if(s[i] == "23"){
#     s[i] <- "43"
#   }
# }

# m <- c()
# for (i in seq(length(s))) {
#   m <- append(m, substring(s[i], 2, 2))
# }
# factors$transect <- as.factor(m)



rm(ab
   # ,i,m,s
   )

