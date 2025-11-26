# pedram@ghrm@gmail.com
#8-22-2024

# library(factoextra)
library(gawdis)
library(NbClust)
# library(gtools)
library(mclust)
library(vegan)
library(tidyverse)

sp.trait<- read.csv("raw data/spXtraits.csv", row.names = 1)
sp.trait.n<- read.csv("raw data/spXtraits(new).csv", row.names = 1)%>%
  select(starts_with(c('S.','MV','FD','BT','T','RT','LS')))
dis.sp.trait<- as.dist(read.csv("output/dist.spxtrait(gowdis).csv", row.names = 1))



c<- hclust(dis.sp.trait , method = "ward.D") # calculating

########################determining the number of groups and  members #################

          ##############using adonis and amova ##################.

# Initialize variables to store results
max_clusters <- 10  # Set this based on your data, could be higher or lower
explained_variance <- numeric(max_clusters - 1)
anosim_results <- numeric(max_clusters - 1)
# Loop through different numbers of clusters to determine the optimal number
for (k in 2:max_clusters) {
  # Create grouping factor based on the number of clusters
  groups <- cutree(c, k)
  # Perform AMOVA using adonis
  amova_result <- adonis(dis.sp.trait ~ factor(groups), data = sp.trait.n)
  anosim_result <- anosim(dis.sp.trait, factor(groups), permutations = 999)
  
  
  # Store the R-squared (explained variance) for each number of clusters
  explained_variance[k - 1] <- amova_result$aov.tab$R2[1]
  # Store the R statistic for each number of clusters
  anosim_results[k - 1] <- anosim_result$statistic
}

# Plot the explained variance to visually inspect the optimal number of clusters
plot(2:max_clusters, explained_variance, type = "b", xlab = "Number of Clusters",
     ylab = "Explained Variance (R2)",
     main = "Determining Optimal Number of Clusters")

plot(2:max_clusters, anosim_results, type = "b", xlab = "Number of Clusters", ylab = "ANOSIM R Statistic",
     main = "Determining Optimal Number of Clusters Using ANOSIM")


 ##################### using nbClust fucntion with various indices ############.
res.groups <- NbClust(
                      # data =
                      # sp.trait[-1],
                      sp.trait.n,
                      min.nc = 4, max.nc = 8,
                      diss = dis.sp.trait, distance = NULL,
                      method = "ward.D", index =
                        # "ch"
                      # "frey"
                      "mcclain"
                      # "cindex"
                      # "dunn"
                      # "silhouette"
)

########################## using MClust criteria ############

# Perform model-based clustering using Mclust
# Assuming 'sp.trait' is your data frame or matrix with functional traits
mclust_model <- Mclust(
  # sp.trait[-1],
  sp.trait.n,
  G = 5:10)

# Extract clustering results
best_model <- mclust_model$classification

max(best_model)
max(res.groups$Best.partition)

# Mc<- # choose one 
#    best_model                    # MClust
#  Nc<- res.groups$Best.partition     # nClust

num<- 
  6


  
 ############## normal dendrogram dendrogram ##########################

# Use rainbow colors for clusters
cluster_colors <- rainbow(num)

######### draw and save  
png("figs/sp_clust6.png", width = 1800, height = 900)

plot(
  c,
  main = "",
  sub = "",
  xlab = "",
  ylab = "",
  cex =1,
  cex.main = 1.5,  # Title size
  cex.lab = 1,   # Axis label size
  cex.axis = 1.5,  # Axis tick size
  hang = -1,       # Ensure leaves are aligned at the bottom
  yaxt = "n",
  lwd = 2
  # labels = FALSE   # Disable default labels
)


rect.hclust(c, k = num, border = cluster_colors)


legend(
  "topright",
  legend = paste("Group", 1:num), 
  fill = cluster_colors,
  border = NA,
  bty = "n",  # No box around legend
  cex = 2   # Legend text size
)

dev.off()

 ######################### circular dendrogram ###############################

# Load necessary libraries
library(dendextend)
library(circlize)
library(RColorBrewer)

# Assuming the hclust object 'c' is already calculated
# sp.trait <- read.csv("raw data/spXtraits(new).csv", row.names = 1)
# dis.sp.trait <- as.dist(read.csv("output/dist.spxtrait(gowdis).csv", row.names = 1))
# c <- hclust(dis.sp.trait, method = "ward.D")

# Convert hclust to dendrogram
dend <- as.dendrogram(c)



## colors
display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE, 
                   colorblindFriendly=T)
brewer<-brewer.pal(n = 11, name = 
                     # "Paired"
                     # "RdYlBu"
                    "Dark2"
                   )

a<- brewer[c(1,11,2,10,3,9
             ,4
             # ,8
             )]
a<- brewer[1:num]

a<- brewer[c(5,7,6,3,2,1)]
group_colors <-
  # c("#3CB371", "#FFD700", "#FFA500", "#FF69B4", "#1E90FF")  # Green, Yellow, Orange, Pink, Blue
 # c(  "#7570b3", "#1b9e77", "#d95f02", "#0072B2", "#CC79A7")
 # c("#1f77b4", "#aec7e8", "#ff7f0e", "#2ca02c", "#9467bd")
# c("#0072B2", "#E69F00", "#56B4E9", "#009E73", "#F0E442")
a
# Color branches based on 5 clusters with chosen colors
dend <- color_branches(dend, k = num, col = group_colors)

# Color labels based on clusters with the same colors
dend <- color_labels(dend, k = num, col = group_colors)

# Set branch thickness (line width)
dend <- set(dend, "branches_lwd", 4)  # Set the thickness to 3 (you can increase/decrease this as needed)

dend <- set(dend, "labels_cex", 3) 


png("figs/circular_dendrogram6.png", width = 2000, height = 2000)  # Adjust resolution if needed


# Plot the circular dendrogram using circlize with thicker lines
circlize_dendrogram(dend, labels_track_height = 0.4, dend_track_height = 0.5)

# Approximate angles for the group labels (determined based on the circular plot)
angles <- c(-35, -88, -125,-180, -220, 30)  # Degrees for each group label
radii <- 0.35 # Distance from the center (adjust if needed)


label_width <- 0.06  # Width and height of the square background (adjust as needed)
label_height <- 0.03

# Add group labels at specific angles and radii
for (i in 1:6) {
  # Convert angle and radius into x and y Cartesian coordinates
  x <- radii * cos(angles[i] * pi / 180)
  y <- radii * sin(angles[i] * pi / 180)
  
  # Draw a square rectangle as the background for the label
  rect(x - label_width, y - label_height, x + label_width, y + label_height, 
       col = a[i], border = NA, density = 20)  # Square background with the same color as the group
  
  # Add the text label at the calculated position
  text(x, y, labels = paste("Group", i), col = #group_colors[i]
         "gray0"
       , cex = 2.5, font = 2)
}

# Close the PNG file
dev.off()


################ output table ################################

group4<- cutree(c, 4)
group5<- cutree(c, 5)
group6<- cutree(c, 6)
group7<- cutree(c, 7)
group8<- cutree(c, 8)
group9<- cutree(c, 9)
group10<- cutree(c, 10)

sp.trait1<- cbind(group4, group5, group6, group7, group8, group9, group10, Mclust = best_model, nclust = res.groups$Best.partition, sp.trait.n)



#### Label clusters with species names if available in row names
# labels <- rownames(sp.trait)
# text(
#   x = 1:length(labels),
#   y = rep(-0.05 * max(c$height), length(labels)),  # Position labels below the plot
#   labels = labels,
#   srt = 90,  # Rotate labels
#   adj = c(1, 0),
#   xpd = TRUE,
#   cex = 0.6  # Adjust label size
# )


##############  histogram, cluster of other distance, correlation between mods ###############

# d<- hclust(dis , method = "ward.D")
# plot(d, main = "all" )
# res.groups <- NbClust(diss = dis, distance = NULL, min.nc = 3,
#                       max.nc = 10, method = "ward.D", index = "silhouette")
# rect.hclust(c, k = res.groups$Best.nc[1],
#             border = 1:res.groups$Best.nc[1])
#
#
# # hist(dis.sp.trait)
#
#
# # Calculate the correlation matrix
# corr <- cor(sp.trait[c(2:10, 14:19, 23:25, 30:36)])
# which(corr > 0.8 & corr < 1)
# # Find indices where the correlation is greater than 0.8 (excluding self-correlations)
# high_corr_indices <- which(corr > 0.6 & corr < 1, arr.ind = TRUE)
#
# # Display the names and values of these correlations
# if (nrow(high_corr_indices) > 0) {
#   for (i in 1:nrow(high_corr_indices)) {
#     row_name <- rownames(corr)[high_corr_indices[i, 1]]
#     col_name <- colnames(corr)[high_corr_indices[i, 2]]
#     corr_value <- corr[high_corr_indices[i, 1], high_corr_indices[i, 2]]
#
#     cat(sprintf("Correlation between %s and %s: %.2f\n", row_name, col_name, corr_value))
#   }
# } else {
#   cat("No correlations above 0.8 found.\n")
# }
#

final_groups<- 
  # best_model
  # res.groups$Best.partition
group6
agg<- 100*(aggregate(sp.trait.n, by=list(cluster=final_groups), mean)[-1])/5
write.csv(agg, "output/final_group6.csv")


######### export #################
write.csv(sp.trait1, "output/grouped.csv")






df<- sp.trait.n
km.res <- kmeans(df, 6, nstart = 25)
print(km.res)
agg<- aggregate(sp.trait.n, by=list(cluster=km.res$cluster), mean)
agg






df<- sp.trait.n

km.res <- kmeans(df, 5, nstart = 25)


group<- 
  # km.res$cluster
  # group4
  group6
  # best_model
  # res.groups$Best.partition


pca<- prcomp(df)
psum<-summary(pca)
perc1<- as.character(round(psum$importance[2,1],3)*100)
perc2<- as.character(round(psum$importance[2,2],3)*100)

pc<- as.data.frame(pca[["x"]])[1:2]
pc1<- cbind(pc, g = factor(group))
load<- data.frame(var = rownames(pca$rotation), pca$rotation) 

# i=1
# for(gr in c('a','b','c','d','e','f')){
#   pc1$g[pc1$gr == i] <- gra
#   i = i+1
# }


ggplot(pc1, aes(PC1,PC2))+
  geom_point(aes(color = g),
             stroke = 1, size = 2.2, alpha = 0.8)+
  theme_classic()+
  labs(x= paste("PC1","(",perc1,"%",")"),
       y= paste("PC2","(",perc2,"%",")"))




