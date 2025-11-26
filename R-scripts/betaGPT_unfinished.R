# Required packages
library(tidyverse)
library(vegan)       # diversity(), specnumber(), vegdist()
library(betapart)    # turnover/nestedness partitioning
library(reshape2)    # data manipulation

# ==== 1. Load your data ====
# Assumed format: Site, Station, Replicate, Species1, Species2, ... SpeciesN
source("R-scripts/1.factors.r")

ab<- read.csv("input/abundance.csv", row.names = 1)
ab[is.na(ab)]<- 0.0
data <-  cbind(ab, factors)
# ==== 2. Aggregate replicates to site level ====
# First aggregate replicates to station level (mean abundance)
station_data <- data %>%
  group_by(Site, Station) %>%
  summarise(across(where(is.numeric), sum), .groups = "drop")

# Then aggregate stations to site level (mean abundance across stations)
site_data <- station_data %>%
  group_by(Site) %>%
  summarise(across(where(is.numeric), sum), .groups = "drop")

# ==== 3. Alpha diversity (per site) ====
alpha_richness <- specnumber(site_data[,-1]) # species richness
alpha_shannon  <- diversity(site_data[,-1], index = "shannon") # Shannon H'

alpha_df <- data.frame(
  Site = site_data$Site,
  Alpha_Richness = alpha_richness,
  Alpha_Shannon = alpha_shannon
)

# ==== 4. Gamma diversity ====
gamma_richness <- specnumber(colSums(site_data[,-1] > 0))
gamma_shannon  <- diversity(colSums(site_data[,-1]), index = "shannon")

# ==== 5. Whittaker multiplicative beta ====
beta_whittaker_richness <- gamma_richness / mean(alpha_richness)
beta_whittaker_shannon  <- gamma_shannon / mean(alpha_shannon)

# ==== 6. Additive partitioning beta ====
beta_additive_richness <- gamma_richness - mean(alpha_richness)
beta_additive_shannon  <- gamma_shannon - mean(alpha_shannon)

# ==== 7. Pairwise dissimilarities ====
# Jaccard and Sorensen (presence/absence)
site_pa <- decostand(site_data[,-1], method = "pa")
jaccard_dist <- vegdist(site_pa, method = "jaccard")
sorensen_dist <- vegdist(site_pa, method = "bray", binary = TRUE) # Bray binary = Sorensen

# Bray–Curtis (abundance)
bray_dist <- vegdist(site_data[,-1], method = "bray")

# ==== 8. Turnover & Nestedness (Baselga) ====
beta_multi <- beta.multi(site_pa, index.family = "jaccard")
# beta_multi$beta.JTU = turnover
# beta_multi$beta.JNE = nestedness
# beta_multi$beta.JAC = total beta

# ==== 9. Results summary ====
beta_summary <- list(
  Alpha = alpha_df,
  Gamma = data.frame(Gamma_Richness = gamma_richness, Gamma_Shannon = gamma_shannon),
  Beta_Whittaker = data.frame(Richness = beta_whittaker_richness, Shannon = beta_whittaker_shannon),
  Beta_Additive = data.frame(Richness = beta_additive_richness, Shannon = beta_additive_shannon),
  Beta_Pairwise = list(Jaccard = jaccard_dist, Sorensen = sorensen_dist, Bray_Curtis = bray_dist),
  Beta_Turnover_Nestedness = beta_multi
)

# ==== 10. Save results ====
saveRDS(beta_summary, "beta_diversity_results.rds")

# Optional: print a quick summary
beta_summary
