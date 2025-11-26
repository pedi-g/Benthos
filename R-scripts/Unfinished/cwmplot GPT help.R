#chat gpt help with cwm ploting

library(ggplot2)
library(dplyr)
library(tidyr)

#Create a sample data frame:
set.seed(123)
df <- data.frame(
  sample = 1:50,
  weight = runif(50, 50, 100),
  fuzzy1_1 = runif(50),
  fuzzy1_2 = runif(50),
  fuzzy1_3 = runif(50),
  fuzzy1_4 = runif(50),
  fuzzy2_1 = runif(50),
  fuzzy2_2 = runif(50),
  fuzzy2_3 = runif(50),
  fuzzy2_4 = runif(50),
  fuzzy3_1 = runif(50),
  fuzzy3_2 = runif(50),
  fuzzy3_3 = runif(50),
  fuzzy3_4 = runif(50),
  fuzzy3_5 = runif(50),
  fuzzy4_1 = runif(50),
  fuzzy4_2 = runif(50),
  fuzzy4_3 = runif(50),
  season = rep(c("w", "s"), 25),
  area = rep(c("tis", "gw"), each = 25),
  habitat = sample(c("crk", "mng", "bch"), 50, replace = TRUE)
)

#Melt the data for easier plotting:
df_melted <- df %>%
  pivot_longer(cols = starts_with("fuzzy"), names_to = "trait", values_to = "value")

plot_numerical_trait <- function(data, trait, factors) {
  ggplot(data, aes_string(x = factors, y = trait)) +
    geom_bar(stat = "identity") +
    facet_wrap(as.formula(paste("~", factors)), scales = "free_x") +
    labs(title = paste(trait, "by", factors), x = "Factors", y = trait)
}

plot_fuzzy_traits <- function(data, factors) {
  ggplot(data, aes_string(x = factors, y = "value", fill = "trait")) +
    geom_bar(stat = "identity", position = "stack") +
    facet_wrap(as.formula(paste("~", factors)), scales = "free_x") +
    labs(title = paste("Fuzzy Traits by", factors), x = "Factors", y = "Value")
}

factors_list <- c("season", "area", "habitat", "interaction(season, area)", "interaction(season, habitat)", "interaction(area, habitat)", "interaction(season, area, habitat)")

for (factors in factors_list) {
  print(plot_numerical_trait(df, "weight", factors))
}

for (factors in factors_list) {
  print(plot_fuzzy_traits(df_melted, factors))
}





# Assuming the fuzzy traits are named in a pattern
fuzzy_traits <- c("fuzzy1", "fuzzy2", "fuzzy3", "fuzzy4")

# Melting data for easier plotting
df_melted <- df %>%
  pivot_longer(cols = starts_with("fuzzy"), names_to = "trait", values_to = "value") %>%
  separate(trait, into = c("fuzzy", "modality"), sep = "_")

# Function to create plots for each fuzzy trait by a given factor
plot_fuzzy_by_factor <- function(data, fuzzy_trait, factor) {
  ggplot(data %>% filter(fuzzy == fuzzy_trait), aes_string(x = factor, y = "value", fill = "modality")) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = paste("Trait", fuzzy_trait, "by", factor), x = factor, y = "Value")
}

# Plot fuzzy traits by season
for (trait in fuzzy_traits) {
  print(plot_fuzzy_by_factor(df_melted, trait, "season"))
}

# Plot fuzzy traits by area
for (trait in fuzzy_traits) {
  print(plot_fuzzy_by_factor(df_melted, trait, "area"))
}

# Plot fuzzy traits by habitat
for (trait in fuzzy_traits) {
  print(plot_fuzzy_by_factor(df_melted, trait, "habitat"))
}

# Function to create plots for each fuzzy trait by the interaction of two factors
plot_fuzzy_by_interaction <- function(data, fuzzy_trait, factor1, factor2) {
  ggplot(data %>% filter(fuzzy == fuzzy_trait), aes(x = interaction(data[[factor1]], data[[factor2]]), y = value, fill = modality)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = paste("Trait", fuzzy_trait, "by", factor1, "and", factor2), x = paste(factor1, "and", factor2), y = "Value")
}

# Plot fuzzy traits by season and area
for (trait in fuzzy_traits) {
  print(plot_fuzzy_by_interaction(df_melted, trait, "season", "area"))
}

# Plot fuzzy traits by season and habitat
for (trait in fuzzy_traits) {
  print(plot_fuzzy_by_interaction(df_melted, trait, "season", "habitat"))
}

# Plot fuzzy traits by area and habitat
for (trait in fuzzy_traits) {
  print(plot_fuzzy_by_interaction(df_melted, trait, "area", "habitat"))
}

# Function to create plots for each fuzzy trait by the interaction of three factors
plot_fuzzy_by_all_factors <- function(data, fuzzy_trait) {
  ggplot(data %>% filter(fuzzy == fuzzy_trait), aes(x = interaction(season, area, habitat), y = value, fill = modality)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = paste("Trait", fuzzy_trait, "by Season, Area, and Habitat"), x = "Season, Area, and Habitat", y = "Value")
}

# Plot fuzzy traits by all three factors
for (trait in fuzzy_traits) {
  print(plot_fuzzy_by_all_factors(df_melted, trait))
}

