library(dplyr)
library(readr)
library(tidyr)
library(scales)
library(psych)
library(vdemdata)
library(reshape2)
library(ggplot2)

data <- data %>%
  filter(v2x_elecreg != 0)

data$gallagher <- ifelse(
  data$gallagher == "-", 
  999,
  data$gallagher
)

#mapping
chamber_mapping <- c("B" = 1, "U" = 0)
list_mapping <- c("O" = 3, "C-O" = 2, "C" = 1, "N" = 0, "-" = 0)
comp_fine_mapping <- c("0" = 1, "1" = 0)

data$chamber_type <- chamber_mapping[data$chamber_type]
data$list_type <- list_mapping[data$list_type]
data$fine <- comp_fine_mapping[data$fine]
data$comp_vote <- comp_fine_mapping[data$comp_vote]


indicators <- colnames(data[2:ncol(data)])
data[indicators] <- data[indicators] %>% mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
normalized_data <- data[indicators] %>% mutate(across(everything(), scales::rescale))
standardized_data <- data[indicators] %>% mutate(across(everything(), scale))
zero_variance_columns <- sapply(normalized_data, function(col) var(col, na.rm = TRUE) == 0)
filtered_data <- normalized_data[, !zero_variance_columns]
pca_model <- prcomp(filtered_data, center = TRUE, scale. = TRUE)
explained_variance <- summary(pca_model)$importance[2, ] # Proportion of variance explained
weights <- explained_variance
weights[17] <- weights[17] * (-1)
weights[21] <- weights[21] * (-1)
weights[22] <- weights[22] * (-1)
if (length(weights) < ncol(normalized_data)) {
  weights <- rep(weights, length.out = ncol(normalized_data))
}
weighted_data <- as.data.frame(sweep(normalized_data, 2, weights, `*`))

data$inclusiveness_score <- rowSums(weighted_data)
data$inclusiveness_score_geometric <- apply(weighted_data + 1e-9, 1, function(row) exp(sum(log(row))))

cat("Variance explained by PCA components:\n")
print(explained_variance)

if ("benchmark_index" %in% colnames(data)) {
  rmse <- sqrt(mean((data$benchmark_index - data$inclusiveness_score)^2, na.rm = TRUE))
  cat(sprintf("RMSE with benchmark index: %.3f\n", rmse))
}

data1 <- data %>%
  mutate(rank = rank(-inclusiveness_score)) %>%
  select(country_name, inclusiveness_score, inclusiveness_score_geometric, rank)

library(rnaturalearth) 
library(rnaturalearthdata)
library(sf)           

world <- ne_countries(scale = "medium", returnclass = "sf")
map_data <- world %>%
  left_join(ranking, by = c("name" = "country"))

ggplot(map_data) +
  geom_sf(aes(fill = inclusiveness_score)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80", name = "Inclusiveness Score") +
  theme_minimal() +
  labs(title = "World Map of Inclusiveness Scores",
       caption = "Grey regions show either no data or no elections in the country") +
  theme(legend.position = "bottom")
