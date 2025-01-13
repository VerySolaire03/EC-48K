loocv_results <- vector("numeric", nrow(normalized_data)) 
data <- data[indicators]

for (i in 1:nrow(data)) {
  train_data <- data[-i, ]
  test_data <- data[i, , drop = FALSE]
  
  pca_model <- prcomp(train_data, center = TRUE, scale. = TRUE)
  
  explained_var <- summary(pca_model)$importance[2, ]
  weights <- explained_variance
  
  weights[17] <- weights[17] * (-1)
  weights[21] <- weights[21] * (-1)
  weights[22] <- weights[22] * (-1)
  
  if (length(weights) < ncol(normalized_data)) {
    weights <- rep(weights, length.out = ncol(normalized_data))
  }
  
  weighted_data <- as.data.frame(sweep(train_data, 2, weights, `*`))

  test_weighted_data <- as.data.frame(sweep(test_data, 2, weights, `*`))
  inclusiveness_score <- rowSums(test_weighted_data)

  loocv_results[i] <- inclusiveness_score
}


library(ggplot2)
library(dplyr)
score_difference <- data[c("country_name", "score_difference", "loocv_inclusiveness_score", "inclusiveness_score")]
score_difference <- score_difference %>%
  filter(score_difference < 1)
                                  
data$score_difference <- data$inclusiveness_score - data$loocv_inclusiveness_score
ggplot(score_difference, aes(x = inclusiveness_score, y = score_difference)) +
  geom_point(color = "blue", alpha = 0.6) + 
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) + 
  labs(
    title = "Original Scores vs. LOOCV Differences",
    x = "Original Inclusiveness Scores",
    y = "Difference (Original - LOOCV Scores)",
    subtitle = paste("Correlation:", round(correlation, 2))
  ) +
  theme_minimal()

correlation <- cor(data$loocv_inclusiveness_score, data$inclusiveness_score)