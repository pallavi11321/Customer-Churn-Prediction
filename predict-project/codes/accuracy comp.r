# Assuming the accuracy values from your previous code
accuracy_knn <- 0.7566667 * 100
accuracy_svm <- 75.2
accuracy_random <- 75.2
accuracy_naive <- 75.33

# Create a data frame for model names and their corresponding accuracy values
accuracy_df <- data.frame(
  Model = c("K-Nearest Neighbors", "Support Vector Machine", "Random Forest", "Naive Bayes"),
  Accuracy = c(accuracy_knn, accuracy_svm, accuracy_random, accuracy_naive)
)

# Plotting the accuracy values using ggplot2
library(ggplot2)

ggplot(accuracy_df, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(Accuracy, 2), "%")), vjust = -0.3, color = "black", size = 5) +
  labs(title = "Accuracy Comparison of Different Classification Models", x = "Model", y = "Accuracy (%)") +
  theme_minimal() +
  theme(legend.position = "none")
