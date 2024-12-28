data <- read.csv(file.choose())
View(data)
data <- data[-1]
data <- data[-4]  

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data_n <- as.data.frame(lapply(data[1:5],
                               normalize))


#K-Nearest Neigbour Classification
data_n$Churn <- data$Churn

install.packages("caTools")
library(caTools)

set.seed(42)
split <- sample.split(data_n$Churn, SplitRatio = 0.7)
data_train <- subset(data_n, split == TRUE)
data_test <- subset(data_n, split == FALSE)

data_train_labels <- data_train$Churn
data_test_labels <- data_test$Churn

data_train <- data_train[-ncol(data_train)]
data_test <- data_test[-ncol(data_test)]

install.packages("class")
library(class)

data_test_pred <- knn(train = data_train, test = data_test, cl = data_train_labels, k = 22)
print(data_test_pred)

install.packages("gmodels")
library(gmodels)

# Confusion Matrix
CrossTable(x = data_test_labels, y = data_test_pred)

# Confusion Matrix using table
cm <- table(data_test_labels, data_test_pred)
print(cm)

# Add ggplot2 visualization for Confusion Matrix
install.packages("ggplot2")
library(ggplot2)

# Convert the confusion matrix to a data frame for ggplot2
cm_df <- as.data.frame(cm)
colnames(cm_df) <- c("Actual", "Predicted", "Count")

# Plotting the confusion matrix using ggplot2
ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "white", size = 6) +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  labs(title = "Confusion Matrix", x = "Predicted Labels", y = "Actual Labels") +
  theme_minimal()

# Prediction of Accuracy
Accuracy_KNN <- sum(data_test_pred == data_test_labels)/length(data_test_labels)
print(paste("Accuracy:", round(Accuracy_KNN * 100, 2), "%"))

Accuracy_KNN 

