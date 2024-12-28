# Importing the dataset
dataset <- read.csv(file.choose())
dataset <- dataset[-1]  
dataset <- dataset[-4]
View(dataset)

#Naive Bayes classifier

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Normalize specified columns
data_n <- as.data.frame(lapply(dataset[1:5], normalize))
data_n$Churn <- dataset$Churn

# Loading packages
library(e1071)
library(caTools)

# Splitting data into train and test data
set.seed(123)
split <- sample.split(data_n$Churn, SplitRatio = 0.7)
train_cl <- subset(data_n, split == TRUE)
test_cl <- subset(data_n, split == FALSE)

# Feature Scaling
train_scale <- scale(train_cl[, 1:5])
train_scale
test_scale <- scale(test_cl[, 1:5])
test_scale

# Fitting Naive Bayes Model to training dataset
classifier_cl <- naiveBayes(Churn ~ ., data = train_cl)
classifier_cl

# Predicting on test data
y_pred <- predict(classifier_cl, newdata = test_cl)
y_pred

# Confusion Matrix
cm <- table(test_cl$Churn, y_pred)
cm

# ggplot2 Visualization for Confusion Matrix
install.packages("ggplot2")
library(ggplot2)

# Convert the confusion matrix to a data frame for ggplot2
cm_df <- as.data.frame(cm)
colnames(cm_df) <- c("Actual", "Predicted", "Count")

# Plotting the confusion matrix using ggplot2
ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "red", size = 6) +
  scale_fill_gradient(low = "yellow", high = "lightblue") +
  labs(title = "Confusion Matrix", x = "Predicted Labels", y = "Actual Labels") +
  theme_minimal()


Accuracy_naive <- sum(diag(cm)) / sum(cm)
print(paste("Accuracy:", round(Accuracy_naive * 100, 2), "%"))
