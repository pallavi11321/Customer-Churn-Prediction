dataset <- read.csv(file.choose())
dataset <- dataset[-1]
dataset <- dataset[-4]
View(dataset)


#Support Vector Machine Classification

dataset$Churn <- factor(dataset$Churn, levels = c(0, 1))

install.packages('caTools')
library(caTools)
set.seed(123)
split <- sample.split(dataset$Churn, SplitRatio = 0.75)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

train_scale <- scale(training_set[, 1:5])
train_scale
test_scale <- scale(test_set[, 1:5])
test_scale

# Fitting SVM to the Training set
# install.packages("e1071")
library(e1071)
classifier1 <- svm(formula = Churn ~ .,
                   data = training_set,
                   type = 'C-classification',
                   kernel = 'linear')

# Predicting the Test set results
y_pred = predict(classifier1, newdata = test_set[1:5])
y_pred

# Making the Confusion Matrix
cm <- table(test_set$Churn, y_pred)
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
  geom_text(aes(label = Count), color = "yellow", size = 6) +
  scale_fill_gradient(low = "blue", high = "orange") +
  labs(title = "Confusion Matrix", x = "Predicted Labels", y = "Actual Labels") +
  theme_minimal()


Accuracy_SVM <- sum(diag(cm)) / sum(cm)
print(paste("Accuracy:", round(Accuracy_SVM * 100, 2), "%"))
