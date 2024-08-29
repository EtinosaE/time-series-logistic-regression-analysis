# Install packages needed
install.packages("tidyverse","caret","pROC","ggplot2")
# Load required libraries
library(tidyverse)
library(caret)
library(gridExtra)#function "grid.arrange
library(pROC)# Load the pROC package for the curve
library(ggplot2)

# Set working directory
setwd("C:/Users/Admin/Desktop/stat_project_CA2")

# Load dataset
cardiac <- read.csv("cardiac.csv", header = TRUE, na.string = c(""), stringsAsFactors = TRUE)

###################### Explore the dataset #####################

summary(cardiac)# view the data summary
head(cardiac)
str(cardiac)#view the data frame
dim(cardiac)# 100 rows and  6 columns
sapply(cardiac, class)# list the types for every columns

# Check for missing values
missing_values <- colSums(is.na(cardiac))
missing_values

dup_count <- sum(duplicated(cardiac))
cat("Number of duplicated rows: ", dup_count, "\n")  # Number of duplicated rows: 0

# Create a list of variables you want to plot
num_variables <- c("caseno", "age", "weight", "fitness_score")
plots <- list()#list to store the plots

# Loop through the variables and plots
for (variable in num_variables) {
  plot <- ggplot(cardiac, aes(x = .data[[variable]])) +
    geom_histogram(fill = "blue", bins = 30, color = "black") +
    labs(title = paste("Distribution of", variable), x = variable, y = "Frequency")
  
  plots[[variable]] <- plot
}
# Arrange and plot the plots in a grid with a 2x2 layout
grid.arrange(grobs = plots, ncol = 2, nrow = 2)

# plot for categorical variables(factor variables)
gender <- ggplot(cardiac, aes(x = as.factor(gender ))) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of gender", x = "gender", y = "Frequency")
cardiac_condition <- ggplot(cardiac, aes(x = as.factor(cardiac_condition))) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of cardiac_condition", x = "cardiac_condition", y = "Frequency")

# Arrange plots in a 2x2 grid
grid.arrange(gender, cardiac_condition, ncol = 2)
par(mfrow = c(1, 1))# Reset the layout to the default

# check for outliers using boxplot for visualizing the variables
variables <- c("caseno", "age", "weight", "fitness_score","gender","cardiac_condition")
# Set up the layout for the plots
par(mfrow = c(2, 3))
# Create boxplots for each variable
for (variable in variables) {
  boxplot(cardiac[[variable]], main = paste("Box Plot of", variable))
}
# Reset the layout to the default
par(mfrow = c(1, 1))

#Linearity check 
plot(cardiac, col = "lightblue")

# Split data into training and testing sets
set.seed(23138548)  # Set seed for reproducibility
# Split the dataset into training and testing sets
train_indices <- createDataPartition(cardiac$cardiac_condition, p = 0.75, list = FALSE)
train_data <- cardiac[train_indices, ]
test_data <- cardiac[-train_indices, ]
dim(train_data)
dim(test_data)

#standardize the  numerical variables 
numeric_columns <- c("caseno", "age", "weight", "fitness_score")
standardize <- function(x) { return ((x - mean(x, na.rm = TRUE)) / sd(x))
}

# now apply standardization to train_data
train_data[, numeric_columns] <- apply(train_data[, numeric_columns], 2, standardize)
summary(train_data)
#Also apply standardization to test_data
test_data[, numeric_columns] <- apply(test_data[, numeric_columns], 2, standardize)
summary(test_data)

###################### logistic regression model ###############################

logistic_model <- glm(cardiac_condition ~ caseno + age + weight + gender + fitness_score,
                      data = train_data, family = "binomial")

# check the model summary
summary(logistic_model)

# Check the levels of test_data$cardiac_condition
levels(test_data$cardiac_condition)

# Make predictions on the test set
predictions <- predict(logistic_model, newdata = test_data, type = "response")

# Convert predicted_labels to factor with levels
predicted_labels <- ifelse(predictions > 0.5, "Present", "Absent")
predicted_labels <- factor(predicted_labels, levels = levels(test_data$cardiac_condition))

# Create a confusion matrix
conf_matrix <- confusionMatrix(predicted_labels, test_data$cardiac_condition)
conf_matrix

#Cross-validation
#Define your control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

#Define your model
cvmodel_results <- train(cardiac_condition ~ ., data = train_data, method = "glm", trControl = ctrl)

#Print the results
print(cvmodel_results)

#the test to predict and plot the curve
predictions <- predict(logistic_model, newdata = test_data, type = "response")
roc_curve <- roc(test_data$cardiac_condition, predictions)#### to Generate the curve
print(roc_curve)#Area under the curve: 0.8401

#### Plot the curve
plot(roc_curve, main = "Logistic Regression - ROC Curve", col = "red", lwd = 3)
text(0.8, 0.1, paste("AUC =", round(auc(roc_curve), 5)), col = "blue")
legend("bottomright", legend = paste("AUC =", round(auc(roc_curve), 5)), col = "blue", lty = 1, cex = 1)

threshold <- 0.5
confusion_matrix <- table(test_data$cardiac_condition, ifelse(predictions > threshold, 1, 0))
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy: ", accuracy, "\n")# Print the accuracy: Accuracy:  0.7916667 

precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("Precision: ", precision, "\n")#Precision:  0.6666667
cat("Recall: ", recall, "\n")# Recall:  0.75 
cat("F1 Score: ", f1_score, "\n")#F1 Score:  0.7058824

# Calculate residuals
residuals <- residuals(glm(cardiac_condition ~ age + weight + fitness_score, data = train_data, family = "binomial"))

# Create a residual plot
plot(train_data$cardiac_condition, residuals, main = "Residual Plot",xlab = "Observed Values",ylab = "Residuals",
     pch = 16, col = ifelse(train_data$cardiac_condition == 1, "red", "blue"))
abline(h = 0, col = "black", lty = 2)# Add a horizontal line at y = 0 for reference

# Add a smooth line to identify trends
lines(lowess(train_data$cardiac_condition, residuals), col = "blue")

# Add legend
legend("topright", legend = c("Absent", "Present"), col = c("blue", "red"), pch = 16)

residual_model <- residuals(logistic_model)
ggplot(train_data, aes(x = residual_model)) +
  geom_density(fill = "#3498db", color = "#e74c3c", alpha = 0.7) +
  labs(title = "Kernel Density Plot for logistic regression", x = "Residuals", y = "Density") +
  theme_minimal()

