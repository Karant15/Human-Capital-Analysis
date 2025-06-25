##-------------------HUMAN CAPITAL ANALYTICS------------------------------
###3.2 Data Exploration
# Load necessary libraries
library(tidyverse)
library(patchwork)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(class)
library(e1071)
library(ggplot2)
library(pROC)
library(randomForest)

# Read the dataset
employee.data <- read.csv("C:/Users/13142/Desktop/M.S/Analytical Practicum/Case 1/Employee.csv")

# Data Exploration
# Check dimensions and initial rows
dim(employee.data)
head(employee.data)

# Summary statistics
summary(employee.data)

# Structure of the dataset
str(employee.data)

# Check for missing values
print(colSums(is.na(employee.data)))

# Check for zeros in relevant columns
print(colSums(employee.data == 0))

## Exploring Data using Plots
# Define a consistent color palette
color_palette <- c("grey70", "grey50", "grey30", "grey10")

# Create the individual plots with improved aesthetics and consistent colors
p1 <- ggplot(employee.data, aes(x = left)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = color_palette[1]) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Distribution of Employee Turnover", x = "Turnover", y = "Percentage") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10))

p2 <- ggplot(employee.data, aes(x = salary)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = color_palette[2]) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Distribution of Salary Levels", x = "Salary Level", y = "Percentage") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10))

p3 <- ggplot(employee.data, aes(x = sales)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = color_palette[3]) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Distribution of Employees by Department", x = "Department", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title = element_text(size = 12))

p4 <- ggplot(employee.data, aes(x = satisfaction_level)) +
  geom_histogram(binwidth = 0.1, fill = color_palette[1], color = "black") +
  labs(title = "Distribution of Satisfaction Levels", x = "Satisfaction Level", y = "Count") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10))

p5 <- ggplot(employee.data, aes(x = average_montly_hours)) +
  geom_histogram(binwidth = 10, fill = color_palette[2], color = "black") +
  labs(title = "Distribution of Average Monthly Hours", x = "Average Monthly Hours", y = "Count") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10))

# Combine the plots using patchwork with previous alignment
combined_plot <- p1 + p2 + p3 + p4 + p5 +
  plot_layout(ncol = 2) +  # Arrange plots in a 2-column layout
  plot_annotation(title = "Employee Data Distributions", theme = theme(plot.title = element_text(size = 16, face = "bold")))

# Display the combined plot
print(combined_plot)

# Convert variables to factors
employee.data <- employee.data %>%
  mutate(
    left = factor(left, levels = c(0, 1), labels = c("Stayed", "Left")),
    sales = factor(sales),
    salary = factor(salary, levels = c("low", "medium", "high")),
    Work_accident = factor(Work_accident, levels = c(0, 1), labels = c("No Accident", "Accident")),
    promotion_last_5years = factor(promotion_last_5years, levels = c(0, 1), labels = c("No Promotion", "Promotion"))
  )

# Job Satisfaction Distribution by Turnover
ggplot(employee.data, aes(x = satisfaction_level, fill = left)) +
  geom_density(alpha = 0.5) +
  labs(title = "Job Satisfaction Distribution by Turnover", x = "Satisfaction Level", y = "Density", fill = "Turnover") +
  theme_minimal()

# Promotion History by Turnover
ggplot(employee.data, aes(x = promotion_last_5years, fill = left)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Promotion History by Turnover", x = "Promotion in Last 5 Years", y = "Percentage", fill = "Turnover") +
  theme_minimal()

# Job Satisfaction vs. Promotion History
ggplot(employee.data, aes(x = satisfaction_level, y = promotion_last_5years, color = left)) +
  geom_jitter(alpha = 0.5) +
  labs(title = "Job Satisfaction vs. Promotion History", x = "Satisfaction Level", y = "Promotion in Last 5 Years", color = "Turnover") +
  theme_minimal()

# Average Monthly Hours by Turnover
ggplot(employee.data, aes(x = left, y = average_montly_hours, fill = left)) +
  geom_boxplot() +
  labs(title = "Average Monthly Hours by Turnover", x = "Turnover", y = "Average Monthly Hours", fill = "Turnover") +
  theme_minimal()

# Employee Turnover by Department and Salary Level
ggplot(employee.data, aes(x = sales, fill = interaction(left, salary))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Employee Turnover by Department and Salary Level", x = "Department", y = "Percentage", fill = "Turnover and Salary Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

# Employee Turnover by Work Accident Status
ggplot(employee.data, aes(x = Work_accident, fill = left)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Employee Turnover by Work Accident Status", x = "Work Accident Status", y = "Percentage", fill = "Turnover") +
  theme_minimal()

# Number of Employees Who Left by Department
ggplot(employee.data, aes(x = sales, fill = left)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Employee Turnover by Department",
       x = "Department",
       y = "Percentage",
       fill = "Turnover") +
  theme_minimal()

## 3.4 Correlation Analysis
# Convert categorical variables to numeric
employee.data_encoded <- employee.data %>%
  mutate(
    # Convert 'left' to numeric (0 and 1)
    left = as.numeric(left) - 1,
    
    # Convert categorical variables to factors, then to numeric
    sales = as.numeric(factor(sales)),
    salary = as.numeric(factor(salary)),
    
    # Ensure 'Work_accident' and 'promotion_last_5years' are numeric
    Work_accident = as.numeric(Work_accident),
    promotion_last_5years = as.numeric(promotion_last_5years)
  )

# Select all variables for correlation analysis
all_vars <- employee.data_encoded %>%
  select(satisfaction_level, last_evaluation, number_project, average_montly_hours, 
         time_spend_company, Work_accident, left, promotion_last_5years, sales, salary)

# Compute the correlation matrix
correlation_matrix <- cor(all_vars, use = "complete.obs")  # Ensure no missing values

# Convert the correlation matrix to a long format data frame for plotting
correlation_long <- as.data.frame(as.table(correlation_matrix))
colnames(correlation_long) <- c("Variable1", "Variable2", "Correlation")

# Plot the correlation matrix
ggplot(correlation_long, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", Correlation)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Matrix Including All Variables", x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4.0 Predictor Analysis and Relevancy
## 4.1 Feature Importance Using Logistic Regression
# Fit the logistic regression model
logistic_model <- glm(left ~ ., data = employee.data, family = binomial)

# Create a data frame of coefficients excluding the intercept
coefficients_df <- data.frame(
  Feature = names(coef(logistic_model)),
  Coefficient = coef(logistic_model)
) %>%
  filter(Feature != "(Intercept)") %>%
  arrange(desc(abs(Coefficient)))
print(coefficients_df)

# Plot the coefficients
ggplot(coefficients_df, aes(x = reorder(Feature, Coefficient), y = Coefficient, fill = Coefficient > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Feature Importance in Predicting Employee Turnover",
       x = "Feature",
       y = "Coefficient Value") +
  scale_fill_manual(values = c("red", "blue"), name = "Coefficient Sign", labels = c("Negative", "Positive")) +
  theme_minimal()

# 5.0 Data Transformation
## 5.1 Standardizing Numeric Variables
numeric_vars <- employee.data %>%
  select(satisfaction_level, last_evaluation, number_project, average_montly_hours, time_spend_company)

# Standardize numeric variables
numeric_vars_scaled <- scale(numeric_vars)

# Convert standardized data to a data frame
numeric_vars_scaled_df <- as.data.frame(numeric_vars_scaled)
names(numeric_vars_scaled_df) <- names(numeric_vars)

# Plot distribution before standardization
numeric_vars %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Value)) +
  facet_wrap(~ Variable, scales = "free") +
  geom_histogram(bins = 30, fill = "gray", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Numeric Variables Before Standardization")

# Plot distribution after standardization
numeric_vars_scaled_df %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Value)) +
  facet_wrap(~ Variable, scales = "free") +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Numeric Variables After Standardization")

## 5.2 One-Hot Encoding for Categorical Variables

# Encode categorical variables
employee.data_encoded <- employee.data %>%
  select(-left) %>%
  mutate(across(where(is.factor), as.character)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  model.matrix(~ . - 1, data = .) %>%
  as.data.frame()

# Add the 'left' column back for splitting
employee.data_encoded$left <- employee.data$left

# 6.0 Data Splitting
# Splitting original data into training and test sets
set.seed(123)
train_index <- createDataPartition(employee.data_encoded$left, p = 0.7, list = FALSE)
train_data <- employee.data_encoded[train_index, ]
test_data <- employee.data_encoded[-train_index, ]

# Ensure that the factor levels are consistent
train_data$left <- factor(train_data$left, levels = c("Stayed", "Left"))
test_data$left <- factor(test_data$left, levels = c("Stayed", "Left"))

# Further split the training data into training and validation sets
train_index <- createDataPartition(train_data$left, p = 0.8, list = FALSE)
train_set <- train_data[train_index, ]
validation_set <- train_data[-train_index, ]

# Ensure that the factor levels are consistent
train_set$left <- factor(train_set$left, levels = c("Stayed", "Left"))
validation_set$left <- factor(validation_set$left, levels = c("Stayed", "Left"))

# Checking the results
head(train_set)
head(validation_set)
head(test_data)

# 7.0 Model Selection
## 7.1 Logistic Regression Model
### 7.1.1 Model Fitting

# Logistic Regression
logistic_model <- glm(left ~ ., data = train_data, family = binomial)
coefficients_df <- data.frame(
  Feature = names(coef(logistic_model)),
  Coefficient = coef(logistic_model)
) %>%
  filter(Feature != "(Intercept)") %>%
  arrange(desc(abs(Coefficient)))

### 7.1.2 Performance Evaluation
logistic_predictions <- predict(logistic_model, newdata = test_data, type = "response")
logistic_predictions_class <- factor(ifelse(logistic_predictions > 0.5, "Left", "Stayed"), levels = c("Stayed", "Left"))
confusion_logistic <- confusionMatrix(logistic_predictions_class, test_data$left)
print(confusion_logistic)

## 7.2 Decision Tree Model
### 7.2.1 Model Fitting
tree_model <- rpart(left ~ ., data = train_data, method = "class")
rpart.plot(tree_model, main = "Decision Tree for Employee Turnover")

### 7.2.2 Performance Evaluation
tree_predictions <- predict(tree_model, newdata = test_data, type = "class")
confusion_tree <- confusionMatrix(tree_predictions, test_data$left)
print(confusion_tree)

## 7.3 k-Nearest Neighbors (k-NN) Model
### 7.3.1 Model Fitting
train_knn <- train_data %>% select(-left)
test_knn <- test_data %>% select(-left)
train_labels <- train_data$left
test_labels <- test_data$left

# Normalize the data for k-NN
pre_process <- preProcess(train_knn, method = c("center", "scale"))
train_knn_scaled <- predict(pre_process, train_knn)
test_knn_scaled <- predict(pre_process, test_knn)

# Find optimal k
k_values <- 1:20
accuracies <- sapply(k_values, function(k) {
  knn_pred <- knn(train = train_knn_scaled, test = test_knn_scaled, cl = train_labels, k = k)
  mean(knn_pred == test_labels)
})

optimal_k <- k_values[which.max(accuracies)]
knn_predictions <- knn(train = train_knn_scaled, test = test_knn_scaled, cl = train_labels, k = optimal_k)

### 7.3.2 Performance Evaluation
confusion_knn <- confusionMatrix(knn_predictions, test_labels)
print(confusion_knn)

## 7.4 Support Vector Machines (SVM) Model
### 7.4.1 Model Fitting
svm_model <- svm(left ~ ., data = train_set, kernel = "radial", probability = TRUE)
print(svm_model)

# Make predictions on the test set
svm_probabilities <- predict(svm_model, newdata = test_data, probability = TRUE)
svm_probabilities <- attr(svm_probabilities, "probabilities")[, "Left"]

# Convert probabilities to binary class predictions
svm_predictions_class <- factor(ifelse(svm_probabilities > 0.5, "Left", "Stayed"), levels = c("Stayed", "Left"))

# Create confusion matrix
confusion_svm <- confusionMatrix(svm_predictions_class, test_data$left)
print(confusion_svm)

### 7.4.2 Performance Evaluation
# Define the function to calculate performance metrics
calculate_metrics <- function(conf_matrix, probabilities, true_labels) {
  accuracy <- conf_matrix$overall["Accuracy"]
  sensitivity <- conf_matrix$byClass["Sensitivity"]
  specificity <- conf_matrix$byClass["Specificity"]
  f1_score <- (2 * sensitivity * specificity) / (sensitivity + specificity)
  
  # Compute AUC
  auc_value <- auc(roc(true_labels, probabilities))
  
  return(list(
    Accuracy = accuracy,
    Sensitivity = sensitivity,
    Specificity = specificity,
    F1_Score = f1_score,
    AUC = auc_value
  ))
}

# Compute performance metrics for SVM
metrics_svm <- calculate_metrics(confusion_svm, svm_probabilities, test_data$left)
print(metrics_svm)

# 8.0 Model Comparison and Selection
## 8.1 Performance Metrics Comparison

# Define the function to calculate performance metrics
calculate_metrics <- function(conf_matrix, prob_predictions, true_labels) {
  # Extract metrics from the confusionMatrix object
  accuracy <- conf_matrix$overall["Accuracy"]
  sensitivity <- conf_matrix$byClass["Sensitivity"]
  specificity <- conf_matrix$byClass["Specificity"]
  
  # Calculate F1 score and AUC
  f1_score <- (2 * sensitivity * specificity) / (sensitivity + specificity)
  auc <- roc(true_labels, prob_predictions)$auc
  
  return(list(
    Accuracy = accuracy,
    Sensitivity = sensitivity,
    Specificity = specificity,
    F1_Score = f1_score,
    AUC = auc
  ))
}

# Calculate metrics for each model
# Logistic Regression
logistic_probabilities <- logistic_predictions
logistic_metrics <- calculate_metrics(confusion_logistic, logistic_probabilities, test_data$left)

# Decision Tree
tree_probabilities <- predict(tree_model, newdata = test_data, type = "prob")[, "Left"]
tree_metrics <- calculate_metrics(confusion_tree, tree_probabilities, test_data$left)

# k-NN
knn_probabilities <- as.numeric(knn_predictions) - 1
knn_metrics <- calculate_metrics(confusion_knn, knn_probabilities, test_labels)

# Support Vector Machine (SVM)
svm_metrics <- calculate_metrics(confusion_svm, svm_probabilities, test_data$left)

# Create a data frame of performance metrics
performance_metrics <- data.frame(
  Model = c("Logistic Regression", "Decision Tree", "k-NN", "SVM"),
  Accuracy = c(logistic_metrics$Accuracy, tree_metrics$Accuracy, knn_metrics$Accuracy, svm_metrics$Accuracy),
  Sensitivity = c(logistic_metrics$Sensitivity, tree_metrics$Sensitivity, knn_metrics$Sensitivity, svm_metrics$Sensitivity),
  Specificity = c(logistic_metrics$Specificity, tree_metrics$Specificity, knn_metrics$Specificity, svm_metrics$Specificity),
  F1_Score = c(logistic_metrics$F1_Score, tree_metrics$F1_Score, knn_metrics$F1_Score, svm_metrics$F1_Score),
  AUC = c(logistic_metrics$AUC, tree_metrics$AUC, knn_metrics$AUC, svm_metrics$AUC)
)

# Plot Accuracy
library(ggplot2)
ggplot(performance_metrics, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.2f", Accuracy)), vjust = -0.5, color = "black") +
  labs(title = "Model Accuracy Comparison", x = "Model", y = "Accuracy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot F1 Score
ggplot(performance_metrics, aes(x = Model, y = F1_Score, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.2f", F1_Score)), vjust = -0.5, color = "black") +
  labs(title = "Model F1-Score Comparison", x = "Model", y = "F1 Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot AUC
ggplot(performance_metrics, aes(x = Model, y = AUC, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.2f", AUC)), vjust = -0.5, color = "black") +
  labs(title = "Model AUC Comparison", x = "Model", y = "AUC") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 8.2 Compute ROC Curves
# ROC Curves for all models
logistic_roc <- roc(test_data$left, logistic_probabilities)
tree_roc <- roc(test_data$left, tree_probabilities)
knn_roc <- roc(test_labels, knn_probabilities)
svm_roc <- roc(test_data$left, svm_probabilities)

# Plot ROC Curves
plot.roc(logistic_roc, col = "blue", main = "ROC Curves for All Models")
plot.roc(tree_roc, col = "red", add = TRUE)
plot.roc(knn_roc, col = "green", add = TRUE)
plot.roc(svm_roc, col = "purple", add = TRUE)
legend("bottomright", legend = c("Logistic Regression", "Decision Tree", "k-NN", "SVM"), col = c("blue", "red", "green", "purple"), lwd = 2)

# Adding AUC values to the ROC plot
text(x = 0.6, y = 0.4, labels = paste("Logistic Regression AUC:", sprintf("%.2f", logistic_roc$auc)), col = "blue", cex = 0.8)
text(x = 0.6, y = 0.35, labels = paste("Decision Tree AUC:", sprintf("%.2f", tree_roc$auc)), col = "red", cex = 0.8)
text(x = 0.6, y = 0.30, labels = paste("k-NN AUC:", sprintf("%.2f", knn_roc$auc)), col = "green", cex = 0.8)
text(x = 0.6, y = 0.25, labels = paste("SVM AUC:", sprintf("%.2f", svm_roc$auc)), col = "purple", cex = 0.8)

# Adding a legend
legend("bottomright", legend = c("Logistic Regression", "Decision Tree", "k-NN", "SVM"),
       col = c("blue", "red", "green", "purple"), lwd = 2)

###8.2 Final Model Selection
# Based on performance metrics
best_model <- performance_metrics %>%
  arrange(desc(AUC)) %>%
  slice(1) %>%
  pull(Model)

cat("The final model selected based on the highest AUC is:", best_model, "\n")

### 9.1 Hypothesis Testing
#### 9.1.1 Salary and Employee Turnover
ggplot(employee.data, aes(x = salary, fill = left)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Employee Turnover by Salary Level", x = "Salary Level", y = "Percentage", fill = "Turnover") +
  theme_minimal()
salary_turnover_table <- table(employee.data$salary, employee.data$left)
chisq_test <- chisq.test(salary_turnover_table)
print(chisq_test)
logistic_model_salary <- glm(left ~ salary, data = employee.data, family = binomial)
summary(logistic_model_salary)

#### 9.1.2 Work Accident and Employee Turnover
# Distribution of turnover by work accident status
ggplot(employee.data, aes(x = Work_accident, fill = left)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Employee Turnover by Work Accident Status", x = "Work Accident Status", y = "Percentage", fill = "Turnover") +
  theme_minimal()
work_accident_turnover_table <- table(employee.data$Work_accident, employee.data$left)
chisq_test_work_accident <- chisq.test(work_accident_turnover_table)
print(chisq_test_work_accident)
logistic_model_work_accident <- glm(left ~ Work_accident, data = employee.data, family = binomial)
summary(logistic_model_work_accident)


#### 9.1.3 Professional Growth
# Hypothesis on job satisfaction and promotion indicators
ggplot(employee.data, aes(x = satisfaction_level, fill = left)) +
  geom_density(alpha = 0.5) +
  labs(title = "Job Satisfaction Distribution by Turnover", x = "Satisfaction Level", y = "Density", fill = "Turnover") +
  theme_minimal()
t_test_satisfaction <- t.test(satisfaction_level ~ left, data = employee.data)
print(t_test_satisfaction)
ggplot(employee.data, aes(x = promotion_last_5years, fill = left)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Promotion History by Turnover", x = "Promotion in Last 5 Years", y = "Percentage", fill = "Turnover") +
  theme_minimal()




