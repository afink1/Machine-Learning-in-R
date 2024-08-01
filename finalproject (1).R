#I pledge my honor that I have abided by the stevens honor system
#Jonathan Carbonneau, Alan Fink, Kevin Liao
rm(list=ls())
# Read the data
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(RColorBrewer)
library(caret)
library(caTools)
library(fastDummies)
library(ROSE)
library(class)
library(e1071)
library(C50)
library(randomForest)
library(rpart)


data <- read.csv("bank-full.csv")
# Calculate lower and upper bounds for 'balance'
Q1_balance <- quantile(data$balance, 0.25)
Q3_balance <- quantile(data$balance, 0.75)
IQR_balance <- Q3_balance - Q1_balance

lower_bound_balance <- Q1_balance - 1.5 * IQR_balance
upper_bound_balance <- Q3_balance + 1.5 * IQR_balance

# Identify outliers in 'balance'
outliers_balance <- data[data$balance < lower_bound_balance | data$balance > upper_bound_balance, ]
data <- data[data$balance >= lower_bound_balance & data$balance <= upper_bound_balance, ]

# Calculate lower and upper bounds for 'campaign'
Q1_campaign <- quantile(data$campaign, 0.10)
Q3_campaign <- quantile(data$campaign, 0.90)
IQR_campaign <- Q3_campaign - Q1_campaign

lower_bound_campaign <- Q1_campaign - 1.5 * IQR_campaign
upper_bound_campaign <- Q3_campaign + 1.5 * IQR_campaign

# Identify outliers in 'campaign'
outliers_campaign <- data[data$campaign < lower_bound_campaign | data$campaign > upper_bound_campaign, ]
data <- data[data$campaign >= lower_bound_campaign & data$campaign <= upper_bound_campaign, ]

# Calculate lower and upper bounds for 'duration'
Q1_duration <- quantile(data$duration, 0.20)
Q3_duration <- quantile(data$duration, 0.80)
IQR_duration <- Q3_duration - Q1_duration

lower_bound_duration <- Q1_duration - 1.5 * IQR_duration
upper_bound_duration <- Q3_duration + 1.5 * IQR_duration

# Identify outliers in 'duration'
outliers_duration <- data[data$duration < lower_bound_duration | data$duration > upper_bound_duration, ]
data <- data[data$duration >= lower_bound_duration & data$duration <= upper_bound_duration, ]

# Calculate lower and upper bounds for 'previous'
Q1_previous <- quantile(data$previous, 0.05)
Q3_previous <- quantile(data$previous, 0.95)
IQR_previous <- Q3_previous - Q1_previous

lower_bound_previous <- Q1_previous - 1.5 * IQR_previous
upper_bound_previous <- Q3_previous + 1.5 * IQR_previous

# Identify outliers in 'previous'
outliers_previous <- data[data$previous < lower_bound_previous | data$previous > upper_bound_previous, ]
data <- data[data$previous >= lower_bound_previous & data$previous <= upper_bound_previous, ]

# Calculate lower and upper bounds for 'age'
Q1_age <- quantile(data$age, 0.25)
Q3_age <- quantile(data$age, 0.75)
IQR_age <- Q3_age - Q1_age

lower_bound_age <- Q1_age - 1.5 * IQR_age
upper_bound_age <- Q3_age + 1.5 * IQR_age

# Identify outliers in 'age'
outliers_age <- data[data$age < lower_bound_age | data$age > upper_bound_age, ]
data <- data[data$age >= lower_bound_age & data$age <= upper_bound_age, ]

# Display descriptive statistics heatmap
# desc_stats(data)

# Create boxplots for each numeric column
par(mfrow = c(4, 4))  # Change the layout if you have more than 4 numeric columns
for (col in names(data)) {
  if (is.numeric(data[[col]])) {
    boxplot(data[[col]], main = col, col = "skyblue", border = "black")
  }
}
# Function to calculate percentages and create bar plot
calculate_and_plot_percentages <- function(data, x_var, y_var, title, x_label, y_label) {
  df_percentages <- data %>%
    group_by({{ x_var }}, {{ y_var }}) %>%
    summarize(percent = n() / nrow(data) * 100)
  
  ggplot(df_percentages, aes(x = {{ x_var }}, y = percent, fill = {{ y_var }})) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    labs(title = title,
         x = x_label,
         y = y_label) +
    theme_minimal()
}

# Plot relationship between 'age' and 'mortgage'
calculate_and_plot_percentages(data, age, housing,
                               "The relation between age and mortgage",
                               "Age",
                               "Percentage")


# Plot relationship between 'job' and 'mortgage'
calculate_and_plot_percentages(data, job, housing,
                               "The relation between job and mortgage",
                               "Jobs",
                               "Percentage")

# Plot relationship between 'marital' and 'mortgage'
calculate_and_plot_percentages(data, marital, housing,
                               "The relation between marital status and mortgage",
                               "Marital Status",
                               "Percentage")

# Plot relationship between 'default' and 'mortgage'
calculate_and_plot_percentages(data, default, housing,
                               "The relation between default and mortgage",
                               "Default",
                               "Percentage")

# Plot relationship between 'balance' and 'mortgage'
calculate_and_plot_percentages(data, balance, housing,
                               "The relation between balance and mortgage",
                               "Balance",
                               "Percentage")

# Plot relationship between 'loan' and 'mortgage'
calculate_and_plot_percentages(data, loan, housing,
                               "The relation between loan and mortgage",
                               "Loan",
                               "Percentage")
# Plot relationship between 'education' and 'mortgage'
calculate_and_plot_percentages(data, education, housing,
                               "The relation between education and mortgage",
                               "education",
                               "Percentage")
# Convert 'housing' to numeric (1 for 'yes', 0 for 'no')
data$housing_numeric <- as.numeric(data$housing == "yes")

# Point-biserial correlation for continuous variables
cor_age <- cor.test(data$housing_numeric, data$age)
cor_balance <- cor.test(data$housing_numeric, data$balance)
# ... Perform for other continuous variables

# Chi-square Test of Independence for categorical variables with two categories
table_default <- table(data$housing_numeric, data$default)
chi_sq_default <- chisq.test(table_default)

table_loan <- table(data$housing_numeric, data$loan)
chi_sq_loan <- chisq.test(table_loan)
# ... Perform for other categorical variables with two categories

# Logistic Regression for categorical variables with more than two categories
model_job <- glm(housing_numeric ~ job, family = binomial, data = data)
# ... Perform for other categorical variables with more than two categories

# Convert 'housing' to numeric (1 for 'yes', 0 for 'no')
data$housing_numeric <- as.numeric(data$housing == "yes")

# Point-biserial correlation for continuous variables
cor_age <- cor.test(data$housing_numeric, data$age)
cor_balance <- cor.test(data$housing_numeric, data$balance)
# ... Perform for other continuous variables

# Chi-square Test of Independence for categorical variables with two categories
table_default <- table(data$housing_numeric, data$default)
chi_sq_default <- chisq.test(table_default)

table_loan <- table(data$housing_numeric, data$loan)
chi_sq_loan <- chisq.test(table_loan)
# ... Perform for other categorical variables with two categories

# Logistic Regression for categorical variables with more than two categories
model_job <- glm(housing_numeric ~ job, family = binomial, data = data)
model_education <- glm(housing_numeric ~ education, family = binomial, data = data)

# Display results
cat("Point-biserial correlation for age:\n")
print(cor_age)
if (cor_age$p.value < 0.05) {
  cat("Age and housing_numeric are correlated.\n")
} else {
  cat("Age and housing_numeric are not correlated.\n")
}

cat("\nPoint-biserial correlation for balance:\n")
print(cor_balance)
if (cor_balance$p.value < 0.05) {
  cat("Balance and housing_numeric are correlated.\n")
} else {
  cat("Balance and housing_numeric are not correlated.\n")
}
# ... Repeat for other continuous variables

cat("\nChi-square test for default:\n")
print(chi_sq_default)
if (chi_sq_default$p.value < 0.05) {
  cat("Default and housing_numeric are associated.\n")
} else {
  cat("Default and housing_numeric are not associated.\n")
}

cat("\nChi-square test for loan:\n")
print(chi_sq_loan)
if (chi_sq_loan$p.value < 0.05) {
  cat("Loan and housing_numeric are associated.\n")
} else {
  cat("Loan and housing_numeric are not associated.\n")
}
# ... Repeat for other categorical variables with two categories

# Display logistic regression results for job
cat("\nLogistic Regression Results for job:\n")
summary(model_job)

# Extract odds ratios and confidence intervals for job categories
job_coef <- coef(model_job)
job_odds_ratios <- exp(job_coef)
job_ci <- confint(model_job)

# Display odds ratios and confidence intervals for job categories
cat("\nOdds Ratios and Confidence Intervals for job:\n")
for (i in seq_along(job_coef)) {
  cat("Category:", names(job_coef)[i], "\n")
  cat("Odds Ratio:", job_odds_ratios[i], "\n")
  cat("95% Confidence Interval:", job_ci[i,], "\n\n")
}

# Display logistic regression results for education
cat("\nLogistic Regression Results for education:\n")
summary(model_education)

# Extract odds ratios and confidence intervals for education categories
education_coef <- coef(model_education)
education_odds_ratios <- exp(education_coef)
education_ci <- confint(model_education)

# Display odds ratios and confidence intervals for education categories
cat("\nOdds Ratios and Confidence Intervals for education:\n")
for (i in seq_along(education_coef)) {
  cat("Category:", names(education_coef)[i], "\n")
  cat("Odds Ratio:", education_odds_ratios[i], "\n")
  cat("95% Confidence Interval:", education_ci[i,], "\n\n")
}
# Print categorical features
cat_features <- names(data)[sapply(data, function(x) is.factor(x) | is.character(x))]

cat("Categorical Features:", cat_features, "\n")


# Encoding 'job'
job_titles <- c("admin.", "unknown", "unemployed", "management", "housemaid", "entrepreneur", "student",
                "blue-collar", "self-employed", "retired", "technician", "services")
data$job_encoded <- match(data$job, job_titles)

data$education_encoded <- ifelse(data$education == "primary", 1,
                                 ifelse(data$education == "secondary", 2,
                                        ifelse(data$education == "tertiary", 3, 0))) # 0 for 'unknown'

# Encoding 'contact' (unknown = 0, telephone = 1, cellular = 2)
data$contact_encoded <- ifelse(data$contact == "unknown", 0,
                               ifelse(data$contact == "telephone", 1, 2))

# Encoding 'month' (Jan = 1, Feb = 2, ..., Dec = 12)
months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
data$month_encoded <- match(tolower(data$month), months)

# Encoding 'poutcome' (unknown = 0, other = 1, failure = 2, success = 3)
data$poutcome_encoded <- ifelse(data$poutcome == "unknown", 0,
                                ifelse(data$poutcome == "other", 1,
                                       ifelse(data$poutcome == "failure", 2, 3)))
# Encoding 'marital' status (single = 0, married = 1, divorced = 2)
data$marital_encoded <- ifelse(data$marital == "single", 0,
                               ifelse(data$marital == "married", 1, 2)) # Assuming 'divorced' is the only other category

# Encoding 'housing' (yes = 1, no = 0)
data$housing_encoded <- as.numeric(data$housing == "yes")

data$y_encoded <- as.numeric(data$y == "yes")

# Encoding 'loan' (yes = 1, no = 0)
data$loan_encoded <- as.numeric(data$loan == "yes")

# Encoding 'default' (yes = 1, no = 0)
data$default_encoded <- as.numeric(data$default == "yes")

# One-hot encoding using fastDummies
data_one_hot <- dummy_cols(data, select_columns = c("job", "marital", "education", "contact", "month", "poutcome", "default", "loan"))
# data <- dummy_cols(data, select_columns = c("job", "marital", "education", "contact", "month", "poutcome", "default", "loan"))

# To view all columns, use rstudioapi::writeRStudioPreference("data_viewer_max_columns", 69L)

encoded_columns <- names(data_one_hot)[grepl("_", names(data_one_hot))]  
encoded_data <- data_one_hot[, encoded_columns]
encoded_data[] <- lapply(encoded_data, function(x) x == 1)
# View(encoded_data)

encoded_data <- data.frame(
  age = data$age,
  education = data$education_encoded,
  balance = data$balance,  # Corrected spelling from 'balace' to 'balance'
  day = data$day,
  month = data$month_encoded,
  duration = data$duration,
  campaign = data$campaign,
  pdays = data$pdays,
  previous = data$previous,
  y = data$y_encoded
)

encoded_columns <- names(data_one_hot)[grepl("_", names(data_one_hot))]  
one_hot_encoded_data <- data_one_hot[, encoded_columns]
one_hot_encoded_data[] <- lapply(one_hot_encoded_data, function(x) x == 1)

data_encoded_df <- cbind(encoded_data, one_hot_encoded_data)
# data_encoded_df <- as.data.frame(data_encoded)
names(data_encoded_df)[names(data_encoded_df) == "job_blue-collar"] <- "job_blue_collar"
names(data_encoded_df)[names(data_encoded_df) == "job_self-employed"] <- "job_self_employed"
View(data_encoded_df)

# Set seed for reproducibility
set.seed(123)

# Splitting the data into training and testing sets
split <- createDataPartition(data_encoded_df$housing_encoded, p = 0.7, list = FALSE)
training_set <- data_encoded_df[split, ]
testing_set <- data_encoded_df[-split, ]

# Checking for imbalance in the training set
table(training_set$housing_encoded)

# Applying ROSE to the training data

# Converting all logical variables back to numeric
training_set[] <- lapply(training_set, function(x) if(is.logical(x)) as.numeric(x) else x)
testing_set[] <- lapply(testing_set, function(x) if(is.logical(x)) as.numeric(x) else x)
# Now apply ROSE
# Calculate the number of instances in the minority class
minority_class_size <- sum(training_set$housing_encoded == 1)

# Set N
N_value <- minority_class_size / 17

print(N_value)
# Apply ROSE with the adjusted N value
rose_data <- ROSE(housing_encoded ~ ., data = training_set, N = N_value)$data


# table(rose_data$housing_encoded)
# View(rose_data)
# View(testing_set)
# View(training_set)
# Prepare features and labels for training and testing sets
train_features <- rose_data[, -which(names(rose_data) == "housing_encoded")]
test_features <- testing_set[, -which(names(testing_set) == "housing_encoded")]
train_labels <- rose_data$housing_encoded
test_labels <- testing_set$housing_encoded


# Convert 'housing_encoded' to a factor in both training and testing sets
rose_data$housing_encoded <- as.factor(rose_data$housing_encoded)
testing_set$housing_encoded <- as.factor(testing_set$housing_encoded)


# Scale the features
train_features_scaled <- scale(train_features)
test_features_scaled <- scale(test_features)

# Running KNN for different values of k on scaled features
predictions_k3_scaled <- knn(train_features_scaled, test_features_scaled, cl = train_labels, k = 3)
predictions_k5_scaled <- knn(train_features_scaled, test_features_scaled, cl = train_labels, k = 5)
predictions_k10_scaled <- knn(train_features_scaled, test_features_scaled, cl = train_labels, k = 10)

# Evaluating model accuracy on scaled features
accuracy_k3_scaled <- sum(predictions_k3_scaled == test_labels) / length(test_labels)
accuracy_k5_scaled <- sum(predictions_k5_scaled == test_labels) / length(test_labels)
accuracy_k10_scaled <- sum(predictions_k10_scaled == test_labels) / length(test_labels)

# Print accuracy on scaled features
print(paste("Accuracy for k=3 (scaled features):", accuracy_k3_scaled))
print(paste("Accuracy for k=5 (scaled features):", accuracy_k5_scaled))
print(paste("Accuracy for k=10 (scaled features):", accuracy_k10_scaled))

# Train the Naive Bayes model
nb_model <- naiveBayes(housing_encoded ~ ., data = rose_data)

# Predict using the test data
nb_predictions <- predict(nb_model, newdata = testing_set)

# Evaluate the model
nb_accuracy <- sum(nb_predictions == testing_set$housing_encoded) / nrow(testing_set)
print(paste("Accuracy of Naive Bayes model:", nb_accuracy))

# You can also compute other performance metrics such as confusion matrix, precision, recall, etc.
confusionMatrix <- table(Predicted = nb_predictions, Actual = testing_set$housing_encoded)
print(confusionMatrix)


# Build a Random Forest classification model
rf_model <- randomForest(housing_encoded ~ ., data = rose_data, ntree = 100, mtry = 5, nodesize = 175)

# Make predictions using the Random Forest model
rf_predictions <- predict(rf_model, newdata = testing_set)

# Evaluate the Random Forest model's performance
rf_accuracy <- sum(rf_predictions == testing_set$housing_encoded) / nrow(testing_set)
cat("Accuracy of Random Forest model:", rf_accuracy, "\n")

# Confusion matrix for Random Forest
confusionMatrix_rf <- table(Predicted = rf_predictions, Actual = testing_set$housing_encoded)
print("Confusion Matrix for Random Forest:")
print(confusionMatrix_rf)


# Train the SVM model
svm_model <- svm(housing_encoded ~ ., data = rose_data, kernel = "radial", cost = 0.1)

# Make predictions using the SVM model
svm_predictions <- predict(svm_model, newdata = testing_set)

# Evaluate the SVM model's performance
svm_accuracy <- sum(svm_predictions == testing_set$housing_encoded) / nrow(testing_set)
cat("Accuracy of SVM model:", svm_accuracy, "\n")

# Confusion matrix for SVM
confusionMatrix_svm <- table(Predicted = svm_predictions, Actual = testing_set$housing_encoded)
print("Confusion Matrix for SVM:")
print(confusionMatrix_svm)

# Create an empty data frame for model accuracies
model_accuracies <- data.frame(Model = character(), Accuracy = numeric())

# Add accuracy values for each model
model_accuracies <- rbind(model_accuracies,
                          data.frame(Model = "k-NN (k=3)", Accuracy = accuracy_k3_scaled),
                          data.frame(Model = "k-NN (k=5)", Accuracy = accuracy_k5_scaled),
                          data.frame(Model = "k-NN (k=10)", Accuracy = accuracy_k10_scaled),
                          data.frame(Model = "Naive Bayes", Accuracy = nb_accuracy),
                          data.frame(Model = "SVM", Accuracy = svm_accuracy),
                          data.frame(Model = "Random Forest", Accuracy = rf_accuracy)
)

# Print the model accuracies
print("Model Accuracies:")
print(model_accuracies)