data=read.csv("C:\\chamodi\\AQI\\pre_processed.csv")
head(data)

str(data)

# convert discipline to ordered factor
data$AQI.Category <- ordered(data$AQI.Category, 
                             levels = c("Good", "Moderate", "Unhealthy"))

# check conversion
str(data)

# Drop the "City" column
data <- data[, !names(data) %in% "City"]

# Drop the "City" column
data <- data[, !names(data) %in% "?..Country" ]

# Convert the "Continent" column to a factor
data$Continent <- as.factor(data$Continent)

str(data)

# Split into training and testing subsets
train_indices <- sample(nrow(data), round(0.8 * nrow(data)))  # 80% for training
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# run proportional odds model
library(MASS)

model <- polr(
  formula = AQI.Category ~.,
  data = train_data
)

# get summary
summary(model)


# get coefficients (it's in matrix form)
coefficients <- summary(model)$coefficients

# calculate p-values
p_value <- (1 - pnorm(abs(coefficients[ ,"t value"]), 0, 1))*2

# bind back to coefficients
(coefficients <- cbind(coefficients, p_value))

# calculate odds ratios
odds_ratio <- exp(coefficients[ ,"Value"])

# combine with coefficient and p_value
(coefficients <- cbind(
  coefficients[ ,c("Value", "p_value")],
  odds_ratio
))

head(fitted(model))

# diagnostics of simpler model
DescTools::PseudoR2(
  model, 
  which = c("McFadden", "CoxSnell", "Nagelkerke", "AIC")
)


library(generalhoslem)
# lipsitz test 
generalhoslem::lipsitz.test(model)

# pulkstenis-robinson test 
# (requires the vector of categorical input variables as an argument)
generalhoslem::pulkrob.chisq(model, catvars = cats)


# create binary variable for "mode" or "unhe" versus "good"
train_data$moderate <- ifelse(data$AQI.Categor== "Good", 0, 1)

# create binary variable for "unhe" versus "mofe" or "good"
train_data$unheal <- ifelse(data$AQI.Categor== "Unhealthy", 1, 0)

colnames(data)

mode_model <- glm(moderate ~ . - AQI.Category - unheal- moderate, data = train_data, family = "binomial")
# Summarize the model
summary(mode_model)
unhel_model <- glm(unheal ~ . - AQI.Category - moderate- unheal, data = train_data, family = "binomial")
# Summarize the model
summary(unhel_model)


(coefficient_comparison <- data.frame(
  moderate = summary(mode_model)$coefficients[ , "Estimate"],
  unhealthy = summary(unhel_model)$coefficients[ ,"Estimate"],
  diff = summary(unhel_model)$coefficients[ ,"Estimate"] - 
    summary(mode_model)$coefficients[ , "Estimate"]
))

l








# Predict on training data
train_pred <- predict(model, train_data, type = "class")

# Predict on test data
test_pred <- predict(model, test_data, type = "class")

# Confusion matrix for training data
conf_matrix_train <- table(train_pred, train_data$AQI.Category)

# Confusion matrix for test data
conf_matrix_test <- table(test_pred, test_data$AQI.Category)

# Calculate metrics for training data
accuracy_train <- sum(diag(conf_matrix_train))/sum(conf_matrix_train)
precision_train <- diag(conf_matrix_train)/rowSums(conf_matrix_train)
recall_train <- diag(conf_matrix_train)/colSums(conf_matrix_train)
f1_score_train <- 2 * (precision_train * recall_train) / (precision_train + recall_train)

# Calculate metrics for test data
accuracy_test <- sum(diag(conf_matrix_test))/sum(conf_matrix_test)
precision_test <- diag(conf_matrix_test)/rowSums(conf_matrix_test)
recall_test <- diag(conf_matrix_test)/colSums(conf_matrix_test)
f1_score_test <- 2 * (precision_test * recall_test) / (precision_test + recall_test)


library(knitr)

# Create a data frame for the metrics
metrics_table <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Training = c(accuracy_train, mean(precision_train, na.rm = TRUE), mean(recall_train, na.rm = TRUE), mean(f1_score_train, na.rm = TRUE)),
  Testing = c(accuracy_test, mean(precision_test, na.rm = TRUE), mean(recall_test, na.rm = TRUE), mean(f1_score_test, na.rm = TRUE))
)

# Print the table
knitr::kable(metrics_table, caption = "Model Evaluation Metrics")













################ rf model ########################

# Load the randomForest library
library(randomForest)

# Fit random forest model

rf_model <- randomForest(AQI.Category ~ CO.AQI.Value+Ozone.AQI.Value+NO2.AQI.Value+ PM2.5.AQI.Value+SO2 + Continent , data = train_data, ntree = 100, max_depth = 10)

# Evaluate random forest model on training data
rf_train_pred <- predict(rf_model, train_data)
rf_conf_matrix_train <- table(rf_train_pred, train_data$AQI.Category)
rf_accuracy_train <- sum(diag(rf_conf_matrix_train))/sum(rf_conf_matrix_train)
rf_precision_train <- diag(rf_conf_matrix_train)/rowSums(rf_conf_matrix_train)
rf_recall_train <- diag(rf_conf_matrix_train)/colSums(rf_conf_matrix_train)
rf_f1_score_train <- 2 * (rf_precision_train * rf_recall_train) / (rf_precision_train + rf_recall_train)

# Evaluate random forest model on test data
rf_test_pred <- predict(rf_model, test_data)
rf_conf_matrix_test <- table(rf_test_pred, test_data$AQI.Category)
rf_accuracy_test <- sum(diag(rf_conf_matrix_test))/sum(rf_conf_matrix_test)
rf_precision_test <- diag(rf_conf_matrix_test)/rowSums(rf_conf_matrix_test)
rf_recall_test <- diag(rf_conf_matrix_test)/colSums(rf_conf_matrix_test)
rf_f1_score_test <- 2 * (rf_precision_test * rf_recall_test) / (rf_precision_test + rf_recall_test)

# Create a data frame for all metrics
all_metrics_table <- data.frame(
  Metric = rep(c("Accuracy", "Precision", "Recall", "F1 Score"), 2),
  Model = rep(c("Proportional Odds", "Random Forest"), each = 4),
  Training = c(accuracy_train, mean(precision_train, na.rm = TRUE), mean(recall_train, na.rm = TRUE), mean(f1_score_train, na.rm = TRUE),
               rf_accuracy_train, mean(rf_precision_train, na.rm = TRUE), mean(rf_recall_train, na.rm = TRUE), mean(rf_f1_score_train, na.rm = TRUE)),
  Testing = c(accuracy_test, mean(precision_test, na.rm = TRUE), mean(recall_test, na.rm = TRUE), mean(f1_score_test, na.rm = TRUE),
              rf_accuracy_test, mean(rf_precision_test, na.rm = TRUE), mean(rf_recall_test, na.rm = TRUE), mean(rf_f1_score_test, na.rm = TRUE))
)

# Print the combined table
knitr::kable(all_metrics_table, caption = "Model Comparison Metrics")

