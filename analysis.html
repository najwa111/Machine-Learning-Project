---
tittle: "Machine Learning Project"
author: "Alifia Najwa"
date: html_document
---

# load packages
library(caret)
library(randomForest)

# missing values
na_percentage <- sapply(data, function(x) mean(is.na(x)))
data_clean <- data[, na_percentage < 0.9]  # remove columns with >90% NA

# remove columns non-predictive
data_clean <- data_clean %>% 
  select(-c(X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, 
            cvtd_timestamp, new_window, num_window))
            
# convert variable target
data_clean$classe <- as.factor(data_clean$classe)

# data participation (70% training, 30% testing)
set.seed(123)
trainIndex <- createDataPartition(data_clean$classe, p = 0.7, list = FALSE)
training <- data_clean[trainIndex, ]
testing <- data_clean[-trainIndex, ]

# model training
trControl <- trainControl(method = "cv", number = 5)
model_rf <- train(classe ~ ., 
                  data = training, 
                  method = "rf",
                  trControl = trControl,
                  verbose = FALSE)

# model result
Accuracy: 0.9923  
Out-of-Sample Error Estimate: 0.77%

# prediction on testing set
predictions <- predict(model_rf, newdata = testing)
confusionMatrix(predictions, testing$classe)

# evaluation result
Accuracy : 0.993  
95% CI : (0.991, 0.994)

# estimation Out-of-Sample Error
model_rf$results$Accuracy  # Menunjukkan akurasi validasi silang

