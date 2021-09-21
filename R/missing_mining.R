library(tidyverse)

prepare_data <- function(data, outcome){
  #' Dummy data and impute missing values for supplied data set, subdividing into test and train set.
  #' 
  #' @param data Data frame of values to be divided.
  #' @param outcome The column of data frame to be treated as the outcome state.
  #' 
  #' @return list(train_data, test_data)
  y <- data[,outcome] %>% is.missing() %>% factor(levels=c(FALSE, TRUE))
  x <- data %>% select(-outcome)

  dummier <- dummyVars( ~ ., data = x)
  dummied <- predict(dummier, x)
  
  processer <- caret::preProcess(dummied, method="knnImpute")
  processed <- data.frame(predict(processer, dummied))
  
  frame <- cbind(processed, y)
  
  trainIndex <- caret::createDataPartition(y, p=0.8)$Resample1
  
  list(
    train_data = frame[trainIndex, ],
    test_data = frame[-trainIndex, ]
  )  
}

build_model <- function(data, method="rpart"){
  #' Constuct model via `caret` call
  #' 
  #' @param data Data frame for analysis
  #' @param method Method call from `carat::train` to use for binary-classication
  #' 
  #' @return `caret` model 
  caret::train(y ~ ., data=data, method=method)
}

model_roc <- function(data, model){
  #' Calculates model quality via ROC AUC.
  #' 
  #' @param data Data frame of values to be analyzed.
  #' @param model `caret` model that has been pre-trained.
  #' 
  #' @return ROC value
  outcome <- data.frame(
    obs = data[,"y"],
    pred = predict(model, data)
  )
  outcome <- cbind(
    outcome,
    predict(model, data, type="prob")
  )
  result <- twoClassSummary(outcome, lev=c(FALSE, TRUE))
  result["ROC"]
}


missing_mine <- function(data, method="rpart"){
  #' Analyze each column of a data set that has missing values for patterns of missingness.
  #' 
  #' @param data Data frame to be analyzed
  #' @param method `caret::train` method to be applied.
  #' 
  #' @return Vector of ROC values.  Values greater than 0.5 imply some predictve power.
  any_missing <- apply(is.missing(data), 2, any)
  missing_frame <- data[any_missing]
  results <- blank_result(data)

  for (name in names(missing_frame)){
    test.train <- prepare_data(data, name)
    
    model <- build_model(test.train$train_data, method)
    
    results[name] <- model_roc(test.train$test_data, model)  
  }
  results
}
