library(modelr)

is.categorical <- function(i) {
  #'
  #'
  #' @noRd  
  !(is.POSIXct(i) | is.numeric(i))
}

not.singular <- function(i){
  #'
  #' @noRd  
  length(unique(i)) > 1
}

train.test.split <- function(data, target){
  #'
  #'
  #' @noRd  
  trainIndex <- caret::createDataPartition(target, p = 0.8)$Resample1
  list(train_data = data[trainIndex, ], train_target = target[trainIndex],
       test_data = data[-trainIndex, ], test_target = target[-trainIndex])
}

process.recipe <- function(data, target, seed=1337){
  #'
  #'
  #'
  #' @noRd  
  
  set.seed(seed)
  
  X <- data %>% select(-target)
  y <- data[[target]] %>% is.missing() %>% make.names() %>% as.factor()
  tts <- train.test.split(X, y)
  
  dfTrain <- cbind(tts$train_data, Y = tts$train_target)
  colnames(dfTrain) <- dfTrain %>% colnames() %>% make.names()
  
  dfTest <- cbind(tts$test_data, Y = tts$test_target)  
  colnames(dfTest) <- dfTrain %>% colnames() %>% make.names()
  
  dfTrain$`Incident Number` %>% is.numeric()
  encode.recipe <- recipe(Y ~ ., data=dfTrain) %>% 
    step_unknown(all_nominal_predictors(), new_level = "Blank") %>% 
    step_other(all_nominal_predictors(), threshold = 0.005) %>%
    step_zv(all_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_normalize(all_predictors(), - all_nominal_predictors()) %>%
    step_zv(all_predictors()) %>%
    prep()
  
  r1 <- recipe(Y ~ ., data=dfTrain) %>% 
    step_unknown(all_nominal_predictors(), new_level = "Blank") %>% 
    step_other(all_nominal_predictors(), threshold = 0.005) %>%
    step_zv(all_predictors()) %>%
    prep ()
  
  
  e1 %>% select(is.categorical) %>% colnames()
  
  e1 <- juice(r1)
  encoder <- e1 %>% select(is.categorical) %>% make.limited.class.encoder()
  encoder(e1)
  
  step_dummy(all_nominal_predictors()) %>%
    step_normalize(all_predictors(), - all_nominal_predictors()) %>%
    step_zv(all_predictors()) %>%
    prep()
  
  tidy(encode.recipe)
  tidy(encode.recipe, number=6)
  dfTrain$Cause.1 %>% max()
  summary(encode.recipe) %>% View()
  
  enc <- juice(encode.recipe)
  encTest <- bake(encode.recipe, dfTest)
  warnings()
  stdev <- enc %>% apply(2, sd)
  nill.var <- which(stdev == 0) %>% names()
  
  fitControl <- trainControl(
    method = "none", allowParallel = TRUE,
    summaryFunction = twoClassSummary, classProbs = TRUE,
    preProcOptions = list(thresh = 0.8))
  
  
  model <- discrim_linear() %>% fit(Y ~ ., data=)
  

  {
    model0 <- enc %>% select(-nill.var) %>% caret::train(
      Y ~ ., data=., method = "lda",
      trControl = fitControl, preProcess  = c("pca"))
  }
  phat <- predict(model0, newdata = encTest)
  embed <- cbind(, Y=as.character(enc$Y))
  encTest %>% dim  
  enc %>% select(-nill.var) %>% predict(model0, newdata = .)
  enc %>% is.na() %>% sum()
  
  encTest[is.na(encTest)] <- "other"
  
  hattest <- predict(model, encTest)
  
  enc %>% make.names()
  
  enc
  
  length(hattest)
  dim(encTest)
  enc %>% colnames()  
  encTest %>% colnames()
  
  data[[target]] <- data[[target]] %>% is.missing()
  string.formula <- glue("{target} ~ .")
  f <- as.formula(string.formula)
  recipe(f, data=data)
}


process_data <- function(data, missing_types = MISSING_TYPES){
  #'
  #'
  #'
  #' @noRd  
  
  date.str.match <- data %>% 
    apply(2, function(i) str_detect(i, "[0-9]{2}/[0-9]{2}/[0-9]{4}")) %>%
    apply(2, mean)
  date.index <- which(date.str.match > 0.1)
  
  data[date.index] <- sapply(
    date.index, 
    function(d) lubridate::parse_date_time(unlist(data[d]), "%d/%m/%Y")
  )
  
  categorical.data <- data %>% select_if(is.categorical) 
  categorical.data[is.missing(categorical.data)] <- "Unknown"
  
  trainIndex <- caret::createDataPartition(1:nrow(data), p = 0.8)$Resample1
  
  encode.function <- categorical.data[trainIndex,] %>% make.limited.class.encoder()
  encoded.data <- data %>% encode.function()
  
  train_data <- encoded.data[trainIndex,] %>% select_if(not.singular)
  train_target <- data[trainIndex,] %>% is.missing(missing_types = missing_types) %>% data.frame()
  colnames(train_target) <- colnames(data)
  test_data <- encoded.data[-trainIndex,]%>% select(colnames(train_data))
  test_target <- data[-trainIndex,] %>% is.missing(missing_types = missing_types) %>% data.frame()
  colnames(test_target) <- colnames(data)
  
  processer <- caret::preProcess(train_data, method = "medianImpute")
  imputed.train.data <- data.frame(predict(processer, train_data))
  colnames(imputed.train.data) <- colnames(train_data)
  imputed.test.data <- data.frame(predict(processer, test_data))
  colnames(imputed.test.data) <- colnames(test_data)
  
  list(imputed.train= imputed.train.data, imputed.test=imputed.test.data,
       missing.train = train_target, missing.test=test_target)
}

miss.mine.2 <- function(target, model.data=model.data){
  #'
  #'
  #'
  #' @noRd  
  xTrain <- model.data$imputed.train %>% dplyr::select(-target)
  yTrain <- model.data$missing.train %>% select(target) %>% unlist() %>% make.names()
  
  df <- cbind(xTrain, yTrain)
  colnames(df) <- c(colnames(xTrain), "Y")
  colnames(df) <- df %>% colnames() %>% make.names()
  df <- df %>% mutate(across(function(i)  ! is.categorical(i), as.numeric))
  
  fitControl <- trainControl(
    method = "none", allowParallel = TRUE,
    summaryFunction = twoClassSummary, classProbs = TRUE,
    preProcOptions = list(thresh = 0.8))
  
  df$Y <- as.factor(df$Y) 
  
  model <- caret::train(
    Y ~ ., data=df, method = "lda",
    trControl = fitControl, preProcess = c("center", "scale", "pca"))
  
  xTest <- model.data$imputed.test %>% select(-target)
  yTest <- model.data$missing.test %>% select(target) %>% unlist() %>% make.names()
  
  df.Test <- cbind(xTest, yTest)
  colnames(df.Test) <- c(colnames(xTest), "Y")
  colnames(df.Test) <- df.Test %>% colnames() %>% make.names()
  df.Test <- df.Test %>% mutate(across(function(i)  ! is.categorical(i), as.numeric))
  
  
  model.roc <-  model_roc(df.Test, model, "TRUE.")
  df.Test %>% colnames()
  list(
    model = model,
    roc = model.roc,
    varImp = tryCatch(varImp(model), error = function(e) NA)
  )

}



model_roc <- function (data, model, true.state="TRUE.") {
  #'
  #'
  #' @noRd  
  outcome <- data.frame(obs = data[, "Y"], pred = predict(model, 
                                                          data))
  outcome <- cbind(outcome, predict(model, data, type = "prob"))
  result <- outcome %>% roc_("obs", true.state)
  as.numeric(result$auc)
}


missing.on.missing <- function(data, target){
  #'
  #'
  #' @noRd  
  X <- data$missing.train %>% select(-target) %>% 
    select_if(not.singular)
  if (ncol(X) == 0){
    return(list(
      model = NA,
      roc = 0.5,
      varImp = NA
    ))
  }
  Y <- data$missing.train[[target]] %>% as.factor()  
  
  df <- cbind(X, Y)
  colnames(df) <- colnames(df) %>% make.names()
  model <- caret::train(
    Y ~ ., data=df, method = "rpart")
  
  X.Test <- data$missing.test %>% select(-target) 
  Y <- data$missing.test[[target]] %>% as.factor()
  df.Test <- cbind(X.Test, Y)
  colnames(df.Test) <- colnames(df.Test) %>% make.names()
  
  model.roc <-  model_roc(df.Test, model, "TRUE")
  df.Test %>% colnames()
  list(
    model = model,
    roc = model.roc,
    varImp = tryCatch(varImp(model), error = function(e) NA)
  )
}


