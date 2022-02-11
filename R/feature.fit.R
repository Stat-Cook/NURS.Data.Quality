feature.fit <- function(target, feature){
  #'
  #'
  #' @noRd  
  tts <- prepare.data(feature, target)
  
  training <- data.frame(x=tts$xTrain, y=tts$yTrain)
  test <- data.frame(x=tts$xTest, y=tts$yTest)
  
  naming.f <-  function(var, lvl, ordinal = FALSE, sep = "_"){
    lvl
  }
  
  rec <- recipe(y ~ x, data=training) %>%
    step_dummy(x, naming = naming.f, one_hot = T) %>% 
    prep()
  
  embed <- juice(rec)
  embedTest <- bake(rec, new_data=test)
  
  model <- MASS::lda(y ~ -1 + ., data=embed)
  
  phat <- predict(model, embedTest, type="response")
  h <- phat$posterior   %>% as.data.frame()
  phat <- h[["TRUE"]]
  
  result <- tibble(obs = embedTest$y, pred=phat) %>% roc_("obs", "pred")
  list(AUC = result$auc,  
       model = model, 
       naming_vector = tts$naming_vector)
}
