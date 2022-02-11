sqrt_comparison <- function(l_unique, l_total){
  #' Function for comparing two values.  
  #' 
  #' @param l_unique number of unique values
  #' @param l_total number of observations
  #' 
  #' @return Boolean - TRUE: more unique values than the square root of the number of observations.
  #' 
  #' @noRd  
  l_unique > sqrt(l_total)
}

compare_frequencies <- function(values, limit=0.8){
  #' Vector function - check that no single value of a 
  #' variable is used more than a set limit.
  #' 
  #'  @param values Vector of values to check
  #'  @param limit The upper limit on how often one value should occur.  Real value [0, 1]. 
  #'  
  #'  @return Boolean 
  #'  @export
  
  freq <- table(values)
  any(freq > limit*sum(freq))
}

compare_frame_frequencies <- function(data, limit=0.8){
  #' Tests all variables in a data frame to check values aren't used too often.
  #'
  #' @param data Data frame of variables to check each column of
  #' @param limit Max percentage of a variable that can be the same value
  #' 
  #' @return Boolean-vector
  #' @export
  results <- blank_result(data)
  for (name in names(data)){
    values <- data[,name]
    results[name] <- compare_frequencies(values, limit)
  }
  results
}

compare_classes <- function(values, comparison=sqrt_comparison){
  #' Checks if there are enough examples for the number of classes in a variable.
  #' 
  #' @param values Vector of observations
  #' @param comparison  [optional] A function to compare the number of unique classes with the number of observations.
  #' 
  #' @return Boolean - TRUE: not enough examples present
  #' @export
  l_total <- length(values)
  l_unique <- length(unique(values))
  comparison(l_unique, l_total)
}

compare_frame_classes <- function(data, comparison=sqrt_comparison){
  #' Check each variable of a data frame to determine if there are enough examples for the number of distinct values.  
  #' NB: will return a spurious result for continous valued variables.
  #' 
  #' @param data Dataframe of variables for checking
  #' @param comparison [optional] A function to compare the number of unique classes with the number of observations.
  #' 
  #' @return Boolean-vector -  TRUE: not enough examples present
  #' @export
  #' 
  results <- blank_result(data)
  for (name in names(data)){
    values <- data[,name]
    results[name] <- compare_classes(values, comparison)
  }
  results
}


