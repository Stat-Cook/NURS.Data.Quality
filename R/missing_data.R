MISSING_TYPES <- c(NA, "", " ", "NA", "nan")

is.missing <- function(values, missing_types=MISSING_TYPES){
  #' Vector function - identify which values are 'missing'
  #' 
  #' @param values Vector of values to check for missingness
  #' @param missing_types  Vector of values classified as missing.
  #' 
  #' @return Vector of which values are 'missing'
  #' @export
  
  sapply(values, function(i) i %in% missing_types)
}

average_missing <- function(data, missing_types=MISSING_TYPES){
  #' Dataframe function - measure the average rate of missingness on a column
  #' 
  #' @param data The dataframe to measure missingness from
  #' @param missing_type Vector of values classified as missing.
  #' 
  #' @return Vector of prevalence of missing values.
  
  missing_func <- function(values) mean(is.missing(values, missing_types))
  apply_to_dataframe(data, missing_func)
}

average_missing_heuristic <- function(data, limit=0.1, missing_types=MISSING_TYPES){
  #' Check if missing data rate is less than set limit.
  #' 
  #' @param data Data.frame of variables to be analyzed
  #' @param limit Allowable rate of missingness, valued [0, 1] 
  #' @param missing_types [optional] vector of valuse to define as missing.
  #' 
  #' @return Boolean-vector - TRUE: missing more data than the limit.
  #' @export
  
  average_missing(data, missing_types) > limit
  
}

