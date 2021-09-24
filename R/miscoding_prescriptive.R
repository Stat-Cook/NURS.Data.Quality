compare_functions <- function(value){
  #' Determines which function to use for validation
  #' 
  #' @param value 
  #' 
  #' @return  validation function
  #' @export
  #' 
  if (value[1] == "Numeric"){
    return(is.numeric)    
  }
  if (value[1] == "DateTime"){
    return(is.Date)
  }
  return(
    function(i) i %in% value
  )
}


compare <- function(data, col_name, validation){
  #' Apply a comparison function to a column of data
  #' 
  #' @param data The data set to be analyzed
  #' @param col_name The column of data to be considered
  #' @param validation A list of validation types.
  #' 
  #' @return quantity of data that obeys validation rule,  valued [0, 1].
  #' @export
  valid_set <- validation[[col_name]]
  valid_func <- compare_functions(valid_set)
  
  bool <- sapply(data[,col_name], valid_func)
  mean(bool)
}