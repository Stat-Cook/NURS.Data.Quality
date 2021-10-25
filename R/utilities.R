blank_result <- function(data){
  #' Generate a blank result vector from a data frame
  #' 
  #' @param data The data frame to template the result vector from.
  #' @export

  K <- dim(data)[2]
  result <- rep(NA, K)
  names(result) <- colnames(data)
  result
}

apply_to_dataframe <- function(data, column_function){
  #' Apply a function to every variable of a data frame.
  #' 
  #' @param data Data Frame to be analyzed
  #' @param column_function Function to be applied to each varaible.
  #' 
  #' @return Vector of results.
  #' @export
  result <- blank_result(data)
  for (name in names(result)){
    values <- data[,name]
    result[name] <- column_function(values)
  }
  result
}



unique2clipboard <- function(values, column_name = NA){
  #' Send values to clipboard
  #' 
  #' Finds all unique values of the vector given, concatenates with new lines and 
  #' sends to the clipboard.  Can be pasted into excel as a column.
  #' 
  #' @param values A vector of values to be processed
  #'
  uni <- unique(values)
  string <- paste(uni, collapse="\n")
  writeClipboard(string)
}