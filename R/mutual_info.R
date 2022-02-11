encode_values <- function(values){
  #' Perform a label encoding on a categorical varaible.
  #' 
  #' @param values Vector of values to be encoded.
  #' 
  #' @return Vector of integer values
  #' @noRd  
  as.numeric(as.factor(values))
}

discrete.data <- function(data, ...){
  #' Discretize data after encoding factors
  #' 
  #' Converts non-numeric data to factors and discretizes all features.
  #' 
  #' @param data The data set to be discretized. 
  #' @param ... Extra arguments to pass to the infotheo::discretize call.
  #' 
  #' @export
  #' @noRd
  copied <- data
  numeric_columns <- sapply(copied, is.numeric)
  factor_columns <- colnames(copied[!numeric_columns])
  
  for (name in factor_columns){
    values <- unlist(copied[, name])
    copied[, name] <- encode_values(values)
  }
  
  infotheo::discretize(copied, ...)
}

mutual_info <- function(data, ...){
  #' Calculate mutual information between pairs of variables in a data frame
  #' 
  #' TODO: replace uses with infotheo::mutinformation.
  #' 
  #' @param data Data frame (n by k) of observations
  #' 
  #' @return matrix (k by k) of mutual information.  Diagonal represents total information a varaible has.
  #' @noRd
  disc.df <- discrete.data(data, ...)

  infotheo::mutinformation(disc.df)
}

column_info_func <- function(col1, col2){
  #' Function factory for mutual information.
  #' 
  #' Creates a function to be called in the tidyverse that calculates 
  #' mutual information between variables of a data set.  Intended to be used as 
  #' part of a group_by(...) %>% group_map(column_info_func(...)) workflow. 
  #' 
  #' @param col1 Name of the first column
  #' @param col2 Name of the second column
  #' 
  #' @examples 
  #' ChickWeight %>% group_by(Diet)  %>% 
  #'     group_map(column_info_func("Chick", "weight")) %>% 
  #'     do.call(rbind, .)
  #' 
  #' @noRd
  #' 
  f <- function(data, ...){
    args <- c(...)
    c(unlist(args), infotheo::mutinformation(data[,c(col1, col2)]))
  }
  f
}

