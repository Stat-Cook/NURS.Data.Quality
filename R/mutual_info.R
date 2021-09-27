encode_values <- function(values){
  #' Perform a label encoding on a categorical varaible.
  #' 
  #' @param values Vector of values to be encoded.
  #' 
  #' @return Vector of integer values
  #' @export 
  as.numeric(as.factor(values))
}

mutual_info <- function(data){
  #' Calculate mutual information between pairs of variables in a data frame
  #' 
  #' @param data Data frame (n by k) of observations
  #' 
  #' @return matrix (k by k) of mutual information.  Diagonal represents total information a varaible has.
  #' @export
  copied <- data
  numeric_columns <- sapply(copied, is.numeric)
  factor_columns <- colnames(copied[!numeric_columns])

  for (name in factor_columns){
    values <- unlist(copied[, name])
    copied[, name] <- encode_values(values)
  }
  
  disc.df <- infotheo::discretize(copied)
  K <- dim(copied)[2]

  info <- sapply(
    1:K, 
    function(i) sapply(
      1:K, 
      function(j) infotheo::multiinformation(disc.df[,c(i,j)])
    )
  )
  
  rownames(info) <- colnames(copied)
  colnames(info) <- colnames(copied)
  
  info
}

