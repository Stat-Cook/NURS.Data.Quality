library(yaml)

validation_list <- list()

load_validation <- function(file_path){
  #' @export
  load_yaml(file_path)
}

write_validation <- function(validation, file_path){
  
  #' @export
  write_yaml(validation, file_path)
}

add_validation_date_time <- function(validation, column){
  #' @export
  validation[[column]] <- "DateTime"
  validation
}

add_validation_numeric <- function(validation, column){
  #' @export
  validation[[column]] <- "Numeric"
  validation
}

add_validation_values <- function(validation, column, values=NA){
  #' @export
  validation[[column]] <- values
  validation
}
