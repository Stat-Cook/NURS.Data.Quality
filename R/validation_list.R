library(yaml)

validation_list <- list()

load_validation <- function(file_path){
  load_yaml(file_path)
}

write_validation <- function(validation, file_path){
  write_yaml(validation, file_path)
}

add_validation_date_time <- function(validation, column){
  validation[[column]] <- "DateTime"
  validation
}

add_validation_numeric <- function(validation, column){
  validation[[column]] <- "Numeric"
  validation
}

add_validation_values <- function(validation, column, values=NA){
  validation[[column]] <- values
  validation
}
