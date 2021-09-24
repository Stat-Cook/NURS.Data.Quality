data.quality <- function(data) {
  #' Data quality measures
  #' 
  #' Perform missing and misscoding checks on supplied data set
  #' 
  #' @param data Data frame to perform data quality checks on.
  #' 
  #' @return Data frame summarizing data quality metrics.
  #' @export
  tibble(
    Variable = colnames(data),
    `Singular data` = compare_frame_frequencies(data),
    `Too many distinct values` = compare_frame_classes(data),
    `High Missing Rate` = average_missing_heuristic(data),
    `Missing ROC` = missing_mine(data)
  )
}


start_mutual_info_app <- function(){
  #' Start app for visualizing mutual info
  #' 
  #' 
  #' 
  #' @export

  # appDir <- system.file("app", "myapp", package = "mypackage")
  # if (appDir == "") {
  #   stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  # }
  shiny::runApp("Mutual_Info/app.R")
}
