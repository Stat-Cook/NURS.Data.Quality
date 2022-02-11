data.quality <- function(data) {
  #' Data quality measures
  #' 
  #' Perform missing and misscoding checks on supplied data set
  #' 
  #' @param data Data frame to perform data quality checks on.
  #' 
  #' @return Data frame summarizing data quality metrics.
  #' 
  #' @example 
  #' data.quality(ChickWeight)
  #' 
  #' @importFrom tibble tibble
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
  #' TODO: remap so works on install.
  #' 
  #' @export
  #' 
  #' 
  path <- system.file("shiny_apps", "Mututal_Info", "app.R",  
    package="NURS.Data.Quality")
  
  shiny::runApp(path)
}
