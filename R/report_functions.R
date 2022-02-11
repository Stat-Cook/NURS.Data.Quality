loc_date_DQ_report <- function(data, location.col, date.col,
                            output_path){
  #' Automatically generate a RMD report of data quality issues.
  #' 
  #' @param data The data set to be reported on
  #' @param location.col The column of `data` to treat as a location
  #' @param date.col The column of `data` to treat as a date,
  #' @param output_path The location on file path at which to save the report.
  #'     NB: '_Loc_Date_Report.html' will be appended
  #'     
  #' @return  
  #' @export
  location.string <- substitute(location.col)
  date.string <- substitute(date.col)
  
  temp <- tempfile()
  saveRDS(data, temp)
  
  render.params <- list(
    "location_variable" =  location.string, 
    "date_variable" = date.string,
    path = temp
  )

  output.string <- glue("{getwd()}/{output_path}_Loc_Date_Report.html")
  set.seed(1337)

  template.path <- system.file("rmarkdown", "Loc_Date_DQ_Template.Rmd", 
                   package="NURS.Data.Quality")
  
  rmarkdown::render(template.path, 
                    output_file = output.string, 
                    params  = render.params)
}

missing_DQ_report <- function(data, output_path){
  #' Automatically generate a RMD report of data quality issues.
  #' 
  #' @param data The data set to be reported on
  #' @param output_path The location on file path at which to save the report.
  #'     NB: '_Misising_DQ_Report.html' will be appended
  #'     
  #' @return  
  #' @export

  temp <- tempfile()
  saveRDS(data, temp)
  
  render.params <- list(
    path = temp
  )
  
  output.string <- glue("{getwd()}/{output_path}_Missing_DQ_Report.html")
  set.seed(1337)
  
  template.path <- system.file("rmarkdown", "Missing_Report_DQ_Template.Rmd", 
                               package="NURS.Data.Quality")
  
  rmarkdown::render(template.path, 
                    output_file = output.string, 
                    params  = render.params)
}

