
choose.categories <- function(values, cutoff = 0.8, min.use = 1, maxitems=NA){
  #'
  #'
  #' @noRd  
  v <- values %>% table(useNA = "ifany")
  total <- sum(v)
  sorted <- v %>% sort()
  if (is.na(maxitems)){
    top.percentage <- sorted  %>% cumsum() %>% .[. >= (1-cutoff)*total] %>% names()
    non.unique <- v %>% .[. > min.use] %>% names()
    overlap <- intersect(top.percentage, non.unique)
    return(overlap)
  }
  
  rev(sorted)[1:maxitems] %>% names()
}

make.limited.class.encoder <- function(trainingData){
  #'
  #'
  #' @export
  common.classes <- list()
  temp <- trainingData
  for (key in colnames(trainingData)){
    values <- trainingData[[key]]
    common.classes[[key]] <- choose.categories(values, 
                                               cutoff = 0.8, min.use = 1)
  }
  
  f <- function(data) {
    for (key in names(data)) {
      if(key %in% names(common.classes)) {
        values <- data[[key]]
        classes <- common.classes[[key]]
        names(classes) <- classes
        data[[key]] <- classes[values] %>% replace_na("Other")
      }
    }
    data
  }
  
  return(f)
}