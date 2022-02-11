prepare.data <- function(feature, target, extract_name_f=extract_name){
  #'
  #' @noRd  
  UseMethod("prepare.data", feature)
}


prepare.data.character <- function(feature, target, extract_name_f=extract_name){
  #'
  #' @noRd  
  dp <- createDataPartition(target, p = .8, 
                            list = FALSE, 
                            times = 1)

  feature <- replace_na(feature, "Unknown")
  categories <- choose.categories(feature[dp], maxitems = 30)
  names(categories) <- categories
  feature <- categories[feature] %>% replace_na("Other")
  naming_vector <- unique(feature)
  names(naming_vector) <- naming_vector
  
  list(xTrain =feature[dp], yTrain = target[dp],
       xTest = feature[-dp], yTest = target[-dp],
       naming_vector = naming_vector)
}

prepare.data.POSIXct <- function(feature, target, extract_name_f=extract_name){
  #'
  #' @noRd  
  feature.numeric <- feature %>% as.numeric()
  
  prepare.data(feature.numeric, target, extract_name.POSIXct)
}

prepare.data.default <- function(feature, target, extract_name_f=extract_name){
  #'
  #' @noRd  
  dp <- createDataPartition(target, p = .8, 
                            list = FALSE, 
                            times = 1)
  
  xTrain <- feature[dp]
  xTest <- feature[-dp]
  
  disc <- recipes::discretize(xTrain, na.rm=T, cuts =20)
  
  ref <- data.frame(Group = predict(disc, xTrain), Value = xTrain)  
  ref <- extract_name_f(ref)
  
  naming_vector <- ref$name
  names(naming_vector) <- ref$Group
  
  list(xTrain = predict(disc, xTrain), yTrain = target[dp],
       xTest = predict(disc, xTest), yTest = target[-dp],
       naming_vector = naming_vector)
  
}

year_month <- function(value){
  #'
  #' @noRd  
  paste(month(value), year(value), sep="/")
}

extract_name <- function(data){
  #' 
  #' @noRd  
  data %>% group_by(Group) %>% summarise(start = min(Value), end=max(Value)) %>% 
    mutate(name = paste(start, "-", end))
}

extract_name.POSIXct <- function(data){
  #'
  #' @noRd  
  data %>% group_by(Group) %>% 
    summarise(
      start = as.POSIXct(min(Value), origin='1970-01-01'), 
      end=as.POSIXct(max(Value), origin='1970-01-01')
    ) %>% 
    mutate(name = paste(year_month(start), "-", year_month(end)))
}
