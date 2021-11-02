proportions.frame <- function(values, ...){
  #' 
  #'
  #'
  values <- unlist(values)
  uni <- unique(values)
  results <- c()
  for (i in uni){
    results <- c(results, mean(values == i))
  }
  
  frame <- data.frame(value = 100*results, name=uni)
  frame
}

crosstab.proportion <- function(data, target, group_var){
  #'
  #' @export
  grps <- data %>% group_by({{group_var}})
  joined <- grps %>% select({{target}}) %>% 
    group_map(proportions.frame) %>% reduce(full_join, by="name")
  joined.vals <- joined %>% select(-name)
  colnames(joined.vals) <-  grps %>% select({{target}}) %>% group_map(~ .y) %>% unlist()
  rownames(joined.vals) <- joined$name
  
  joined.vals  %>% t()
}