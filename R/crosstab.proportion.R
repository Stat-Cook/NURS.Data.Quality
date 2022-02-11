crosstab.proportion <- function(data, target, group_var){
  #' Calculate cross tab and row proportions between two variables.
  #' 
  #' @param target Variable to calculate proportions of.
  #' @param group_var Variable to group data by. 
  #' 
  #' @noRd  
  target.string <- substitute(target)
  group_var.string <- substitute(group_var)
    
  f <- formula(glue("~ {group_var.string} + {target.string}"))
  
  proportions <- data %>% xtabs(f, data=.) %>% prop.table(1)
  proportions %>% as.matrix() %>% as.data.frame.matrix()*100
}
