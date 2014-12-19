###############################
# Cluster facult into groups
# Based on research
###############################

get_values <- function(key, medline_records, unique=FALSE, sep=FALSE){
  vals <- lapply(medline_records, function(x) x[key][[1]])
  vals <- unlist(vals)
  if(is.character(sep)){
    vals <- lapply(vals, function(x) strsplit(x, sep)[[1]])
    vals <- unlist(vals)
  }
  if(unique){
    return(unique(vals))
  }
  else {
    return(vals)
  }
}
    
