standardizeColumnNames <-
function(X) {
  for (i in 1:dim(X)[2]) {
    s <- strsplit(colnames(X)[i], ":")[[1]]
    s <- sort(s)
    s <- paste(s, collapse = ":")
    colnames(X)[i] <- s
  }
  return(X) 
}
