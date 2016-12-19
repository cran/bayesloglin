checkArgsMC3 <-
function (init, alpha, iterations, replicates, data) {

  if (!is.character(init) && !is.null(init)) {
    stop ("class(init) must be 'character' or 'NULL'")
  }
  else if (!is.numeric(alpha)) {
    stop("class(alpha) != 'numeric'")
  }
  else if (!is.numeric(iterations)) {
    stop ("class(iterations) != 'numeric'")
  }
  else if (!is.numeric(replicates)) {
    stop ("class(replicates) != 'numeric'")
  }
  else if (!is.data.frame(data)) {
    stop ("class(data) != 'data.frame'")
  }
  else if (!is.numeric(data[,dim(data)[2]])) {
    stop ("last column of data must be cell frequencies")
  }
  else if (sum(data[,dim(data)[2]] >= 0) != dim(data)[1]) {
    stop ("all frequencies must be non-negative")
  }
  else if (dim(data)[2] < 3) {
     stop ("data must have at least 2 variables")
  } 
  else if (alpha <= 0) {
    stop ("alpha must be positive")
  }
  else if (iterations - floor(iterations) != 0 || iterations <= 0) {
    stop ("replicates must be a positive integer")
  }
  else if (replicates - floor(replicates) != 0 || replicates <= 0) {
    stop ("iterations must be a positive integer")
  }
    
  for (i in 1:length(init))
    x <- as.formula(init[i])
}
