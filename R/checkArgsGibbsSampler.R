checkArgsGibbsSampler <-
function (formula, alpha, data, nSamples) {

  if (!is.numeric(alpha)) {
    stop("class(alpha) != 'numeric'")
  }
  else if (alpha <= 0) {
    stop ("alpha must be positive")
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
  else if (!is.numeric(nSamples)) {
    stop ("class(nSamples) != 'numeric'")
  }
  else if (nSamples - floor(nSamples) != 0 || nSamples <= 0) {
    stop ("nSamples must be a positive integer")
  }
  x <- as.formula(formula)
}
