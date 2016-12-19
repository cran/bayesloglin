findLogMargLik <-
function (data, tData, graph, dimens, tools) {

  cliques <- maximal.cliques (graph)
  nCliques <- length(cliques)
  
  separators <- minimal.st.separators (graph)
  nSeparators <- length(separators)
  m <- multiplicity (graph, cliques, separators, tools)
  alpha <- tools$alpha
  
  term1 <- 0
  term2 <- 0

  for (i in 1:nCliques) {
    margFreqs <- findMargFreqs (data, tData, cliques[[i]], dimens)
    nMargFreqs <- length(margFreqs)  
    term1 <- term1 + sum(lgamma(margFreqs + alpha / nMargFreqs))  
    term2 <- term2 + nMargFreqs * lgamma (alpha / nMargFreqs)
  }
  
  for (i in 1:nSeparators) 
    if (length(separators[[i]]) > 0) {
      margFreqs <- findMargFreqs (data, tData, separators[[i]], dimens)
      nMargFreqs <- length(margFreqs)  
      term1 <- term1 - m[i] * sum(lgamma(margFreqs + alpha / nMargFreqs))  
      term2 <- term2 - m[i] * nMargFreqs * lgamma (alpha / nMargFreqs)
    }
    else {
      # empty separators 
      term1 <- term1 - m[i] * lgamma(sum(data$freq) + alpha) 
      term2 <- term2 - m[i] * lgamma(alpha)
    }

  #term1 <- term1 - (sum(data$freq) + alpha) * log(alpha + 1)
  #term2 <- term2 - alpha * log(alpha)

  return(term1 - term2)

}
