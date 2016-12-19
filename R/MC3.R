MC3 <-
function (init = NULL, alpha = 1, iterations = 5000, replicates = 1, data, mode = c("Hierarchical", "Graphical", "Decomposable")) {
  
  options(warn=-1)
  
  #igraph <- require(igraph)
  #if (!igraph) 
  #  stop ("igraph package required")

  checkArgsMC3 (init, alpha, iterations, replicates, data) 
  colnames(data)[dim(data)[2]] <- "freq"
  
  mode <- match.arg(mode)

  if (mode == "Hierarchical")
    return(MC3.Hierarchical(init, alpha, iterations, replicates, data))
  else if (mode == "Graphical")
    return(MC3.Graphical(init, alpha, iterations, replicates, data))
  else 
    return(MC3.Decomposable(init, alpha, iterations, replicates, data))

}
