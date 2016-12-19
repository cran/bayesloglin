findGraph <-
function (formula, tools) {

  model <- findModel(formula, tools)
  model[tools$lenVarSets != 2] <- 0
  adjacencyMatrix <- array (0, c(tools$n,tools$n))
  colnames(adjacencyMatrix) <- rownames(adjacencyMatrix) <- tools$varNames

  for (i in 1:tools$nVarSets) {
    if (model[i] == 1) {
      k <- which(tools$varSets[i,] != 0)
      adjacencyMatrix[k[1], k[2]] <- adjacencyMatrix[k[2], k[1]] <- 1 
    }
  }          

  return(graph.adjacency (adjacencyMatrix, mode = "undirected"))

}
