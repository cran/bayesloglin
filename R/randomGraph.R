randomGraph <-
function (p, tools) {
  
  adjacencyMatrix <- matrix (nrow = tools$n, ncol = tools$n)
  colnames(adjacencyMatrix) <- rownames(adjacencyMatrix) <- tools$varNames
  diag(adjacencyMatrix) <- rep(0,tools$n)  
 
  for (i in 1:(tools$n-1))
    for (j in (i+1):tools$n) 
      adjacencyMatrix[i,j] <- adjacencyMatrix[j,i] <- rbinom(1, 1, p)  
    
  return(graph.adjacency (adjacencyMatrix, mode = "undirected"))
  
}
