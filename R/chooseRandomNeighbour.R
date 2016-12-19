chooseRandomNeighbour <-
function (graph, tools) {
  
  n <- tools$n
  adjacencyMatrix <- get.adjacency(graph)
  numNeighbours <- n*(n-1)/2
  m <- sample (x = numNeighbours, size = 1, prob = rep(1, numNeighbours))  
  
  k <- 0

  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      k <- k + 1
      if (k == m) {
        adjacencyMatrix[i,j] <- 1 - adjacencyMatrix[i,j]
        adjacencyMatrix[j,i] <- adjacencyMatrix[i,j]
        return(graph.adjacency (as.matrix(adjacencyMatrix), mode = "undirected"))     
      }
    }         
  }
}
