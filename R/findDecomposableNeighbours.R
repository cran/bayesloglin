findDecomposableNeighbours <-
function (graph, tools) {

  adjacencyMatrix <- get.adjacency(graph)
  neighbours <- array(0, c(tools$n,tools$n))
  nNeighbours <- 0

  for (i in 1:(tools$n-1)) {
    for (j in (i+1):tools$n) {
      adjacencyMatrix [i,j] <- 1 - adjacencyMatrix[i,j]
      adjacencyMatrix[j,i] <- adjacencyMatrix[i,j]
      potentialNeighbourGraph <- graph.adjacency(as.matrix(adjacencyMatrix), mode = "undirected")
      if (is.chordal(potentialNeighbourGraph)$chordal == T) {
        neighbours[i,j] <- 1
        nNeighbours <- nNeighbours + 1
      }
      adjacencyMatrix [i,j] <- 1 - adjacencyMatrix[i,j]
      adjacencyMatrix[j,i] <- adjacencyMatrix[i,j]
    }
  }
  
  return(as.matrix(neighbours))

}
