randomDecomposableGraph <-
function (p, tools) {

  graph <- randomGraph(p = 0.1, tools)

  x <- is.chordal(graph, fillin = T)

  if (x$chordal == T)
    return(graph)
  else 
    adjacencyMatrix <- get.adjacency(graph)

  for (i in seq(1,length(x$fillin),2)) 
    adjacencyMatrix[x$fillin[i],x$fillin[i+1]] <- adjacencyMatrix[x$fillin[i+1],x$fillin[i]] <- 1
  
  return(graph.adjacency(as.matrix(adjacencyMatrix), mode = "undirected"))      
}
