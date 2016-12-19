multiplicity <-
function (graph, cliques, separators, tools) {

  nSeparators <- length(separators)
  nCliques <- length(cliques)
  m <- array (0, nSeparators)
  varNames <- tools$varNames 
   
  for (i in 1:nSeparators) {

    if (length(separators[[i]]) == 0)
        m[i] <- clusters(graph)$no - 1
    else {     
      clusters <- clusters(delete_vertices(graph, separators[[i]]))
      nClusters <- clusters$no
      clusterVars <- varNames[-separators[[i]]]

      for (j in 1:nClusters) {
        v <- union(separators[[i]]$name, clusterVars[clusters$membership == j])
        tempGraph <- delete_vertices(graph, varNames[!varNames %in% v])
        tempCliques <- maximal.cliques(tempGraph)
        nTempCliques <- length(tempCliques)
        isClique <- F
        for (k in 1:nTempCliques) { 
          if (sum(!tempCliques[[k]]$name %in% separators[[i]]$name) == 0) { 
             isClique = T
             break
          }
        }  
        if (isClique == F)
          m[i] <- m[i] + 1
      }  
      m[i] <- m[i] - 1  
    }   
  }
  if (sum(m) != nCliques - 1) {
      print (maximal.cliques(graph))
      print ("Bug: nSeparators != nCliques - 1 including multiplicities")
  } 
  return(m) 
}
