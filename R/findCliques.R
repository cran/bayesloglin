findCliques <-
function (graph, tools) {

  cliques <- rep (0, tools$nVarSets) 
  m <- maximal.cliques (graph)

  for (i in 1:length(m))
    cliques [binToDec(tools$varNames %in% V(graph)$name[m[[i]]]) + 1] <- 1    
 
  return(cliques)    

}
