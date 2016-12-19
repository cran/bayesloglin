is.graphical <-
function (formula, tools) {

  graph <- findGraph (formula, tools)
  
  cliques <- findCliques(graph, tools)
 
  model <- findModel (formula, tools)
  generators <- findGenerators (model, tools)

  if (sum (generators == cliques) == tools$nVarSets)
    return (TRUE)
  else
    return (FALSE)

}
