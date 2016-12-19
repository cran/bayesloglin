MC3.Decomposable <-
function (init, alpha, iterations, replicates, data) {
  
  tools <- getTools(alpha, data)
  nVarSets <- tools$nVarSets
  n <- tools$n

  dimens <- array (0, c(dim(data)[2]-1))
  
  for (j in 1:(dim(data)[2]-1)) {
    v <- as.factor(data[,j])
    dimens[j] <- length(levels(v))  
    data[,j] <- as.double(v) - 1  
  }
  
  tData <- t(data)
  
  row.names(dimens) <- colnames(data[,1:dim(data)[2]-1])

  tools <- getTools(alpha, data)
  numStartModels <- length(init)
  
  masterList <- c()
  
  for (r in 1:replicates) {
  
    if (r > numStartModels) {
      currentGraph <- randomDecomposableGraph(p = 0.2, tools)
      currentGenerators <- findCliques (currentGraph, tools)
      currentFormula <- findFormula(currentGenerators, tools)
      currentModel <- findModel(currentFormula, tools)
    }
    else { 
      currentModel <- findModel(init[r], tools)
      currentGenerators <- findGenerators(currentModel, tools)  
      currentFormula <- findFormula(currentGenerators, tools)
      currentGraph <- findGraph(currentFormula, tools)
      if (!is.chordal(currentGraph)$chordal)
        stop(paste("model [", r, "] in startList is not decomposable"))
    }
   
    currentDualGenerators <- findDualGenerators (currentModel, tools)
    currentPrettyFormula <- prettyFormula(currentGenerators, tools)
    currentLogPostProb <- findLogMargLik (data, tData, currentGraph, dimens, tools)
    #currentLogPostProb <- logLaplace(currentFormula, postData) - logLaplace(currentFormula, priorData)      
    currentNeighbourList <- findDecomposableNeighbours(currentGraph, tools)
    currentNumNeighbours <- sum(currentNeighbourList)
    currentAdjacencyMatrix <- get.adjacency(currentGraph)   

    masterList <- data.frame(formula = currentPrettyFormula, logPostProb = currentLogPostProb, stringsAsFactors = F)

    i <- 1
    rejected <- 0
    cat("\n") 

    while(i <= iterations) {

      # choose a random neighbour
      chosenNeighbour <- sample (x = currentNumNeighbours, size = 1, prob = rep(1, currentNumNeighbours)) 
      
      proposalAdjacencyMatrix <- currentAdjacencyMatrix
      k <- 0

      for (row in 1:(n-1)) {
        for (col in (row+1):n) {
          if (currentNeighbourList[row,col])
            k <- k + 1
          if (k == chosenNeighbour) {
            proposalAdjacencyMatrix[row,col] <- 1 - proposalAdjacencyMatrix[row,col]
            proposalAdjacencyMatrix[col,row] <- proposalAdjacencyMatrix[row,col]
            break
          }  
        }
        if (k == chosenNeighbour)
          break
      }         
            
      proposalGraph <- graph.adjacency(as.matrix(proposalAdjacencyMatrix), mode = "undirected")
      proposalGenerators <- findCliques(proposalGraph, tools) 
      proposalFormula <- findFormula(proposalGenerators, tools)
      proposalModel <- findModel(proposalFormula, tools)
      proposalDualGenerators <- findDualGenerators (proposalModel, tools)
      
      proposalNeighbourList  <- findDecomposableNeighbours (proposalGraph, tools)
      proposalNumNeighbours <- sum(proposalNeighbourList)
      proposalPrettyFormula <- prettyFormula(proposalGenerators, tools)    

      if (!is.chordal(proposalGraph)$chordal) {
        g <- currentGraph
        stop()
      }

      inMasterList <- proposalPrettyFormula %in% masterList$formula
  
      if (inMasterList) 
        proposalLogPostProb <- masterList$logPostProb[which(masterList$formula == proposalPrettyFormula)]
      else {
        #proposalLogPostProb <- logLaplace(proposalFormula, postData) - logLaplace(proposalFormula, priorData)
        proposalLogPostProb <- findLogMargLik (data, tData, proposalGraph, dimens, tools)
        masterList <- rbind(masterList, data.frame(formula = proposalPrettyFormula, logPostProb = proposalLogPostProb, stringsAsFactors = F)) 
      }

      outputMessage1 <- paste ("replicate [", r, "], ", "iteration [", i, "].", sep = "")
      outputMessage2 <- paste ("proposal model = ", proposalPrettyFormula, sep = "")
      outputMessage3 <- paste ("proposal logPostProb = ", round(proposalLogPostProb,digits = 2), sep = "")
      
      u <- runif(n = 1,min = 0,max = 1)
            
      ratio <- proposalLogPostProb - currentLogPostProb - log(proposalNumNeighbours) + log(currentNumNeighbours)

      if(ratio>0) 
        ratio <- 0

      if (log(u) <= ratio) { 
        # Move is accepted
        currentGraph <- proposalGraph
        currentPrettyFormula <- proposalPrettyFormula
        currentLogPostProb <- proposalLogPostProb
        currentNeighbourList <- proposalNeighbourList
        currentNumNeighbours <- proposalNumNeighbours
        currentAdjacencyMatrix <- proposalAdjacencyMatrix
        outputMessage4 <- paste ("proposal accepted", sep = "")
      }
      else {
        rejected <- rejected + 1
        outputMessage4 <- paste ("proposal rejected", sep = "")        
      }

      i <- i + 1
      outputMessage5 <- paste ("rejection rate = ", round(rejected / i, digits = 2))   
      cat(outputMessage1, "\n", outputMessage2, "\n", outputMessage3, "\n", outputMessage4, "\n", outputMessage5, "\n\n", sep = "")
       
    }
  } 

  masterList <- masterList[order(masterList$logPostProb, decreasing = T),]
  rownames(masterList) <- rep(1:dim(masterList)[1])
  return(masterList)  

}
