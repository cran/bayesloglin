MC3.Graphical <-
function (init, alpha, iterations, replicates, data) {
  
  tools <- getTools(alpha, data)
  nVarSets <- tools$nVarSets

  postData <- priorData <- data
  postData$freq <- data$freq + alpha / length(data$freq)
  priorData$freq <- array (alpha / length(data$freq), c(length(data$freq)))

  numStartModels <- length(init)
  
  masterList <- c()
  
  for (r in 1:replicates) {
  
    i <- 1 
    rejected <- 0 

    if (r > numStartModels) {
      currentGraph <- randomGraph(p = 0.2, tools)
      currentGenerators <- findCliques (currentGraph, tools)
      currentFormula <- findFormula(currentGenerators, tools)
      currentModel <- findModel(currentFormula, tools)
    }
    else { 
      currentModel <- findModel(init[r], tools)
      currentGenerators <- findGenerators(currentModel, tools)  
      currentFormula <- findFormula(currentGenerators, tools)
      currentGraph <- findGraph(currentFormula, tools)
      if (!is.graphical(currentGraph, tools))
        stop(paste("model [", r, "] in startList is not graphical"))
    }
   
    currentDualGenerators <- findDualGenerators (currentModel, tools)
    currentPrettyFormula <- prettyFormula(currentGenerators, tools)
    currentLogPostProb <- logLaplace(currentFormula, postData) - logLaplace(currentFormula, priorData)      

    masterList <- data.frame(formula = currentPrettyFormula, logPostProb = currentLogPostProb, stringsAsFactors = F)

    i <- 1

    while(i < iterations) {

      # choose a random neighbour
      proposalGraph <- chooseRandomNeighbour (currentGraph, tools) 
      proposalGenerators <- findCliques(proposalGraph, tools) 
      proposalFormula <- findFormula(proposalGenerators, tools)
      proposalModel <- findModel(proposalFormula, tools)
      proposalDualGenerators <- findDualGenerators (proposalModel, tools)
      
      proposalPrettyFormula <- prettyFormula(proposalGenerators, tools)    

      inMasterList <- proposalPrettyFormula %in% masterList$formula
  
      if (inMasterList) 
        proposalLogPostProb <- masterList$logPostProb[which(masterList$formula == proposalPrettyFormula)]
      else {
        proposalLogPostProb <- logLaplace(proposalFormula, postData) - logLaplace(proposalFormula, priorData)
        masterList <- rbind(masterList, data.frame(formula = proposalPrettyFormula, logPostProb = proposalLogPostProb, stringsAsFactors = F)) 
      }

      outputMessage1 <- paste ("replicate [", r, "], ", "iteration [", i, "].", sep = "")
      outputMessage2 <- paste ("proposal model = ", proposalPrettyFormula, sep = "")
      outputMessage3 <- paste ("proposal logPostProb = ", round(proposalLogPostProb,digits = 2), sep = "")
      
      u <- runif(n = 1,min = 0,max = 1)
      ratio = proposalLogPostProb - currentLogPostProb
      if(ratio>0) 
        ratio = 0

      if (log(u) <= ratio) { 
        # Move is accepted
        currentGraph <- proposalGraph
        currentPrettyFormula <- proposalPrettyFormula
        currentLogPostProb <- proposalLogPostProb
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
