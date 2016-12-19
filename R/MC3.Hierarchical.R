MC3.Hierarchical <-
function (init, alpha, iterations, replicates, data) {

  tools <- getTools(alpha, data)
  nVarSets <- tools$nVarSets

  postData <- priorData <- data
  postData$freq <- data$freq + alpha / length(data$freq)
  priorData$freq <- array (alpha / length(data$freq), c(length(data$freq))) 
  numStartModels <- length(init)

  for (r in 1:replicates) {
   
    i <- 1 
    rejected <- 0 
    cat("\n")

    if (r > numStartModels) {
      # reasonable starting point
      currentGraph <- randomGraph(p = 0.2, tools)
      currentGenerators <- findCliques (currentGraph, tools)
      currentFormula <- findFormula(currentGenerators, tools)
      currentModel <- findModel(currentFormula, tools)
    }
    else 
      currentModel <- findModel(init[r], tools)

    currentGenerators <- findGenerators(currentModel, tools)
    currentDualGenerators <- findDualGenerators (currentModel, tools)
    currentFormula <- findFormula(currentGenerators, tools)
    currentPrettyFormula <- prettyFormula(currentGenerators, tools)
    currentLogPostProb <- logLaplace(currentFormula, postData) - logLaplace(currentFormula, priorData) 
    print (logLaplace(currentFormula, priorData))
    masterList <- data.frame(formula = currentPrettyFormula, logPostProb = currentLogPostProb, stringsAsFactors = F)
    
    # find the neighbours of current model
    currentNeighbourList <- findHierNeighbours (currentModel, currentGenerators, currentDualGenerators, tools)
    currentNumNeighbours <- dim(currentNeighbourList)[1]

    while (i < iterations) {

      # propose a model
      m <- sample (x = currentNumNeighbours, size = 1, prob = rep(1, currentNumNeighbours))
      proposalModel <- currentNeighbourList[m,]    
       
      proposalGenerators <- findGenerators(proposalModel, tools)
      proposalDualGenerators<- findDualGenerators (proposalModel, tools)
      proposalFormula <- findFormula(proposalGenerators, tools)
      proposalPrettyFormula <- prettyFormula(proposalGenerators, tools)      

      inMasterList <- proposalPrettyFormula %in% masterList$formula
  
      if (inMasterList) 
        proposalLogPostProb <- masterList$logPostProb[which(masterList$formula == proposalPrettyFormula)]
      else {
        proposalLogPostProb <- logLaplace(proposalFormula, postData) - logLaplace(proposalFormula, priorData)
        masterList <- rbind(masterList, data.frame(formula = proposalPrettyFormula, logPostProb = proposalLogPostProb, stringsAsFactors = F)) 
      }

      # find the neighbours of the proposal model
      proposalNeighbourList <- findHierNeighbours (proposalModel, proposalGenerators, proposalDualGenerators, tools)      
      proposalNumNeighbours <- dim(proposalNeighbourList)[1] 

      outputMessage1 <- paste ("replicate [", r, "], ", "iteration [", i, "].", sep = "")
      outputMessage2 <- paste ("proposal model = ", proposalPrettyFormula, sep = "")
      outputMessage3 <- paste ("proposal logPostProb = ", round(proposalLogPostProb,digits = 2), sep = "")
      
      u <- runif(n = 1,min = 0,max = 1)
      print(proposalLogPostProb)
      ratio <- proposalLogPostProb - currentLogPostProb - log(proposalNumNeighbours) + log(currentNumNeighbours)
      if(ratio>0) 
        ratio <- 0

      if (log(u) <= ratio) { 
        # Move is accepted
        currentPrettyFormula <- proposalPrettyFormula
        currentLogPostProb <- proposalLogPostProb
        currentNumNeighbours <- proposalNumNeighbours
        currentNeighbourList <- proposalNeighbourList
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
