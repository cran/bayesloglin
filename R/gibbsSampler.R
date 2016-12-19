gibbsSampler <-
function (formula, alpha = 1, data, nSamples = 10000, verbose = T) {
  
  checkArgsGibbsSampler (formula, alpha, data, nSamples)
  
  dimens <- array (0, c(dim(data)[2]-1))
  
  for (j in 1:(dim(data)[2]-1)) {
    v <- as.factor(data[,j])
    dimens[j] <- length(levels(v))
    data[,j] <- as.factor(v)     
  }

  colnames(data)[dim(data)[2]] <- "freq"
  rownames(dimens) <- colnames(data[,1:dim(data)[2]-1]) 
  
  tools <- getTools(alpha, data)

  model <- findModel(formula, tools)
  generators <- findGenerators (model, tools)
  activeVarSets <- tools$varSets[which(generators == 1),]
  nGenerators <- sum(generators)
  generators <- list()
  
  for (i in 1:nGenerators) 
    generators[[i]] <- tools$varNames[activeVarSets[i,] == 1]   

  generatorFormulas <- list()
  for (i in 1:nGenerators) {
    generatorFormulas[[i]] <- paste("freq", generators[[i]][length(generators[[i]])], sep = " ~ ")
    if (length(generators[[i]]) > 1)
      for (j in (length(generators[[i]])-1):1)
        generatorFormulas[[i]] <- paste (generatorFormulas[[i]], generators[[i]][j], sep = "+") 
    generatorFormulas[[i]] <- as.formula(generatorFormulas[[i]])
  }
 
  generatorMargins <- list()
  for (i in 1:nGenerators) {
    generatorMargins[[i]] <- xtabs (generatorFormulas[[i]], data)  
    generatorMargins[[i]] <- as.data.frame.table(generatorMargins[[i]], responseName = "freq")   
  }

  X <- model.matrix (object = formula, data = data)
  X <- standardizeColumnNames(X)

  generatorFormulas <- list()
  for (i in 1:nGenerators) {
    generatorFormulas[[i]] <- paste("freq", generators[[i]][length(generators[[i]])], sep = " ~ ")
    if (length(generators[[i]]) > 1)
      for (j in (length(generators[[i]])-1):1)
        generatorFormulas[[i]] <- paste (generatorFormulas[[i]], generators[[i]][j], sep = "*") 
    generatorFormulas[[i]] <- as.formula(generatorFormulas[[i]])
  }

  generatorModelMatrices <- list()
  for (i in 1:nGenerators) {
    generatorModelMatrices[[i]] <- model.matrix (object = generatorFormulas[[i]], data = generatorMargins[[i]]) 
    generatorModelMatrices[[i]] <- standardizeColumnNames(generatorModelMatrices[[i]])
    generatorModelMatrices[[i]] <- solve(generatorModelMatrices[[i]])
  }
  
  compModelMatrices <- list()
  for (i in 1:nGenerators) {
    compModelMatrices[[i]] <- X[,!colnames(X) %in% rownames(generatorModelMatrices[[i]])]
    compModelMatrices[[i]] <- standardizeColumnNames(compModelMatrices[[i]])
  }
  
  generatorFormulas <- list()
  for (i in 1:nGenerators) {
    generatorFormulas[[i]] <- paste("freq", generators[[i]][length(generators[[i]])], sep = " ~ ")
    if (length(generators[[i]]) > 1)
      for (j in (length(generators[[i]])-1):1)
        generatorFormulas[[i]] <- paste (generatorFormulas[[i]], generators[[i]][j], sep = "+") 
    generatorFormulas[[i]] <- as.formula(generatorFormulas[[i]])
  }

  nthetas <- length(colnames(X))
  theta <- array (NA, c(nSamples, nthetas))
  theta[1,] <- rep(1,nthetas)
  colnames(theta) <- colnames(X)
  tempData <- data
  
  # sampling
  for (i in 2:nSamples) {
    if (verbose)
      print(i)
    theta[i,] <- theta[i-1,]
    for (j in 1:nGenerators) {
      tempData$freq <- exp(compModelMatrices[[j]] %*% theta [i,colnames(compModelMatrices[[j]])])  
      s <- findMargFreqs (tempData, t(tempData), generators[[j]], dimens)
      randomMargins <- rgamma(n = length(generatorMargins[[j]]$freq), shape = alpha / length(generatorMargins[[j]]$freq) + generatorMargins[[j]]$freq, scale = 1 / (alpha+1))
      theta[i,rownames(generatorModelMatrices[[j]])] <- generatorModelMatrices[[j]] %*% as.vector(log(randomMargins) - log(s)) 
    }  
  }
  
  return(theta)

}
