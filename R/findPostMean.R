findPostMean <-
function (formula, alpha, data) {
  
  checkArgsFindPostMean(formula, alpha, data)

  dimens <- array (0, c(dim(data)[2]-1))
  
  for (j in 1:(dim(data)[2]-1)) {
    v <- as.factor(data[,j])
    dimens[j] <- length(levels(v))
    data[,j] <- as.factor(v)     
  }

  colnames(data)[dim(data)[2]] <- "freq"
  rownames(dimens) <- colnames(data[,1:dim(data)[2]-1]) 

  tools <- getTools(alpha, data)  

  graph <- findGraph (formula, tools)
  if(!is.chordal(graph)$chordal) {
    stop("graph must be decomposable")
  }

  cliques <- maximal.cliques (graph)
  nCliques <- length(cliques)
  separators <- minimal.st.separators(graph)
  nSeparators <- length(separators)
  
  cliqueFormulas <- list()
  for (i in 1:nCliques) {
    cliqueFormulas[[i]] <- paste("freq", tools$varNames[cliques[[i]][1]], sep = " ~ ")
    if (length(cliques[[i]]) > 1)
      for (j in 2:length(cliques[[i]]))
        cliqueFormulas[[i]] <- paste (cliqueFormulas[[i]], tools$varNames[cliques[[i]][j]], sep = "+") 
    cliqueFormulas[[i]] <- as.formula(cliqueFormulas[[i]])
  }

  separatorFormulas <- list()
  for (i in 1:nSeparators) {
    if (length(separators[[i]]) > 0) {
      separatorFormulas[[i]] <- paste("freq", tools$varNames[separators[[i]][1]], sep = " ~ ")
      if (length(separators[[i]]) > 1)
        for (j in 2:length(separators[[i]]))
          separatorFormulas[[i]] <- paste (separatorFormulas[[i]], tools$varNames[separators[[i]][j]], sep = "+") 
      separatorFormulas[[i]] <- as.formula(separatorFormulas[[i]])
    }
    else
      separatorFormulas[[i]] <- NA
  }

  cliqueMargins <- list()
  for (i in 1:nCliques) {
    cliqueMargins[[i]] <- xtabs (cliqueFormulas[[i]], data)  
    cliqueMargins[[i]] <- as.data.frame.table(cliqueMargins[[i]], responseName = "freq")   
  }

  separatorMargins <- list()
  for (i in 1:nSeparators) {
    if (length(separators[[i]]) > 0) {
      separatorMargins[[i]] <- xtabs (separatorFormulas[[i]], data)  
      separatorMargins[[i]] <- as.data.frame.table(separatorMargins[[i]], responseName = "freq")
    }
    else
      separatorMargins[[i]] <- NA
  }

  X <- model.matrix (object = formula, data = data)
  X <- standardizeColumnNames(X)

  cliqueFormulas <- list()
  for (i in 1:nCliques) {
    cliqueFormulas[[i]] <- paste("freq", tools$varNames[cliques[[i]][1]], sep = " ~ ")
    if (length(cliques[[i]]) > 1)
      for (j in 2:length(cliques[[i]]))
        cliqueFormulas[[i]] <- paste (cliqueFormulas[[i]], tools$varNames[cliques[[i]][j]], sep = "*") 
    cliqueFormulas[[i]] <- as.formula(cliqueFormulas[[i]])
  }

  separatorFormulas <- list()
  for (i in 1:nSeparators) {
    if (length(separators[[i]]) > 0) {
      separatorFormulas[[i]] <- paste("freq", tools$varNames[separators[[i]][1]], sep = " ~ ")
      if (length(separators[[i]]) > 1)
        for (j in 2:length(separators[[i]]))
          separatorFormulas[[i]] <- paste (separatorFormulas[[i]], tools$varNames[separators[[i]][j]], sep = "*") 
      separatorFormulas[[i]] <- as.formula(separatorFormulas[[i]])
    }
    else
      separatorFormulas[[i]] <- NA
  }

  cliqueModelMatrices <- list()
  for (i in 1:nCliques) {
    cliqueModelMatrices[[i]] <- model.matrix (object = cliqueFormulas[[i]], data = cliqueMargins[[i]]) 
    cliqueModelMatrices[[i]] <- standardizeColumnNames(cliqueModelMatrices[[i]])
    cliqueModelMatrices[[i]] <- solve(cliqueModelMatrices[[i]])
  }

  separatorModelMatrices <- list()
  for (i in 1:nSeparators) {
    if (length(separators[[i]]) > 0) {
      separatorModelMatrices[[i]] <- model.matrix (object = separatorFormulas[[i]], data = separatorMargins[[i]]) 
      separatorModelMatrices[[i]] <- standardizeColumnNames(separatorModelMatrices[[i]])
      separatorModelMatrices[[i]] <- solve(separatorModelMatrices[[i]])
    }
    else
      separatorModelMatrices[[i]] <- NA
  }
    
  meanVector <- array (0, c(dim(X)[2]))
  row.names(meanVector) <- colnames(X)
  m <- multiplicity (graph, cliques, separators, tools)
    
  for (i in 1:nCliques) {
    for (j in 1:dim(cliqueModelMatrices[[i]])[2]) {
      v <- array (0, c(dim(X)[2]))
      rownames(v) <- colnames(X)
      v[names(cliqueModelMatrices[[i]][,j])] <- cliqueModelMatrices[[i]][,j]
      meanVector <- meanVector + digamma(cliqueMargins[[i]]$freq[j] + alpha / length(cliqueMargins[[i]]$freq)) * v 
    }
  }
  
  for (i in 1:nSeparators) {
    if (length(separators[[i]]) > 0) {
      for (j in 1:dim(separatorModelMatrices[[1]])[2]) {
        v <- array (0, c(dim(X)[2]))
        rownames(v) <- colnames(X)
        v[names(separatorModelMatrices[[i]][,j])] <- separatorModelMatrices[[i]][,j]
        meanVector <- meanVector - m[i] * digamma(separatorMargins[[i]]$freq[j] + alpha / length(separatorMargins[[i]]$freq)) * v 
      }
    }
    else {
      # empty separator
      v <- array (0, c(dim(X)[2]))
      v[1] <- 1
      meanVector <- meanVector - m[i] * digamma(sum(data$freq) + alpha) * v 
    }
  }
  
  meanVector[1] <- meanVector[1] - log(alpha + 1) 

  return(meanVector)

}
