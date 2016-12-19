getTools <-
function (alpha = 1, data) {
  
  varNames <- colnames(data)[-dim(data)[2]]
  n <- length(varNames)
  varSets <- decToBin (0:(2**n-1),n)
  colnames(varSets) <- varNames
  lenVarSets <- rowSums(varSets)
  nVarSets <- 2 ** n   

  # lattice
  
  downLinks <- array(NA, c(nVarSets,n))
  nDownLinks <- lenVarSets
     
  upLinks <- array(NA, c(nVarSets,n))
  nUpLinks <- lenVarSets

  # downLinks

  for(i in 1:nVarSets) {
    k = 1
    for(j in 1:n) {
      if(varSets[i,j] == 1) {
        varSets[i,j] <- 0
        downLinks[i,k] <- binToDec(varSets[i,]) + 1
        k <- k + 1
        varSets[i,j] <- 1
      }
    }
  }

  # upLinks

  for(i in 1:nVarSets) {
    k = 1
    for(j in 1:n) {
      if(varSets[i,j] == 0) {
        varSets[i,j] <- 1
        upLinks[i,k] <- binToDec(varSets[i,]) + 1
        k <- k + 1
        varSets[i,j] <- 0
      }
    }
  }  

  return(list(varNames = varNames, n = n, varSets = varSets, lenVarSets = lenVarSets, nVarSets = nVarSets, downLinks = downLinks, nDownLinks = nDownLinks, upLinks = upLinks, nUpLinks = nUpLinks, alpha = alpha)) 

}
