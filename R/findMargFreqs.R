findMargFreqs <-
function (data, tData, margin, dimens) {
  
  nMargFreqs <- prod(dimens[margin])
  margFreqs <- array (0, nMargFreqs)
  .C("findMargFreqs", as.integer (tData[margin,]), as.integer(dim(data)[1]), as.integer(length(margin)), as.integer(dimens[margin]), as.double(data$freq), margFreqs = as.double(margFreqs), PACKAGE = "bayesloglin")$margFreqs
  
}
