logLaplace <-
function (formula, data) {

  formula <- as.formula(formula)
  X <- model.matrix(formula, data)
  y <- data$freq
  fit <- glm (formula, data, family = poisson())
  W <- diag (fit$fitted.values)
  maxloglik <- sum(y  * log(fit$fitted.values))
  fisherMatrix <- t(X) %*% W %*% X
  J <- dim(X)[2]
  value <- maxloglik + 0.5 * J * log (2*pi) - 0.5 * log(det(fisherMatrix)) 
  return(value)   

}
