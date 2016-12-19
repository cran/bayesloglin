\name{gibbsSampler}
\alias{gibbsSampler}
\Rdversion{1.0}
\title{A blocked Gibbs sampler.}
\description{Generates samples from the posterior distribution of the log-linear parameters.}
\usage{gibbsSampler (formula, alpha = 1, data, nSamples = 10000, verbose = T)}
\arguments{
  \item{formula}{A model formula.}
  \item{alpha}{The value of the hyperparameter alpha.}
  \item{data}{A data frame containing the contingency table. All cells must be included in data and the last column must be the cell counts. The number of variables in the contingency table must be at least 2.}
  \item{nSamples}{Number of iterations of the Gibbs sampler.}
  \item{verbose}{Displays current iteration number of the sampler.}
}
\value{
\item{theta}{An array where each row represents a sample from the Posterior distribution of the log-linear parameters. The first 5000 or so samples should be discarded as a burn-in period.}
}
\references{see vignette}
\author{Matthew Friedlander} 
\examples{
data(czech)
formula <- freq ~ a*c + b*c + a*d + a*e + c*e + d*e + f
#s <- gibbsSampler (formula, alpha = 1, data = czech, 
#                   nSamples =  15000, verbose = TRUE)
#postMean <- colSums(s[5000:15000,]) / 10000
#postCov <- cov(s[5000:15000,])
#postVar <- diag(postCov)
}
\keyword{htest}
\keyword{models}
