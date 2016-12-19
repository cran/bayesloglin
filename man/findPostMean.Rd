\name{findPostMean}
\alias{findPostMean}
\Rdversion{1.0.1}
\title{Posterior covariance matrix for a decomposable model.}
\description{Computes the posterior mean, which for decomposable models, is known in closed form.}
\usage{findPostMean(formula, alpha, data)}
\arguments{
  \item{formula}{A decomposable model formula.}
  \item{alpha}{The value of the hyperparameter alpha.}
  \item{data}{A data frame containing the contingency table. All cells must be included in data and the last column must be the cell counts. The number of variables in the contingency table must be at least 2.}
  }
\value{
\item{theta}{An array giving the posterior mean of the log-linear parameters.}
}
\references{see vignette}
\author{Matthew Friedlander} 
\examples{
data(czech)
formula <- freq ~ b*c + a*c*e + d*e + f
s <- findPostMean (formula, alpha = 1, data = czech)
print(s)
}
\keyword{htest}
\keyword{models}