\name{MC3}
\alias{MC3}
\Rdversion{1.0.1}
\title{Implementation of the MC3 algorithm}
\description{The MC3 algorithm is used to find the log-linear models with the highest posterior probability. The function is capable of searching the space of decomposable, graphical, and hierarchical models.}
\usage{MC3 (init = NULL, alpha = 1, iterations = 5000, replicates = 1, data, 
            mode = c("Hierarchical", "Graphical", "Decomposable")) }
\alias{MC3}
\arguments{
  \item{init}{A starting list of models for the MOSS algorithm. If not NULL, this list should consist of R formulas.}
  \item{alpha}{The value of the hyperparameter alpha.}
  \item{iterations}{Number of iterations of the MC3 algorithm}
  \item{replicates}{The number of instances the MC3 algorithm will be run. The top models are culled from the results of all the replicates.}
  \item{data}{A data frame containing the contingency table. All cells must be included in data and the last column must be the cell counts. The number of variables in the contingency table must be at least 3.}
  \item{mode}{The scope of the loglinear model search. The search can be over the space of decomposable, graphical, or hierarchical models.}
}
\value{A data frame with the top models found and their log posterior probability up to a constant.}
\references{see vignette}
\author{Matthew Friedlander} 
\examples{
data(czech)
s1 <- MC3 (init = NULL, alpha = 1, iterations = 5, 
            replicates = 1, data = czech, 
            mode = "Decomposable")
s2 <- MC3 (init = NULL, alpha = 1, iterations = 5, 
            replicates = 1, data = czech, 
            mode = "Graphical")
s3 <- MC3 (init = NULL, alpha = 1, iterations = 5, 
            replicates = 1, data = czech, 
            mode = "Hierarchical")
}
\keyword{htest}
\keyword{models}