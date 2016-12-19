\name{bayesloglin}
\Rdversion{1.0.1}
\title{Bayesian analysis of contingency table data}
\description{Functions for Bayesian model selection and inference for log-linear models.}
\details{
\tabular{ll}{
Package: \tab bayesloglin\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2016-12-23\cr
License: \tab GPL-2\cr
}
The function MC3 searches for log-linear models with the highest posterior probability. The function gibbsSampler is a blocked Gibbs sampler for sampling fronm the posterior distribution of the log-linear parameters. The functions findPostMean and findPostCov compute the posterior mean and covariance matrix for decomposable models which, for these models, is available in closed form.}
\author{Author: Matthew Friedlander
Maintainer: Matthew Friedlander <friedla@yorku.ca>
}
\references{see vignette}
\examples{
data(czech)
s1 <- MC3 (init = NULL, alpha = 1, iterations = 5, 
           replicates = 1, data = czech, mode = "Decomposable")
s2 <- MC3 (init = NULL, alpha = 1, iterations = 5,   
            replicates = 1, data = czech, mode = "Graphical")
s3 <- MC3 (init = NULL, alpha = 1, iterations = 5,   
            replicates = 1, data = czech, mode = "Hierarchical")
}
\keyword{htest}
\keyword{models}