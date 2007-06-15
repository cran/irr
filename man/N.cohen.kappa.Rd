\name{N.cohen.kappa}
\alias{N.cohen.kappa}
\title{Sample Size Calculation for Cohen's Kappa Statistic}
\description{
 This function is a sample size estimator for the Cohen's Kappa
 statistic for a binary outcome. 
 Note that any value of "hypokappa" in the interval [0,1] is
 acceptable (i.e. Kappa=0 is a valid null hypothesis).
}
\usage{
 N.cohen.kappa(rate1, rate2, kappa, hypokappa, power=.8, alpha=.05, twosided=FALSE)
}

\arguments{
  \item{rate1}{The probability that the first rater will record
   a positive diagnosis}
  \item{rate2}{The probability that the second rater will record
   a positive diagnosis}
  \item{kappa}{The true Cohen's Kappa statistic}
  \item{hypokappa}{The value of kappa under the null hypothesis}
  \item{alpha}{Type I error of test}
  \item{power}{The desired power to detect the difference between
   kappa and hypokappa}
  \item{twosided}{TRUE if test is two-sided}
}

\value{
    returns required sample size
}
\references{
  Cantor, A. B. (1996) Sample-size calculation for Cohen's kappa.
  Psychological Methods, 1, 150-153.
}
\author{Ian Fellows \email{ifellows@ucsd.edu}}
\seealso{
  \code{\link[irr]{kappa2}}
}
\examples{
 # Testing H0: kappa = 0.7 vs. HA: kappa > 0.7 given that
 # kappa = 0.85 and both raters classify 50% of subjects as positive.
 N.cohen.kappa(0.5, 0.5, 0.7, 0.85)
}
\keyword{misc}
