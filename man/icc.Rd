\name{icc}
\alias{icc}
\title{Intraclass correlation coefficient (ICC) for oneway and twoway models}
\description{
Computes single score or average score ICCs as an index of interrater reliability of quantitative data. Additionally, F-test and confidence interval are computed.
}
\usage{
icc(ratings, model = c("oneway", "twoway"), 
    type = c("consistency", "agreement"), 
    k = 1, r0 = 0, conf.level = 0.95)
}
\arguments{
  \item{ratings}{n*m matrix or dataframe, n subjects m raters.}
  \item{model}{a character string specifying if a '"oneway"' model (default) with row effects random, or a '"twoway"' model with column and row effects random should be applied. You can specify just the initial letter.}
  \item{type}{a character string specifying if '"consistency"' (default) or '"agreement"' between raters should be estimated. If a '"oneway"' model is used, only '"consistency"' could be computed. You can specify just the initial letter.}
  \item{k}{number of ratings for the estimation of average score ICCs. When considering single values k equals 1.}
  \item{r0}{specification of the null hypothesis r = r0.}
  \item{conf.level}{confidence level of the interval.}
}
\details{
Missing data are omitted in a listwise way.
\crWhen considering which form of ICC is appropriate for an actual set of data, one has take several decisions (Shrout & Fleiss, 1979):
\cr\cr1. Should only the subjects be considered as random effects ('"oneway"' model) or are subjects and raters randomly chosen from a bigger pool of persons ('"twoway"' model).
\cr\cr2. If differences in judges' mean ratings are of interest, interrater '"agreement"' instead of '"consistency"' should be computed.
\cr\cr3. If the unit of analysis is a mean of several ratings, k should be adjusted to the number of averaged ratings. In most cases, however, single values (k=1) are regarded.
}
\value{
A list with class '"icclist"' containing the following components:
  \item{$subjects}{the number of subjects examined.}
  \item{$raters}{the number of raters.}
  \item{$model}{a character string describing the selected model for the analysis.}
  \item{$type}{a character string describing the selected type of interrater reliability.}
  \item{$k}{the number of averaged ratings.}
  \item{$icc.name}{a character string specifying the name of ICC according to McGraw & Wong (1996).}
  \item{$value}{the intraclass correlation coefficient.}
  \item{$r0}{the specified null hypothesis.}
  \item{$Fvalue}{the value of the F-statistic.}
  \item{$df1}{the numerator degrees of freedom.}
  \item{$df2}{the denominator degrees of freedom.}
  \item{$p.value}{the p-value for a two-sided test.}
  \item{$conf.level}{the confidence level for the interval.}
  \item{$lbound}{the lower bound of the confidence interval.}
  \item{$ubound}{the upper bound of the confidence interval.}
}
\references{
Bartko, J.J. (1966). The intraclass correlation coefficient as a measure of reliability. Psychological Reports, 19, 3-11.
\cr\crMcGraw, K.O., & Wong, S.P. (1996), Forming inferences about some intraclass correlation coefficients. Psychological Methods, 1, 30-46.
\cr\crShrout, P.E., & Fleiss, J.L. (1979), Intraclass correlation: uses in assessing rater reliability. Psychological Bulletin, 86, 420-428.
}
\author{Matthias Gamer}
\seealso{
  \code{\link[irr]{finn}}
  \code{\link[irr]{meancor}}
  \code{\link[irr]{robinson}}
}
\examples{
data(anxiety)
icc(anxiety, model="twoway", type="agreement")

r1 <- round(rnorm(20, 10, 4))
r2 <- round(r1 + 10 + rnorm(20, 0, 2))
r3 <- round(r1 + 20 + rnorm(20, 0, 2))
icc(cbind(r1, r2, r3), "twoway")              # High consistency
icc(cbind(r1, r2, r3), "twoway", "agreement") # Low agreement
}
\keyword{univar}
