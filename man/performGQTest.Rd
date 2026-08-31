\name{performGQTest}
\alias{performGQTest}
\title{Perform Goldfeld-Quandt test for heteroscedasticity}
\description{Implements the Goldfeld-Quandt test on a fitted linear model with directional or two-sided alternatives.}
\details{Observations are ordered by a suspected variance-driving variable. With the split point fixed at the sample midpoint, a central fraction is omitted and the original model is re-estimated on the lower and upper segments. The statistic is the residual mean square of segment 2 divided by the residual mean square of segment 1 and is therefore directional rather than being forced above one. Split arithmetic and p-value conventions match \code{lmtest::gqtest(..., point = 0.5)}.}
\usage{performGQTest(model, data, order_by, fraction = 0.2,
  alternative = c("greater", "two.sided", "less"))}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{data frame used to fit \code{model}.}
  \item{order_by}{single column name used to order observations.}
  \item{fraction}{fraction of observations omitted from the middle; must lie strictly between zero and one.}
  \item{alternative}{alternative hypothesis: \code{"greater"} for increasing variance from segment 1 to segment 2, \code{"less"} for decreasing variance, or \code{"two.sided"} for either direction.}
}
\value{An object of class \code{htest} containing the directional GQ statistic, p-value, degrees of freedom and alternative hypothesis.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performGQTest(m, mtcars, order_by = "wt")
 performGQTest(m, mtcars, order_by = "wt", alternative = "two.sided")
}
\references{
Goldfeld, S. M., & Quandt, R. E. (1965). Some tests for homoscedasticity. \emph{Journal of the American Statistical Association}, 60(310), 539--547. \doi{10.1080/01621459.1965.10480811}

Greene, W. H. (2018). \emph{Econometric Analysis} (8th ed.). Pearson.
}
