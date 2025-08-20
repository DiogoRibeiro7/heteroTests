\name{performGQTest}
\alias{performGQTest}
\title{Perform Goldfeld-Quandt test for heteroscedasticity}
\description{Implements the Goldfeld-Quandt test on a fitted linear model.}
\details{Observations are ordered by a suspected variable and split into two groups, omitting a middle fraction. The ratio of residual sums of squares forms an \eqn{F} statistic to test for scale differences.}
\usage{performGQTest(model, data, order_by, fraction = 0.2)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{data frame used to fit \code{model}.}
  \item{order_by}{variable name to order the data by.}
  \item{fraction}{fraction of observations omitted from the middle.}
}
\value{An object of class \code{htest} containing the F statistic, p-value and degrees of freedom.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performGQTest(m, mtcars, order_by = "wt")
}
\references{
Goldfeld, S. M., & Quandt, R. E. (1965). Some tests for homoscedasticity. \emph{Journal of the American Statistical Association}, 60(310), 539--547. \doi{10.1080/01621459.1965.10480811}

Greene, W. H. (2018). \emph{Econometric Analysis} (8th ed.). Pearson.
}
