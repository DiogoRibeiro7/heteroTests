\name{performGlejserTest}
\alias{performGlejserTest}
\title{Perform Glejser test for heteroscedasticity}
\description{Regresses absolute residuals on a transformation of a suspected variable.}
\details{A variety of transformations of the explanatory variable (e.g. absolute value, square root) can reveal a relationship between scale and the covariate. Significance of the slope parameter is assessed with a t test.}
\usage{performGlejserTest(model, data, variable, transformation = c("abs", "sqrt", "inverse", "inverse_sqrt"))}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{data frame used to fit \code{model}.}
  \item{variable}{name of the suspected variable.}
  \item{transformation}{transformation applied to \code{variable}.}
}
\value{An object of class \code{htest} containing the t statistic, p-value and degrees of freedom.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performGlejserTest(m, mtcars, "wt")
}
\references{Glejser, H. (1969). A new test for heteroskedasticity. \emph{Journal of the American Statistical Association}, 64(325), 316--323.}
