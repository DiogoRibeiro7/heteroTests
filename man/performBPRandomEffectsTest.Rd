\name{performBPRandomEffectsTest}
\alias{performBPRandomEffectsTest}
\title{Breusch-Pagan test for random effects}
\description{Lagrange Multiplier test for heteroscedasticity in random effects models.}
\details{The statistic is calculated as \eqn{n \sum \bar{e}_i^2 / \hat{\sigma}^2}, where \eqn{\bar{e}_i} are average residuals by individual and \eqn{\hat{\sigma}^2} is the pooled error variance. It follows a chi-square distribution with one degree of freedom.}
\usage{performBPRandomEffectsTest(model, data, id)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{data frame used to fit \code{model}.}
  \item{id}{individual identifier column.}
}
\value{An object of class \code{htest}.}
\examples{
 df <- data.frame(id = rep(1:5, each = 4), time = rep(1:4, 5), x = runif(20), y = rnorm(20))
 m <- lm(y ~ x, data = df)
 performBPRandomEffectsTest(m, df, "id")
}
\references{Breusch, T. S., & Pagan, A. R. (1980). The Lagrange Multiplier Test and Its Applications to Model Specification in Econometrics. \emph{Review of Economic Studies}, 47(1), 239--253.}
