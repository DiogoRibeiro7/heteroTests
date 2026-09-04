\name{performBPRandomEffectsTest}
\alias{performBPRandomEffectsTest}
\title{Breusch-Pagan LM test for random effects}
\description{Lagrange Multiplier test for the presence of a random individual effect in panel data. This is not a test for heteroscedasticity and does not respond to one; use the auxiliary-regression diagnostics for that.}
\details{The statistic is Breusch and Pagan (1980) equation 5, \eqn{LM = nT / (2(T-1)) [ \sum_i (\sum_t e_{it})^2 / \sum_{it} e_{it}^2 - 1 ]^2}, which follows a chi-square distribution with one degree of freedom under the null of no individual effect. The bracketed ratio is close to one under the null and the statistic measures its squared departure from one. Before 0.11.0 the "- 1" and the square were absent and the scaling used T^2 rather than nT, which left the statistic sitting at the critical value: it rejected about a third of the time when no individual effect was present.}
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
