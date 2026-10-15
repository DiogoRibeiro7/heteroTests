\name{performPesaranTest}
\alias{performPesaranTest}
\title{Perform Pesaran CD test}
\description{Checks for cross-sectional dependence in panel residuals.}
\details{The statistic averages pairwise correlation coefficients of the residuals and is standardized to follow a normal distribution under the null of independence.}
\usage{performPesaranTest(model, data = NULL, id, time)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{data frame used to fit \code{model}. When \code{NULL}, the default, the model frame of \code{model} is used, so \code{id} and \code{time} must then be variables of the model. The model frame holds the terms of the model rather than its variables (\code{log(x)} rather than \code{x}), so a model that transforms a variable needs \code{data}, as does a model with an \code{offset()} term.}
  \item{id}{individual identifier column.}
  \item{time}{time column.}
}
\value{An object of class \code{htest}.}
\examples{
 df <- data.frame(id = rep(1:3, each = 5), time = rep(1:5, 3), x = runif(15), y = rnorm(15))
 m <- lm(y ~ x, data = df)
 performPesaranTest(m, df, "id", "time")
}
\references{Pesaran, M. H. (2004). General diagnostic tests for cross section dependence in panels. \emph{Cambridge Working Papers in Economics}.}
