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
\section{Supported models}{
It accepts an \code{lm} and a \code{glm} with the Gaussian family and the identity link, unweighted
or with positive weights, a \code{fitWLS()} fit and, since 0.16.0, a rank-deficient fit, which it
tests on the estimable part of its design: it gives the value of the same model fitted without the
aliased columns. It refuses a \code{glm} of another family or link, a \code{survey::svyglm()} fit, a
fit with a zero weight, a coefficient that is not finite for another reason, and \code{data} that
lack rows or variables of the fit. The model is a pooled \code{lm} of the panel, with the units and
the periods as columns of \code{data}; a \code{plm::plm()} fit is not an \code{lm} and is refused.
Which model each test accepts, and why, is set out in \code{\link{test_results}}, under
\emph{Supported models}.
}
\examples{
 df <- data.frame(id = rep(1:3, each = 5), time = rep(1:5, 3), x = runif(15), y = rnorm(15))
 m <- lm(y ~ x, data = df)
 performPesaranTest(m, df, "id", "time")
}
\references{Pesaran, M. H. (2004). General diagnostic tests for cross section dependence in panels. \emph{Cambridge Working Papers in Economics}.}
