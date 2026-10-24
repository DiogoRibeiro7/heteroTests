\name{performPesaranTest}
\alias{performPesaranTest}
\title{Perform Pesaran CD test}
\description{Checks for cross-sectional dependence in panel residuals.}
\details{The statistic averages pairwise correlation coefficients of the residuals and is standardized to follow a normal distribution under the null of independence.}
\usage{performPesaranTest(model, data = NULL, id, time)}
\arguments{
  \item{model}{an object of class \code{lm}, the pooled \code{lm()} of the panel.}
  \item{data}{The data frame \code{model} was fitted to, holding the panel identifiers named by \code{id} and \code{time}, or \code{NULL}, the default. The residuals and weights are read from the fit itself, and \code{id} and \code{time} from the rows of \code{data} that hold the observations of the fit. Without \code{data} they are read from the model frame of \code{model}, which holds them only when they are variables of the model under their own names (\code{id}, not \code{factor(id)}); identifiers the model does not use need \code{data}. Since 0.16.0, data that are given are matched to the observations of the fit: rows with row names of their own by name, in any order; rows that only number themselves (a tibble, a data.table, a data frame whose row names were reset) by the row numbers of the fit, or in the order of the fit when they hold one row per observation. The rows the fit dropped may be there or not. Data whose rows cannot be shown to hold the observations of the fit are refused; see \code{\link{test_results}}.}
  \item{id}{individual identifier column.}
  \item{time}{time column. Each pair of \code{id} and \code{time} must hold one observation of the fit: since 0.16.0 a pair that holds more is refused.}
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
The statistic is computed on a balanced panel, every unit observed once in every period. Since
0.16.0 a pair of \code{id} and \code{time} that holds two observations of the fit is refused, and so
is an unbalanced panel, as that of a fit that dropped some rows of some units: the test stopped on
it, or, when the number of periods of a unit divided that of the panel, returned a wrong value
computed on its residuals recycled over the periods it lacks. A fit that drops every row of a unit
keeps a balanced panel. \code{plm::pcdtest()} computes the statistic of an unbalanced panel from the
periods each pair of units shares (Pesaran 2015); the package will too, on the roadmap, and the
refusal will then go.
Which model each test accepts, and why, is set out in \code{\link{test_results}}, under
\emph{Supported models}.
}
\examples{
 df <- data.frame(id = rep(1:3, each = 5), time = rep(1:5, 3), x = runif(15), y = rnorm(15))
 m <- lm(y ~ x, data = df)
 performPesaranTest(m, df, "id", "time")
}
\references{Pesaran, M. H. (2004). General diagnostic tests for cross section dependence in panels. \emph{Cambridge Working Papers in Economics}.}
