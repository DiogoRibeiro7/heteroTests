\name{performPesaranTest}
\alias{performPesaranTest}
\title{Perform Pesaran CD test}
\description{Checks for cross-sectional dependence in panel residuals.}
\details{The statistic averages pairwise correlation coefficients of the residuals and is standardized to follow a normal distribution under the null of independence. On a balanced panel of \eqn{N} units observed at the same \eqn{T} periods it is
\deqn{\mathrm{CD} = \sqrt{\frac{2T}{N(N - 1)}} \sum_{i < j} \hat{\rho}_{ij},}{CD = sqrt(2 T / (N (N - 1))) sum_{i < j} rho_ij,}
where \eqn{\hat{\rho}_{ij}}{rho_ij} is the correlation of the residuals of units \eqn{i} and \eqn{j}. On an unbalanced panel, whose units are not all observed at the same periods, it is computed, since 0.16.0, from the periods each pair of units shares (Pesaran 2004, Section 9, equation 31), as \code{plm::pcdtest(test = "cd")} computes it:
\deqn{\mathrm{CD} = \sqrt{\frac{1}{P}} \sum_{i < j} \sqrt{T_{ij}}\, \hat{\rho}_{ij},}{CD = sqrt(1 / P) sum_{i < j} sqrt(T_ij) rho_ij,}
where \eqn{T_{ij}}{T_ij} is the number of periods at which units \eqn{i} and \eqn{j} are both observed, \eqn{\hat{\rho}_{ij}}{rho_ij} the correlation of their residuals over those periods, each centred at its mean over them, and \eqn{P} the number of pairs that share at least two periods, the pairs of the sum: \eqn{N(N - 1)/2} when every pair does. On a balanced panel, where \eqn{T_{ij} = T}{T_ij = T}, the two are the same statistic. A pair that shares fewer than two periods has no correlation and is left out, with a warning, as \code{plm::pcdtest()} leaves it out, and a panel in which no pair shares two periods is refused. A correlation that is not defined, the residuals of a unit constant over the periods of a pair, adds nothing to the sum and the pair stays in the count, as on a balanced panel, with a warning; \code{plm::pcdtest()} returns \code{NA} there. Pesaran gives the statistic its standard normal limit for \eqn{T_{ij} > 3}{T_ij > 3}.}
\usage{performPesaranTest(model, data = NULL, id, time)}
\arguments{
  \item{model}{an object of class \code{lm}, the pooled \code{lm()} of the panel.}
  \item{data}{The data frame \code{model} was fitted to, holding the panel identifiers named by \code{id} and \code{time}, or \code{NULL}, the default. The residuals and weights are read from the fit itself, and \code{id} and \code{time} from the rows of \code{data} that hold the observations of the fit. Without \code{data} they are read from the model frame of \code{model}, which holds them only when they are variables of the model under their own names (\code{id}, not \code{factor(id)}); identifiers the model does not use need \code{data}. Since 0.16.0, data that are given are matched to the observations of the fit: rows with row names of their own by name, in any order; rows that only number themselves (a tibble, a data.table, a data frame whose row names were reset) by the row numbers of the fit, or in the order of the fit when they hold one row per observation. The rows the fit dropped may be there or not. Data whose rows cannot be shown to hold the observations of the fit are refused; see \code{\link{test_results}}.}
  \item{id}{individual identifier column.}
  \item{time}{time column. Each pair of \code{id} and \code{time} must hold one observation of the fit: since 0.16.0 a pair that holds more is refused. The units need not be observed at the same periods (Details).}
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
Since 0.16.0 a pair of \code{id} and \code{time} that holds two observations of the fit is refused.
An unbalanced panel, as that of a fit that dropped some rows of some units, is tested since 0.16.0,
from the periods each pair of units shares (Details), and gives the value of
\code{plm::pcdtest(test = "cd")} on the residuals of the fit: before, the test stopped on it, or,
when the number of periods of a unit divided that of the panel, returned a wrong value computed on
its residuals recycled over the periods it lacks. A fit with \code{na.action = na.exclude} is read
as before, by position, until a later step: the panel of the rows of \code{data} must then be
balanced, and an unbalanced one is refused, with a message that points to \code{na.omit}.
Which model each test accepts, and why, is set out in \code{\link{test_results}}, under
\emph{Supported models}.
}
\examples{
 df <- data.frame(id = rep(1:3, each = 5), time = rep(1:5, 3), x = runif(15), y = rnorm(15))
 m <- lm(y ~ x, data = df)
 performPesaranTest(m, df, "id", "time")
}
\references{
Pesaran, M. H. (2004). General diagnostic tests for cross section dependence in panels. \emph{CESifo Working Paper} 1229 (also \emph{Cambridge Working Papers in Economics} 0435). Section 9, \dQuote{CD tests in the case of unbalanced panels}, equation 31.

Pesaran, M. H. (2015). Testing weak cross-sectional dependence in large panels. \emph{Econometric Reviews} 34(6-10), 1089--1117. \doi{10.1080/07474938.2014.956623}.

Croissant, Y. and Millo, G. (2008). Panel data econometrics in R: the \pkg{plm} package. \emph{Journal of Statistical Software} 27(2), 1--43. \doi{10.18637/jss.v027.i02}. The statistic of an unbalanced panel is that of \code{plm::pcdtest(test = "cd")} (plm 2.6-6), whose help page says its tests are \dQuote{suitable also for unbalanced panels}.
}
