\name{performGQTest}
\alias{performGQTest}
\title{Perform Goldfeld-Quandt test for heteroscedasticity}
\description{Implements the Goldfeld-Quandt test on a fitted linear model with directional or two-sided alternatives.}
\details{Observations are ordered by a suspected variance-driving variable. With the split point fixed at the sample midpoint, a central fraction is omitted and the model is fitted again on the lower and upper segments: each segment is fitted to its rows of the design, the response and the offset of the fit, as \code{lmtest::gqtest()} fits them to the rows of the design. The statistic is the residual mean square of segment 2 divided by the residual mean square of segment 1 and is therefore directional rather than being forced above one. Split arithmetic and p-value conventions match \code{lmtest::gqtest(..., point = 0.5)}.

Before 0.16.0 the segments of an unweighted fit were refitted from the formula, which computed again, on the rows of each segment, a term that depends on the rows it is computed from: \code{ns(x)} and \code{bs(x)} placed their knots at quantiles of the segment, and \code{I(x > median(x))} took its median, so the segments were fitted to columns the fit never used; \code{poly(x)} computed another basis of the same columns, which changed the last bits only. The refit also left out an offset given as an argument, and a weighted fit was refitted without its offset.}
\section{Weighted fits}{On a fit with weights each segment is refitted with its own weights and the segments are compared on the weighted residual sum of squares, \eqn{\sum w_i e_i^2}. That is the Goldfeld-Quandt statistic of the equivalent unweighted regression, so the test asks whether the weights are adequate. Releases before 0.12.0 refitted the segments without the weights.}
\usage{performGQTest(model, data = NULL, order_by, fraction = 0.2,
  alternative = c("greater", "two.sided", "less"))}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{the data frame \code{model} was fitted to, or \code{NULL}, the default. The response, design, weights and offset are read from the fit itself (since 0.16.0), and \code{order_by} from the rows of \code{data} that hold the observations of the fit. Without \code{data} it is read from the model frame of \code{model}, which holds it only when it is a variable of the model under its own name (\code{x}, not \code{log(x)}); a model that transforms the variable it is ordered by needs \code{data}. Data that are given are matched to the observations of the fit: rows with row names of their own by name, in any order; rows that only number themselves (a tibble, a data.table, a data frame whose row names were reset) by the row numbers of the fit, or in the order of the fit when they hold one row per observation. The rows the fit dropped may be there or not. Data whose rows cannot be shown to hold the observations of the fit are refused; see \code{\link{test_results}}.}
  \item{order_by}{single column name used to order observations.}
  \item{fraction}{fraction of observations omitted from the middle; must lie strictly between zero and one.}
  \item{alternative}{alternative hypothesis: \code{"greater"} for increasing variance from segment 1 to segment 2, \code{"less"} for decreasing variance, or \code{"two.sided"} for either direction.}
}
\value{An object of class \code{htest} containing the directional GQ statistic, p-value, degrees of freedom and alternative hypothesis.}
\section{Supported models}{
It accepts an \code{lm} and a \code{glm} with the Gaussian family and the identity link, unweighted
or with positive weights, a \code{fitWLS()} fit and, since 0.16.0, a rank-deficient fit, which it
tests on the estimable part of its design: it gives the value of the same model fitted without the
aliased columns. It refuses a \code{glm} of another family or link, a \code{survey::svyglm()} fit, a
fit with a zero weight, a coefficient that is not finite for another reason, and \code{data} that
lack rows or variables of the fit. It accepts a model with an offset, given as an argument or as an
\code{offset()} term, weighted or not, and gives the value of the model fitted to the response minus
the offset (since 0.16.0).
It refuses a multivariate \code{lm} ("needs a model with one response"), and, since 0.16.0, an
\code{mgcv::gam()} fit with smooth terms: a smooth term is usually penalised, and a penalised fit
is not the least-squares fit of the design the test fits again. A later step settles which tests
take such a fit; a \code{gam} without smooth terms is tested as the Gaussian \code{glm} it is.
Which model each test accepts, and why, is set out in
\code{\link{test_results}}, under \emph{Supported models}.
}
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
