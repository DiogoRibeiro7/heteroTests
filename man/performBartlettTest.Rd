\name{performBartlettTest}
\alias{performBartlettTest}
\title{Perform Bartlett's test for equality of variances}
\description{Uses \code{bartlett.test} on model residuals grouped by a factor.}
\details{The test statistic compares the pooled variance to the individual group variances. It is computed as \eqn{\chi^2 = (N - k) \ln S_p^2 - \sum (n_i - 1) \ln s_i^2}, where \eqn{S_p^2} is the pooled variance and \eqn{s_i^2} the group variances. Under the null it approximates a chi-square distribution with \eqn{k-1} degrees of freedom.}
\usage{performBartlettTest(model, data = NULL, group)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{The data frame \code{model} was fitted to, or \code{NULL}, the default. The residuals and weights are read from the fit itself, and \code{group} from the rows of \code{data} that hold the observations of the fit; the observations whose group is missing are left out, with a warning. Without \code{data} it is read from the model frame of \code{model}, which holds it only when it is a variable of the model under its own name (\code{x}, not \code{log(x)}); a grouping variable the model does not use needs \code{data}. Since 0.16.0, data that are given are matched to the observations of the fit: rows with row names of their own by name, in any order; rows that only number themselves (a tibble, a data.table, a data frame whose row names were reset) by the row numbers of the fit, or in the order of the fit when they hold one row per observation. The rows the fit dropped may be there or not. Data whose rows cannot be shown to hold the observations of the fit are refused; see \code{\link{test_results}}.}
  \item{group}{name of the grouping variable.}
}
\value{An object of class \code{htest} containing the chi-squared statistic, p-value and degrees of freedom.}
\section{Supported models}{
It accepts an \code{lm} and a \code{glm} with the Gaussian family and the identity link, unweighted
or with positive weights, a \code{fitWLS()} fit and, since 0.16.0, a rank-deficient fit, which it
tests on the estimable part of its design: it gives the value of the same model fitted without the
aliased columns. It refuses a \code{glm} of another family or link, a \code{survey::svyglm()} fit, a
fit with a zero weight, a coefficient that is not finite for another reason, and \code{data} that
lack rows or variables of the fit.
It refuses a multivariate \code{lm} ("needs a model with one response"), since 0.16.0; before, it
stopped with a coercion error. An \code{mgcv::gam()} fit is tested as before 0.16.0, with penalised
smooth terms too: the test reads its residuals and does not fit it again, so it does not take them
for least-squares residuals (decision 37 of the design of the model context); since 0.16.0 its
observations are matched to the rows of \code{data} by the row names of its model frame. Before,
they were matched by position, since the residuals of a \code{gam} carry no names, and \code{data}
with the rows of the fit in another order gave a wrong value.
Which model each test accepts, and why, is set out in
\code{\link{test_results}}, under \emph{Supported models}.
}
\examples{
 data(mtcars)
 mtcars$cyl <- factor(mtcars$cyl)
 m <- lm(mpg ~ wt, data = mtcars)
 performBartlettTest(m, mtcars, "cyl")
}
\references{
Bartlett, M. S. (1937). Properties of sufficiency and statistical tests. \emph{Proceedings of the Royal Society of London}, 160(901), 268--282. \doi{10.1098/rspa.1937.0109}

Hartley, H. O. (1950). The maximum F-ratio as a short-cut test for heterogeneity of variance. \emph{Biometrika}, 37(3/4), 308--312. \doi{10.2307/2332383}
}
