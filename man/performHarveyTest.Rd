\name{performHarveyTest}
\alias{performHarveyTest}
\title{Perform Harvey test for multiplicative heteroscedasticity}
\description{Harvey's (1976) Lagrange multiplier test, which regresses \code{log(residuals^2)} on a set of variance regressors.}
\details{
Harvey (1976) models the error variance as \eqn{\sigma_i^2 = \exp(z_i^\top \gamma)} and tests \eqn{\gamma = 0} through the auxiliary regression of \eqn{\log \hat{e}_i^2} on \eqn{z_i}. Under the null the auxiliary error behaves like a centred \eqn{\log \chi^2_1} variate with variance \eqn{\pi^2 / 2 \approx 4.9348}, so the classical statistic is \eqn{\mathrm{ESS} / (\pi^2 / 2)}, asymptotically chi-square with \eqn{q} degrees of freedom.

Setting \code{studentize = TRUE} replaces the fixed constant \eqn{\pi^2 / 2} with the auxiliary residual mean square and refers the overall F statistic to an F distribution. The two forms are asymptotically equivalent under normal errors; the studentized form is more reliable when normality is doubtful, because \eqn{\pi^2 / 2} is the null variance of \eqn{\log \hat{e}^2} only for Gaussian errors.

Degrees of freedom are taken from the realised rank of the auxiliary design, so a rank-deficient variance model is not credited with degrees of freedom for aliased columns.
}
\usage{performHarveyTest(model, data = NULL, auxiliary = c("regressors", "fitted"),
                  studentize = FALSE)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{optional data frame the model was fitted to. When \code{NULL}, the default, the model frame of \code{model} is used. The statistic is computed from the residuals, fitted values and model matrix of \code{model}, so \code{data} does not change the result; it is accepted so that every test takes \code{(model, data, ...)}. Before 0.15.0 the second argument was \code{auxiliary} and the third \code{studentize}: a string passed second by position, and a logical value after it, are still read as those two, with a warning that shows the new call; that form stops working in 1.0.0.}
  \item{auxiliary}{character scalar choosing the variance regressors. \code{"regressors"} (the default) uses the model's own explanatory variables, the specification in Harvey (1976). \code{"fitted"} uses the fitted values and their square, which was the behaviour of releases before 0.7.0.}
  \item{studentize}{logical; if \code{FALSE} (the default) the chi-square statistic \eqn{\mathrm{ESS} / (\pi^2 / 2)} is reported, otherwise the auxiliary regression F statistic.}
}
\value{An object of class \code{htest} containing the test statistic, its degrees of freedom and the p-value.}
\section{Validation}{
The default statistic reproduces an independent reconstruction of Harvey (1976) to within \code{1e-8}; see \file{tests/testthat/test-pass-a-reference.R}. Simulated size and power are recorded in \file{inst/validation/pass-a-size-power.csv}.
}
\section{Supported models}{
It accepts an \code{lm} and a \code{glm} with the Gaussian family and the identity link, unweighted
or with positive weights, a \code{fitWLS()} fit and, since 0.16.0, a rank-deficient fit, which it
tests on the estimable part of its design: it gives the value of the same model fitted without the
aliased columns. It refuses a \code{glm} of another family or link, a \code{survey::svyglm()} fit, a
fit with a zero weight, a coefficient that is not finite for another reason, and \code{data} that
lack rows or variables of the fit. Since 0.16.0 it is built on the model context, and also refuses
a multivariate \code{lm} ("needs a model with one response"), a fit whose coefficients, residuals or
weights were changed after the fit, and a fit with \code{model = FALSE} whose model frame, rebuilt
from the data it was fitted to, no longer holds its response, because those data changed after the
fit, with or without \code{data}. A fit with \code{model = FALSE} whose data are gone is tested on
\code{data} that give its response and fitted values, and refused without them. The test reads
nothing from \code{data}, and since 0.16.0 matches it to the observations of the fit as every
test does: data with row names of their own in any order, data that number their rows (a tibble,
a data.table) in the order of the fit; other numbered data, rows in front of those of the fit and
repeated row names are refused, where it gave the value of the fit. Which model each test
accepts, and why, is set out in
\code{\link{test_results}}, under \emph{Supported models}.
}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performHarveyTest(m)

 # Studentized variant, which does not assume normal errors
 performHarveyTest(m, studentize = TRUE)

 # Variance driven by the conditional mean rather than by the regressors
 performHarveyTest(m, auxiliary = "fitted")
}
\references{
Harvey, A. C. (1976). Estimating regression models with multiplicative heteroscedasticity. \emph{Econometrica}, 44(3), 461--465. \doi{10.2307/1913974}

Greene, W. H. (2018). \emph{Econometric Analysis} (8th ed.). Pearson. Section 9.5 derives the \eqn{\mathrm{ESS} / 4.9348} form of the statistic.
}
\seealso{
  \code{\link{performParkTest}}, \code{\link{performGlejserTest}}, \code{\link{performBPTest}}
}
