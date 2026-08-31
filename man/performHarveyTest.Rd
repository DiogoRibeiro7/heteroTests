\name{performHarveyTest}
\alias{performHarveyTest}
\title{Perform Harvey test for multiplicative heteroscedasticity}
\description{Harvey's (1976) Lagrange multiplier test, which regresses \code{log(residuals^2)} on a set of variance regressors.}
\details{
Harvey (1976) models the error variance as \eqn{\sigma_i^2 = \exp(z_i^\top \gamma)} and tests \eqn{\gamma = 0} through the auxiliary regression of \eqn{\log \hat{e}_i^2} on \eqn{z_i}. Under the null the auxiliary error behaves like a centred \eqn{\log \chi^2_1} variate with variance \eqn{\pi^2 / 2 \approx 4.9348}, so the classical statistic is \eqn{\mathrm{ESS} / (\pi^2 / 2)}, asymptotically chi-square with \eqn{q} degrees of freedom.

Setting \code{studentize = TRUE} replaces the fixed constant \eqn{\pi^2 / 2} with the auxiliary residual mean square and refers the overall F statistic to an F distribution. The two forms are asymptotically equivalent under normal errors; the studentized form is more reliable when normality is doubtful, because \eqn{\pi^2 / 2} is the null variance of \eqn{\log \hat{e}^2} only for Gaussian errors.

Degrees of freedom are taken from the realised rank of the auxiliary design, so a rank-deficient variance model is not credited with degrees of freedom for aliased columns.
}
\usage{performHarveyTest(model, auxiliary = c("regressors", "fitted"),
                  studentize = FALSE)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{auxiliary}{character scalar choosing the variance regressors. \code{"regressors"} (the default) uses the model's own explanatory variables, the specification in Harvey (1976). \code{"fitted"} uses the fitted values and their square, which was the behaviour of releases before 0.7.0.}
  \item{studentize}{logical; if \code{FALSE} (the default) the chi-square statistic \eqn{\mathrm{ESS} / (\pi^2 / 2)} is reported, otherwise the auxiliary regression F statistic.}
}
\value{An object of class \code{htest} containing the test statistic, its degrees of freedom and the p-value.}
\section{Validation}{
The default statistic reproduces an independent reconstruction of Harvey (1976) to within \code{1e-8}; see \file{tests/testthat/test-pass-a-reference.R}. Simulated size and power are recorded in \file{inst/validation/pass-a-size-power.csv}.
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
