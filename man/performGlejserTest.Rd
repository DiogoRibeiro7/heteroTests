\name{performGlejserTest}
\alias{performGlejserTest}
\title{Perform Glejser test for heteroscedasticity}
\description{Regresses absolute residuals on a transformation of a suspected variable. With \code{robust = TRUE} the test is the version of Im (2000) and of Machado and Santos Silva (2000), which keeps its level when the errors are asymmetric.}
\details{A variety of transformations of the explanatory variable (e.g. absolute value, square root) can reveal a relationship between scale and the covariate. Significance of the slope parameter is assessed with a t test. For a weighted fit the residuals are the Pearson residuals.

The default statistic has its nominal level when positive and negative errors are equally likely. Use \code{robust = TRUE} when the residuals are skewed or their shape is unknown.

Either form conditions on one variable and one transformation. Scanning several transformations and reporting only the smallest p-value invalidates the nominal level. Simulated size and power are recorded in \code{inst/validation/pass-a-size-power.csv} for the default and in \code{inst/validation/glejser-skewed-errors.csv} for both.}
\usage{performGlejserTest(model, data, variable,
                   transformation = c("abs", "sqrt", "inverse", "inverse_sqrt"),
                   robust = FALSE)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{data frame used to fit \code{model}.}
  \item{variable}{name of the suspected variable.}
  \item{transformation}{transformation applied to \code{variable}.}
  \item{robust}{logical. When \code{FALSE} (the default) the statistic is Glejser's (1969), which has its nominal level when positive and negative errors are equally likely, as they are for symmetric errors. When \code{TRUE} the absolute residuals are corrected for the asymmetry of the errors before the auxiliary regression. See the section on asymmetric errors.}
}
\value{An object of class \code{htest} containing the t statistic, p-value and degrees of freedom. The \code{method} element says which of the two statistics was computed.}
\section{Asymmetric errors}{
Glejser's statistic takes the absolute residuals for the absolute errors. The two differ by the estimation error of the mean equation,
\deqn{|\hat{e}_i| \approx |e_i| - \mathrm{sign}(e_i)\, x_i^\top (\hat{\beta} - \beta),}{|ehat_i| ~ |e_i| - sign(e_i) x_i'(betahat - beta),}
and the second term averages out only when positive and negative errors are equally likely (Godfrey 1996). When they are not, and the variable tested is correlated with the regressors, the test does not in general have its nominal level, and a larger sample does not help. In the designs of \code{inst/validation/glejser-skewed-errors.R} the default statistic rejects a true null hypothesis about 13\% of the time at the 5\% level under exponential errors and about 17\% under lognormal errors, at every sample size from 50 to 1000. For other error distributions the rate can also fall below the nominal level.

\code{robust = TRUE} replaces the regressand \eqn{|\hat{e}_i|}{|ehat_i|} by
\deqn{|\hat{e}_i| - \hat{m}\, \hat{e}_i, \qquad \hat{m} = \frac{1}{n} \sum_{i=1}^{n} \mathrm{sign}(\hat{e}_i),}{|ehat_i| - mhat ehat_i,   mhat = mean(sign(ehat_i)),}
where \eqn{\hat{m}}{mhat} is the proportion of positive residuals less the proportion of negative ones. This is the correction of Im (2000). The regressand of Machado and Santos Silva (2000), \eqn{\hat{e}_i\,[I(\hat{e}_i \ge 0) - \hat{\eta}]}{ehat_i [I(ehat_i >= 0) - etahat]} with \eqn{\hat{\eta}}{etahat} the proportion of non-negative residuals, is half of it when no residual is exactly zero, and the two then give the same test. A residual that is zero up to rounding error is counted as zero, so that the statistic does not depend on the sign of a rounding error.

The auxiliary regression is otherwise the same. Both references state the statistic as \eqn{n R^2}{n R^2} of that regression, referred to a chi-squared distribution with one degree of freedom. The t statistic reported here is the signed form of it, \eqn{n R^2 = n t^2 / (t^2 + n - 2)}{n R^2 = n t^2 / (t^2 + n - 2)}, and its p-value is taken from the t distribution with \eqn{n - 2}{n - 2} degrees of freedom, as for the default. The two p-values agree as the sample grows.

In the same study the corrected statistic is within simulation error of the 5\% level from 150 observations on. At 50 observations it rejects 5.7\% to 6.6\% of the time under skewed errors. Under symmetric errors \eqn{\hat{m}}{mhat} is close to zero and the two statistics differ little: the corrected one rejects about half a percentage point less often at 50 observations and gives up one or two points of power there. For a weighted fit the correction is applied to the Pearson residuals.
}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performGlejserTest(m, mtcars, "wt")

 # The version that keeps its level under asymmetric errors
 performGlejserTest(m, mtcars, "wt", robust = TRUE)

 # Examine an alternative transformation
 performGlejserTest(m, mtcars, "wt", transformation = "inverse")
}
\references{Glejser, H. (1969). A new test for heteroskedasticity. \emph{Journal of the American Statistical Association}, 64(325), 316--323.

Godfrey, L. G. (1996). Some results on the Glejser and Koenker tests for heteroskedasticity. \emph{Journal of Econometrics}, 72(1--2), 275--299. \doi{10.1016/0304-4076(94)01722-0}

Im, K. S. (2000). Robustifying Glejser test of heteroskedasticity. \emph{Journal of Econometrics}, 97(1), 179--188. \doi{10.1016/S0304-4076(99)00061-5}

Machado, J. A. F. and Santos Silva, J. M. C. (2000). Glejser's test revisited. \emph{Journal of Econometrics}, 97(1), 189--202. \doi{10.1016/S0304-4076(00)00016-6}}
\seealso{\code{\link{performKoenkerTest}} for a test on the squared residuals that needs neither normal nor symmetric errors; \code{\link{performParkTest}} and \code{\link{performHarveyTest}} for related parametric tests.}
