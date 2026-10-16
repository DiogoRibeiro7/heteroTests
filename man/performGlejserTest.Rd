\name{performGlejserTest}
\alias{performGlejserTest}
\title{Perform Glejser test for heteroscedasticity}
\description{Regresses absolute residuals on a transformation of a suspected variable. By default, \code{robust = TRUE} since 0.15.0, the absolute residuals are first corrected for the asymmetry of the errors: this is the version of Im (2000) and of Machado and Santos Silva (2000), which keeps its level when the errors are asymmetric, from about 150 observations on (see the section on asymmetric errors for smaller samples). \code{robust = FALSE} gives Glejser's (1969) statistic, the default up to 0.14.0.}
\details{A variety of transformations of the explanatory variable (e.g. absolute value, square root) can reveal a relationship between scale and the covariate. Significance of the slope parameter is assessed with a t test. For a weighted fit the residuals are the Pearson residuals.

Glejser's statistic, \code{robust = FALSE}, has its nominal level only when positive and negative errors are equally likely. The corrected statistic of the default does not need that, and is the one to use when the residuals may be skewed or their shape is unknown.

Either form conditions on one variable and one transformation. Scanning several transformations and reporting only the smallest p-value invalidates the nominal level. Simulated size and power of both statistics are recorded in \code{inst/validation/glejser-skewed-errors.csv}; those of Glejser's statistic in \code{inst/validation/pass-a-size-power.csv}, and those of the default in \code{inst/validation/full-sweep-size-power.csv} and \code{inst/validation/weighted-fits-size.csv}.}
\usage{performGlejserTest(model, data = NULL, variable,
                   transformation = c("abs", "sqrt", "inverse", "inverse_sqrt"),
                   robust = TRUE)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{data frame used to fit \code{model}. When \code{NULL}, the default, the model frame of \code{model} is used, so \code{variable} must then be a variable of the model. The model frame holds the terms of the model rather than its variables (\code{log(x)} rather than \code{x}), so a model that transforms a variable needs \code{data}, as does a model with an \code{offset()} term.}
  \item{variable}{name of the suspected variable.}
  \item{transformation}{transformation applied to \code{variable}.}
  \item{robust}{logical. When \code{TRUE}, the default since 0.15.0, the absolute residuals are corrected for the asymmetry of the errors before the auxiliary regression. When \code{FALSE} the statistic is Glejser's (1969), which has its nominal level when positive and negative errors are equally likely, as they are for symmetric errors; it is what the function returned by default up to 0.14.0. See the sections on asymmetric errors and on the default.}
}
\value{An object of class \code{htest} containing the t statistic, p-value and degrees of freedom. The \code{method} element says which of the two statistics was computed.}
\section{Asymmetric errors}{
Glejser's statistic takes the absolute residuals for the absolute errors. The two differ by the estimation error of the mean equation,
\deqn{|\hat{e}_i| \approx |e_i| - \mathrm{sign}(e_i)\, x_i^\top (\hat{\beta} - \beta),}{|ehat_i| ~ |e_i| - sign(e_i) x_i'(betahat - beta),}
and the second term averages out only when positive and negative errors are equally likely (Godfrey 1996). When they are not, and the variable tested is correlated with the regressors, the test does not in general have its nominal level, and a larger sample does not help. In the designs of \code{inst/validation/glejser-skewed-errors.R} Glejser's statistic, \code{robust = FALSE}, rejects a true null hypothesis about 13\% of the time at the 5\% level under exponential errors and about 17\% under lognormal errors, at every sample size from 50 to 1000. For other error distributions the rate can also fall below the nominal level.

\code{robust = TRUE} replaces the regressand \eqn{|\hat{e}_i|}{|ehat_i|} by
\deqn{|\hat{e}_i| - \hat{m}\, \hat{e}_i, \qquad \hat{m} = \frac{1}{n} \sum_{i=1}^{n} \mathrm{sign}(\hat{e}_i),}{|ehat_i| - mhat ehat_i,   mhat = mean(sign(ehat_i)),}
where \eqn{\hat{m}}{mhat} is the proportion of positive residuals less the proportion of negative ones. This is the correction of Im (2000). The regressand of Machado and Santos Silva (2000), \eqn{\hat{e}_i\,[I(\hat{e}_i \ge 0) - \hat{\eta}]}{ehat_i [I(ehat_i >= 0) - etahat]} with \eqn{\hat{\eta}}{etahat} the proportion of non-negative residuals, is half of it when no residual is exactly zero, and the two then give the same test. A residual that is zero up to rounding error is counted as zero, so that the statistic does not depend on the sign of a rounding error.

The auxiliary regression is otherwise the same. Both references state the statistic as \eqn{n R^2}{n R^2} of that regression, referred to a chi-squared distribution with one degree of freedom. The t statistic reported here is the signed form of it, \eqn{n R^2 = n t^2 / (t^2 + n - 2)}{n R^2 = n t^2 / (t^2 + n - 2)}, and its p-value is taken from the t distribution with \eqn{n - 2}{n - 2} degrees of freedom, as for Glejser's statistic. The two p-values agree as the sample grows.

In the same study, with 5000 replications in each cell, the corrected statistic is within simulation error of the 5\% level from 150 observations on: all 27 of its cells with a true null hypothesis and 150 observations or more are inside the 99\% interval around 5\%, from 4.21\% to 5.79\%, at 4.4\% to 5.7\%. At 50 observations it rejects 5.7\% to 6.6\% of the time under skewed errors, and four of those six cells are above that interval, at 5.80\% to 6.58\%. Under symmetric errors \eqn{\hat{m}}{mhat} is close to zero and the two statistics differ little: the corrected one rejects 0.5 to 0.7 percentage points less often at 50 observations and gives up one or two points of power there. For a weighted fit the correction is applied to the Pearson residuals.
}
\section{The default}{
Up to 0.14.0 the default was \code{robust = FALSE}. Since 0.15.0 it is \code{robust = TRUE}, so a call that does not give \code{robust} returns a different statistic and p-value from those it returned before, and its \code{method} ends in "(robust to asymmetric errors)". \code{robust = FALSE} returns what such a call returned up to 0.14.0.

Glejser's statistic needs positive and negative errors to be equally likely, which is seldom known: in the study above it rejects a true null hypothesis 8.8\% to 17.7\% of the time at the 5\% level when the errors are skewed, at every sample size from 50 to 1000, where the corrected statistic rejects 4.6\% to 5.7\% of the time from 150 observations on. The change costs some power when the errors are symmetric, 1.1 and 1.9 percentage points at 50 observations under Gaussian errors in that study, and the corrected statistic is above the 5\% level at 50 observations under skewed errors, as above.
}
\section{Supported models}{
It accepts an \code{lm} and a \code{glm} with the Gaussian family and the identity link, unweighted
or with positive weights, a \code{fitWLS()} fit and, since 0.16.0, a rank-deficient fit, which it
tests on the estimable part of its design: it gives the value of the same model fitted without the
aliased columns. It refuses a \code{glm} of another family or link, a \code{survey::svyglm()} fit, a
fit with a zero weight, a coefficient that is not finite for another reason, and \code{data} that
lack rows or variables of the fit. Which model each test accepts, and why, is set out in
\code{\link{test_results}}, under \emph{Supported models}.
}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 # The default, which keeps its level under asymmetric errors from about
 # 150 observations
 performGlejserTest(m, mtcars, "wt")

 # Glejser's (1969) statistic, the default up to 0.14.0
 performGlejserTest(m, mtcars, "wt", robust = FALSE)

 # Examine an alternative transformation
 performGlejserTest(m, mtcars, "wt", transformation = "inverse")
}
\references{Glejser, H. (1969). A new test for heteroskedasticity. \emph{Journal of the American Statistical Association}, 64(325), 316--323.

Godfrey, L. G. (1996). Some results on the Glejser and Koenker tests for heteroskedasticity. \emph{Journal of Econometrics}, 72(1--2), 275--299. \doi{10.1016/0304-4076(94)01722-0}

Im, K. S. (2000). Robustifying Glejser test of heteroskedasticity. \emph{Journal of Econometrics}, 97(1), 179--188. \doi{10.1016/S0304-4076(99)00061-5}

Machado, J. A. F. and Santos Silva, J. M. C. (2000). Glejser's test revisited. \emph{Journal of Econometrics}, 97(1), 189--202. \doi{10.1016/S0304-4076(00)00016-6}}
\seealso{\code{\link{performKoenkerTest}} for a test on the squared residuals that needs neither normal nor symmetric errors; \code{\link{performParkTest}} and \code{\link{performHarveyTest}} for related parametric tests.}
