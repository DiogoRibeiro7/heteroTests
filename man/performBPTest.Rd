\name{performBPTest}
\alias{performBPTest}
\alias{performBreuschPaganTest}
\title{Perform Breusch-Pagan test for heteroscedasticity}
\description{Implements the classical Breusch-Pagan (1979) test on a fitted linear model, in which the scaled squared residuals are regressed on the original regressors.}
\details{The test statistic is half the explained sum of squares from regressing the scaled squared residuals \eqn{e_i^2/\hat{\sigma}^2 - 1} (with \eqn{\hat{\sigma}^2 = \sum e_i^2 / n}) on the explanatory variables. Under the null hypothesis of homoscedasticity \emph{and normal disturbances} it follows a chi-square distribution with degrees of freedom equal to the number of regressors, matching \code{lmtest::bptest(..., studentize = FALSE)}. For the studentized \eqn{n R^2} form that drops the normality assumption use \code{\link{performKoenkerTest}}.

The test is computed in memory whatever the size of the data. Up to 0.15.0 it switched to \code{\link{performBPTestStreaming}()} on data above 100 MB; call that function to stream.}
\section{Fixed effects for small groups}{The Breusch-Pagan test, like White's and Koenker's tests, is not valid with fixed effects for small groups: a factor with many levels and few observations of each, such as \code{subject} in \code{lm(y ~ x + subject)} with each subject observed a few times. On homoscedastic data, with normal errors, it rejects too often, and the more often the more levels the factor has and the fewer observations each level has. The excess comes from the dummies of the factor in the auxiliary regression. The residuals of a level are taken from its own mean, so under normal errors the squared residuals of a level of \eqn{T} observations vary between levels \eqn{T / (T - 1)} times as much as the auxiliary regression assumes, and with \eqn{G} levels the statistic exceeds its degrees of freedom by about \eqn{(G - 1) / (T - 1)}, as Koenker's did in the study below. Regressed on \code{x} alone, without the dummies, the squared residuals were rejected by this test in 4.3\% to 5.6\% of the draws (\code{inst/validation/fixed-effects-dummies.R}). The excess grows with \eqn{G} at a fixed \eqn{T}, while the spread of the chi-squared distribution grows as \eqn{\sqrt{2G}}{sqrt(2G)}, so with a fixed number of observations per level the size goes to 1 as the number of levels grows.

Since 0.16.0 the test warns when a term of the model that holds a factor or a character variable has more levels than observations per level: \eqn{L > n / L}, that is \eqn{L^2 > n}, with \eqn{L} the number of distinct combinations of the factors of the term among the \eqn{n} observations of the fit. The term is a factor, such as \code{subject}; an interaction of factors, counted by its cells, such as \code{a:b} of a crossed \code{a * b} or \code{school:class} of a nested \code{school / class}; or a slope for each level, \code{x:subject}. A variable left out of the formula, as in \code{y ~ . - id}, does not count. The warning names the term and gives \eqn{L}, \eqn{n} and the mean number of observations per level; the statistic, the degrees of freedom and the p-value are those the test computes without it. It has class \code{heteroTests_fixed_effects_warning}, which \code{suppressWarnings(expr, classes = "heteroTests_fixed_effects_warning")} muffles, and no other warning. It is given on every call, also when \code{\link{cachedTest}()} or \code{\link{runHeteroTests}()} answers from its cache, and \code{\link{runHeteroTests}()} and \code{\link{runDiagnostics}()} give it once for each test that gives it. \code{\link{performBPTestStreaming}()}, \code{\link{performBPTestRobust}()} with \code{studentize = FALSE}, the deprecated \code{performBreuschPaganTest()} and the registered \code{"breusch_pagan"} give it too.

On a balanced design of \eqn{G} subjects of \eqn{T} observations the rule is \eqn{G \le T}{G <= T}. In the study it kept the three tests within 2.5\% to 7.5\% at the 5\% level where it holds, and put at least one of them outside where it does not, in 68 of the 69 designs where they computed; the exception is this test on 20 subjects of 20, at 8.0\%. Unbalanced groups, a second regressor and a regressor correlated with the subject effects followed the same pattern, with the mean number of observations per level, \eqn{n / G}, in place of \eqn{T}. The rule is a warning, not a correction, and many panels fail it: on 100 firms observed over 10 years the three tests reject in 18\% to 21\% of the draws. Rejection at the 5\% level, in percent, in 2,000 homoscedastic draws of \code{lm(y ~ x + subject)}, with \code{x}, the subject effects and the errors standard normal:

\tabular{rrrrrl}{
Subjects \tab Observations of each \tab White \tab Koenker \tab Breusch-Pagan \tab Warns\cr
10 \tab 10 \tab 6.4 \tab 6.2 \tab 7.3 \tab no\cr
20 \tab 20 \tab 6.0 \tab 6.4 \tab 8.0 \tab no\cr
5 \tab 4 \tab 1.0 \tab 5.4 \tab 7.0 \tab yes\cr
30 \tab 3 \tab 50.7 \tab 57.3 \tab 39.6 \tab yes\cr
30 \tab 4 \tab 22.2 \tab 23.9 \tab 28.7 \tab yes\cr
100 \tab 8 \tab 23.8 \tab 23.8 \tab 25.2 \tab yes\cr
100 \tab 10 \tab 18.1 \tab 18.4 \tab 21.3 \tab yes\cr
200 \tab 4 \tab 97.8 \tab 97.6 \tab 82.5 \tab yes\cr
}

A slope for each level counts too, but there the error goes the other way: with a slope for each subject, \code{lm(y ~ x + x:subject)}, on 30 subjects of 3 and of 4 and on 100 of 8, the three tests rejected too rarely, White's in 0.7\% to 2.2\% of the draws, Koenker's in 0.1\% to 0.8\% and the Breusch-Pagan test in 0.4\% to 2.8\% (\code{inst/validation/fixed-effects-variants-size.R}). The warning therefore says that the p-value is unreliable: in the simulations usually far too small, and too large with a slope per level.

The study is \code{inst/validation/fixed-effects-size.R}, with its results in \code{fixed-effects-size.csv} (\code{system.file("validation", package = "heteroTests")}). Juhl and Sosa-Escudero (2014) derive a test of heteroskedasticity for this setting, after fixed effects estimation of a panel of many units observed a fixed number of times; it needs to be told which factor is the unit, and the package does not implement it.}
\section{Deprecated name}{\code{performBreuschPaganTest()} is deprecated since 0.14.0. It was the same function as \code{performBPTest()} under a second name; it still works, returns what \code{performBPTest()} returns and warns. Use \code{performBPTest()}.}
\section{Weighted fits}{On a fit with weights the test is computed from the Pearson residuals \eqn{\sqrt{w_i}\, e_i}, so it tests whether the weights are adequate; see the section on weighted fits in \code{\link{performKoenkerTest}()}, which also covers weights estimated by \code{\link{fitWLS}()}.}
\usage{performBPTest(model, data = NULL)
performBreuschPaganTest(model, data = NULL)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{The data frame \code{model} was fitted to, or \code{NULL}, the default. The response, design, weights and offset are read from the fit itself, so a model that transforms its variables, has an \code{offset()} term, or takes its \code{weights} or \code{offset} from a column of the data needs no \code{data}, and the model frame of \code{model} is accepted as \code{data} (since 0.16.0; before, such models needed \code{data}). Data that are given are matched to the observations of the fit: rows with row names of their own by name, in any order; rows that only number themselves (a tibble, a data.table, a data frame whose row names were reset) by the row numbers of the fit, or in the order of the fit when they hold one row per observation. The rows the fit dropped may be there or not. Data whose rows cannot be shown to hold the observations of the fit are refused; see \code{\link{test_results}}.}
}
\value{An object of class \code{htest} containing the test statistic, p-value and degrees of freedom.}
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
 performBPTest(m, mtcars)
}
\references{
Breusch, T. S., & Pagan, A. R. (1979). A simple test for heteroscedasticity and random coefficient variation. \emph{Econometrica}, 47(5), 1287--1294. \doi{10.2307/1911963}

Koenker, R. (1981). A note on studentizing a test for heteroscedasticity. \emph{Journal of Econometrics}, 17(1), 107--112. \doi{10.1016/0304-4076(81)90062-2}

Juhl, T., & Sosa-Escudero, W. (2014). Testing for heteroskedasticity in fixed effects models. \emph{Journal of Econometrics}, 178(Part 3), 484--494. \doi{10.1016/j.jeconom.2013.07.005}
}
