\name{runHeteroTests}
\alias{runHeteroTests}
\title{Run multiple heteroscedasticity diagnostics}
\description{Convenience wrapper that executes several heteroscedasticity tests on a fitted linear model.}
\details{By default runs \code{\link{performWhiteTest}} and \code{\link{performKoenkerTest}}, the studentized
Breusch-Pagan test, but additional tests can be requested. Custom diagnostics registered via \code{registerDiagnostic}
are also available.

Up to 0.14.0 the default was \code{c("white", "breusch_pagan")}, whose second test is the classical Breusch-Pagan
statistic of \code{\link{performBPTest}}. That statistic divides by \eqn{2 \hat\sigma^4}{2 sigma^4}, the variance of
the squared errors only when they are normal. In the full sweep of \code{inst/validation/}, with 150 observations and
400 replications, it rejected a true null hypothesis 28.75\% of the time at the 5\% level when the errors had a
\eqn{t_5}{t5} distribution, against 4.75\% for Koenker's statistic. The classical test is still run by name,
\code{tests = "breusch_pagan"}.

When a test stops with an error, the substitute registered for it is run, if one is and it succeeds, and its result
is returned under the name of the test asked for: \code{"white"} falls back on White's test without cross products,
the classical Breusch-Pagan, Koenker and Cook-Weisberg tests, \code{"koenker"} on the Cook-Weisberg test. The
\code{method} of the result names the test that ran, \code{attr(result, "diagnostic")} its registry name and
\code{attr(result, "extras")$status} is \code{"fallback"}. A weighted fit whose weights are a column of the data,
called without \code{data}, is such a case: White's and Koenker's tests stop, and both names hold the Cook-Weisberg
test. Pass \code{data} for such a fit.

The runner runs the exact tests unless \code{chunk_threshold_mb} asks for the streaming ones,
\code{\link{performWhiteTestStreaming}()}, \code{\link{performBPTestStreaming}()} and
\code{\link{performKoenkerTestStreaming}()}. Up to 0.15.0 it switched to them by itself on data above 100 MB, and
\code{\link{performWhiteTest}()}, \code{\link{performBPTest}()} and \code{\link{performKoenkerTest}()} did so on their
own. Streaming saves memory mainly in White's test, whose auxiliary design is several times wider than the model
matrix; the auxiliary design of the Breusch-Pagan and Koenker tests is the model matrix itself. And until 0.17.0 the
streaming White test stopped on any model with a factor or a \code{poly()} term, whose squares are collinear with the other
terms of its auxiliary regression, so that above 100 MB such a model got a fallback under the name \code{"white"} where
smaller data got White's test; since 0.17.0 it drops those terms, as \code{\link{performWhiteTest}()} does.}
\usage{
runHeteroTests(
  model,
  data = NULL,
  tests = c("white", "koenker"),
  use_cache = TRUE,
  chunk_threshold_mb = Inf,
  chunk_size = 10000,
  progress = interactive()
)
}
\arguments{
  \item{model}{a fitted \code{lm}, or a \code{glm} with the Gaussian family and the identity link, a model
  formula, a tidymodels workflow or a parsnip model fit. The tests refuse a \code{glm} of any other family or link
  since 0.15.0, the deprecated registry names \code{"ncv"} and \code{"student_bp"} and the substitutes of the
  fallback (Details) included: the call stops with the error of the first test asked for, and with grouped data
  each test is returned as failed. A workflow, or a parsnip fit, is tested on the fit of its engine, which names its
  response \code{..y} and its predictors as the workflow's preprocessing made them: the data the workflow was fitted
  to are mapped to those variables by that preprocessing (\code{hardhat::forge()}), the outcome as the
  preprocessing transforms it among them, or the outcome of a fit of \code{parsnip::fit_xy()} to \code{..y}, and
  once their rows are matched to the observations of the fit, every column of its model frame compared, its model
  frame is tested, as without \code{data}. Before 0.17.0 they were refused. Data in another order than the workflow
  was fitted in are refused, and so are data that cannot be mapped, such as data without a column the
  preprocessing needs, grouped data, and the data of a workflow whose recipe skips on new data a step that changed
  the outcome or a predictor at the fit, such as \code{step_log(y, skip = TRUE)}, which the message names. The case
  weights of a workflow are compared with those of the fit, on the rows its preprocessing keeps; the data of a
  parsnip fit of \code{fit_xy()} with case weights, whose column the fit does not record, are refused. Each result keeps
  what \code{generics::augment()} needs to map the data in the same way, the blueprint of the preprocessing, the
  number of rows it kept and the column of the case weights, or the outcome of a parsnip fit, as
  \code{attr(, "tidymodels")}, the blueprint as the workflow keeps it: a recipe step keeps the environment it was
  written in, so that a saved result carries what that environment holds, as saving the workflow does. The results
  are otherwise those of the fit of the engine.}
  \item{data}{optional data frame used to fit \code{model}. If omitted, \code{model.frame(model)} is used, which
  serves only the tests that find what they need there (see their \code{data} argument). With
  data grouped by \code{dplyr::group_by()} the model is refitted within each group by evaluating its own call on that
  group, so every variable it uses, including weights, offset and subset, must be a column of \code{data}. A fit
  returned by \code{\link{fitWLS}()} is refused with grouped data since 0.17.0: its call does not hold the weights
  it estimated, so each group was tested as the unweighted fit of its rows; fit \code{fitWLS()} within each group,
  or pass the data ungrouped. So is a \code{MASS::rlm()} fit, which every test refuses. A test that refuses the model
  of a group as the model context refuses a model, such as a multivariate \code{lm} or a penalised
  \code{mgcv::gam()} fit given a test that needs least squares, stops the grouped run with that refusal and its
  class since 0.17.0, as the run on ungrouped data does; before, the group recorded it as a failed test, whose
  statistic was \code{NA}. The failures of a test for another reason are recorded in its group, as before.}
  \item{tests}{character vector of test names to run: any of the names \code{\link{listDiagnostics}()} returns, which
  include those added with \code{registerDiagnostic}. On a fit returned by \code{\link{fitWLS}()} the resampling tests
  \code{"wild_bootstrap"} and \code{"white_bootstrap"} estimate the weights again in each replicate, as
  \code{\link{performWildBootstrapTest}()} and \code{\link{performWhiteTestBootstrap}()} do by default since 0.15.0; to hold
  them fixed, call those functions with \code{reestimate_weights = FALSE}.}
  \item{use_cache}{logical: reuse the results of an earlier call on the same inputs? Requires the \pkg{digest}
  package and defaults to \code{TRUE}. Only deterministic tests are cached, under a key that holds the test and the
  parts of the model and the data it reads (see \code{\link{cachedTest}}, which also says what the key leaves out):
  the resampling tests \code{"wild_bootstrap"}, \code{"white_bootstrap"}, \code{"rank_permutation"} and
  \code{"spatial_hetero"}, and diagnostics added with \code{registerDiagnostic}, run on every call, and a call that
  runs one, or falls back on one, is not stored as a whole. A seeded call returns what it returns with
  \code{use_cache = FALSE}, \code{identical()}, and leaves the random number generator where it leaves it. Each test
  is kept in the cache of \code{\link{cachedTest}}, and each call whose tests are all deterministic in a second cache
  (\code{\link{clearAnalysisCache}}); each holds at most \code{getOption("heteroTests.cache_size", 100)} results and
  drops the one used least recently, and \code{options(heteroTests.cache_size = 0)} turns both off. The checks the
  tests make first have a cache of their own, bounded by \code{getOption("heteroTests.validation_cache_size", 1000)}.
  Before 0.16.0 the resampling tests were cached too, and two fits with the same coefficients and residuals shared
  their results.}
  \item{chunk_threshold_mb}{size of \code{data} in megabytes above which \code{"white"}, \code{"breusch_pagan"} and
  \code{"koenker"} are run by their streaming forms. The default, \code{Inf}, never streams: give a finite value to ask
  for them, such as \code{100}, or \code{0} for data of any size. The result of a streaming test has
  \code{attr(, "extras")$streaming} set to \code{TRUE}. Up to 0.15.0 the default was \code{100}. A warning about the
  size of the data is given above half the threshold, or above 50 MB with the default.}
  \item{chunk_size}{integer number of observations processed per chunk when the streaming tests run
  (\code{chunk_threshold_mb}).}
  \item{progress}{logical flag controlling whether textual progress bars are displayed when running multiple diagnostics.}
}
\value{A \code{\link{hetero_test_suite}}: a named list of \code{htest} objects, one for each test asked for,
which prints as a table of the tests, and whose \code{summary()} is laid out as that of an \code{lm} (see
\code{\link{summary.hetero_test_suite}}). With grouped data, a \code{\link{hetero_grouped_suite}} holding one such
suite for each group. Since 0.16.0 a suite whose model the runner fitted itself, from a formula, to the data of
a survey design or within a group, says so in \code{attr(, "model_origin")} (\code{"formula"}, \code{"survey"} or
\code{"group"}; see \code{\link{hetero_test_suite}}).

A result whose \code{data.name} is \code{"model"} reports the formula of the model instead, as Koenker's test and
the others do. Up to 0.15.0 five tests reported \code{"model"} here, the name of the argument of the function the
registry calls them with: White's test, its bootstrap and its form without cross products, the studentized
Breusch-Pagan statistic and Szroeter's test. Any other \code{data.name}, such as that of a diagnostic added with
\code{registerDiagnostic}, is kept. Called directly, a test still names the object it is given:
\code{performWhiteTest(m, mtcars)$data.name} is \code{"m"}.}
\examples{
data(mtcars)
m <- lm(mpg ~ wt + qsec, data = mtcars)
# Default: White and Koenker
runHeteroTests(m, mtcars)
# Laid out as summary.lm() is, with the significance codes
summary(runHeteroTests(m, mtcars))
# Specify additional diagnostics, or the classical Breusch-Pagan test
runHeteroTests(m, mtcars, tests = c("white", "koenker", "cook_weisberg"))
runHeteroTests(m, mtcars, tests = "breusch_pagan")
custom <- function(model, data) list(stat = 1)
registerDiagnostic("custom", custom)
runHeteroTests(m, mtcars, tests = c("white", "custom"))
}
\references{White, H. (1980). A heteroskedasticity-consistent covariance matrix estimator and a direct test for heteroskedasticity. \emph{Econometrica}, 48(4), 817--838.
Breusch, T. S., & Pagan, A. R. (1979). A Simple Test for Heteroscedasticity and Random Coefficient Variation. \emph{Econometrica}, 47(5), 1287--1294.
Koenker, R. (1981). A note on studentizing a test for heteroscedasticity. \emph{Journal of Econometrics}, 17(1), 107--112. \doi{10.1016/0304-4076(81)90062-2}}
