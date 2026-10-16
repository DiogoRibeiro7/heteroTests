\name{test_results}
\alias{test_results}
\title{What every test returns}
\description{
Every exported \code{perform*Test()} function returns a base-R \code{htest} object, so a result prints, subsets and composes like the result of \code{stats::t.test()} or \code{lmtest::bptest()}. This page lists every field such a result can hold: the five every test returns, the two optional \code{htest} fields some return, and the fields a few tests add. \file{tests/testthat/test-return-contract.R} checks every exported test against it.
}
\section{Fields of every test}{
\describe{
  \item{\code{statistic}}{the test statistic: a numeric value of length one, named after the statistic (\code{"X-squared"}, \code{"F"}, \code{"t"}, \code{"GQ"}, \code{"LM"}, \code{"z"}, ...).}
  \item{\code{parameter}}{the parameters of the null distribution: a numeric vector whose elements are named, usually degrees of freedom (\code{"df"}, or \code{"df1"} and \code{"df2"}). The resampling tests store the number of replications here (\code{"B"}, \code{"permutations"}), and the tests on ordered observations the sizes they use (\code{"n"}, \code{"m"}). It is \code{NULL} for \code{\link{performPesaranTest}()}, whose statistic is referred to the standard normal distribution.}
  \item{\code{p.value}}{the p-value: a single number between 0 and 1.}
  \item{\code{method}}{a character string naming the test. When an argument switches between two statistics, such as \code{studentize} in \code{\link{performHarveyTest}()}, the string says which one was computed.}
  \item{\code{data.name}}{a character string describing what was tested: the formula of the model, followed for some tests by the variance regressors. For \code{\link{performBoxMTest}()}, which tests data rather than a model, it names the data and the grouping.}
}
}
\section{Optional htest fields}{
\describe{
  \item{\code{alternative}}{a character string describing the alternative hypothesis, returned by the tests that have a direction or a stated alternative.}
  \item{\code{estimate}}{estimates that accompany the statistic: a named numeric vector (\code{\link{performSzroeterTest}()}, \code{\link{performSpearmanTest}()}, \code{\link{performVarianceFormTest}()}), or, in \code{\link{performQuantileRegressionTest}()}, a matrix of slope estimates with one column per quantile and named rows and columns.}
}
}
\section{Fields some tests add}{
\describe{
  \item{\code{p.value_bootstrap}}{\code{\link{performWhiteTestRobust}()} and \code{\link{performBPTestRobust}()} with \code{bootstrap = TRUE}: the bootstrap p-value.}
  \item{\code{robust_details}}{\code{\link{performWhiteTestRobust}()} and \code{\link{performBPTestRobust}()}: a list with the confidence interval, effect size, bootstrap results, power and sample size, and \code{method} or \code{studentized}, the setting that chose the statistic.}
  \item{\code{boot_statistics}}{\code{\link{performWhiteTestBootstrap}()}: the bootstrap replicates of the statistic.}
  \item{\code{bootstrap}}{\code{\link{performWildBootstrapTest}()}: a list with the replicates, the number of usable ones, the multiplier distribution and the observed statistic.}
  \item{\code{permutation}}{\code{\link{performRankPermutationTest}()}: a list with the replicates, the number of usable ones and the ordering variable.}
  \item{\code{projection}}{\code{\link{performHighDimensionalTest}()}: a list with the number of principal components kept, the share of the variance they explain and the threshold that chose them.}
  \item{\code{variance_function}}{\code{\link{performVarianceFormTest}()}: a list with the form of the variance function, its regressors and the added terms that were tested.}
  \item{\code{quantiles}, \code{se_type}, \code{iid}}{\code{\link{performQuantileRegressionTest}()}: the quantiles compared and the settings of the standard errors.}
  \item{\code{chunks_processed}, \code{chunk_size}, \code{r_squared}}{\code{\link{performWhiteTestStreaming}()}, \code{\link{performBPTestStreaming}()} and \code{\link{performKoenkerTestStreaming}()}: how the data were read and the \eqn{R^2}{R^2} of the auxiliary regression.}
  \item{\code{res}}{\code{\link{performSpatialHeteroTest}()}: the Monte Carlo replicates of Moran's I from \code{spdep::moran.mc()}, whose class \code{"mc.sim"} the result keeps.}
}
}
\section{Attributes}{
\code{\link{performWhiteTest}()} and \code{\link{performWhiteTestRobust}()} attach the attributes \code{auxiliary_regressors}, \code{original_regressors}, \code{cross_products_included} and \code{r_squared_auxiliary}, which describe the auxiliary regression. \code{\link{runHeteroTests}()} adds attributes of its own to each result it returns.
}
\section{Inputs every test refuses}{
Since 0.15.0 every test on a model refuses four inputs, each with one message: a \code{glm} that is not a least-squares
fit, that is, of any family but \code{gaussian} or with any link but the identity ("needs a model fitted by
stats::lm()"); a model with a coefficient that is not finite, infinite, \code{NaN}, or \code{NA} without the fit being
rank-deficient ("Model coefficients must be finite"); \code{data} that lack rows the model was fitted to; and
\code{data} that lack a variable of the model ("The following variables are missing from"). Until 0.16.0 the
\code{NA} of an aliased term was refused too; since then a rank-deficient fit is tested on the estimable part of its
design (see \emph{Supported models}). The model frame is accepted as \code{data} as it is. A \code{glm} with the Gaussian family and the identity link is accepted and gives the result of the \code{lm} up
to rounding error, except that \code{\link{performWhiteTestStreaming}()}, \code{\link{performHarrisonMcCabeTest}()} and
\code{\link{performVarianceFormTest}()} refuse every \code{glm}. With positive prior weights it gives the result of
\code{lm(..., weights = w)}: \code{\link{performGQTest}()}, \code{\link{performRESETTest}()},
\code{\link{performWhiteTestBootstrap}()} and \code{\link{performWildBootstrapTest}()}, which read the weights of an
\code{lm}, read the prior weights of the \code{glm}, and \code{\link{performQuantileRegressionTest}()}, which refuses
a weighted \code{lm}, refuses it with the same message. With a zero prior weight it is refused as the \code{lm} with a
zero weight is ("The model was fitted with zero or non-finite weights"), since 0.16.0. A \code{survey::svyglm()} fit is
refused by every test ("The model is a \code{survey::svyglm()} fit"). \code{tests/testthat/test-input-validation.R}
gives every test these inputs.

The tests that read \code{data} also refuse \code{data} whose rows cannot be confirmed to hold the observations of
the fit ("cannot match the rows of \code{data} to the observations of the model"). They match the rows of \code{data}
to the residuals by row name, and a tibble, a data.table or a data frame whose row names were reset, as by
\code{dplyr::arrange()}, has row names that only number its rows. Before a test reads \code{data}, the rows the row
names pick are compared with the model frame of the fit, row by row: every column \code{data} holds under its own
name, value by value, and the others (\code{log(x)}, \code{poly(x, 2)}, \code{offset(o)}, the weights and an offset
given as arguments) computed from \code{data} as the fit computed them. A term computed from the rows it is given,
such as \code{I(x - mean(x))}, and a column that names a variable \code{data} does not hold, are not compared; for a
term of variables of \code{data}, the compared columns must then tell the rows of the fit apart. Data that cannot be
confirmed so are refused, even the data of the fit: when no column can be compared, as with \code{lm(d$y ~ d$x)}, or
when the compared columns tie on rows that differ in such a term. Pass the full data the model was fitted on, or the
variables as columns of \code{data}. Data without row names of their own in another order
than the fit, or with rows in
front of those of the fit, are refused, as are repeated row names, and a model frame rebuilt for a fit with
\code{model = FALSE} from data that changed after the fit. Data with row names of their own are matched by name, in
any order, except by
\code{\link{performPesaranTest}()}, \code{\link{performBPRandomEffectsTest}()} and
\code{\link{performScatterDiagnostic}()}, which read \code{data} by position and need its rows in the order of the
fit. \code{tests/testthat/test-data-alignment-guard.R} checks these refusals.
}
\section{Supported models}{
The tests examine the residuals of a least-squares fit for a variance that is not constant, so they accept the models
whose residuals are those of a least-squares fit, and refuse the others early, with the messages above. Which test
accepts which class, measured by running every test on each, is the table of the \emph{Supported models} page of the
documentation site; \code{tests/testthat/test-model-eligibility.R} and \code{test-input-validation.R} check it.
\itemize{
  \item An \code{lm} is accepted by every test.
  \item An \code{lm(..., weights = w)} is accepted: \code{lm()} states that the error variance is
  \eqn{\sigma^2 / w_i}{sigma^2 / w_i}, and the tests read the Pearson residuals \eqn{\sqrt{w_i}\, e_i}{sqrt(w_i) e_i},
  which have constant variance when the weights are right. \code{\link{performQuantileRegressionTest}()}, which refits
  without them, refuses it, and \code{\link{performVarianceFormTest}()} refuses weights that did not come from
  \code{\link{fitWLS}()}.
  \item A zero weight: the observation is not part of the fit, and its residual is an artificial zero that a test would
  count. Refused by every test but \code{\link{performRESETTest}()}, an F test of two nested weighted least-squares
  fits, in which a zero weight drops the observation from both; it gives the value of the fit to the observations with
  positive weight.
  \item A \code{glm} with the Gaussian family and the identity link is a least-squares fit: it gives the result of the
  \code{lm}, weighted or not, but \code{\link{performWhiteTestStreaming}()}, \code{\link{performHarrisonMcCabeTest}()}
  and \code{\link{performVarianceFormTest}()} refuse every \code{glm}. The tests read its prior weights,
  \code{model$prior.weights}, and never \code{model$weights}, which holds the working weights of the last iteration of
  iteratively reweighted least squares, \eqn{w_i (d\mu_i/d\eta_i)^2 / V(\mu_i)}{w_i (dmu_i/deta_i)^2 / V(mu_i)}. For
  this family and link the two are equal; for any other they depend on the fitted means.
  \item Any other \code{glm}, such as a binomial or a Poisson one, is refused: its residuals have a variance that
  depends on the mean by construction, so a test of constant variance tests nothing the help pages describe.
  \item A \code{survey::svyglm()} fit is refused: its residuals are scaled by the sampling weights, which are not
  precision weights. See \code{\link{runSurveyHeteroTests}()}.
  \item Panel models: \code{\link{performPesaranTest}()} and \code{\link{performBPRandomEffectsTest}()}, and so
  \code{\link{runPanelTests}()}, take a pooled \code{lm} and the columns of \code{data} that name the units and the
  periods. A \code{plm::plm()} fit is not an \code{lm} and is refused, by every test.
  \item A rank-deficient fit: a term that is a linear combination of others, or the dummy of a cell of an interaction
  that no observation falls in, leaves an \code{NA} coefficient. Its residuals and fitted values are the projection on
  the column space of the design, which the aliased columns do not change. Since 0.16.0 every test that accepts the
  model without the aliased columns accepts it, computed on the estimable part of the design: the columns that were
  not aliased, with degrees of freedom from the rank, so that it gives the statistic and p-value of the model fitted
  without them. A test that refuses that model refuses the rank-deficient one with the same message.
  \item A \code{fitWLS()} fit is a weighted \code{lm}, with estimated weights: accepted, but the reference
  distributions take the weights as known (\code{\link{performVarianceFormTest}()} accounts for the estimation).
  \item A \code{MASS::rlm()} fit is an \code{lm} by class, so the tests accept it, as before 0.16.0, and test its
  M-estimation residuals as if they were least-squares ones; its case weights, all 1 unless given, are read as
  precision weights, so \code{\link{performQuantileRegressionTest}()} refuses it as a weighted fit. Whether to keep
  accepting it or refuse it is an open decision.
}
\code{\link{checkModel}()} checks the class of a model only, \code{lm} or \code{glm}; it does not check whether a test
accepts it.
}
\seealso{
\code{\link{runHeteroTests}}, \code{\link[stats]{t.test}} for the \code{htest} class
}
