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
stats::lm()"); a model with a coefficient that is not finite, such as the \code{NA} of an aliased term ("Model
coefficients must be finite"); \code{data} that lack rows the model was fitted to; and \code{data} that lack a
variable of the model ("The following variables are missing from"). The model frame is accepted as \code{data} as it
is. A \code{glm} with the Gaussian family and the identity link is accepted and gives the result of the \code{lm} up
to rounding error, except that \code{\link{performWhiteTestStreaming}()}, \code{\link{performHarrisonMcCabeTest}()} and
\code{\link{performVarianceFormTest}()} refuse every \code{glm}, and \code{\link{performGQTest}()},
\code{\link{performRESETTest}()}, \code{\link{performWhiteTestBootstrap}()}, \code{\link{performWildBootstrapTest}()}
and \code{\link{performQuantileRegressionTest}()}, which read the weights of an \code{lm}, refuse a \code{glm} with
prior weights. \code{tests/testthat/test-input-validation.R} gives every test these inputs.
}
\seealso{
\code{\link{runHeteroTests}}, \code{\link[stats]{t.test}} for the \code{htest} class
}
