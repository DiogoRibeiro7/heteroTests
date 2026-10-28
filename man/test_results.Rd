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
design (see \emph{Supported models}). The model frame is accepted as \code{data} as it is. A \code{glm} with the Gaussian family and the identity link is accepted by every \code{perform*Test()} function and gives the result of the \code{lm} up
to rounding error, when the two decide the rank of the design alike (see \emph{Supported models});
\code{\link{performWhiteTestStreaming}()}, \code{\link{performHarrisonMcCabeTest}()} and
\code{\link{performVarianceFormTest}()} refused every \code{glm} until 0.16.0, and \code{\link{bootstrapTestStatistic}()}
until 0.17.0, which bootstraps it as the \code{lm} it is. With positive prior weights it gives the result of
\code{lm(..., weights = w)}: \code{\link{performGQTest}()}, \code{\link{performRESETTest}()},
\code{\link{performWhiteTestBootstrap}()}, \code{\link{performWildBootstrapTest}()} and
\code{\link{performHarrisonMcCabeTest}()}, which read the weights of an
\code{lm}, read the prior weights of the \code{glm}, and \code{\link{performQuantileRegressionTest}()}, which refuses
a weighted \code{lm}, and \code{\link{performVarianceFormTest}()}, which refuses weights that did not come from
\code{\link{fitWLS}()}, refuse it with the same message. With a zero prior weight it is refused as the \code{lm} with a
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
any order; until 0.16.0 \code{\link{performPesaranTest}()}, \code{\link{performBPRandomEffectsTest}()} and
\code{\link{performScatterDiagnostic}()} read \code{data} by position, and refused such data in another order than
the fit. \code{tests/testthat/test-data-alignment-guard.R} checks these refusals.

Since 0.16.0 \code{\link{performWhiteTest}()}, \code{\link{performBPTest}()}, \code{\link{performKoenkerTest}()},
their streaming forms, \code{\link{performWhiteTestStreaming}()}, \code{\link{performBPTestStreaming}()} and
\code{\link{performKoenkerTestStreaming}()}, which read the design of the fit chunk by chunk where each chunk built
its own from the rows of \code{data}, \code{\link{performRankPermutationTest}()}, \code{\link{performHighDimensionalTest}()}, the studentized
Breusch-Pagan statistic (\code{\link{performStudentizedBPTest}()}, \code{\link{performBPTestRobust}()} with
\code{studentize = TRUE}, the registered \code{"student_bp"}), \code{\link{performRESETTest}()},
\code{\link{performGQTest}()}, \code{\link{performQuantileRegressionTest}()}, \code{\link{performGlejserTest}()},
\code{\link{performParkTest}()}, \code{\link{performSzroeterTest}()} with the registered \code{"szroeter"},
\code{\link{performHarrisonMcCabeTest}()}, \code{\link{performHartleyFmaxTest}()},
\code{\link{performSpatialHeteroTest}()}, the five group tests, \code{\link{performLeveneTest}()},
\code{\link{performBrownForsytheTest}()}, \code{\link{performFlignerKilleenTest}()},
\code{\link{performBartlettTest}()} and \code{\link{performOBrienTest}()}, the two panel tests,
\code{\link{performPesaranTest}()} and \code{\link{performBPRandomEffectsTest}()}, and so
\code{\link{runPanelTests}()}, and \code{\link{performScatterDiagnostic}()}, and the tests computed from the model
alone, \code{\link{performSpearmanTest}()}, \code{\link{performHarveyTest}()}, \code{\link{performArchLMTest}()},
\code{\link{performMcLeodLiTest}()}, \code{\link{performCookWeisbergTest}()} and \code{\link{performNCVTest}()},
\code{\link{performSpreadLevelTest}()}, \code{\link{performDavidianCarrollTest}()} and
\code{\link{performVarianceFormTest}()}, with \code{\link{fitWLS}()}, read the response, the design, the
residuals, the weights and the offset from the fit, through the model context of the package, and read from
\code{data} only the rows the comparison above confirmed; the other tests follow in later steps (the design page of
the site, \url{https://diogoribeiro7.github.io/heteroTests/design/model-context/}, sets out the plan). RESET, the
tests computed from the model alone and \code{fitWLS()} without \code{var_formula} read nothing from \code{data},
and match it as the other tests do, so that every test takes and refuses the same data; until 0.16.0 they gave the
value of the fit on numbered data in another order, rows in front of those of the fit and repeated row names, and
refused numbered data in the order of the fit whose row numbers are not the fit's (\code{fitWLS()} ignored
\code{data}). A second formula, the \code{var_formula} of the Cook-Weisberg test, of the
variance form test and of \code{fitWLS()}, and the \code{against} of the variance form test, is evaluated on the
confirmed rows of \code{data}, or, without \code{data}, on the model frame; a variable that neither holds is refused,
where it was found in the workspace.
The Goldfeld-Quandt, Szroeter and Harrison-McCabe tests read their \code{order_by}, Glejser's and Park's their
\code{variable} and Hartley's and the group tests their \code{group} from the confirmed rows of \code{data}, or,
without \code{data}, from the model frame when it holds the variable under its own name ("The following variables
are missing from the model frame of \code{model}" otherwise). Hartley's test refuses a group that is missing at an
observation of the fit; the group tests leave such observations out, with the warning "Removed N observations due
to missing values in ...", as before 0.16.0. The panel tests read their \code{id} and \code{time}, and the scatter
diagnostic its \code{vars}, from the confirmed rows of \code{data}; without \code{data} the panel tests read them from
the model frame, when it holds them under their own names, and the scatter diagnostic needs \code{data}. Pesaran's
test refuses a pair of \code{id} and \code{time} that holds two observations of the fit and a unit or a period
missing at an observation of the fit, and computes the statistic of an unbalanced panel, whose units are not all
observed at the same periods, from the periods each pair of units shares, as \code{plm::pcdtest()} does; a fit with
\code{na.action = na.exclude} is read by the three as before 0.16.0, by position, until a later step, and Pesaran's
test refuses there an unbalanced panel of the rows of \code{data} ("needs a balanced panel"). The registered \code{"szroeter"} orders by the fitted values, which it computes
from the fit, and the spatial test reads no column of \code{data}: its \code{listw} lists the observations of the
fit in its order. Harrison-McCabe takes the rank of the fit, decided at the fit's tolerance (\code{1e-7} for an
\code{lm}, \code{1e-11} for a \code{glm}). These tests
match numbered rows by the row numbers of the fit, or, when \code{data} holds one row per observation and its numbers
are not those of the fit, in the order of the fit, once those rows reproduce the model frame; they drop nothing
before the match, so data that hold the rows a fit with \code{na.omit} dropped, or only the rows it kept, numbered
again, are accepted. Named data in any order, numbered data in the order of the fit: numbered data in another order
are refused, as by the other tests. They need no \code{data} for a model that transforms its variables, has an
\code{offset()} term, or takes its weights or offset from a column of the data, and accept its model frame as
\code{data}. They also refuse a multivariate \code{lm} ("needs a model with one response"), a fit whose coefficients,
residuals or weights were changed after it was fitted ("does not reproduce its residuals from its design and
coefficients", "The weights of the model are not those its coefficients were fitted with"), a fit whose terms do
not rebuild the design of its coefficients, as a \code{fitWLS()} fit of an \code{mgcv::gam()} fit with smooth terms
(the first of these messages, since 0.17.0), and a fit with
\code{model = FALSE} whose model frame can no longer be rebuilt, unless \code{data} gives its response and fitted
values, on which it is then tested. RESET,
Goldfeld-Quandt, the quantile regression test and the studentized Breusch-Pagan statistic refuse an
\code{mgcv::gam()} fit with smooth terms, which are usually penalised, so that the fit is not the least-squares
fit of its design, until a later step settles which tests take it; the others test it as before 0.16.0. The
residuals of a \code{gam} carry no names, and these tests match \code{data} to its observations by the row names of
its model frame; the tests that are not built on the context yet and read a column of \code{data} match them by
position. The check of an edited fit is a guard against accidental edits, not a proof
that a fit is unedited: an edited coefficient leaves the residuals unchanged, so it is seen only at \code{1e-7} of
the largest term of the response, the design times the coefficients and the residuals, and an edit of 0.5 goes
undetected once the response is above about \code{1e7}. These refusals have class \code{heteroTests_model_context_error}, which
\code{\link{runHeteroTests}()} and \code{\link{runDiagnostics}()} keep, as they keep that of a refusal of
\code{data}, without running another test under the name of the one refused. A test that reads a column of
\code{data} the model frame does not hold, as the rank permutation, Goldfeld-Quandt, Szroeter and
Harrison-McCabe tests do with their \code{order_by}, Glejser's and Park's with their \code{variable}, Hartley's
and the group tests with their \code{group}, the panel tests with their \code{id} and \code{time} and the scatter
diagnostic with its \code{vars}, refuses numbered rows taken in the order of the fit if two rows of the
fit tie on every
compared column. On a call with more than one defect, the first one
reported may differ from before 0.16.0, since the rows of \code{data} are now matched before the checks of the model.
\code{tests/testthat/test-model-context.R} checks them.
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
  positive weight. Keeping that exception was decided on 2026-10-17.
  \item An offset, given as \code{offset = o} or as an \code{offset()} term: the residuals of the fit include it, and
  the tests that read them account for it. Four left it out of a refit and returned a value that was not that of the
  model; they refused it in the development of 0.16.0 (decided 2026-10-17), and since the offset fix of 0.16.0 they
  read the offset from the fit and carry it in every refit: \code{\link{performGQTest}()},
  \code{\link{performQuantileRegressionTest}()} and the studentized Breusch-Pagan statistic
  (\code{\link{performStudentizedBPTest}()}, \code{\link{performBPTestRobust}()} with \code{studentize = TRUE}, the
  registered \code{"student_bp"}) give the value of the model fitted to the response minus the offset, and
  \code{\link{performRESETTest}()} the nested F test of the model with its offset against the model augmented with the
  powers of its fitted values, \eqn{X\hat\beta + o}{X b + o} (decided 2026-10-18).
  \item A \code{glm} with the Gaussian family and the identity link is a least-squares fit. With prior weights
  \eqn{w_i}{w_i} it is fitted by maximum likelihood, which for normal errors of variance \eqn{\phi / w_i}{phi / w_i}
  maximises \eqn{-\sum_i w_i (y_i - x_i^\top \beta)^2 / (2\phi)}{-sum(w_i (y_i - x_i'beta)^2) / (2 phi)}: weighted least
  squares. Iteratively reweighted least squares, the algorithm of \code{glm()}, then has the working response \eqn{y_i}
  and the working weights \eqn{w_i} whatever the current estimate, so its first step reaches the solution of
  \code{lm(..., weights = w)} (Nelder and Wedderburn, 1972; McCullagh and Nelder, 1989, sections 2.5 and 3.6). It is
  Fisher scoring, and for the identity link, the canonical link of the Gaussian family, the information does not
  depend on \eqn{y}, so scoring is Newton-Raphson, which converges in one step when the log-likelihood is quadratic in
  \eqn{\beta}{beta} (Green, 1984, section 1.2). Its design, coefficients, fitted values,
  residuals and weights are those of the \code{lm}, up to rounding, so a test whose statistic and null distribution
  are functions of those, under the assumptions of the linear model, which the \code{glm} states, gives the result of
  the \code{lm}: every \code{perform*Test()} function does, weighted or not (\file{tests/testthat/test-input-validation.R}
  and \file{test-model-eligibility.R} compare them). That holds when the two decide the rank of the design alike:
  \code{lm()} aliases a column with the tolerance \code{1e-7} of its QR decomposition, \code{glm.fit()} with
  \code{min(1e-7, epsilon / 1000)}, \code{1e-11} by default, so on a column within that margin of a combination of the
  others, such as \code{x3 = x1 + 1e-9 * rnorm(n)}, the \code{glm} keeps it and gets the values of
  \code{lm(..., tol = 1e-11)}, not those of the default \code{lm}. \code{\link{performWhiteTestStreaming}()},
  \code{\link{performHarrisonMcCabeTest}()} and \code{\link{performVarianceFormTest}()} refused every \code{glm} until
  0.16.0, and \code{\link{fitWLS}()} refits it as that \code{lm} since then. \code{\link{bootstrapTestStatistic}()}
  refused every \code{glm} until 0.17.0, as it refits each replicate by least squares, which would drop a family and
  a link; it bootstraps a Gaussian \code{glm} with the identity link as that \code{lm}, with its prior weights, and
  refuses any other \code{glm}, and an \code{mgcv::gam()} fit, as before.
  The checks of the model read its R-squared,
  which \code{summary()} of a \code{glm} does not report, as that of the \code{lm}: \code{summary.lm()} applied to the
  \code{glm}, whose fitted values, residuals, weights and offset are those of the \code{lm}, so that a near-perfect fit
  is refused as the \code{lm} is, since 0.16.0. The \emph{Supported
  models} page of the documentation site sets out the argument; the references are below, and the section numbers
  of McCullagh and Nelder (1989) are those of its table of contents. The tests read its prior weights,
  \code{model$prior.weights}, and never \code{model$weights}, which holds the working weights of the last iteration of
  iteratively reweighted least squares, \eqn{w_i (d\mu_i/d\eta_i)^2 / V(\mu_i)}{w_i (dmu_i/deta_i)^2 / V(mu_i)}. For
  this family and link the two are equal up to rounding; for any other they depend on the fitted means. A
  \code{glm} fitted with weights that are all 1 has the \code{prior.weights} of one fitted without weights, so the
  tests take it for an unweighted fit, where \code{lm(..., weights = rep(1, n))} is a weighted one:
  \code{\link{performVarianceFormTest}()} and \code{\link{performQuantileRegressionTest}()}, which refuse a weighted fit,
  accept the \code{glm} and refuse the \code{lm}.
  \item Any other \code{glm}, such as a binomial or a Poisson one, is refused: its residuals have a variance that
  depends on the mean by construction, so a test of constant variance tests nothing the help pages describe.
  \item A \code{survey::svyglm()} fit is refused: its residuals are scaled by the sampling weights, which are not
  precision weights. See \code{\link{runSurveyHeteroTests}()}.
  \item Panel models: \code{\link{performPesaranTest}()} and \code{\link{performBPRandomEffectsTest}()}, and so
  \code{\link{runPanelTests}()}, take a pooled \code{lm} and the columns of \code{data} that name the units and the
  periods. A \code{plm::plm()} fit is not an \code{lm}: every test refuses it, as it refuses anything but an
  \code{lm} or a \code{glm}, with one message, which names the class received and points to the pooled
  \code{lm()}.
  \item A rank-deficient fit: a term that is a linear combination of others, or the dummy of a cell of an interaction
  that no observation falls in, leaves an \code{NA} coefficient. Its residuals and fitted values are the projection on
  the column space of the design, which the aliased columns do not change. Since 0.16.0 every test that accepts the
  model without the aliased columns accepts it, computed on the estimable part of the design: the columns that were
  not aliased, with degrees of freedom from the rank, so that it gives the statistic and p-value of the model fitted
  without them. A test that refuses that model refuses the rank-deficient one with the same message. The model
  without them is the one \code{lm()} keeps: its decomposition keeps a column unless it is a combination of the columns
  before it, so of an aliased set the column that comes last, in the order of the terms, is left out. Most statistics
  depend only on the space the columns span, and are the same whichever column is left out. Two depend on which:
  \code{\link{performWhiteTest}()}, and its robust, streaming and bootstrap forms, when the cross products are left out
  (\code{cross_products = FALSE}, \code{method = "reduced"}, or more regressors than \code{max_interactions}), as the
  squares are those of the columns kept; and \code{\link{performHighDimensionalTest}()} when it keeps fewer
  principal components than columns. With \code{form = "power"}, \code{\link{performVarianceFormTest}()} takes the
  logarithms of every column of the design, the aliased ones included, so its value is not that of the model without
  them (and does not depend on the order of the terms); with the exponential form, the default, it is.
  \item A \code{fitWLS()} fit is a weighted \code{lm}, with estimated weights: accepted, but the reference
  distributions take the weights as known (\code{\link{performVarianceFormTest}()} accounts for the estimation).
  \item A \code{MASS::rlm()} fit is an \code{lm} by class, so the tests accept it, as before 0.16.0, and test its
  M-estimation residuals as if they were least-squares ones; its case weights, all 1 unless given, are read as
  precision weights, so \code{\link{performQuantileRegressionTest}()} refuses it as a weighted fit. Whether to keep
  accepting it or refuse it was deferred on 2026-10-17 to the work on the model context.
}
\code{\link{checkModel}()} checks the class of a model only, \code{lm} or \code{glm}; it does not check whether a test
accepts it.
}
\references{
Green, P. J. (1984). Iteratively reweighted least squares for maximum likelihood estimation, and some robust and
resistant alternatives. \emph{Journal of the Royal Statistical Society, Series B, 46}(2), 149--192.
\doi{10.1111/j.2517-6161.1984.tb01288.x}

McCullagh, P., & Nelder, J. A. (1989). \emph{Generalized Linear Models} (2nd ed.). Chapman & Hall. The section numbers
cited are those of its table of contents.

Nelder, J. A., & Wedderburn, R. W. M. (1972). Generalized linear models. \emph{Journal of the Royal Statistical
Society, Series A, 135}(3), 370--384. \doi{10.2307/2344614}
}
\seealso{
\code{\link{runHeteroTests}}, \code{\link[stats]{t.test}} for the \code{htest} class
}
