#!/usr/bin/env Rscript

# Identity harness of the model context (docs/design/model-context.md, "How
# each increment shows identical()").
#
# It runs a corpus of calls on two versions of the package, each in its own R
# process, and lists, call by call, whether the two results are identical().
# A result that differs must be one of the defects the increment closes, in
# the allow-list below; any other difference is unexpected. An allowed change
# to a value must also be verified against a reference, the value of the call
# the oracles of tests/testthat/test-model-context.R relate it to (see
# identity_verify()); one that is not verified is unexpected too. No value is
# stored in the repository: each run writes an .rds file where it is told.
#
# The corpus:
#   - every recipe of tests/testthat/helper-test-recipes.R (every exported
#     perform*Test() but performBoxMTest(), which tests data, not a model),
#     and the recipes of `identity_extra_recipes` below, under set.seed(7), on
#     every cell of the matrix of tests/testthat/helper-model-context.R;
#     among them (increment 5) the scatter diagnostic and runPanelTests() with
#     its options, which are not registered, as the panel tests are not;
#   - the default battery of runHeteroTests() on every cell, and the
#     registered names of increments 2 and 3 (`identity_registry_tests`,
#     `identity_registry_runner_szroeter`, `identity_registry_runner_spatial`);
#   - (increment 7) the streaming tests in chunks of 7 and of 40 besides those
#     of 10,000 of their recipes, and the runner with the streaming tests
#     asked for, `chunk_threshold_mb = 0`, in chunks of 10,000 and of 7
#     (`identity_streaming_chunked`, `identity_streaming_runners`);
#   - the cells of Harrison-McCabe's rank (N4, `identity_rank_cells()`), with
#     every recipe;
#   - the examples of the help pages of the functions built on the context.
#
# Decision 1: on a fit with a poly() term, the value of a function built on
# the context equals, with identical(), what main computes on the fit's
# stored design; against main's rebuilt design, which computes poly() again
# through its predvars, the differences are in the last bits (measured up to
# 1.3e-15 relative on the statistic). Each run therefore also records, for
# those cells, the calls that may change, with main given the stored design:
# the model is refitted with `x = TRUE`, for which model.matrix(model, data)
# returns the stored design, and the five functions built on the context are
# traced so that a model they are given without it, as the refits of a
# bootstrap are, gets its own (`model$x`) first. `compare` checks the branch
# against that value; a poly() cell that is not identical to it is
# unexpected. Where runHeteroTests() falls back on another test on the stored
# design too, as without `data` on a poly() fit before the context, main has
# no stored-design value of the test it names, and those tests are judged by
# the rules of the runner below. Nor has main one of Goldfeld-Quandt on an
# unweighted fit, whose segments it refitted from the formula, computing the
# poly() basis again on the rows of each segment (N1): the basis spans the
# same columns, and the branch must equal main to `identity_last_bits`
# relative on every number.
#
# N2, outside the context: White's degrees of freedom, the rank of its
# auxiliary regression with the intercept less one, decided since its
# follow-up on the design of the centred regressors, with the refusal of a
# saturated auxiliary regression. Each run also records White's test written
# out on every cell (identity_white_by_hand()) and runs the cells of the
# follow-up (identity_white_cells()); a change of White's test is allowed only
# on a cell where the two versions count differently, and only when the
# branch gives that reference, with every other number of its result main's
# or computed from it (identity_n2_check()).
#
# Szroeter's null moments, outside the context ("szroeter_moments"): since
# 0.16.0 performSzroeterTest() standardises h with its null mean and variance
# given the design, where it used (n + 1) / 2 and (n^2 - 1) / (6 n), so every
# Q and p-value of the Szroeter calls changes. Each run traces .szroeter_test()
# and records, for each call that reaches it, h, the moments and Q computed
# without the package from what the test read: the design of the fit without
# its aliased columns, times sqrt(w), in the order of the test, and the
# Pearson residuals (identity_szroeter_by_hand()). A change of a Szroeter call
# is allowed only when every field of the result but the statistic and the
# p-value is identical() to main's, the estimate h included, the two runs
# computed the same reference, and the branch's Q and p-value are those of the
# reference (identity_szroeter_verdict()).
#
# N12, Pesaran's test on an unbalanced panel: the fifth step refused such a
# panel (decision 28), and since the pull request of N12 the test computes its
# statistic from the periods each pair of units shares, as plm::pcdtest()
# does. Each run also records plm::pcdtest() on the Pearson residuals of every
# unbalanced cell whose data carry the names of the residuals
# (identity_plm_cd()); a call of Pesaran's test or of runPanelTests() that
# main refused on such a cell and the branch computes is allowed only when it
# gives plm's statistic and p-value to 1e-10 relative, on the cell or on the
# cell it reproduces, which gives plm's in turn.
#
# N3, outside the context, with the scale of the squared residuals: since
# 0.17.0 the three streaming tests decide the rank of their auxiliary
# regression as performWhiteTest() does, so that White's streaming test drops
# collinear terms where it stopped, and compare the variation of the squared
# residuals with their scale. A change of a value of a streaming test, of the
# runner that streams them or of their help pages is allowed when every
# number is main's to `identity_last_bits`, or when it gives the value of the
# exact test of the same call, nearer to it than main's; an error of those
# kinds that becomes a value, when it gives the value of the exact test
# (identity_n3_verdict()). Each run also records the exact test of the
# examples of the streaming tests.
#
# The data.name of the runner, outside the context: since 0.16.0 a test that
# runHeteroTests() runs and that reported "model" reports the formula of the
# model. Each run records the formula of every cell; a runner's data.name may
# change only from "model" to it (identity_data_names()), and is otherwise
# set aside.
#
# Usage, from the root of the checkout whose corpus is run:
#
#   # main, installed into a private library, never the default one
#   R CMD INSTALL --library=<private library> <checkout of main>
#   Rscript scripts/model-context-identity.R run <private library> main.rds
#   # the checkout, through pkgload::load_all()
#   Rscript scripts/model-context-identity.R run source branch.rds
#   # the list, and the counts
#   Rscript scripts/model-context-identity.R compare main.rds branch.rds [calls.csv]
#
# `compare` exits with status 1 when a difference is unexpected.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L || !args[1L] %in% c("run", "compare")) {
  stop(
    "Usage: model-context-identity.R run <library|source> <out.rds>\n",
    "       model-context-identity.R compare <main.rds> <branch.rds> [calls.csv]",
    call. = FALSE
  )
}

# --- the functions built on the context, and the defects the increments close -

# The registered names of the tests built on the context in increment 2 that
# runHeteroTests() runs when asked: the studentized Breusch-Pagan statistic
# and the quantile regression test. Its runner record runs them on every cell.
identity_registry_tests <- c("student_bp", "quantile_regression")
identity_registry_runner <- "runHeteroTests(tests = c(\"student_bp\", \"quantile_regression\"))"
# Increment 3: the registered Szroeter test, which orders by the fitted
# values, on every cell, and the spatial test, which reads its weights from
# `attr(data, "listw")` (identity_with_listw()), on every cell with `data`:
# without `data` the runner gives it the model frame, which has no weights.
identity_registry_runner_szroeter <- "runHeteroTests(tests = \"szroeter\")"
identity_registry_runner_spatial <- "runHeteroTests(tests = \"spatial_hetero\")"
identity_registry_runner3 <- c(identity_registry_runner_szroeter, identity_registry_runner_spatial)
# Increment 6: the registered tests computed from the model alone that the
# runner runs when asked, on every cell. The registered "ncv", a fallback of
# White's, Koenker's and the Breusch-Pagan tests, is called directly
# (identity_extra_recipes), since the runner warns that its name is deprecated.
identity_registry_tests6 <- c("cook_weisberg", "spread_level", "variance_form")
identity_registry_runner6 <-
  "runHeteroTests(tests = c(\"cook_weisberg\", \"spread_level\", \"variance_form\"))"

# The spatial weights of the recipe of performSpatialHeteroTest()
# (helper-test-recipes.R), one per observation of the fit, attached to `data`
# for the registered spatial test.
identity_listw <- function(m) {
  set.seed(1)
  coords <- cbind(stats::runif(stats::nobs(m)), stats::runif(stats::nobs(m)))
  suppressWarnings(spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(coords, k = 4))))
}
identity_with_listw <- function(m, d) {
  testthat::skip_if_not_installed("spdep")
  if (!is.null(d)) {
    attr(d, "listw") <- identity_listw(m)
  }
  d
}

# Recipes beyond those of helper-test-recipes.R: the calls that compute a
# statistic through the functions built on the context. Increment 1: the
# robust Breusch-Pagan test without studentizing and the bootstraps of
# Koenker's and White's tests. Increment 2: the routes of the studentized
# Breusch-Pagan statistic (the registry, test_factory, the comparison with
# lmtest::bptest(), the robust test's bootstrap), of the quantile regression
# test (the registry, test_factory) and of RESET (runDiagnostics()).
identity_extra_recipes <- list(
  `performBPTestRobust(studentize = FALSE)` = function(m, d) {
    performBPTestRobust(m, d, studentize = FALSE)
  },
  `bootstrapTestStatistic(performKoenkerTest)` = function(m, d) {
    bootstrapTestStatistic(performKoenkerTest, m, d, B = 19, progress = FALSE)
  },
  `performWhiteTestRobust(bootstrap = TRUE)` = function(m, d) {
    performWhiteTestRobust(m, d, bootstrap = TRUE, B = 19)
  },
  `registry student_bp` = function(m, d) {
    get("student_bp", envir = heteroTests:::.diagnostic_registry)(m, d)
  },
  `test_factory student_bp` = function(m, d) test_factory$run_test("student_bp", m, d),
  `rvalidate_against_reference(studentized_bp)` = function(m, d) {
    testthat::skip_if_not_installed("lmtest")
    suppressWarnings(rvalidate_against_reference("studentized_bp", m, d))
  },
  `performBPTestRobust(bootstrap = TRUE)` = function(m, d) {
    performBPTestRobust(m, d, bootstrap = TRUE, B = 19)
  },
  `registry quantile_regression` = function(m, d) {
    testthat::skip_if_not_installed("quantreg")
    get("quantile_regression", envir = heteroTests:::.diagnostic_registry)(m, d)
  },
  `test_factory quantile_regression` = function(m, d) {
    testthat::skip_if_not_installed("quantreg")
    test_factory$run_test("quantile_regression", m, d)
  },
  `runDiagnostics()$reset` = function(m, d) {
    runDiagnostics(m, d, use_cache = FALSE, progress = FALSE)$reset
  },
  # Increment 3: the registered Szroeter test and its test_factory entry,
  # which calls performSzroeterTest() without an ordering (N13, outside the
  # context), and the registered spatial test with its test_factory entry.
  `registry szroeter` = function(m, d) {
    get("szroeter", envir = heteroTests:::.diagnostic_registry)(m, d)
  },
  `test_factory szroeter` = function(m, d) test_factory$run_test("szroeter", m, d),
  `registry spatial_hetero` = function(m, d) {
    d <- identity_with_listw(m, d)
    set.seed(7)
    get("spatial_hetero", envir = heteroTests:::.diagnostic_registry)(m, d)
  },
  `test_factory spatial_hetero` = function(m, d) {
    d <- identity_with_listw(m, d)
    set.seed(7)
    test_factory$run_test("spatial_hetero", m, d)
  },
  # Increment 5: the scatter diagnostic, which returns a correlation for each
  # variable, as in the matrix (context_recipes() of helper-model-context.R),
  # and runPanelTests() with both of its tests, with the random-effects test
  # alone, without `time`, and with Pesaran's test alone.
  performScatterDiagnostic = function(m, d) performScatterDiagnostic(m, d, c("x1", "x2")),
  runPanelTests = function(m, d) runPanelTests(m, d, id = "id", time = "time"),
  `runPanelTests(tests = "bp_random")` = function(m, d) {
    runPanelTests(m, d, id = "id", tests = "bp_random")
  },
  `runPanelTests(tests = "pesaran")` = function(m, d) {
    runPanelTests(m, d, id = "id", time = "time", tests = "pesaran")
  },
  # Increment 6: fitWLS(), which returns a model, as the vector of its
  # coefficients and estimated weights (identity_wls_numbers()), without and
  # with a variance formula; the tests that evaluate a second formula, with a
  # variable that no model frame of the matrix holds, z, and with x1, which
  # the frames of the models that transform it lack; the registered names of
  # the tests computed from the model alone, and runTimeSeriesTests(), which
  # runs the two time-series tests without `data`. And R3: a variable of the
  # workspace, `zw` (identity_workspace_z()), which the formula found in its
  # environment when neither `data` nor the model frame held it.
  fitWLS = function(m, d) identity_wls_numbers(fitWLS(m, d)),
  `fitWLS(var_formula = ~ z)` = function(m, d) {
    identity_wls_numbers(fitWLS(m, d, var_formula = ~ z))
  },
  `fitWLS(var_formula = ~ x1, form = "power")` = function(m, d) {
    identity_wls_numbers(fitWLS(m, d, var_formula = ~ x1, form = "power"))
  },
  `performCookWeisbergTest(var_formula = ~ z)` = function(m, d) {
    performCookWeisbergTest(m, d, var_formula = ~ z)
  },
  `performNCVTest(var_formula = ~ z)` = function(m, d) performNCVTest(m, d, var_formula = ~ z),
  `performVarianceFormTest(var_formula = ~ z)` = function(m, d) {
    performVarianceFormTest(m, d, var_formula = ~ z)
  },
  `performVarianceFormTest(against = ~ z)` = function(m, d) {
    performVarianceFormTest(m, d, against = ~ z)
  },
  `registry cook_weisberg` = function(m, d) {
    get("cook_weisberg", envir = heteroTests:::.diagnostic_registry)(m, d)
  },
  `registry ncv` = function(m, d) get("ncv", envir = heteroTests:::.diagnostic_registry)(m, d),
  `registry spread_level` = function(m, d) {
    get("spread_level", envir = heteroTests:::.diagnostic_registry)(m, d)
  },
  `registry variance_form` = function(m, d) {
    get("variance_form", envir = heteroTests:::.diagnostic_registry)(m, d)
  },
  runTimeSeriesTests = function(m, d) runTimeSeriesTests(m, lags = 2),
  `fitWLS(var_formula = ~ zw)` = function(m, d) {
    zw <- identity_workspace_z(m)
    identity_wls_numbers(fitWLS(m, d, var_formula = ~ zw))
  },
  `performCookWeisbergTest(var_formula = ~ zw)` = function(m, d) {
    zw <- identity_workspace_z(m)
    performCookWeisbergTest(m, d, var_formula = ~ zw)
  },
  `performVarianceFormTest(var_formula = ~ zw)` = function(m, d) {
    zw <- identity_workspace_z(m)
    performVarianceFormTest(m, d, var_formula = ~ zw)
  },
  # From the review of increment 6: a constant of the workspace, `k`, or of
  # base R, `pi`, is not a variable (rformula_constants()), and is evaluated
  # as R evaluates it; Cook-Weisberg's test with `k` is verified against the
  # same formula written with 2.
  `performCookWeisbergTest(var_formula = ~ I(x1^k))` = function(m, d) {
    k <- 2
    performCookWeisbergTest(m, d, var_formula = ~ I(x1^k))
  },
  `performCookWeisbergTest(var_formula = ~ I(x1^2))` = function(m, d) {
    performCookWeisbergTest(m, d, var_formula = ~ I(x1^2))
  },
  `performVarianceFormTest(var_formula = ~ I(x1^k))` = function(m, d) {
    k <- 2
    performVarianceFormTest(m, d, var_formula = ~ I(x1^k))
  },
  `fitWLS(var_formula = ~ I(pi * x1))` = function(m, d) {
    identity_wls_numbers(fitWLS(m, d, var_formula = ~ I(pi * x1)))
  }
)
# The numbers of a fitWLS() fit: its coefficients and its estimated weights.
identity_wls_numbers <- function(fit) c(stats::coef(fit), unname(stats::weights(fit)))
# A variable of the workspace with one value per observation of the fit.
identity_workspace_z <- function(m) 2 + sin(seq_len(NROW(m$residuals)))
# The functions whose values may change: those built on the context, the
# deprecated name and the wrappers and routes that compute their statistic
# through them, and the runner, whose default battery is White's and
# Koenker's tests. performWhiteTestBootstrap() is not among them: it keeps the
# computation of 0.15.0 until increment 8 (rwhite_test_rebuilt_design()), so
# any change of it is unexpected.
identity_affected <- c(
  "performWhiteTest", "performBPTest", "performBreuschPaganTest", "performKoenkerTest",
  "performRankPermutationTest", "performHighDimensionalTest", "performWhiteTestRobust",
  "performStudentizedBPTest", "performBPTestRobust", "performRESETTest", "performGQTest",
  "performQuantileRegressionTest",
  "performGlejserTest", "performParkTest", "performSzroeterTest", "performHarrisonMcCabeTest",
  "performHartleyFmaxTest", "performSpatialHeteroTest",
  "performLeveneTest", "performBrownForsytheTest", "performFlignerKilleenTest",
  "performBartlettTest", "performOBrienTest",
  "performPesaranTest", "performBPRandomEffectsTest",
  "performSpearmanTest", "performHarveyTest", "performArchLMTest", "performMcLeodLiTest",
  "performCookWeisbergTest", "performNCVTest", "performSpreadLevelTest",
  "performDavidianCarrollTest", "performVarianceFormTest",
  names(identity_extra_recipes), "runHeteroTests", identity_registry_runner,
  identity_registry_runner3, identity_registry_runner6
)
# The bootstrap recipes, whose replicates refit the formula until increment 8.
identity_bootstraps <- c(
  "bootstrapTestStatistic(performKoenkerTest)", "performWhiteTestRobust(bootstrap = TRUE)",
  "performBPTestRobust(bootstrap = TRUE)"
)
# The calls of increment 2, by the test they compute.
identity_studentized <- c(
  "performStudentizedBPTest", "performBPTestRobust", "registry student_bp",
  "test_factory student_bp", "rvalidate_against_reference(studentized_bp)",
  "performBPTestRobust(bootstrap = TRUE)"
)
identity_quantile <- c(
  "performQuantileRegressionTest", "registry quantile_regression",
  "test_factory quantile_regression"
)
identity_reset <- c("performRESETTest", "runDiagnostics()$reset")
identity_increment2 <- c(
  identity_studentized, identity_quantile, identity_reset, "performGQTest",
  identity_registry_runner
)
# The calls of increment 3: its six tests, the registered Szroeter and spatial
# tests with their test_factory entries, and the runner that runs the two.
# `identity_reads_data` are those that read a column of `data` besides the
# model: the variable Glejser's and Park's tests regress on, the ordering of
# Szroeter's and Harrison-McCabe's, the groups of Hartley's.
identity_reads_data <- c(
  "performGlejserTest", "performParkTest", "performSzroeterTest", "performHarrisonMcCabeTest",
  "performHartleyFmaxTest"
)
identity_increment3 <- c(
  identity_reads_data, "performSpatialHeteroTest", "registry szroeter", "test_factory szroeter",
  "registry spatial_hetero", "test_factory spatial_hetero", identity_registry_runner3
)
# The calls of Szroeter's test, whose null moments come from the design since
# 0.16.0 ("szroeter_moments"): the function, with its help examples (records
# of kind "example" under its name), the registered test, its test_factory
# entry and the runner that runs it.
identity_szroeter_calls <- c(
  "performSzroeterTest", "registry szroeter", "test_factory szroeter",
  identity_registry_runner_szroeter
)
# The calls of increment 4: the five group tests, which read the `group` of
# `data` besides the model. None of them is registered, in test_factory or in
# a runner, so their calls are the direct ones.
identity_increment4 <- c(
  "performLeveneTest", "performBrownForsytheTest", "performFlignerKilleenTest",
  "performBartlettTest", "performOBrienTest"
)
# The calls of increment 5: the two panel tests, the scatter diagnostic and
# runPanelTests() with its options. `identity_panel_tests` are the calls that
# run a panel test, `identity_pesaran` those that run Pesaran's, which reads
# `time` and refuses a repeated pair of `id` and `time` (R2), and
# `identity_panel_readers` the two tests, which read `id` and `time` from the
# model frame without `data` (M1); the scatter diagnostic and runPanelTests()
# need `data`.
identity_panel_runner_bp <- "runPanelTests(tests = \"bp_random\")"
identity_panel_runner_pesaran <- "runPanelTests(tests = \"pesaran\")"
identity_pesaran <- c("performPesaranTest", "runPanelTests", identity_panel_runner_pesaran)
identity_panel_tests <- c(
  identity_pesaran, "performBPRandomEffectsTest", identity_panel_runner_bp
)
identity_panel_readers <- c("performPesaranTest", "performBPRandomEffectsTest")
identity_increment5 <- c(identity_panel_tests, "performScatterDiagnostic")
# The calls of increment 6. `identity_model_alone`: the tests computed from
# the model alone, their registered names, runTimeSeriesTests() and fitWLS()
# without a variance formula, which read nothing from `data`, with RESET, which
# builds its context as they do since this increment; `identity_second_formula`:
# the calls that evaluate a second formula (`var_formula`, `against`) on the
# rows of `data`, or of the model frame; `identity_workspace`: those whose
# formula names a variable of the workspace (R3); and the runner with the
# registered tests.
identity_model_alone <- c(
  "performSpearmanTest", "performHarveyTest", "performArchLMTest", "performMcLeodLiTest",
  "performCookWeisbergTest", "performNCVTest", "performSpreadLevelTest",
  "performDavidianCarrollTest", "performVarianceFormTest", "registry cook_weisberg",
  "registry ncv", "registry spread_level", "registry variance_form", "runTimeSeriesTests",
  "fitWLS", identity_reset
)
identity_second_formula <- c(
  "fitWLS(var_formula = ~ z)", "fitWLS(var_formula = ~ x1, form = \"power\")",
  "performCookWeisbergTest(var_formula = ~ z)", "performNCVTest(var_formula = ~ z)",
  "performVarianceFormTest(var_formula = ~ z)", "performVarianceFormTest(against = ~ z)",
  "performCookWeisbergTest(var_formula = ~ I(x1^k))", "performCookWeisbergTest(var_formula = ~ I(x1^2))",
  "performVarianceFormTest(var_formula = ~ I(x1^k))", "fitWLS(var_formula = ~ I(pi * x1))"
)
# The call with a constant that Cook-Weisberg's test refused with `data`, as a
# variable `data` lacks ("constant"), and the call it must reproduce.
identity_constant <- "performCookWeisbergTest(var_formula = ~ I(x1^k))"
identity_workspace <- c(
  "fitWLS(var_formula = ~ zw)", "performCookWeisbergTest(var_formula = ~ zw)",
  "performVarianceFormTest(var_formula = ~ zw)"
)
identity_increment6 <- c(
  identity_model_alone, identity_second_formula, identity_workspace, identity_registry_runner6
)
# fitWLS() is not a test: it does not check that the fit reproduces itself
# (M8).
identity_increment6_tests <- setdiff(identity_increment6, grep("^fitWLS", identity_increment6,
                                                                value = TRUE))
# Decision 31 (decided by the author on 2026-10-26): the calls that read
# nothing from `data` and are given it, the tests computed from the model
# alone, their registered names, RESET and fitWLS() without a variance
# formula, and the runner with the registered tests, match `data` through the
# guard, as every test built on the context does. runTimeSeriesTests() and
# runDiagnostics(), which run their tests without `data`, are not among them.
identity_decision31 <- c(
  setdiff(identity_model_alone, c("runTimeSeriesTests", "runDiagnostics()$reset")),
  identity_registry_runner6
)
# The defects of the design whose cells may change, by the kind of change.
# Increment 1: B5, B12, B14, B15, L1, L2 and N7, which the context closes for
# every test built on it, and so for those of increment 2 too. Increment 2:
# B7, the offsets that four tests left out of a refit, refused until now; N1,
# the segments of Goldfeld-Quandt refitted from the formula, and the quantile
# regressions refitted from it; "frame", a fit with model = FALSE whose data
# are gone, whose model frame the context builds from `data`, and which it
# refuses without `data`; N6, the stale model frame RESET read, now refused;
# "null", `data = NULL` given to the registered function of "student_bp"
# (identity_null_routes, below); and "gam", an mgcv::gam() fit with smooth
# terms, which the tests of increment 2 refuse until increment 11 settles which
# tests take it (decided 2026-10-18). R4, a multivariate lm, which RESET tested
# without `data`, is refused by every test built on the context. Increment 3:
# N4, the rank of Harrison-McCabe's null distribution, decided at the
# tolerance of the fit (the cells of context_rank_cells()); N17, an
# mgcv::gam() fit, whose residuals carry no names and to which the tests that
# read a column of `data` matched `data` by position, given the rows of the
# fit in another order under their own names (approved on 2026-10-19); and
# "frame" and "null" for the tests and routes of increment 3. From the review
# of increment 3: "gam_offset", the registered Szroeter test on a gam with an
# `offset` argument, which predict.gam() leaves out: it ordered such a fit by
# X b alone, and orders it by its fitted values, X b + o (decided by the
# coordinator on 2026-10-20); and "frame" on a fit with model = FALSE whose
# data are gone, given numbered data in another order, which the tests of
# increment 3 used, and refuse. Increment 4: N17 and "frame" for the five
# group tests, which matched a gam by position too and used such data; and
# M8, a fit whose coefficients were changed after the fit, which the five
# tested and refuse, as every test built on the context does. Increment 5: B3
# and B4, the panel tests and the scatter diagnostic, which read `data` by
# position and refused data with row names of their own in another order
# than the fit, or rows after those of the fit; B16, the panel tests (and the
# scatter diagnostic) on a fit that dropped rows, whose data they refused for
# their number of rows; R2, a pair of `id` and `time` that holds two
# observations, which Pesaran's test computed with, and refuses (decision 13);
# N12, an unbalanced panel, on which Pesaran's test stopped or recycled the
# residuals of a unit and returned a wrong value, and which it refuses
# (decided by the author on 2026-10-24); and N17, "frame" and M8 for these.
# Increment 6: N6 for the tests computed from the model alone, fitWLS() and
# the variance form test, a model frame that model = FALSE rebuilt from data
# changed after the fit, which they read (Harvey's design, the refits of the
# variance form test and fitWLS()) or ignored, and refuse; B9, a second formula
# (`var_formula`, `against`) evaluated on rows of `data` matched by position,
# unchecked, now matched by the alignment, which refuses numbered data in
# another order, rows in front of those of the fit, repeated row names, and,
# for a variable beyond the model frame, data in the order of the fit whose
# rows tie (identity_defects()); R3, a variable of a second formula found in
# the workspace, refused (decision 9), and a variable that neither `data` nor
# the model frame holds, refused with M1 or the message of `data`; and
# "frame" and M8 for these. Decision 31: the calls of the tests that read
# nothing from `data` match it through the guard, which refuses numbered data
# in another order, rows in front of those of the fit and repeated row names,
# from which they computed the value of the fit, and takes numbered data with
# one row per observation in the order of the fit, and the data of a gam that
# dropped rows, which the check of 0.15.0 refused (identity_defects()).
# From the review of increment 6: "constant", a constant of the workspace in
# a second formula, which Cook-Weisberg's test refused with `data` as a
# variable `data` lacks, and evaluates as R does: an error that becomes the
# value of the same formula written with the number (identity_base_fn()).
# The pull request of N12: Pesaran's test computes the statistic of an
# unbalanced panel from the periods each pair of units shares, as
# plm::pcdtest() does, where the fifth step refused it, directly and through
# runPanelTests() (the scope of N12 below). Each of those errors that become
# values must give plm's value, directly or through the cell it reproduces
# (identity_plm_cd(), identity_compare()). N12 is no longer among the new
# refusals: a call of an unbalanced cell that started refusing is unexpected.
identity_allowed <- list(
  error_to_value = c(
    "B12", "L1", "L2", "B14", "B15", "N7", "B7", "N1", "frame", "null", "B3", "B4", "B16", "N17",
    "decision31", "constant", "N12"
  ),
  value_change = c("B5", "N1", "N4", "N17", "gam_offset"),
  value_to_error = c("N6", "gam", "R4", "frame", "M8", "R2", "B9", "R3", "decision31")
)
# The defects of increments 2 to 6 apply to the calls of the tests they
# concern only: on any other call such a change is unexpected.
identity_defect_scope <- list(
  B7 = identity_increment2,
  N1 = c("performGQTest", identity_quantile, identity_registry_runner),
  frame = c(identity_increment2, identity_increment3, identity_increment4, identity_increment5,
            identity_increment6),
  N6 = c(identity_reset, identity_increment6),
  null = c("registry student_bp", "registry szroeter"),
  gam = identity_increment2,
  N4 = "performHarrisonMcCabeTest",
  N17 = c(identity_reads_data, identity_increment4, identity_increment5, identity_second_formula,
          identity_workspace),
  gam_offset = c("registry szroeter", identity_registry_runner_szroeter),
  M8 = c(identity_increment4, identity_increment5, identity_increment6_tests),
  B3 = identity_panel_tests,
  B4 = "performScatterDiagnostic",
  B16 = identity_increment5,
  R2 = identity_pesaran,
  N12 = identity_pesaran,
  szroeter_moments = identity_szroeter_calls,
  B9 = c(identity_second_formula, identity_workspace),
  R3 = c(identity_second_formula, identity_workspace),
  decision31 = identity_decision31,
  constant = identity_constant
)
# A cut() term, whose replicates would compute other breaks than the stored
# design, is refused by the bootstraps (rdata_reproduces_fit()), as main
# refused it with another message: only that change is allowed for them. The
# robust Breusch-Pagan bootstrap with an offset, refused before, now runs, as
# it does on a fit with model = FALSE whose data are gone, whose base
# statistic stopped before.
identity_bootstrap_allowed <- list(
  error_to_value = c("B12", "L1", "L2", "B15", "B7", "frame")
)
# The registered function of "student_bp" called the implementation behind
# performStudentizedBPTest() directly, which read `data` as it was given and
# refused `data = NULL` ("Expected a data.frame for input data"), where every
# other route reads the model frame; built on the context, it reads the model
# frame too. On a cell without `data` its error may become a value, or the
# refusal of the context. The registered Szroeter test (increment 3) matched
# its fitted values to the row names of `data`, and refused `data = NULL`
# ("Missing rows"); it reads the model frame too.
identity_null_routes <- c("registry student_bp", "registry szroeter")
# A value of the runner that fell back on another test, or stopped, where the
# test it names now runs; and the registered Szroeter test on a gam with an
# `offset` argument, which the runner runs ("gam_offset").
identity_runner_allowed <- c(
  "B12", "L1", "L2", "B14", "B15", "N7", "B5", "B7", "N1", "frame", "gam_offset", "decision31"
)
# A message, of a test or of the runner that runs it, that changes only to the
# test's own documented refusal: M7, a model with more than one response
# (R4/N9); for an offset that was refused first (B7), the refusal the call
# meets next, of `data` by the guard or of the variables of `data` by the
# robust Breusch-Pagan test, which keeps its own preparation of the data until
# increment 8; for Goldfeld-Quandt ordered by a variable that the model frame
# does not hold under its own name (L1, L2), the message of the context (M1)
# or the variable alone; for a fit whose model frame cannot be rebuilt, the
# message of the context ("frame"); for `data = NULL` given to the registered
# function of "student_bp", which stopped on `NULL`, the refusal the test makes
# of the fit, read from its model frame: a stale frame, a zero weight, a
# multivariate lm, a smooth term ("null"). Increment 3: for a test that reads
# a column of `data` beyond the model frame, numbered data with one row per
# observation whose rows tie on every compared column are refused by the
# guard (the rule on ties), where they lacked rows of the fit before (B14).
# Increment 4: the same for the five group tests, and M8 for an edited fit
# without `data` or with its model frame, where the five lacked the group of
# `data` first. Increment 5: for the panel tests, runPanelTests() and the
# scatter diagnostic, the rule on ties (B14), the frame message and M8, as
# for the group tests; for Pesaran's test on an unbalanced panel (N12), as
# that of a fit that dropped some rows of some units, whose data it refused
# for their number of rows (B16) or on whose rows it stopped ("number of
# items to replace is not a multiple of replacement length"), the refusal of
# an unbalanced panel (decided by the author on 2026-10-24); and for
# Pesaran's test on a repeated pair of `id` and `time`, where it stopped on
# the unbalanced panel of a fit to repeated rows or refused data with row
# names in another order, the refusal of the pair (R2). Since the pull
# request of N12 that refusal is left to the positional reading of a fit with
# na.exclude that dropped rows, and only its message there, which names
# na.exclude, is allowed for B16 and N12 (`identity_n12_message`). Any other
# change of a message is unexpected, but those of identity_m1() and
# identity_m1_panel() below.
identity_m8 <- c(
  "does not reproduce its residuals from its design and coefficients",
  "not those its coefficients were fitted with"
)
identity_n12_message <- paste(
  "reads a fit with na.action = na.exclude by position, on a matrix of periods by units,",
  "and needs a balanced panel, every unit observed at the same periods"
)
identity_r2_message <- "needs one observation of the fit for each pair of `id` and `time`"
# Increment 6: the refusals of a second formula. The guard's, on rows of
# `data` it cannot match (B9), and of a variable that neither `data` nor the
# model frame holds (R3): M1 without `data`, the message of `data` with it.
identity_b9_message <- "cannot match the rows of `data`"
identity_r3_messages <- c(
  "The following variables are missing from the model frame of `model`",
  "The following variables are missing from input data"
)
identity_messages <- list(
  R4 = "needs a model with one response",
  B7 = c("cannot match the rows of `data`", "The following variables are missing from input data"),
  L1 = "The following variables are missing from the model frame of `model`: x1.",
  L2 = "The following variables are missing from input data: x1.",
  frame = "cannot use the model frame of `model`",
  null = c(
    "cannot use the model frame of `model`", "zero or non-finite weights",
    "needs a model with one response", "cannot test an mgcv::gam() fit with smooth terms"
  ),
  gam = "cannot test an mgcv::gam() fit with smooth terms",
  B14 = "tie on every column of the model frame that is compared",
  M8 = identity_m8,
  B16 = identity_n12_message,
  N12 = identity_n12_message,
  R2 = identity_r2_message,
  N6 = "cannot use the model frame of `model`",
  B9 = identity_b9_message,
  R3 = identity_r3_messages,
  # Decision 8: a variable of `var_formula` missing at an observation of the
  # fit, which Cook-Weisberg's test refused as a missing row, is named.
  decision8 = "the `var_formula` variable, is missing.",
  # Decision 31: data the guard refuses, where the check of 0.15.0 refused them
  # otherwise (repeated row names, which lacked a row of the fit).
  decision31 = identity_b9_message
)
identity_message_scope <- list(
  B7 = identity_increment2, L1 = c("performGQTest", identity_reads_data),
  L2 = c("performGQTest", identity_reads_data),
  frame = c(identity_increment2, identity_increment3, identity_increment4, identity_increment5,
            identity_increment6),
  null = c("registry student_bp", "registry szroeter"),
  gam = identity_increment2,
  B14 = c(identity_reads_data, identity_increment4, identity_increment5),
  M8 = c(identity_increment4, identity_increment5, identity_increment6_tests),
  B16 = identity_pesaran,
  N12 = identity_pesaran,
  R2 = identity_pesaran,
  N6 = identity_increment6,
  B9 = c(identity_second_formula, identity_workspace),
  R3 = c(identity_second_formula, identity_workspace),
  decision8 = identity_second_formula,
  decision31 = identity_decision31
)
# M1, for the tests that read a variable of `data` besides the model (the
# variable, ordering or groups they name): a call that stopped because `data`,
# or the model frame without `data`, lacked variables ("The following
# variables are missing from input data: o, group") now names only those
# among them that the test still lacks, from the model frame (M1) or from
# `data`. The model frame holds the terms of the model and is accepted as it
# is, so the variables of an offset() term, which the test needs no more, are
# no longer named. A message that names any other variable is unexpected.
# Increment 4: the five group tests, which read `group`.
identity_m1_scope <- c("performGQTest", identity_reads_data, identity_increment4)
# The registered Szroeter test matched the row names of `data` to its fitted
# values, and restricted `data` to those rows, before performSzroeterTest()
# compared them with the fit: the guard then saw the rows it had picked, under
# their row names, and a row name absent from `data` stopped it with "Missing
# rows". Built on the context, it gives `data` to the guard as it is. A
# refusal of `data` by the guard, or by that match, stays a refusal of the
# guard, with its class, and its message may change to the one the guard gives
# performSzroeterTest() on the same data; on no other route.
identity_rows_scope <- c("registry szroeter", identity_registry_runner_szroeter)
identity_rows <- function(record, before, after) {
  if (!record$fn %in% identity_rows_scope) {
    return(FALSE)
  }
  refused <- c(
    "cannot match the rows of `data`",
    "Szroeter test requires `data` to contain the rows used to fit the model. Missing rows"
  )
  a <- before$value$message
  b <- after$value$message
  any(vapply(refused, grepl, logical(1), x = a, fixed = TRUE)) &&
    grepl("cannot match the rows of `data`", b, fixed = TRUE) &&
    "heteroTests_data_rows_error" %in% after$value$class
}
identity_m1 <- function(record, before, after) {
  if (!record$fn %in% identity_m1_scope) {
    return(FALSE)
  }
  lacked <- "^The following variables are missing from input data: (.*?)\\. "
  lacks <- paste0(
    "^The following variables are missing from (input data|the model frame of `model`): ",
    "(.*?)\\. "
  )
  a <- before$value$message
  b <- after$value$message
  if (!grepl(lacked, a) || !grepl(lacks, b)) {
    return(FALSE)
  }
  named_before <- strsplit(sub(paste0(lacked, ".*$"), "\\1", a), ", ", fixed = TRUE)[[1L]]
  named_after <- strsplit(sub(paste0(lacks, ".*$"), "\\2", b), ", ", fixed = TRUE)[[1L]]
  length(named_after) > 0L && all(named_after %in% named_before)
}
# M1 for the panel tests (increment 5): without `data`, on a model frame that
# does not hold `id` or `time` under their own names, they stopped with
# "`id` and `time` must be columns in `data`." (or "`id` must be a column in
# `data`."), and now with M1, of the class of the context, naming those of
# `id` and `time` the model frame lacks. runPanelTests() and the scatter
# diagnostic need `data` and give their message of before.
identity_m1_panel <- function(record, after) {
  if (!record$fn %in% identity_panel_readers) {
    return(FALSE)
  }
  a <- record$value$message
  b <- after$value$message
  lacks <- "^The following variables are missing from the model frame of `model`: (.*?)\\. "
  if (!grepl("^`id`( and `time`)? must be (a column|columns) in `data`\\.$", a) ||
        !grepl(lacks, b)) {
    return(FALSE)
  }
  named <- strsplit(sub(paste0(lacks, ".*$"), "\\1", b), ", ", fixed = TRUE)[[1L]]
  length(named) > 0L && all(named %in% c("id", "time")) &&
    "heteroTests_model_context_error" %in% after$value$class
}
# Decision 1 for Goldfeld-Quandt on an unweighted fit: main refitted each
# segment from the formula, which computed a poly() basis again on the rows of
# the segment, so it has no stored-design value there. The basis spans the
# same columns, and the branch may differ from main by rounding only: every
# number to this relative tolerance.
identity_last_bits <- 1e-12
# The cells whose reference is another fit of the same model, equal to it up
# to rounding, as context_rounded_cells of helper-model-context.R, which the
# comparison does not source: the rank-deficient fit and the gam fits.
identity_rounded_cells <- c(
  "class/rank_deficient", "class/gam", "class/gam_offset_term", "class/gam_offset"
)
identity_segment_refits <- "performGQTest"
# A call refused by a new refusal (value_to_error) must give this message.
identity_new_refusals <- list(
  N6 = "cannot use the model frame of `model`",
  gam = "cannot test an mgcv::gam() fit with smooth terms",
  # RESET tested a multivariate lm without `data`, and returned a value.
  R4 = "needs a model with one response",
  # The registered names, without `data`, on a fit with model = FALSE whose
  # data are gone: the runner put the Cook-Weisberg fallback under them. The
  # tests of increments 3 and 4 on such a fit given numbered data in another
  # order.
  frame = "cannot use the model frame of `model`",
  # The five group tests on a fit whose coefficients were changed after the
  # fit (increment 4), and the functions of increment 5.
  M8 = identity_m8,
  # Pesaran's test on a repeated pair of `id` and `time` (increment 5,
  # decision 13). Its refusal of an unbalanced panel (N12, decided
  # 2026-10-24) is no longer a new refusal since the pull request of N12.
  R2 = identity_r2_message,
  # Increment 6: a second formula on rows of `data` the alignment refuses,
  # which it read by position (B9), and a variable of the workspace (R3).
  B9 = identity_b9_message,
  R3 = identity_r3_messages,
  # Decision 31: data the guard refuses, given to a test that reads nothing
  # from them and returned the value of the fit.
  decision31 = identity_b9_message
)
# The bootstraps: on a cell whose base statistic stopped before (B12, L1, L2,
# N7), or that refused an offset (B7, the robust Breusch-Pagan bootstrap), the
# call now reaches the checks of the bootstrap itself, and is refused by one
# of them, as it was refused before by the base statistic: a frame that
# cannot be rebuilt from `data` (a cut() term, or the model frame given as
# `data`), a glm (N8, increment 8), a transformed response under null-imposed
# resampling. Only these messages are allowed.
identity_bootstrap_messages <- c(
  "does not reproduce the fit", "supports Gaussian `lm` models only",
  "Null-imposed resampling needs a plain response variable"
)
identity_bootstrap_defects <- c("B12", "L1", "L2", "N7", "B7")

# N2 and its follow-up, outside the context (decision 22): White's degrees of
# freedom are the rank of its auxiliary regression with the intercept less
# one, decided on the auxiliary design of the centred regressors. The kinds of
# cell on which a call of White's test may change, read from the reference of
# the cell (identity_white_by_hand()):
#   "spans"     the columns of Z span the constant, and a count of the rank of
#               Z alone (0.15.0) was one too many, with the same statistic,
#               which must stay identical();
#   "lost"      Z is so nearly collinear that a decomposition of Z without the
#               intercept kept a column that lm() then aliased given the
#               intercept (0.15.0, on a gam with a smooth term): the
#               statistic may change too;
#   "located"   a regressor located far from zero, whose square the rank of
#               [1, Z] took for a combination of the intercept and the
#               regressor, where the design of the centred regressors keeps it;
#   "saturated" as many independent terms as observations, which the test now
#               refuses;
#   "exact"     fewer, and still an exact fit of the squared residuals, as
#               with two observations per level of a factor: refused too;
#   "names"     a regressor named like a column the test generates (`a` and
#               `a_sq`), on which the formula of the auxiliary regression
#               stopped.
# A change to a value must give the reference, White's test written out on the
# centred design: the statistic, the degrees of freedom and the p-value to
# `identity_n2_tolerance`, relative; and every other number of the result must
# be main's, identical(), but those that follow from the degrees of freedom or
# the statistic, which must be what they compute to from them
# (identity_n2_expected()). On a saturated cell the test must give its refusal
# and the runner, in its place, White's test without cross products. The
# resampling routes on a located or a names cell, whose replicates change
# with the statistic, and the routes of a names cell, on which main stopped,
# are verified as the same call on the reference of their cell: the same
# design located at zero, or with other names.
identity_white_routes <- c(
  "performWhiteTest", "performWhiteTestRobust", "performWhiteTestRobust(bootstrap = TRUE)",
  "runHeteroTests", "performWhiteTestBootstrap", "performWhiteTestStreaming"
)
identity_white_resampling <- c(
  "performWhiteTestBootstrap", "performWhiteTestRobust(bootstrap = TRUE)"
)
# The calls that run White's test in a battery and stop when the runner stops
# on its refusal: runDiagnostics() runs runHeteroTests().
identity_white_battery <- "runDiagnostics()$reset"
identity_n2_tolerance <- 1e-10
# The refusal of an exact fit, saturated ("There are too few observations for
# White's test ...") or forced by the design ("The design forces the exact
# fit ...").
identity_exact_fit_message <- "White's auxiliary regression fits the squared residuals exactly"
# An auxiliary regression fits the squared residuals exactly when its
# residual sum of squares is at most this fraction of their total, the
# threshold of the test (rwhite_exact_tolerance in R/performWhiteTest.R).
identity_exact_tolerance <- sqrt(.Machine$double.eps)
# The cells of a regressor located far from zero equal their reference, the
# same design located at zero, up to the rounding of the shifted regressor.
identity_located_cells <- c("white/located_1e4", "white/located_1e6")
identity_located_tolerance <- 1e-8

# White's test written out on a fit, without the package: the columns of the
# design the fit estimated, without the intercept, X; the auxiliary design of
# the centred regressors, their squares and, for at most ten of them, their
# pairwise products, Z_c; the Pearson residuals; and lm(e^2 ~ Z_c), whose
# n R^2 is the statistic and whose rank less one the degrees of freedom, with
# and without the products (`simplified`, the test the runner falls back on).
# With them what says why a version counted otherwise: the ranks of Z, built
# from X, of [1, Z] and of [1, Z_c] at the tolerance of qr(); the count of
# 0.15.0, the number of columns its decomposition of Z kept, against the rank
# of the regression it fitted on them; and whether the names of the columns
# the test generates collide.
identity_white_by_hand <- function(m) {
  X <- stats::model.matrix(m)
  estimated <- !is.na(stats::coef(m))[colnames(X)]
  X <- X[, estimated, drop = FALSE]
  X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  p <- ncol(X)
  if (p == 0L) {
    stop("no regressors besides the intercept")
  }
  products_kept <- p > 1L && p <= 10L
  build <- function(M, products) {
    Z <- cbind(M, M^2)
    if (products && products_kept) {
      for (i in seq_len(p - 1L)) {
        for (j in seq.int(i + 1L, p)) Z <- cbind(Z, M[, i] * M[, j])
      }
    }
    unname(Z)
  }
  e2 <- unname(stats::residuals(m, type = "pearson"))^2
  n <- length(e2)
  total <- sum((e2 - mean(e2))^2)
  exact <- function(aux) total > 0 && sum(aux$residuals^2) <= identity_exact_tolerance * total
  centred_test <- function(products) {
    Zc <- build(scale(X, scale = FALSE), products)
    if (qr(cbind(1, Zc))$rank >= n) {
      return(NULL)
    }
    aux <- stats::lm(e2 ~ Zc)
    if (exact(aux)) {
      return(NULL)
    }
    r_squared <- summary(aux)$r.squared
    statistic <- n * r_squared
    df <- aux$rank - 1L
    list(statistic = c("X-squared" = statistic), parameter = c(df = df),
         p.value = stats::pchisq(statistic, df, lower.tail = FALSE), r_squared = r_squared)
  }
  Z <- build(X, TRUE)
  qr_z <- qr(Z)
  kept <- sort(qr_z$pivot[seq_len(qr_z$rank)])
  names_x <- colnames(X)
  generated <- c(names_x, paste0(names_x, "_sq"))
  if (products_kept) {
    pairs <- utils::combn(p, 2L)
    generated <- c(generated, paste0(names_x[pairs[1L, ]], "_x_", names_x[pairs[2L, ]]))
  }
  Zc <- build(scale(X, scale = FALSE), TRUE)
  rank_1zc <- qr(cbind(1, Zc))$rank
  c(
    centred_test(TRUE),
    list(
      simplified = centred_test(FALSE), n = n,
      rank_z = qr_z$rank, rank_1z = qr(cbind(1, Z))$rank, rank_1zc = rank_1zc,
      main_df = length(kept), main_regression_df = stats::lm(e2 ~ Z[, kept])$rank - 1L,
      names_collide = anyDuplicated(generated) > 0L,
      # The auxiliary regression fits the squared residuals exactly with fewer
      # independent terms than observations; and whether the regressors alone do.
      exact_fit = rank_1zc < n && exact(stats::lm(e2 ~ Zc)),
      regressors_exact = exact(stats::lm(e2 ~ scale(X, scale = FALSE)))
    )
  )
}

# The cells of the follow-up of N2, run through every recipe besides the
# matrix: the model of "fit/ols/none/none/data_frame" with x1 moved by 1e4 and
# by 1e6, whose reference is that cell; a saturated design, 6 regressors and
# their 21 squares and products for 22 observations; a regressor `a` beside a
# regressor `a_sq`, whose reference is the same data with `a_sq` named `b`;
# an ordered factor under its default contr.poly, which spans the constant;
# and two exact fits: 30 subjects with two observations each, and a factor of
# 10 levels with two observations each beside a regressor.
identity_white_cells <- function() {
  white_cell <- function(id, build, reference = NULL) {
    list(id = id, block = "white", build = build, fit = "ols", offset = "none",
         terms = "none", form = "data_frame", needs = character(), reference = reference,
         expect = "value", defect = character())
  }
  d <- context_data()
  located <- function(id, shift) {
    dl <- d
    dl$x1 <- d$x1 + shift
    white_cell(id, function() list(m = stats::lm(y ~ x1 + x2, data = dl), data = dl),
               reference = "fit/ols/none/none/data_frame")
  }
  set.seed(22)
  saturated <- data.frame(matrix(stats::rnorm(22 * 6), 22, 6))
  names(saturated) <- paste0("v", 1:6)
  saturated$y <- stats::rnorm(22, sd = exp(saturated$v1 / 2))
  set.seed(3)
  collide <- data.frame(a = stats::runif(100, 1, 3))
  collide$a_sq <- log(collide$a) + stats::rnorm(100, sd = 0.3)
  collide$y <- collide$a + collide$a_sq + stats::rnorm(100, sd = collide$a)
  renamed <- collide
  names(renamed)[names(renamed) == "a_sq"] <- "b"
  ordered <- d
  ordered$o4 <- factor(rep(c("lo", "mid", "hi", "top"), length.out = nrow(d)),
                       levels = c("lo", "mid", "hi", "top"), ordered = TRUE)
  # Two observations per level: the residuals of each pair sum to zero, the
  # squared residuals are a combination of the dummies, and the auxiliary
  # regression fits them exactly with fewer terms than observations.
  set.seed(30)
  paired <- data.frame(subject = factor(rep(1:30, each = 2)), treat = rep(0:1, 30))
  paired$y <- 2 + 0.5 * paired$treat + stats::rnorm(30)[as.integer(paired$subject)] +
    stats::rnorm(60)
  set.seed(9)
  pairs_x1 <- data.frame(x1 = stats::rnorm(20), f = factor(rep(letters[1:10], 2)))
  pairs_x1$y <- stats::rnorm(20)
  list(
    located("white/located_1e4", 1e4),
    located("white/located_1e6", 1e6),
    white_cell("white/saturated", function() {
      list(m = stats::lm(y ~ v1 + v2 + v3 + v4 + v5 + v6, data = saturated), data = saturated)
    }),
    white_cell("white/names", function() {
      list(m = stats::lm(y ~ a + a_sq, data = collide), data = collide)
    }, reference = "white/names_renamed"),
    white_cell("white/names_renamed", function() {
      list(m = stats::lm(y ~ a + b, data = renamed), data = renamed)
    }),
    white_cell("white/ordered", function() {
      list(m = stats::lm(y ~ x1 + o4, data = ordered), data = ordered)
    }),
    white_cell("white/paired", function() {
      list(m = stats::lm(y ~ treat + subject, data = paired), data = paired)
    }),
    white_cell("white/pairs_x1", function() {
      list(m = stats::lm(y ~ x1 + f, data = pairs_x1), data = pairs_x1)
    })
  )
}

# The kinds of N2 that concern a call (above), with the reference of its cell,
# or NULL when none does.
identity_n2 <- function(record, records) {
  if (!record$fn %in% c(identity_white_routes, identity_white_battery)) {
    return(NULL)
  }
  ref <- records[[paste("reference", "white_by_hand", record$cell, sep = " | ")]]$value
  if (!is.list(ref) || isTRUE(ref$.error) || is.null(ref$rank_z)) {
    return(NULL)
  }
  kinds <- c(
    if (ref$rank_z == ref$rank_1z) "spans",
    if (ref$main_regression_df < ref$main_df) "lost",
    if (ref$rank_1zc != ref$rank_1z) "located",
    if (ref$rank_1zc >= ref$n) "saturated",
    if (isTRUE(ref$exact_fit)) "exact",
    if (isTRUE(ref$names_collide)) "names"
  )
  if (length(kinds) == 0L) NULL else list(kinds = kinds, reference = ref)
}

# How a call of N2 is judged: "check", against the reference and main
# (identity_n2_check()); "refusal", the refusal of an auxiliary regression
# that fits the squared residuals exactly, with as many terms as observations
# ("saturated") or fewer ("exact"), which every route of White's test must
# give, whether or not main gave a value (identity_n2_refusal()); "fallback",
# the runner's White's test without cross products in its place, only where
# the regressors alone do not fit the squared residuals exactly; "chain",
# against the same call on the reference of the cell (identity_verify()), the
# exact routes also against the reference of N2. Where the regressors alone fit
# them exactly, the Breusch-Pagan and Koenker tests the runner would fall back
# on fit them exactly too: the runner must refuse, and so must the batteries
# that run it (identity_white_battery), which are main's elsewhere.
identity_n2_mode <- function(fn, n2, change) {
  # Since main holds N2 and its follow-up, a call identical to main has the
  # degrees of freedom and the refusals of N2 already: it needs no check of its
  # own, and counts as identical (increment 5, the first run against such a
  # main).
  if (is.null(n2) || identical(change, "identical")) {
    return(NULL)
  }
  kinds <- n2$kinds
  exact_fit <- any(c("saturated", "exact") %in% kinds)
  regressors_exact <- isTRUE(n2$reference$regressors_exact)
  if (fn %in% identity_white_battery) {
    return(if (exact_fit && regressors_exact) "refusal")
  }
  if (exact_fit) {
    if (identical(fn, "runHeteroTests") && identical(change, "value_change") &&
          !regressors_exact) {
      return("fallback")
    }
    return("refusal")
  }
  if ("names" %in% kinds || fn %in% identity_white_resampling && !all(kinds == "spans")) {
    return("chain")
  }
  if (identical(fn, "performWhiteTestStreaming")) {
    return(NULL)
  }
  "check"
}

identity_n2_close <- function(x, y) {
  relative <- identity_relative(as.double(unname(x)), as.double(unname(y)))
  isTRUE(relative <= identity_n2_tolerance)
}

# White's statistic, degrees of freedom and p-value of a result agree with
# the reference.
identity_n2_agrees <- function(v, ref) {
  !is.null(v) && !is.null(v$statistic) && !is.null(v$parameter) && !is.null(ref) &&
    identity_n2_close(v$statistic, ref$statistic) &&
    identical(as.double(unname(v$parameter)), as.double(unname(ref$parameter))) &&
    identity_n2_close(v$p.value, ref$p.value)
}

# Cramer's V of a chi-squared statistic, as the robust tests compute it
# (.calculate_effect_size() of R/effect_sizes.R, with `type = "cramers_v"`).
identity_cramers_v <- function(chi_sq, df, n) {
  effect_size <- sqrt(max(0, chi_sq / (n * max(1, df))))
  magnitude <- if (!is.finite(effect_size)) {
    "unknown"
  } else if (effect_size < 0.1) {
    "negligible"
  } else if (effect_size < 0.3) {
    "small"
  } else if (effect_size < 0.5) {
    "medium"
  } else {
    "large"
  }
  practical <- is.finite(effect_size) && effect_size >= 0.3
  interpretation <- if (!is.finite(effect_size)) {
    "Effect size could not be determined."
  } else {
    sprintf("Effect size %.3f (%s) suggests %s practical impact.", effect_size, "cramers_v",
            if (practical) "meaningful" else "limited")
  }
  list(effect_size = effect_size, magnitude = magnitude, practical_significance = practical,
       interpretation = interpretation, type = "cramers_v")
}

# The power of a chi-squared test, as the robust tests compute it
# (.estimate_test_power() of R/bootstrap_methods.R), but for its call.
identity_power <- function(statistic, df, n, effect_size, alpha, target_power = 0.8,
                           max_multiplier = 5) {
  chi_sq <- max(unname(statistic[[1L]]), .Machine$double.eps)
  if (!is.finite(effect_size) || effect_size <= 0) {
    return(list(power = NA_real_, recommended_n = NA_integer_, details = list(
      message = "Effect size not available or non-positive; cannot estimate power.",
      df = df, sample_size = n, call = NULL
    )))
  }
  critical <- stats::qchisq(1 - alpha, df)
  observed_power <- 1 - stats::pchisq(critical, df, ncp = chi_sq)
  power_for_n <- function(n_val) {
    lambda <- max(1e-8, effect_size^2) * max(1, df) * n_val
    1 - stats::pchisq(critical, df, ncp = lambda)
  }
  recommended_n <- n
  achieved_power <- power_for_n(recommended_n)
  if (!is.na(achieved_power) && achieved_power < target_power) {
    upper <- ceiling(n * max_multiplier)
    if (upper <= recommended_n) {
      upper <- recommended_n + 1L
    }
    while (power_for_n(upper) < target_power && upper < n * max_multiplier) {
      upper <- ceiling(upper * 1.2)
    }
    if (power_for_n(upper) < target_power) {
      recommended_n <- NA_integer_
    } else {
      root <- tryCatch(
        stats::uniroot(function(x) power_for_n(x) - target_power,
                       lower = recommended_n, upper = upper),
        error = function(e) NULL
      )
      recommended_n <- if (is.null(root)) NA_integer_ else ceiling(root$root)
    }
  }
  curve_effect_sizes <- seq(0.05, max(0.5, effect_size * 1.5), by = 0.05)
  power_curve <- vapply(curve_effect_sizes, function(es) {
    lambda <- max(1e-8, es^2) * max(1, df) * n
    1 - stats::pchisq(critical, df, ncp = lambda)
  }, numeric(1L))
  list(power = observed_power, recommended_n = recommended_n, details = list(
    df = df, sample_size = n, lambda_observed = chi_sq, effect_size = effect_size,
    power_curve = data.frame(effect_size = curve_effect_sizes, power = power_curve),
    alpha = alpha, target_power = target_power, call = NULL
  ))
}

# The robust details of main's result with what follows from the degrees of
# freedom and the statistic computed from them: the asymptotic interval, the
# effect size and the power, with main's call.
identity_robust_at <- function(details, statistic, df) {
  df <- as.numeric(df)
  n <- details$sample_size
  level <- details$confidence_interval$level
  if (identical(details$confidence_interval$source, "asymptotic")) {
    lower_tail <- (1 - level) / 2
    details$confidence_interval$estimate <- stats::setNames(
      c(stats::qchisq(lower_tail, df), stats::qchisq(1 - lower_tail, df)), c("lower", "upper")
    )
  }
  effect <- identity_cramers_v(as.numeric(statistic[[1L]]), df, n)
  details$effect_size <- effect
  call <- details$power$details$call
  power <- identity_strip(identity_power(statistic, df, n, effect$effect_size, 1 - level))
  power$details$call <- call
  details$power <- power
  details
}

# What the branch must return for a call of White's test of N2: main's result,
# with the degrees of freedom of the reference, the branch's p-value and,
# unless only the count changed ("spans"), its statistic and R^2, and what
# follows from them; NULL when the branch's R^2 is not the reference's.
identity_n2_expected <- function(a, b, ref, pure_df) {
  df <- unname(ref$parameter[[1L]])
  expected <- a
  expected$parameter[[1L]] <- df
  expected$p.value <- b$p.value
  if (!pure_df) {
    expected$statistic <- b$statistic
  }
  if (!is.null(attr(a, "auxiliary_regressors"))) {
    attr(expected, "auxiliary_regressors") <- df
  }
  if (!pure_df && !is.null(attr(a, "r_squared_auxiliary"))) {
    r_squared <- attr(b, "r_squared_auxiliary")
    if (!identity_n2_close(r_squared, ref$r_squared)) {
      return(NULL)
    }
    attr(expected, "r_squared_auxiliary") <- r_squared
  }
  if (!is.null(a$robust_details)) {
    expected$robust_details <- identity_robust_at(a$robust_details, expected$statistic, df)
  }
  expected
}

# A change of N2 judged by "check": the statistic, degrees of freedom and
# p-value of the reference, the statistic of main (on the stored design,
# where main has one) when only the count changed, and every other number of
# main's result; for the runner, White's entry so, and its other tests main's.
identity_n2_check <- function(n2, record, after, stored = NULL) {
  main_value <- if (!is.null(stored) && is.list(stored$value) && !isTRUE(stored$value$.error)) {
    stored$value
  } else {
    record$value
  }
  ref <- n2$reference
  pure_df <- all(n2$kinds == "spans")
  b_all <- after$value
  if (!is.list(main_value) || isTRUE(main_value$.error) || !is.list(b_all) ||
        isTRUE(b_all$.error)) {
    return(FALSE)
  }
  if (identical(record$kind, "runner")) {
    a <- main_value[["white"]]
    b <- b_all[["white"]]
    if (is.null(a) || is.null(b) || !is.null(a$status) || !is.null(b$status) ||
          !identity_n2_agrees(b, ref)) {
      return(FALSE)
    }
    if (pure_df && !identical(a$statistic, b$statistic)) {
      return(FALSE)
    }
    expected <- main_value
    expected$white$parameter <- unname(ref$parameter[[1L]])
    expected$white$p.value <- b$p.value
    expected$white$statistic <- b$statistic
    return(identical(expected, b_all))
  }
  if (!identity_n2_agrees(b_all, ref)) {
    return(FALSE)
  }
  if (pure_df && !identical(unname(main_value$statistic), unname(b_all$statistic))) {
    return(FALSE)
  }
  identical(identity_n2_expected(main_value, b_all, ref, pure_df), b_all)
}

# On a cell whose auxiliary regression fits the squared residuals exactly, a
# route of White's test must refuse it: the branch stops with the refusal, or,
# as main did, with another error (the White bootstrap on fewer than 50
# observations), and "identical" is kept for that. A value is unexpected,
# whatever main gave.
identity_n2_refusal <- function(before, after) {
  b <- after$value
  if (!is.list(b) || !isTRUE(b$.error)) {
    return("unexpected")
  }
  if (grepl(identity_exact_fit_message, b$message, fixed = TRUE)) {
    return("allowed")
  }
  if (identical(before$value, after$value)) "identical" else "unexpected"
}

# On a saturated cell: the runner gives White's test without cross products
# under the name "white", as a fallback, and its other tests are main's.
identity_n2_fallback <- function(n2, record, after) {
  b <- after$value[["white"]]
  others <- setdiff(names(after$value), "white")
  !is.null(b) && identical(b$status, "fallback") &&
    identity_n2_agrees(b, n2$reference$simplified) &&
    identical(record$value[others], after$value[others])
}

# A change of N2 judged by "chain": the exact routes give the reference; the
# rest is identity_verify()'s, on the reference of the cell.
identity_n2_chain <- function(n2, record, after) {
  if (record$fn %in% identity_white_resampling ||
        identical(record$fn, "performWhiteTestStreaming")) {
    return(TRUE)
  }
  v <- after$value
  if (identical(record$kind, "runner")) {
    v <- v[["white"]]
    if (is.null(v) || !is.null(v$status)) {
      return(FALSE)
    }
  }
  identity_n2_agrees(v, n2$reference)
}

# Szroeter's null moments ("szroeter_moments"). While a run is in a call, the
# id of its record is in `identity_sz$id`; the exit tracer of .szroeter_test()
# (identity_szroeter_trace()) keeps, under that id, what the test read: the
# design of the fit, its prior weights, the rank the null takes (the fit's,
# or for an mgcv::gam() that of its design at the tolerance of the fit), the
# Pearson residuals and the order of the test. Both versions of the package
# compute these as `ctx`, `residuals` and `ord`.
identity_sz <- new.env()
identity_sz$id <- NULL
identity_sz$captured <- list()

identity_szroeter_capture <- function(frame) {
  id <- identity_sz$id
  if (is.null(id) || !all(c("ctx", "ord", "residuals") %in% ls(frame, all.names = TRUE))) {
    return(invisible(NULL))
  }
  ctx <- get("ctx", envir = frame)
  coefs <- stats::coef(ctx$model)
  keep <- if (length(coefs) == ncol(ctx$design)) !is.na(coefs) else rep(TRUE, ncol(ctx$design))
  identity_sz$captured[[id]] <- list(
    design = ctx$design[, keep, drop = FALSE], weights = ctx$weights,
    rank = if (inherits(ctx$model, "gam")) NULL else ctx$model$rank,
    tol = ctx$qr_tol, residuals = unname(get("residuals", envir = frame)),
    ord = get("ord", envir = frame)
  )
  invisible(NULL)
}

identity_szroeter_trace <- function(on) {
  ns <- asNamespace("heteroTests")
  if (on) {
    suppressMessages(trace(
      ".szroeter_test", exit = quote(identity_szroeter_capture(environment())),
      where = ns, print = FALSE
    ))
  } else {
    suppressMessages(untrace(".szroeter_test", where = ns))
  }
  invisible(NULL)
}

# h, its null mean and variance and Q written out from what the test read,
# with the full n x n residual-maker M of the ordered, weighted design and
# A = diag(1, ..., n): E h = tr(MA) / nu, Var h = 2 (nu tr(MAMA) - tr(MA)^2) /
# (nu^2 (nu + 2)), nu = n - k. The package computes the traces from the
# leverages and Q1' A Q1 instead (rszroeter_moments()).
identity_szroeter_by_hand <- function(cap) {
  if (is.null(cap)) {
    return(NULL)
  }
  X <- cap$design
  if (!is.null(cap$weights)) {
    X <- sqrt(cap$weights) * X
  }
  X <- X[cap$ord, , drop = FALSE]
  e <- cap$residuals[cap$ord]
  n <- length(e)
  a <- seq_len(n)
  basis <- qr(X, tol = cap$tol)
  k <- if (is.null(cap$rank)) basis$rank else cap$rank
  q1 <- qr.Q(basis)[, seq_len(k), drop = FALSE]
  B <- (diag(n) - tcrossprod(q1)) * rep(a, each = n)
  trace_ma <- sum(diag(B))
  trace_mama <- sum(B * t(B))
  nu <- n - k
  h <- sum(a * e^2) / sum(e^2)
  mean <- trace_ma / nu
  var <- 2 * (nu * trace_mama - trace_ma^2) / (nu^2 * (nu + 2))
  list(n = n, k = k, h = h, mean = mean, var = var, Q = (h - mean) / sqrt(var))
}

# The Szroeter result of a record's value: the test, or the runner's entry.
identity_szroeter_part <- function(record) {
  v <- record$value
  if (!is.list(v) || isTRUE(v$.error) || isTRUE(v$.skip)) {
    return(NULL)
  }
  if (record$kind %in% c("runner", "stored_runner", "reference_runner")) v[["szroeter"]] else v
}

identity_szroeter_tolerance <- 1e-9

# NULL when the rule does not apply (not a Szroeter call, or not a change of
# value); otherwise whether the change is the new null: every field but the
# statistic and the p-value identical() to main's (of each test of a runner),
# the estimate h included; main's and the branch's references identical, so
# that the test read the same design, residuals and order; and the branch's h,
# Q and p-value those of the reference, to `identity_szroeter_tolerance`.
identity_szroeter_verdict <- function(before, after, verdict) {
  if (!before$fn %in% identity_szroeter_calls || !identical(verdict$change, "value_change")) {
    return(NULL)
  }
  a <- identity_szroeter_part(before)
  b <- identity_szroeter_part(after)
  ref <- after$szroeter
  if (is.null(a) || is.null(b) || is.null(ref) || !identical(before$szroeter, ref)) {
    return(FALSE)
  }
  rest <- function(v) {
    v$statistic <- NULL
    v$p.value <- NULL
    v
  }
  runner <- before$kind %in% c("runner", "stored_runner", "reference_runner")
  same_rest <- if (runner) {
    identical(lapply(before$value, rest), lapply(after$value, rest))
  } else {
    identical(rest(before$value), rest(after$value))
  }
  if (!same_rest) {
    return(FALSE)
  }
  close <- function(x, y) {
    x <- as.double(unname(x))
    length(x) == 1L && is.finite(x) && abs(x - y) <= identity_szroeter_tolerance * max(1, abs(y))
  }
  if (!is.null(b$estimate) && !close(b$estimate, ref$h)) {
    return(FALSE)
  }
  alternative <- if (is.null(b$alternative) || grepl("increases", b$alternative, fixed = TRUE)) {
    "greater"
  } else if (grepl("decreases", b$alternative, fixed = TRUE)) {
    "less"
  } else {
    "two.sided"
  }
  p_ref <- switch(alternative,
    greater = stats::pnorm(ref$Q, lower.tail = FALSE),
    less = stats::pnorm(ref$Q),
    two.sided = 2 * stats::pnorm(-abs(ref$Q))
  )
  close(b$statistic, ref$Q) && close(b$p.value, p_ref)
}

# 0.16.0, outside the context: through runHeteroTests() a test whose data.name
# is "model" reports the formula of the model instead, as Koenker's test does.
# White's test, its bootstrap and its form without cross products, the
# studentized Breusch-Pagan statistic and Szroeter's test reported "model",
# the name of the argument of the function the registry calls them with; every
# other data.name is kept. A runner's data.name may change only from "model"
# to the formula of the model of its cell, deparse(formula(model)), which
# each run records ("reference | formula | <cell>"), and no runner result of
# the branch may report "model". This holds for the calls and for the runner
# records on the stored design and of the verification; the data.name of
# every runner record is then set aside, and the rest of the record is judged
# by the rules above: a call whose data.name alone changed is allowed, as the
# change "data_name".
identity_runner_kinds <- c("runner", "stored_runner", "reference_runner")

identity_tests_of <- function(v) is.list(v) && !isTRUE(v$.error) && !isTRUE(v$.skip)

identity_drop_data_name <- function(v) {
  if (!identity_tests_of(v)) {
    return(v)
  }
  for (k in seq_along(v)) {
    if (is.list(v[[k]])) v[[k]]$data.name <- NULL
  }
  v
}

# "formula" when a data.name changed, each from "model" to the formula of the
# model; "none" when none changed; "unexpected" when one changed otherwise,
# or the branch names a test "model".
identity_data_name_change <- function(before, after, formula) {
  if (!identity_tests_of(after) || !is.character(formula)) {
    return("none")
  }
  had_tests <- identity_tests_of(before)
  changed <- FALSE
  for (k in names(after)) {
    db <- after[[k]]$data.name
    if (identical(db, "model")) {
      return("unexpected")
    }
    if (!had_tests) next
    da <- before[[k]]$data.name
    if (identical(da, db)) next
    if (identical(da, "model") && identical(db, formula)) {
      changed <- TRUE
      next
    }
    # Increment 7: an entry that runs the test the runner names where it ran a
    # fallback on main carries the data.name of that test, the formula of the
    # model; the rules of the runner judge the rest.
    if (identical(before[[k]]$status, "fallback") && is.null(after[[k]]$status) &&
          identical(after[[k]]$diagnostic, k) && identical(db, formula)) {
      next
    }
    return("unexpected")
  }
  if (changed) "formula" else "none"
}

# The records of the two runs with the data.name of their runners set aside,
# and the verdict on it of each runner record: the calls, and the records on
# the stored design and of the verification, each against the formula of its
# own cell.
identity_data_names <- function(main, branch) {
  verdicts <- character()
  for (id in names(main)) {
    if (!main[[id]]$kind %in% identity_runner_kinds) next
    before <- main[[id]]$value
    after <- branch[[id]]$value
    formula <- branch[[paste("reference", "formula", main[[id]]$cell, sep = " | ")]]$value
    verdicts[[id]] <- identity_data_name_change(before, after, formula)
    main[[id]]$value <- identity_drop_data_name(before)
    branch[[id]]$value <- identity_drop_data_name(after)
  }
  list(main = main, branch = branch, verdicts = verdicts)
}

# --- increment 7: the streaming tests --------------------------------------------

# The three streaming tests read the stored design chunk by chunk. Their
# recipes of helper-test-recipes.R read chunks of 10,000 observations, one
# chunk on every cell; these read chunks of 7, 18 chunks on 120 observations
# and a last chunk of one, and of 40. The runner runs them on any data with
# `chunk_threshold_mb = 0`, in chunks of 10,000 and of 7
# (`identity_streaming_runners`, run on every cell by run_cell()); without
# `data` it runs the exact tests, as on main.
identity_streaming_chunked <- list(
  `performWhiteTestStreaming(chunk_size = 7)` = function(m, d) {
    performWhiteTestStreaming(m, d, chunk_size = 7, progress = FALSE)
  },
  `performBPTestStreaming(chunk_size = 7)` = function(m, d) {
    performBPTestStreaming(m, d, chunk_size = 7, progress = FALSE)
  },
  `performKoenkerTestStreaming(chunk_size = 7)` = function(m, d) {
    performKoenkerTestStreaming(m, d, chunk_size = 7, progress = FALSE)
  },
  `performWhiteTestStreaming(chunk_size = 40)` = function(m, d) {
    performWhiteTestStreaming(m, d, chunk_size = 40, progress = FALSE)
  },
  `performBPTestStreaming(chunk_size = 40)` = function(m, d) {
    performBPTestStreaming(m, d, chunk_size = 40, progress = FALSE)
  },
  `performKoenkerTestStreaming(chunk_size = 40)` = function(m, d) {
    performKoenkerTestStreaming(m, d, chunk_size = 40, progress = FALSE)
  }
)
identity_extra_recipes <- c(identity_extra_recipes, identity_streaming_chunked)
identity_streaming_runner_tests <- c("white", "breusch_pagan", "koenker")
identity_streaming_runners <- c(
  `runHeteroTests(chunk_threshold_mb = 0)` = 10000,
  `runHeteroTests(chunk_threshold_mb = 0, chunk_size = 7)` = 7
)
identity_streaming <- c(
  "performWhiteTestStreaming", "performBPTestStreaming", "performKoenkerTestStreaming",
  names(identity_streaming_chunked)
)
identity_increment7 <- c(identity_streaming, names(identity_streaming_runners))
identity_affected <- c(identity_affected, identity_increment7)
# The calls of the streaming tests, by the test they compute: a chunked recipe
# is seen as its test (identity_base_fn(), context_cell_for()).
identity_streaming_base <- function(fn) sub("\\(chunk_size = [0-9]+\\)$", "", fn)
# B6: on a term computed from the rows it is given, I(x1 > median(x1)) or
# cut(x1, quantile(x1)), each chunk built its own design from the formula, on
# its rows of `data`: values that change in chunks smaller than the data, and
# errors that become values where cut() computed other breaks in a chunk
# ("factor has new levels", N7, also in one chunk of the rows of a fit that
# dropped the row at the minimum of x1). White's streaming test stopped there
# on the collinear squares of the dummies until the pull request of N3, where
# it had stopped on the new levels. The defects of the cells that concern
# every test built on the context (B5, B12, B14, B15, L1, L2, N7, R4) apply to
# these calls too, and N17, "frame" and M8 are scoped to them as to the tests
# of increments 3 to 6.
identity_allowed$value_change <- c(identity_allowed$value_change, "B6")
identity_allowed$error_to_value <- c(identity_allowed$error_to_value, "B6")
identity_runner_allowed <- c(identity_runner_allowed, "B6", "N17")
identity_defect_scope$B6 <- identity_increment7
for (defect in c("frame", "N17", "M8")) {
  identity_defect_scope[[defect]] <- c(identity_defect_scope[[defect]], identity_increment7)
}
# Increment 7 allowed White's streaming test, on a cell of collinear terms
# where it stopped before reaching its auxiliary regression (L1, L2, N7, B6),
# to stop on those terms instead ("Auxiliary regression became rank
# deficient"). Since the pull request of N3 it no longer stops on them, and
# that rule is gone (below).
for (defect in c("frame", "M8")) {
  identity_message_scope[[defect]] <- c(identity_message_scope[[defect]], identity_increment7)
}
# Decision 1: the streaming tests built the design of each chunk from the
# formula, through the `predvars` of its terms, and so computed a poly() basis
# again on the rows of each chunk, as `x = TRUE` does not change; main has no
# stored-design value of them, and the branch must equal main to
# `identity_last_bits` relative on every number, as Goldfeld-Quandt's
# segments, where both ran the same tests: where the runner ran a fallback on
# main and the test it names on the branch, the rules of the runner decide
# (identity_same_tests()).
identity_rebuilt_per_chunk <- identity_increment7

# Whether the two results of a call hold the same tests: for a runner, the
# same names, each run as the same diagnostic with the same status.
identity_same_tests <- function(before, after) {
  if (!before$kind %in% identity_runner_kinds) {
    return(TRUE)
  }
  a <- before$value
  b <- after$value
  if (!identity_tests_of(a) || !identity_tests_of(b) || !identical(names(a), names(b))) {
    return(FALSE)
  }
  all(vapply(names(a), function(k) {
    identical(a[[k]]$diagnostic, b[[k]]$diagnostic) && identical(a[[k]]$status, b[[k]]$status)
  }, logical(1)))
}

# --- N3 and the scale of the squared residuals: the streaming tests ------------

# The pull request of N3, outside the context (decision 22), with the threshold
# of the streaming tests on squared residuals that do not vary (ROADMAP.md,
# 0.17.0). The three streaming tests reduce each chunk of their auxiliary
# design by a QR decomposition stacked on the factor of the chunks before it,
# instead of accumulating its cross-products, build that design from the
# regressors centred at their means, and decide its rank as performWhiteTest()
# does, at the tolerance of qr() in the order of its columns
# (chunked_regression_r2(), R/performance.R). So White's streaming test drops
# the collinear terms of its auxiliary regression, the squares of dummies or of
# a poly() basis, where it stopped ("Auxiliary regression became rank
# deficient"); the cross-products of a regressor located far from zero no
# longer look collinear, in the three tests ("... auxiliary regression became
# rank deficient"); and the squared residuals are constant when their total
# sum of squares is at most .Machine$double.eps of their sum of squares, not of
# 1 ("requires variation in squared residuals"). For the calls of the three
# tests, in chunks of any size, of the runner that streams them and of their
# help pages (`identity_n3_calls`):
#   - an error of those kinds that becomes a value must give the value of the
#     exact test of the same call, performWhiteTest(), performBPTest() or
#     performKoenkerTest() (`identity_n3_exact`): the statistic and the
#     p-value to `identity_n3_tolerance`, relative, the tolerance of the
#     streaming tests against the exact tests in the matrix, and the degrees
#     of freedom identical();
#   - a value that changes must be main's to `identity_last_bits` on every
#     number, the precision to which main's normal equations computed it on a
#     well-conditioned design, every other field identical(); or, where those
#     equations, whose condition is the square of that of the auxiliary
#     design, lost more digits, give the value of the exact test as above and
#     be nearer to it than main's;
#   - a runner, each of its entries so: the streaming test where main fell
#     back on another test because the streaming test stopped, or the
#     streaming test of main but for the last bits, or nearer to the exact
#     test, as above.
# Any other change of these calls is judged by the rules above. The exact test
# of a help-page example is the same call to the exact test, without
# `chunk_size` and `progress`, which each run records ("reference").
identity_n3_calls <- c(identity_streaming, names(identity_streaming_runners))
identity_n3_tolerance <- 1e-8
identity_n3_exact <- c(
  performWhiteTestStreaming = "performWhiteTest", performBPTestStreaming = "performBPTest",
  performKoenkerTestStreaming = "performKoenkerTest"
)
identity_n3_runner_exact <- c(
  white = "performWhiteTest", breusch_pagan = "performBPTest", koenker = "performKoenkerTest"
)
identity_n3_errors <- c(
  "Auxiliary regression became rank deficient", "auxiliary regression became rank deficient",
  "requires variation in squared residuals"
)

# The record of the exact test of a call of a streaming test, in `records`.
identity_n3_reference <- function(record, test, records) {
  id <- if (identical(record$kind, "example")) {
    paste("reference", test, paste("example", record$fn, record$cell), sep = " | ")
  } else {
    paste("recipe", test, record$cell, sep = " | ")
  }
  records[[id]]$value
}

# The statistic of `v` relative to that of the exact test `ref`.
identity_n3_distance <- function(v, ref) {
  identity_relative(as.double(unname(v$statistic)), as.double(unname(ref$statistic)))
}

# Whether a result `v` gives the value of the exact test `ref`: the statistic
# and the p-value to identity_n3_tolerance, the degrees of freedom identical().
identity_n3_agrees <- function(v, ref) {
  if (!is.list(v) || !is.list(ref) || isTRUE(ref$.error) || isTRUE(ref$.skip) ||
        is.null(v$statistic) || is.null(ref$statistic)) {
    return(FALSE)
  }
  close <- function(x, y) {
    isTRUE(identity_relative(as.double(unname(x)), as.double(unname(y))) <= identity_n3_tolerance)
  }
  close(v$statistic, ref$statistic) && close(v$p.value, ref$p.value) &&
    identical(as.double(unname(v$parameter)), as.double(unname(ref$parameter)))
}

# The verdict on a value of a streaming test that changed, `a` main's and `b`
# the branch's, against the exact test `ref`: "last bits", "exact test", or NA
# when the change is not N3's.
identity_n3_value <- function(a, b, ref) {
  if (!is.list(a) || !is.list(b) || !identical(names(a), names(b)) ||
        !identical(attributes(a), attributes(b))) {
    return(NA_character_)
  }
  rest <- function(v) Filter(Negate(is.numeric), unclass(v))
  if (!identical(rest(a), rest(b))) {
    return(NA_character_)
  }
  if (isTRUE(identity_relative(identity_numbers(a), identity_numbers(b)) <= identity_last_bits)) {
    return("last bits")
  }
  if (identity_n3_agrees(b, ref) &&
        isTRUE(identity_n3_distance(b, ref) <= identity_n3_distance(a, ref))) {
    return("exact test")
  }
  NA_character_
}

# The verdict on an error of main that became a value.
identity_n3_error <- function(a, b, ref) {
  of_n3 <- is.list(a) && isTRUE(a$.error) &&
    any(vapply(identity_n3_errors, grepl, logical(1), x = a$message, fixed = TRUE))
  if (of_n3 && identity_n3_agrees(b, ref)) "exact test" else NA_character_
}

# NULL when the rule does not apply (not a call of `identity_n3_calls`, or not
# a change to a value); otherwise a list with the verdict, "last bits", "exact
# test" or NA, and the statistic of each version relative to the exact test,
# the largest over the entries of a runner.
identity_n3_verdict <- function(before, after, verdict, branch) {
  if (!before$fn %in% identity_n3_calls ||
        !verdict$change %in% c("error_to_value", "value_change")) {
    return(NULL)
  }
  a <- before$value
  b <- after$value
  if (!before$kind %in% identity_runner_kinds) {
    ref <- identity_n3_reference(before, identity_n3_exact[[identity_streaming_base(before$fn)]],
                                 branch)
    outcome <- if (identical(verdict$change, "error_to_value")) {
      identity_n3_error(a, b, ref)
    } else {
      identity_n3_value(a, b, ref)
    }
    return(list(
      verdict = outcome,
      branch_exact = if (is.list(ref) && !is.null(ref$statistic)) identity_n3_distance(b, ref) else NA,
      main_exact = if (is.list(ref) && !is.null(ref$statistic) && !is.null(a$statistic)) {
        identity_n3_distance(a, ref)
      } else {
        NA
      }
    ))
  }
  # A runner, entry by entry.
  failed <- list(verdict = NA_character_, branch_exact = NA_real_, main_exact = NA_real_)
  if (!identity_tests_of(a) || !identity_tests_of(b) || !identical(names(a), names(b))) {
    return(failed)
  }
  outcomes <- character()
  branch_exact <- 0
  main_exact <- 0
  for (k in names(b)) {
    if (identical(a[[k]], b[[k]])) {
      next
    }
    if (!k %in% names(identity_n3_runner_exact)) {
      return(failed)
    }
    ref <- identity_n3_reference(before, identity_n3_runner_exact[[k]], branch)
    streams <- is.null(b[[k]]$status) && identical(b[[k]]$diagnostic, k)
    if (!streams) {
      return(failed)
    }
    outcome <- if (identical(a[[k]]$status, "fallback")) {
      # The streaming test stopped on main, and the runner fell back.
      if (identity_n3_agrees(b[[k]], ref)) "exact test" else NA_character_
    } else {
      identity_n3_value(a[[k]], b[[k]], ref)
    }
    if (is.na(outcome)) {
      return(failed)
    }
    outcomes <- c(outcomes, outcome)
    branch_exact <- max(branch_exact, identity_n3_distance(b[[k]], ref))
    if (is.null(a[[k]]$status)) {
      main_exact <- max(main_exact, identity_n3_distance(a[[k]], ref))
    }
  }
  list(verdict = if ("exact test" %in% outcomes) "exact test" else "last bits",
       branch_exact = branch_exact, main_exact = main_exact)
}

# --- canonical results ---------------------------------------------------------

# A result without the environments, functions and calls it carries, which
# differ between two processes for no reason of value.
identity_strip <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.environment(x) || is.function(x)) {
    return(paste0("<", typeof(x), ">"))
  }
  if (is.language(x) || inherits(x, "formula")) {
    return(paste(deparse(x), collapse = " "))
  }
  at <- attributes(x)
  if (is.list(x)) {
    x <- lapply(unclass(x), identity_strip)
  }
  if (!is.null(at)) {
    at[names(at) == "model"] <- "<model>"
    attributes(x) <- lapply(at, identity_strip)
  }
  x
}

identity_outcome <- function(f) {
  warnings <- character()
  value <- withCallingHandlers(
    tryCatch(
      f(),
      # A missing argument has class "getvarError" in byte-compiled code, as
      # in an installed package, and "evalError" in code that
      # pkgload::load_all() leaves uncompiled: neither says anything of the
      # call, and both are left out.
      error = function(e) {
        list(.error = TRUE, class = setdiff(class(e), c("simpleError", "getvarError", "evalError")),
             message = conditionMessage(e))
      },
      skip = function(e) list(.skip = TRUE, message = conditionMessage(e))
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) invokeRestart("muffleMessage")
  )
  if (!(is.list(value) && (isTRUE(value$.error) || isTRUE(value$.skip)))) {
    value <- identity_strip(value)
  }
  list(value = value, warnings = sort(unique(warnings)))
}

# The fields of a runner's results that say what was run and what it gave.
identity_runner_summary <- function(suite) {
  lapply(unclass(suite), function(r) {
    extras <- attr(r, "extras")
    list(
      method = r$method, statistic = unname(r$statistic), parameter = unname(r$parameter),
      p.value = r$p.value, diagnostic = attr(r, "diagnostic"),
      status = extras$status, issue = extras$issue, data.name = r$data.name
    )
  })
}

# The five functions built on the context, traced, or untraced, so that a
# model they are given without its design gets its own first: on main,
# model.matrix(model, data) then returns the stored design, also for the
# refits of a bootstrap.
identity_trace <- function(on) {
  fns <- c("performWhiteTest", "performBPTest", "performKoenkerTest",
           "performRankPermutationTest", "performHighDimensionalTest")
  places <- list(asNamespace("heteroTests"), as.environment("package:heteroTests"))
  tracer <- quote(
    if (inherits(model, "lm") && is.null(model[["x"]])) model$x <- stats::model.matrix(model)
  )
  for (where in places) {
    for (fn in fns) {
      if (on) {
        suppressMessages(trace(fn, tracer = tracer, where = where, print = FALSE))
      } else {
        suppressMessages(untrace(fn, where = where))
      }
    }
  }
}

# --- the references of a call ----------------------------------------------------

# Every allowed change to a value is verified against a reference
# (identity_verify()): the call on the cell the value must reproduce (the data
# frame of the fit, the fit to the complete rows, the fit to the subset, the
# fit to repeated rows), itself unchanged from main or verified; the fit with
# the columns of its terms computed beforehand ("precomputed"); the model of
# the response minus the offset ("shifted"); for RESET, the nested F test of
# the model with its offset, from anova() ("nested"); or main's value on the
# stored design. These are the oracles of tests/testthat/test-model-context.R.

# The test a call computes.
identity_base_fn <- function(fn) {
  # Increment 7: a streaming test in chunks of another size is that test.
  if (fn %in% names(identity_streaming_chunked)) {
    return(identity_streaming_base(fn))
  }
  if (fn %in% identity_studentized) {
    return("performStudentizedBPTest")
  }
  if (fn %in% identity_quantile) {
    return("performQuantileRegressionTest")
  }
  if (fn %in% identity_reset) {
    return("performRESETTest")
  }
  # The test_factory entry of the spatial test calls it as the registered
  # spatial test does, with the weights of `data` and the default number of
  # permutations, from the same seed.
  if (identical(fn, "test_factory spatial_hetero")) {
    return("registry spatial_hetero")
  }
  # runPanelTests() with one of its tests sees the cell as that test does.
  if (identical(fn, identity_panel_runner_bp)) {
    return("performBPRandomEffectsTest")
  }
  if (identical(fn, identity_panel_runner_pesaran)) {
    return("performPesaranTest")
  }
  # Increment 6: the registered tests computed from the model alone.
  # And the call with a constant, the same formula written with the number.
  registered <- c(
    `registry cook_weisberg` = "performCookWeisbergTest", `registry ncv` = "performNCVTest",
    `registry spread_level` = "performSpreadLevelTest",
    `registry variance_form` = "performVarianceFormTest",
    `performCookWeisbergTest(var_formula = ~ I(x1^k))` =
      "performCookWeisbergTest(var_formula = ~ I(x1^2))"
  )
  if (fn %in% names(registered)) {
    return(registered[[fn]])
  }
  fn
}

# The cell whose value the call must reproduce, as the test sees the cell
# (context_cell_for(), helper-model-context.R).
identity_chain_reference <- function(fn, cell) {
  if (is.null(cell$block)) {
    return(NULL)
  }
  base <- identity_base_fn(fn)
  if (base %in% c(context_migrated, context_second_formula)) {
    context_cell_for(base, cell)$reference
  } else {
    cell$reference
  }
}

# The references outside the matrix that the call must reproduce.
identity_external_ids <- function(fn, cell) {
  if (is.null(cell$block) || !identical(cell$form, "data_frame")) {
    return(character())
  }
  ids <- character()
  if (identical(cell$block, "terms") && cell$terms %in% names(context_precomputed()$formulas)) {
    ids <- c(ids, paste("precomputed", cell$fit, cell$terms, sep = "/"))
  }
  if (identical(cell$block, "fit") && !identical(cell$offset, "none")) {
    ids <- c(ids, if (fn %in% identity_reset) {
      paste("nested", cell$fit, cell$offset, sep = "/")
    } else {
      paste("shifted", cell$fit, sep = "/")
    })
  }
  ids
}

# The external reference of a cell of Harrison-McCabe's rank (N4): the test
# written out with the rank of the fit (context_hmc_by_hand()).
identity_rank_reference <- function(fn, cell) {
  if (identical(fn, "performHarrisonMcCabeTest") && startsWith(cell$id, "rank/")) {
    sub("^rank/", "byhand/", cell$id)
  } else {
    character()
  }
}

# The external reference of Pesaran's test on an unbalanced panel (N12),
# directly and through runPanelTests(): plm::pcdtest() on the cell
# (identity_plm_cd()). It comes first among the references of the call, so
# that a value is verified against plm wherever plm can be run on the cell.
identity_plm_reference <- function(fn, cell) {
  if (fn %in% identity_pesaran && "N12" %in% cell$defect) {
    paste0("plm/", cell$id)
  } else {
    character()
  }
}

# Pesaran's CD of plm::pcdtest(test = "cd") on the Pearson residuals of a
# fit, a pseries indexed by the units and periods of its observations, which
# plm takes as the residuals of a pooled model (its help page: a pseries is
# tested as the pooled regression of the variable on a constant). The units
# and periods are read from the rows of `data` that carry the names of the
# residuals, when `data` is a data frame with such rows, whose response is
# that of the fit; on another cell the reference is skipped, and the call is
# verified through the cell it reproduces.
identity_plm_cd <- function(m, d) {
  testthat::skip_if_not_installed("plm")
  r <- stats::residuals(m, type = "pearson")
  if (!is.data.frame(d) || inherits(d, c("tbl_df", "data.table")) || is.null(names(r)) ||
        !all(names(r) %in% rownames(d))) {
    testthat::skip("the rows of `data` do not carry the names of the residuals")
  }
  rows <- d[names(r), , drop = FALSE]
  response <- stats::fitted(m) + stats::residuals(m, type = "response")
  if (!isTRUE(all.equal(unname(response), rows$y, tolerance = 1e-10))) {
    testthat::skip(paste("the rows of `data` that carry the names of the residuals are not",
                         "those of the fit"))
  }
  pd <- plm::pdata.frame(data.frame(id = rows$id, time = rows$time, e = unname(r)),
                         index = c("id", "time"))
  reference <- suppressWarnings(plm::pcdtest(pd$e, test = "cd"))
  list(statistic = c(z = unname(reference$statistic)), parameter = NULL,
       p.value = reference$p.value)
}

# RESET of a fit with an offset written out: anova() of the fit, with its
# offset and weights, against the fit augmented with the squares and cubes of
# its fitted values, X b + o, with the same offset and weights.
identity_nested_reset <- function(m, d) {
  ref <- d
  ref$f2 <- stats::fitted(m)^2
  ref$f3 <- stats::fitted(m)^3
  weights_of_fit <- stats::weights(m)
  ref$wt <- if (is.null(weights_of_fit)) rep(1, nrow(d)) else weights_of_fit
  restricted <- stats::lm(y ~ x1 + x2, data = ref, weights = wt, offset = o)
  augmented <- stats::lm(y ~ x1 + x2 + f2 + f3, data = ref, weights = wt, offset = o)
  a <- stats::anova(restricted, augmented)
  list(statistic = c(F = a$F[2]), parameter = c(df1 = a$Df[2], df2 = a$Res.Df[2]),
       p.value = a$`Pr(>F)`[2])
}

# --- run -----------------------------------------------------------------------

identity_run <- function(where, out, root = ".") {
  if (identical(where, "source")) {
    suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
  } else {
    .libPaths(c(where, .libPaths()))
    suppressPackageStartupMessages(library(heteroTests, lib.loc = where))
  }
  invisible(ht_set_log_level("SILENT"))
  source(file.path(root, "tests", "testthat", "helper-test-recipes.R"), local = globalenv())
  source(file.path(root, "tests", "testthat", "helper-model-context.R"), local = globalenv())
  cells <- context_cells()
  # IDENTITY_CELLS=k runs the first k cells only, to try the harness out.
  first <- as.integer(Sys.getenv("IDENTITY_CELLS", unset = NA))
  if (!is.na(first)) {
    cells <- utils::head(cells, first)
  }
  recipes <- c(contract_recipes[setdiff(names(contract_recipes), "performBoxMTest")],
               identity_extra_recipes)
  records <- list()
  # The call is run when `outcome` is forced, with its id in identity_sz$id,
  # so that the tracer of .szroeter_test() files what the test read under it
  # (identity_szroeter_capture()).
  add <- function(kind, fn, cell, outcome) {
    id <- paste(kind, fn, cell$id, sep = " | ")
    identity_sz$id <- id
    force(outcome)
    identity_sz$id <- NULL
    records[[id]] <<- c(
      list(id = id, kind = kind, fn = fn, cell = cell$id, form = cell$form,
           defect = cell$defect, expect = cell$expect,
           reference = identity_chain_reference(fn, cell),
           externals = c(identity_plm_reference(fn, cell), identity_external_ids(fn, cell),
                         identity_rank_reference(fn, cell))),
      outcome,
      list(szroeter = identity_szroeter_by_hand(identity_sz$captured[[id]]))
    )
    identity_sz$captured[[id]] <- NULL
  }
  identity_szroeter_trace(TRUE)
  on.exit(identity_szroeter_trace(FALSE), add = TRUE)
  started <- Sys.time()
  run_cell <- function(cell, built, fns, recipe_kind, runner_kind) {
    # The data.name the runner gives a test that names its argument "model"
    # (identity_data_names()), for the runner records of every kind; a cell on
    # the stored design has the formula of its cell.
    add("reference", "formula", cell,
        identity_outcome(function() deparse(stats::formula(built$m))))
    for (fn in fns) {
      add(recipe_kind, fn, cell, identity_outcome(function() {
        set.seed(7)
        recipes[[fn]](built$m, built$data)
      }))
    }
    add(runner_kind, "runHeteroTests", cell, identity_outcome(function() {
      set.seed(7)
      identity_runner_summary(
        runHeteroTests(built$m, built$data, use_cache = FALSE, progress = FALSE)
      )
    }))
    # Increment 7: the runner with the streaming tests, asked for on data of
    # any size, in chunks of 10,000 and of 7.
    for (streaming in names(identity_streaming_runners)) {
      add(runner_kind, streaming, cell, identity_outcome(function() {
        set.seed(7)
        identity_runner_summary(runHeteroTests(
          built$m, built$data, tests = identity_streaming_runner_tests, use_cache = FALSE,
          chunk_threshold_mb = 0, chunk_size = identity_streaming_runners[[streaming]],
          progress = FALSE
        ))
      }))
    }
    add(runner_kind, identity_registry_runner, cell, identity_outcome(function() {
      testthat::skip_if_not_installed("quantreg")
      set.seed(7)
      identity_runner_summary(runHeteroTests(
        built$m, built$data, tests = identity_registry_tests, use_cache = FALSE,
        progress = FALSE
      ))
    }))
    add(runner_kind, identity_registry_runner_szroeter, cell, identity_outcome(function() {
      set.seed(7)
      identity_runner_summary(runHeteroTests(
        built$m, built$data, tests = "szroeter", use_cache = FALSE, progress = FALSE
      ))
    }))
    add(runner_kind, identity_registry_runner_spatial, cell, identity_outcome(function() {
      if (is.null(built$data)) {
        testthat::skip("the runner gives the spatial test no weights without `data`")
      }
      data <- identity_with_listw(built$m, built$data)
      set.seed(7)
      identity_runner_summary(runHeteroTests(
        built$m, data, tests = "spatial_hetero", use_cache = FALSE, progress = FALSE
      ))
    }))
    add(runner_kind, identity_registry_runner6, cell, identity_outcome(function() {
      set.seed(7)
      identity_runner_summary(runHeteroTests(
        built$m, built$data, tests = identity_registry_tests6, use_cache = FALSE,
        progress = FALSE
      ))
    }))
  }
  for (cell in cells) {
    built <- suppressWarnings(suppressMessages(cell$build()))
    run_cell(cell, built, names(recipes), "recipe", "runner")
    # N2: White's test written out on the cell, the reference of its changes.
    add("reference", "white_by_hand", cell,
        identity_outcome(function() identity_white_by_hand(built$m)))
  }
  # Decision 1: the cells of a poly() term on the stored design.
  identity_trace(TRUE)
  for (cell in cells) {
    if ("decision1" %in% cell$defect) {
      stored <- suppressWarnings(suppressMessages(cell$build(x = TRUE)))
      run_cell(cell, stored, intersect(names(recipes), identity_affected),
               "stored", "stored_runner")
    }
  }
  identity_trace(FALSE)
  # The references of the verification (identity_verify()): the term cells
  # with their columns computed beforehand, the fits with an offset as the
  # model of the response minus the offset, and RESET of those fits written
  # out as the nested F test of anova().
  # Increment 5: the cut() term drops the row at the minimum of x1, whose data
  # the panel tests and the scatter diagnostic refused (B16); they give the
  # value of the fit with the factor computed beforehand.
  # Increment 7: the streaming tests, whose cut() terms stopped on the breaks
  # of the rows they were given, and whose terms computed from the rows of a
  # chunk give the values of the columns computed beforehand (B6).
  verified_fns <- intersect(
    names(recipes),
    c(identity_increment2, identity_increment3, identity_increment5, identity_increment7)
  )
  reference_cell <- function(id) list(id = id, form = "data_frame", defect = character())
  # N12: plm::pcdtest() on each unbalanced cell (identity_plm_cd()), the
  # reference of Pesaran's test, of runPanelTests() with Pesaran's test alone,
  # and of runPanelTests() with both tests, whose random-effects test is the
  # one on the same cell.
  for (cell in cells) {
    if (!"N12" %in% cell$defect) {
      next
    }
    built <- suppressWarnings(suppressMessages(cell$build()))
    plm_id <- reference_cell(paste0("plm/", cell$id))
    add("reference", "performPesaranTest", plm_id,
        identity_outcome(function() identity_plm_cd(built$m, built$data)))
    add("reference", identity_panel_runner_pesaran, plm_id, identity_outcome(function() {
      list(pesaran = identity_plm_cd(built$m, built$data))
    }))
    add("reference", "runPanelTests", plm_id, identity_outcome(function() {
      list(bp_random = performBPRandomEffectsTest(built$m, built$data, "id"),
           pesaran = identity_plm_cd(built$m, built$data))
    }))
  }
  for (fit in c("ols", "wls")) {
    for (terms in names(context_precomputed()$formulas)) {
      built <- suppressWarnings(suppressMessages(context_precomputed_fit(fit, terms)))
      run_cell(reference_cell(paste("precomputed", fit, terms, sep = "/")), built, verified_fns,
               "reference", "reference_runner")
    }
  }
  d <- context_data()
  shifted <- d
  shifted$y <- d$y - d$o
  for (fit in context_fits) {
    built <- suppressWarnings(suppressMessages(list(m = context_model(shifted, fit),
                                                    data = shifted)))
    run_cell(reference_cell(paste("shifted", fit, sep = "/")), built, verified_fns,
             "reference", "reference_runner")
    for (offset in c("argument", "term")) {
      nested <- identity_outcome(function() identity_nested_reset(context_model(d, fit, offset), d))
      for (fn in intersect(verified_fns, identity_reset)) {
        add("reference", fn, reference_cell(paste("nested", fit, offset, sep = "/")), nested)
      }
    }
  }
  # The cells of Harrison-McCabe's rank (N4), run through every recipe, and
  # their reference, the test written out with the rank of the fit.
  for (cell in context_rank_cells()) {
    built <- suppressWarnings(suppressMessages(cell$build()))
    run_cell(cell, built, names(recipes), "recipe", "runner")
    add("reference", "white_by_hand", cell,
        identity_outcome(function() identity_white_by_hand(built$m)))
    add("reference", "performHarrisonMcCabeTest",
        reference_cell(sub("^rank/", "byhand/", cell$id)),
        identity_outcome(function() context_hmc_by_hand(built$m, built$data)))
  }
  # The cells of the follow-up of N2, through every recipe, with White's test
  # written out on each.
  for (cell in identity_white_cells()) {
    built <- suppressWarnings(suppressMessages(cell$build()))
    run_cell(cell, built, names(recipes), "recipe", "runner")
    add("reference", "white_by_hand", cell,
        identity_outcome(function() identity_white_by_hand(built$m)))
  }
  # The examples of the help pages, each expression whose value is a test, or
  # that calls the function of its page: the scatter diagnostic returns its
  # correlations, and runPanelTests() a list of tests (increment 5).
  scratch <- dirname(normalizePath(out, mustWork = FALSE))
  for (topic in c("performWhiteTestStreaming", "performBPTestStreaming",
                  "performKoenkerTestStreaming", "performWhiteTest", "performBPTest",
                  "performKoenkerTest",
                  "performRankPermutationTest", "performHighDimensionalTest",
                  "performStudentizedBPTest", "performBPTestRobust", "performRESETTest",
                  "performGQTest", "performQuantileRegressionTest", "performGlejserTest",
                  "performParkTest", "performSzroeterTest", "performHarrisonMcCabeTest",
                  "performHartleyFmaxTest", "performSpatialHeteroTest", "performLeveneTest",
                  "performBrownForsytheTest", "performFlignerKilleenTest", "performBartlettTest",
                  "performOBrienTest", "performPesaranTest", "performBPRandomEffectsTest",
                  "performScatterDiagnostic", "runPanelTests", "performSpearmanTest",
                  "performHarveyTest", "performArchLMTest", "performMcLeodLiTest",
                  "performCookWeisbergTest", "performNCVTest", "performSpreadLevelTest",
                  "performDavidianCarrollTest", "performVarianceFormTest", "fitWLS",
                  "runTimeSeriesTests")) {
    example_file <- file.path(scratch, paste0(topic, "-examples.R"))
    tools::Rd2ex(file.path(root, "man", paste0(topic, ".Rd")), example_file)
    expressions <- parse(example_file)
    unlink(example_file)
    env <- new.env(parent = globalenv())
    set.seed(1)
    for (k in seq_along(expressions)) {
      # The expression runs here, before add(), under the id of its record.
      identity_sz$id <- paste("example", topic, paste("expression", k), sep = " | ")
      outcome <- identity_outcome(function() eval(expressions[[k]], env))
      identity_sz$id <- NULL
      calls_topic <- is.call(expressions[[k]]) &&
        identical(deparse(expressions[[k]][[1L]]), topic)
      if (inherits(outcome$value, "htest") || calls_topic || is.list(outcome$value) &&
            isTRUE(outcome$value$.error)) {
        add("example", topic, list(id = paste("expression", k), defect = character()), outcome)
      }
      # N3: the exact test of an example of a streaming test, the same call
      # without `chunk_size` and `progress` (identity_n3_reference()).
      if (calls_topic && topic %in% names(identity_n3_exact)) {
        exact_call <- expressions[[k]]
        exact_call[[1L]] <- as.name(identity_n3_exact[[topic]])
        exact_call$chunk_size <- NULL
        exact_call$progress <- NULL
        add("reference", identity_n3_exact[[topic]],
            list(id = paste("example", topic, paste("expression", k)), defect = character()),
            identity_outcome(function() eval(exact_call, env)))
      }
    }
  }
  identity_sz$captured <- list()
  meta <- list(
    where = where,
    version = as.character(utils::packageVersion("heteroTests")),
    path = find.package("heteroTests"),
    r = R.version.string,
    seconds = as.numeric(difftime(Sys.time(), started, units = "secs"))
  )
  saveRDS(list(meta = meta, records = records), out)
  message(sprintf("%d calls in %.0f s, heteroTests %s from %s, written to %s",
                  length(records), meta$seconds, meta$version, meta$path, out))
}

# --- compare -------------------------------------------------------------------

identity_numbers <- function(x) {
  if (is.list(x)) {
    return(unlist(lapply(x, identity_numbers), use.names = FALSE))
  }
  if (is.numeric(x)) as.double(x) else numeric()
}

identity_relative <- function(a, b) {
  if (length(a) != length(b) || length(a) == 0L) {
    return(if (length(a) == length(b)) 0 else Inf)
  }
  ok <- is.finite(a) & is.finite(b)
  if (!identical(is.finite(a), is.finite(b)) || !identical(a[!ok], b[!ok])) {
    return(Inf)
  }
  scale <- pmax(abs(a[ok]), abs(b[ok]), .Machine$double.xmin)
  max(c(0, abs(a[ok] - b[ok]) / scale))
}

identity_classify <- function(before, after) {
  a <- before$value
  b <- after$value
  failed <- function(v) is.list(v) && isTRUE(v$.error)
  if (identical(a, b)) {
    return(list(change = "identical", detail = ""))
  }
  if (failed(a) && failed(b)) {
    return(list(change = "message_change", detail = b$message))
  }
  if (failed(a)) {
    return(list(change = "error_to_value", detail = a$message))
  }
  if (failed(b)) {
    return(list(change = "value_to_error", detail = b$message))
  }
  # The scatter diagnostic returns its correlations as a named vector, its
  # statistic.
  statistic_of <- function(v) if (is.list(v)) v$statistic else v
  statistic <- identity_relative(identity_numbers(statistic_of(a)),
                                 identity_numbers(statistic_of(b)))
  everything <- identity_relative(identity_numbers(a), identity_numbers(b))
  list(change = "value_change", statistic = statistic, everything = everything,
       detail = sprintf("statistic %.2g, all numbers %.2g", statistic, everything))
}

# Decision 1 for one call: "identical" or "allowed" when the branch is main's
# value on the stored design, "unexpected" when it is not, NULL when main has
# no such value for it (it refused the call on the stored design too, or the
# runner fell back on another test) and the other rules decide.
identity_stored_verdict <- function(record, verdict, after, stored) {
  # A call that stopped or was skipped, as the runner of the spatial test is
  # without `data`, has no value on the stored design.
  failed <- function(v) is.list(v) && (isTRUE(v$.error) || isTRUE(v$.skip))
  if (is.null(stored) || failed(stored$value) || failed(after$value)) {
    return(NULL)
  }
  same <- if (identical(record$kind, "runner")) {
    # The tests of the battery that main ran on the stored design, not the
    # fallback it put under their names when one stopped.
    own <- names(stored$value)[vapply(stored$value, function(e) is.null(e$status), logical(1))]
    if (length(own) == 0L) {
      return(NULL)
    }
    if (!identical(after$value[own], stored$value[own])) {
      return("unexpected")
    }
    # The tests it fell back on have no stored-design value of the tests they
    # stand for: the rules of the runner decide the call.
    if (length(own) < length(stored$value)) {
      return(NULL)
    }
    TRUE
  } else {
    identical(after$value, stored$value)
  }
  if (!isTRUE(same)) {
    return("unexpected")
  }
  if (identical(verdict$change, "identical")) "identical" else "allowed"
}

# The defects of a cell that concern the call: those of increment 2 apply to
# the calls of the tests they concern only (`scope`).
identity_defects <- function(record, scope = identity_defect_scope) {
  defects <- record$defect
  if (record$fn %in% identity_null_routes && identical(record$form, "null")) {
    defects <- c(defects, "null")
  }
  # Every call of Szroeter's test, on any cell (identity_szroeter_verdict()).
  if (record$fn %in% identity_szroeter_calls) {
    defects <- c(defects, "szroeter_moments")
  }
  # Increment 6. A second formula reads its variables from `data` or the
  # model frame, never from the workspace (R3), on every cell; and on the
  # cells whose `data` the guard refuses, or whose rows, in the order of the
  # fit, tie (the tibble of a fit to repeated rows), the alignment refuses
  # what was read by position (B9).
  if (record$fn %in% c(identity_second_formula, identity_workspace)) {
    defects <- c(defects, "R3")
    if (identical(record$expect, "guard") || identical(record$cell, "ident/repeated_fit_tibble")) {
      defects <- c(defects, "B9")
    }
    # N17 for a second formula: the residuals of an mgcv::gam() fit carry no
    # names, and Cook-Weisberg's test matched `data` to them by position, or
    # refused the full data of a gam that dropped rows for their number of
    # rows; it now matches them by the row names of its model frame.
    if (identical(record$cell, "panel/unit_gam_full")) {
      defects <- c(defects, "N17")
    }
    # Decision 8: z missing at an observation of the fit.
    if (identical(record$cell, "variance/z_missing")) {
      defects <- c(defects, "decision8")
    }
    # A constant of the workspace, which Cook-Weisberg's test refused with
    # `data`.
    if (record$fn %in% identity_constant) {
      defects <- c(defects, "constant")
    }
  }
  # Decision 31: the calls that read nothing from `data` match it through the
  # guard, on every cell with `data`.
  if (record$fn %in% identity_decision31 && !identical(record$form, "null")) {
    defects <- c(defects, "decision31")
  }
  keep <- vapply(defects, function(d) is.null(scope[[d]]) || record$fn %in% scope[[d]],
                 logical(1))
  defects[keep]
}

# Decision 1 for a test that refitted the formula on each segment (N1): the
# branch must be main's value but for rounding. "identical" or "allowed", or
# "unexpected" beyond `identity_last_bits`; NULL when one of the two stopped,
# and the other rules decide.
identity_last_bits_verdict <- function(verdict) {
  if (identical(verdict$change, "identical")) {
    return("identical")
  }
  if (!identical(verdict$change, "value_change")) {
    return(NULL)
  }
  if (isTRUE(verdict$everything <= identity_last_bits)) "allowed" else "unexpected"
}

# `stored` is main's value on the stored design, for a cell of decision 1, or
# NULL. `n2` is identity_n2() of the call and `n2_mode` identity_n2_mode(): a
# change of White's test on a cell of N2 is allowed when its rule verifies it,
# and is unexpected otherwise, whatever the other rules say.
identity_allow <- function(record, verdict, after, stored = NULL, n2 = NULL, n2_mode = NULL) {
  if (identical(n2_mode, "refusal")) {
    return(identity_n2_refusal(record, after))
  }
  if (!is.null(n2_mode)) {
    ok <- switch(n2_mode,
      check = identity_n2_check(n2, record, after, stored),
      fallback = identity_n2_fallback(n2, record, after),
      chain = verdict$change %in% c("value_change", "error_to_value") &&
        identity_n2_chain(n2, record, after)
    )
    return(if (isTRUE(ok)) "allowed" else "unexpected")
  }
  defects <- identity_defects(record)
  # Decision 1: a value on a poly() term must be main's value on the stored
  # design, identical(), whether or not it is main's on its rebuilt one; for a
  # test that refitted the segments from the formula, or built the design of
  # each chunk from it (increment 7), main's value but for rounding.
  if ("decision1" %in% defects && record$fn %in% identity_affected) {
    decided <- if (record$fn %in% identity_segment_refits && "N1" %in% defects) {
      identity_last_bits_verdict(verdict)
    } else if (record$fn %in% identity_rebuilt_per_chunk) {
      if (identity_same_tests(record, after)) identity_last_bits_verdict(verdict)
    } else {
      identity_stored_verdict(record, verdict, after, stored)
    }
    if (!is.null(decided)) {
      return(decided)
    }
  }
  if (identical(verdict$change, "identical")) {
    return("identical")
  }
  if (!record$fn %in% identity_affected) {
    return("unexpected")
  }
  bootstrap <- record$fn %in% identity_bootstraps
  # A message that changes only to the function's own documented refusal.
  if (identical(verdict$change, "message_change")) {
    message_defects <- identity_defects(
      list(defect = defects, fn = record$fn), identity_message_scope
    )
    own <- unlist(identity_messages[intersect(message_defects, names(identity_messages))])
    if (bootstrap && any(defects %in% identity_bootstrap_defects)) {
      own <- c(own, identity_bootstrap_messages)
    }
    if (length(own) > 0L &&
          any(vapply(own, grepl, logical(1), x = after$value$message, fixed = TRUE))) {
      return("allowed")
    }
    if (identical(record$kind, "recipe") && identity_m1(record, record, after)) {
      return("allowed")
    }
    if (identical(record$kind, "recipe") && identity_m1_panel(record, after)) {
      return("allowed")
    }
    if (identity_rows(record, record, after)) {
      return("allowed")
    }
    return("unexpected")
  }
  # A new refusal must be the refusal of its defect.
  if (identical(verdict$change, "value_to_error")) {
    own <- unlist(identity_new_refusals[intersect(defects, identity_allowed$value_to_error)])
    if (length(own) > 0L &&
          any(vapply(own, grepl, logical(1), x = after$value$message, fixed = TRUE))) {
      return("allowed")
    }
    return("unexpected")
  }
  if (identical(record$kind, "runner")) {
    if (verdict$change %in% c("value_change", "error_to_value") &&
          any(defects %in% identity_runner_allowed)) {
      return("allowed")
    }
    return("unexpected")
  }
  allowed <- if (bootstrap) identity_bootstrap_allowed else identity_allowed
  if (any(defects %in% allowed[[verdict$change]])) {
    return("allowed")
  }
  "unexpected"
}

# What a call returns that its verification compares: every number of the
# result, the statistic, parameter and p-values and also the estimates, the
# replicates and the robust details, with the names of its parts; of the test
# inside a comparison with a reference implementation; or of each test of a
# runner, with the test it ran and its status. The metadata test_factory adds
# (`test_metadata`) and the labels that name the call (`method`, `data.name`)
# are left out, so that a route compares with the test it computes. `fields`
# restricts the comparison to some parts, for a reference that has no others.
identity_key <- function(value, fields = NULL) {
  # The correlations of the scatter diagnostic, one per variable.
  if (is.numeric(value) && is.null(fields)) {
    return(list(numbers = as.double(value), labels = names(value)))
  }
  if (!is.list(value) || isTRUE(value$.error) || isTRUE(value$.skip)) {
    return(NULL)
  }
  if (is.list(value$ours)) {
    value <- value$ours
  }
  core <- function(v) {
    v <- v[setdiff(names(v), c("test_metadata", "method", "data.name"))]
    if (!is.null(fields)) {
      v <- v[intersect(fields, names(v))]
    }
    v
  }
  if (!is.null(value$statistic)) {
    part <- core(value)
    return(list(numbers = identity_numbers(part), labels = names(part)))
  }
  parts <- Filter(function(e) is.list(e) && !is.null(e$statistic), value)
  if (length(parts) == 0L || length(parts) != length(value)) {
    return(NULL)
  }
  list(
    numbers = unlist(lapply(parts, function(e) identity_numbers(core(e))), use.names = FALSE),
    labels = vapply(parts, function(e) paste(c(e$diagnostic, e$status), collapse = " "), "")
  )
}

identity_same_key <- function(a, b, tolerance) {
  if (is.null(a) || is.null(b) || !identical(a$labels, b$labels) ||
        length(a$numbers) != length(b$numbers)) {
    return(FALSE)
  }
  if (tolerance == 0) {
    return(identical(a$numbers, b$numbers))
  }
  isTRUE(identity_relative(a$numbers, b$numbers) <= tolerance)
}

# The reference that verifies the value the branch gives a call, or NA: main's
# value on the stored design, a reference outside the matrix (to 1e-10, 1e-8
# for a poly() basis), or the call on the cell it must reproduce,
# `identical()` (1e-10 for the rank-deficient fit and the gam, as in the
# matrix), when that call is unchanged from main or verified in turn.
identity_verify <- function(record, main, branch, seen = character()) {
  key <- identity_key(branch[[record$id]]$value)
  if (is.null(key)) {
    return(NA_character_)
  }
  stored_id <- sub("^runner \\| ", "stored_runner | ", sub("^recipe \\| ", "stored | ", record$id))
  stored <- main[[stored_id]]
  if (!is.null(stored) && !is.null(identity_key(stored$value)) &&
        identical(branch[[record$id]]$value, stored$value)) {
    return("stored design")
  }
  reference_kind <- if (identical(record$kind, "runner")) "reference_runner" else "reference"
  for (external in record$externals) {
    reference <- branch[[paste(reference_kind, record$fn, external, sep = " | ")]]
    # Harrison-McCabe written out from the fit's own decomposition agrees with
    # the test, which decomposes the design again in the order of `order_by`,
    # to 1e-8 relative on a design with a column at 1e-9 of the others, the
    # precision to which either decomposition fixes that direction
    # (context_hmc_by_hand()).
    tolerance <- if (grepl("/poly$", external)) {
      1e-8
    } else if (startsWith(external, "byhand/")) {
      1e-6
    } else {
      1e-10
    }
    # The nested F test written out, Harrison-McCabe's test written out with
    # the rank of the fit and plm's Pesaran CD (N12) have the statistic, the
    # degrees of freedom or the sizes of the parts, and the p-value only.
    fields <- if (startsWith(external, "nested/") || startsWith(external, "byhand/") ||
                    startsWith(external, "plm/")) {
      c("statistic", "parameter", "p.value")
    }
    if (!is.null(reference) &&
          identity_same_key(identity_key(branch[[record$id]]$value, fields),
                            identity_key(reference$value, fields), tolerance)) {
      return(sub("/.*$", "", external))
    }
  }
  # A route gives the value of the test it computes, on the same cell: a route
  # with checks of its own, as test_factory has, may refuse the cell the test
  # reproduces and accept this one.
  base <- identity_base_fn(record$fn)
  base_id <- paste("recipe", base, record$cell, sep = " | ")
  if (identical(record$kind, "recipe") && !identical(base, record$fn) && !base_id %in% seen &&
        !is.null(branch[[base_id]]) &&
        identity_same_key(key, identity_key(branch[[base_id]]$value), 0)) {
    if (identical(main[[base_id]]$value, branch[[base_id]]$value)) {
      return("test")
    }
    deeper <- identity_verify(main[[base_id]], main, branch, c(seen, base_id))
    if (!is.na(deeper)) {
      return(paste("test", deeper, sep = ", "))
    }
  }
  reference <- record$reference
  if (is.null(reference) || reference %in% seen) {
    return(NA_character_)
  }
  reference_id <- paste(record$kind, record$fn, reference, sep = " | ")
  before <- main[[reference_id]]
  after <- branch[[reference_id]]
  tolerance <- if (record$cell %in% identity_rounded_cells) {
    1e-10
  } else if (record$cell %in% identity_located_cells) {
    identity_located_tolerance
  } else {
    0
  }
  if (is.null(after) || !identity_same_key(key, identity_key(after$value), tolerance)) {
    return(NA_character_)
  }
  if (identical(before$value, after$value)) {
    return("cell")
  }
  deeper <- identity_verify(before, main, branch, c(seen, record$cell))
  if (is.na(deeper)) NA_character_ else paste("cell", deeper, sep = ", ")
}

identity_compare <- function(main_file, branch_file, csv = NULL) {
  main <- readRDS(main_file)
  branch <- readRDS(branch_file)
  ids <- union(names(main$records), names(branch$records))
  missing_ids <- setdiff(ids, intersect(names(main$records), names(branch$records)))
  if (length(missing_ids) > 0L) {
    stop("The two runs have different corpora: ",
         paste(utils::head(missing_ids, 5L), collapse = "; "), call. = FALSE)
  }
  # The records on the stored design, and those of the verification, are
  # references, not calls.
  stored_kinds <- c("stored", "stored_runner", "reference", "reference_runner")
  ids <- ids[!vapply(main$records[ids], function(r) r$kind %in% stored_kinds, logical(1))]
  # The data.name of the runners, set aside (identity_data_names()).
  data_names <- identity_data_names(main$records, branch$records)
  main$records <- data_names$main
  branch$records <- data_names$branch
  stored_id <- function(id) {
    sub("^runner \\| ", "stored_runner | ", sub("^recipe \\| ", "stored | ", id))
  }
  rows <- lapply(ids, function(id) {
    before <- main$records[[id]]
    after <- branch$records[[id]]
    stored <- main$records[[stored_id(id)]]
    verdict <- identity_classify(before, after)
    decision1 <- "decision1" %in% before$defect && before$fn %in% identity_affected
    by_segments <- decision1 && (before$fn %in% identity_segment_refits &&
      "N1" %in% identity_defects(before) ||
      before$fn %in% identity_rebuilt_per_chunk && identity_same_tests(before, after))
    # The streaming tests have no stored-design value on main (above).
    decided <- if (decision1 && !by_segments && !before$fn %in% identity_rebuilt_per_chunk) {
      identity_stored_verdict(before, verdict, after, stored)
    }
    rounded <- if (by_segments) identity_last_bits_verdict(verdict)
    checked <- !is.null(decided)
    n2 <- identity_n2(before, main$records)
    n2_mode <- identity_n2_mode(before$fn, n2, verdict$change)
    # A change of a Szroeter call that is the new null and nothing else is
    # allowed, verified by its reference; any other is judged by the rules
    # above, as before.
    szroeter <- identity_szroeter_verdict(before, after, verdict)
    # N3: a change of a streaming test that its rule verifies is allowed,
    # whatever the rules of decision 1 and of N2 say; one it does not verify is
    # judged by the other rules.
    n3 <- identity_n3_verdict(before, after, verdict, branch$records)
    n3_allowed <- !is.null(n3) && !is.na(n3$verdict)
    if (n3_allowed) {
      n2_mode <- NULL
    }
    allowed <- if (isTRUE(szroeter) || n3_allowed) {
      "allowed"
    } else {
      identity_allow(before, verdict, after, stored, n2, n2_mode)
    }
    n2_change <- !is.null(n2_mode) && !identical(allowed, "identical")
    # An allowed change to a value must be verified against a reference; one
    # of N2 is, by identity_n2_check() or identity_n2_fallback(), against
    # White's test written out on the centred design, and by "chain" against
    # the same call on the reference of the cell; one of Szroeter's null
    # against h and the moments written out from its design.
    to_value <- identical(allowed, "allowed") &&
      verdict$change %in% c("error_to_value", "value_change")
    verified <- if (to_value && isTRUE(szroeter)) {
      "Q from h and the design's moments"
    } else if (to_value && n3_allowed) {
      if (identical(n3$verdict, "last bits")) "main, last bits" else "exact test"
    } else if (to_value && isTRUE(n2_mode %in% c("check", "fallback"))) {
      "lm(e^2 ~ Z_c)"
    } else if (to_value) {
      identity_verify(before, main$records, branch$records)
    } else {
      NA
    }
    if (to_value && is.na(verified)) {
      allowed <- "unexpected"
      verdict$detail <- paste("not verified against a reference:", verdict$detail)
    }
    # N12: a value of Pesaran's test on an unbalanced panel that main refused
    # must be plm's, on the cell or on the cell it reproduces.
    if (to_value && !is.na(verified) && identical(verdict$change, "error_to_value") &&
          "N12" %in% identity_defects(before) && !grepl("plm", verified, fixed = TRUE)) {
      allowed <- "unexpected"
      verdict$detail <- paste("not verified against plm:", verdict$detail)
    }
    data_name <- if (id %in% names(data_names$verdicts)) {
      data_names$verdicts[[id]]
    } else {
      NA_character_
    }
    if (identical(data_name, "unexpected")) {
      allowed <- "unexpected"
      verdict$detail <- paste("a data.name is not the formula of the model;", verdict$detail)
    } else if (identical(data_name, "formula") && identical(allowed, "identical")) {
      allowed <- "allowed"
      verdict$change <- "data_name"
    }
    data.frame(
      id = id, kind = before$kind, fn = before$fn, cell = before$cell,
      # The defects of the cell that concern the call (identity_defects()),
      # N2 where White's count of the cell was wrong, and N3 where the rule of
      # the streaming tests allowed the change.
      defect = paste(c(identity_defects(before), if (n2_change) "N2", if (n3_allowed) "N3"),
                     collapse = " "),
      n2 = if (n2_change) paste(n2$kinds, collapse = " ") else NA_character_,
      n2_mode = if (n2_change) n2_mode else NA_character_,
      szroeter = if (is.null(szroeter)) NA else szroeter,
      n3 = if (is.null(n3)) NA_character_ else n3$verdict,
      n3_branch_exact = if (is.null(n3)) NA_real_ else n3$branch_exact,
      n3_main_exact = if (is.null(n3)) NA_real_ else n3$main_exact,
      change = verdict$change,
      verdict = allowed,
      verified_by = if (to_value) verified else NA_character_,
      statistic_relative = if (is.null(verdict$statistic)) NA_real_ else verdict$statistic,
      everything_relative = if (is.null(verdict$everything)) NA_real_ else verdict$everything,
      stored_checked = checked,
      stored_identical = checked && !identical(decided, "unexpected"),
      last_bits_checked = !is.null(rounded),
      # Whether the branch returned a value, not an error or a skip.
      with_value = !(is.list(after$value) &&
                       (isTRUE(after$value$.error) || isTRUE(after$value$.skip))),
      # "formula" when a data.name of the runner changed, to the formula of the
      # model; "none" when none did; NA for a call that is not a runner's.
      data_name = data_name,
      warnings_differ = !identical(before$warnings, after$warnings),
      detail = substr(gsub("\n", " ", verdict$detail), 1L, 160L),
      stringsAsFactors = FALSE
    )
  })
  calls <- do.call(rbind, rows)
  if (!is.null(csv)) {
    utils::write.csv(calls, csv, row.names = FALSE)
  }
  cat(sprintf("main:   heteroTests %s from %s, %d records in %.0f s\n", main$meta$version,
              main$meta$path, length(main$records), main$meta$seconds))
  cat(sprintf("branch: heteroTests %s from %s, %d records in %.0f s\n\n", branch$meta$version,
              branch$meta$path, length(branch$records), branch$meta$seconds))
  cat(sprintf("%-12s %6d\n", c("calls", "identical", "allowed", "unexpected"),
              c(nrow(calls), sum(calls$verdict == "identical"), sum(calls$verdict == "allowed"),
                sum(calls$verdict == "unexpected"))), sep = "")
  cat("\nChanges by kind and defect:\n")
  changed <- calls[calls$verdict != "identical", , drop = FALSE]
  if (nrow(changed) > 0L) {
    changed$calls <- 1L
    print(stats::aggregate(calls ~ verdict + change + defect, data = changed, FUN = sum),
          row.names = FALSE)
    cat("\nCalls by function:\n")
    print(table(changed$fn, changed$verdict))
  }
  checked <- calls[calls$stored_checked, , drop = FALSE]
  if (nrow(checked) > 0L) {
    changed_poly <- checked[checked$change == "value_change", , drop = FALSE]
    largest <- if (nrow(changed_poly) > 0L) max(changed_poly$statistic_relative) else 0
    template <- paste0(
      "\nDecision 1: %d poly() calls with a value checked against main on the stored design, ",
      "%d identical() to it and %d changed by N2 (below); %d differ from main's rebuilt ",
      "design, by at most %.2g relative on the statistic\n"
    )
    cat(sprintf(template, nrow(checked), sum(checked$stored_identical),
                sum(!is.na(checked$n2)), nrow(changed_poly), largest))
  }
  n2 <- calls[!is.na(calls$n2), , drop = FALSE]
  if (nrow(n2) > 0L) {
    cat(sprintf(paste0(
      "\nN2: %d calls of White's test change, %d allowed: by the kinds of their cells and how ",
      "they are verified (\"check\" and \"fallback\" against White's test written out on ",
      "the centred design to %.0e, \"refusal\" by the refusal of an auxiliary regression ",
      "that fits the squared residuals exactly, \"chain\" against the same call on the ",
      "reference of the cell):\n"
    ), nrow(n2), sum(n2$verdict == "allowed"), identity_n2_tolerance))
    n2$calls <- 1L
    print(stats::aggregate(calls ~ n2 + n2_mode + verdict, data = n2, FUN = sum),
          row.names = FALSE)
    cells <- stats::aggregate(cell ~ n2, data = n2, FUN = function(x) length(unique(x)))
    names(cells)[2L] <- "cells"
    print(cells, row.names = FALSE)
  }
  sz <- calls[!is.na(calls$szroeter), , drop = FALSE]
  if (nrow(sz) > 0L) {
    cat(sprintf(paste0(
      "\nszroeter_moments: %d calls of Szroeter's test change value; %d are the null of the ",
      "design and nothing else (every field but the statistic and the p-value identical(), ",
      "h, Q and the p-value those written out from the ordered, weighted design to %.0e), ",
      "and %d are judged by the other rules\n"
    ), nrow(sz), sum(sz$szroeter), identity_szroeter_tolerance, sum(!sz$szroeter)))
  }
  n3 <- calls[calls$fn %in% identity_n3_calls, , drop = FALSE]
  if (nrow(n3) > 0L) {
    changed_n3 <- n3[!is.na(n3$n3), , drop = FALSE]
    cat(sprintf(paste0(
      "\nN3: %d calls of the streaming tests, of the runner that streams them and of their help ",
      "pages, %d identical() to main; %d changes to a value, %d allowed by the rule of N3 (main's ",
      "value to %.0e on every number, \"last bits\", or the exact test's to %.0e and nearer to ",
      "it than main's, \"exact test\"):\n"
    ), nrow(n3), sum(n3$change == "identical"), nrow(changed_n3),
    sum(!is.na(changed_n3$n3) & changed_n3$verdict == "allowed"), identity_last_bits,
    identity_n3_tolerance))
    if (nrow(changed_n3) > 0L) {
      changed_n3$calls <- 1L
      changed_n3$n3[is.na(changed_n3$n3)] <- "not verified"
      changed_n3$group <- ifelse(changed_n3$kind == "runner", "runner",
                                 identity_streaming_base(changed_n3$fn))
      print(stats::aggregate(calls ~ group + change + n3 + verdict, data = changed_n3, FUN = sum),
            row.names = FALSE)
      bits <- changed_n3[changed_n3$n3 == "last bits", , drop = FALSE]
      exact <- changed_n3[changed_n3$n3 == "exact test", , drop = FALSE]
      cat(sprintf(paste0(
        "  last bits: at most %.2g relative on any number; exact test: the branch within %.2g of ",
        "it, main within %.2g where it had a value\n"
      ), if (nrow(bits) > 0L) max(bits$everything_relative, na.rm = TRUE) else 0,
      if (nrow(exact) > 0L) max(exact$n3_branch_exact, na.rm = TRUE) else 0,
      if (nrow(exact) > 0L) suppressWarnings(max(exact$n3_main_exact, na.rm = TRUE)) else 0))
    }
  }
  renamed <- calls[!is.na(calls$data_name) & calls$data_name == "formula", , drop = FALSE]
  runners <- calls[!is.na(calls$data_name), , drop = FALSE]
  if (nrow(runners) > 0L) {
    template <- paste0(
      "\ndata.name: %d of %d runner calls give a test the formula of the model as its ",
      "data.name where main gave \"model\", each verified against deparse(formula(model)); ",
      "in %d of them nothing else changes\n"
    )
    cat(sprintf(template, nrow(renamed), nrow(runners), sum(renamed$change == "data_name")))
  }
  # The runner records on the stored design and of the verification, which are
  # references rather than calls, are held to the same rule.
  held <- data_names$verdicts[!names(data_names$verdicts) %in% ids]
  if (length(held) > 0L) {
    template <- paste0(
      "data.name of the %d runner records on the stored design and of the verification: ",
      "%d from \"model\" to the formula of the model, %d unexpected\n"
    )
    cat(sprintf(template, length(held), sum(held == "formula"), sum(held == "unexpected")))
  }
  bad_references <- names(held)[held == "unexpected"]
  segments <- calls[calls$last_bits_checked, , drop = FALSE]
  if (nrow(segments) > 0L) {
    rounded <- segments[segments$change == "value_change", , drop = FALSE]
    largest <- if (nrow(rounded) > 0L) max(rounded$everything_relative) else 0
    valued <- segments[segments$with_value, , drop = FALSE]
    streaming <- segments$fn %in% identity_rebuilt_per_chunk
    template <- paste0(
      "Decision 1, terms rebuilt from the formula before (the segments of Goldfeld-Quandt, N1, ",
      "and the chunks of the streaming tests, increment 7): %d poly() calls (%d of the ",
      "streaming tests), %d of them with a value (%d of the streaming tests); of these %d ",
      "identical() to main and %d differing by at most %.2g relative on any number; the other ",
      "%d stop as on main\n"
    )
    cat(sprintf(template, nrow(segments), sum(streaming), nrow(valued),
                sum(valued$fn %in% identity_rebuilt_per_chunk),
                sum(valued$change == "identical"), nrow(rounded), largest,
                sum(!segments$with_value & segments$change == "identical")))
  }
  to_value <- calls[calls$verdict != "identical" &
                      calls$change %in% c("error_to_value", "value_change"), , drop = FALSE]
  if (nrow(to_value) > 0L) {
    verified <- to_value[!is.na(to_value$verified_by), , drop = FALSE]
    cat(sprintf(paste0(
      "\nChanges to a value: %d allowed and verified against a reference, %d allowed and ",
      "not verified (counted as unexpected)\n"
    ), nrow(verified), nrow(to_value) - nrow(verified)))
    if (nrow(verified) > 0L) {
      verified$calls <- 1L
      print(stats::aggregate(calls ~ verified_by, data = verified, FUN = sum), row.names = FALSE)
    }
  }
  cat(sprintf("\nCalls whose warnings differ (not counted above): %d\n",
              sum(calls$warnings_differ)))
  unexpected <- calls[calls$verdict == "unexpected", , drop = FALSE]
  if (nrow(unexpected) > 0L) {
    cat("\nUnexpected:\n")
    print(unexpected[, c("id", "change", "detail")], row.names = FALSE)
  }
  if (length(bad_references) > 0L) {
    cat("\nRunner references whose data.name is not the formula of the model:\n")
    cat(paste0("  ", bad_references), sep = "\n")
  }
  if (nrow(unexpected) > 0L || length(bad_references) > 0L) {
    quit(status = 1L)
  }
  invisible(calls)
}

if (identical(args[1L], "run")) {
  identity_run(args[2L], args[3L], root = if (length(args) > 3L) args[4L] else ".")
} else {
  identity_compare(args[2L], args[3L], if (length(args) > 3L) args[4L] else NULL)
}
