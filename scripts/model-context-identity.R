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
#   - the default battery of runHeteroTests() on every cell, and the
#     registered names of increments 2 and 3 (`identity_registry_tests`,
#     `identity_registry_runner_szroeter`, `identity_registry_runner_spatial`);
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
# auxiliary regression with the intercept less one. Each run also records
# White's test written out on every cell (identity_white_by_hand()); a change
# of White's test is allowed only on a cell where main's count was wrong, and
# only when the branch gives that reference (identity_n2_check()).
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
  }
)
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
  names(identity_extra_recipes), "runHeteroTests", identity_registry_runner,
  identity_registry_runner3
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
# The calls of increment 4: the five group tests, which read the `group` of
# `data` besides the model. None of them is registered, in test_factory or in
# a runner, so their calls are the direct ones.
identity_increment4 <- c(
  "performLeveneTest", "performBrownForsytheTest", "performFlignerKilleenTest",
  "performBartlettTest", "performOBrienTest"
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
# tested and refuse, as every test built on the context does.
identity_allowed <- list(
  error_to_value = c("B12", "L1", "L2", "B14", "B15", "N7", "B7", "N1", "frame", "null"),
  value_change = c("B5", "N1", "N4", "N17", "gam_offset"),
  value_to_error = c("N6", "gam", "R4", "frame", "M8")
)
# The defects of increments 2, 3 and 4 apply to the calls of the tests they
# concern only: on any other call such a change is unexpected.
identity_defect_scope <- list(
  B7 = identity_increment2,
  N1 = c("performGQTest", identity_quantile, identity_registry_runner),
  frame = c(identity_increment2, identity_increment3, identity_increment4),
  N6 = identity_reset,
  null = c("registry student_bp", "registry szroeter"),
  gam = identity_increment2,
  N4 = "performHarrisonMcCabeTest",
  N17 = c(identity_reads_data, identity_increment4),
  gam_offset = c("registry szroeter", identity_registry_runner_szroeter),
  M8 = identity_increment4
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
  "B12", "L1", "L2", "B14", "B15", "N7", "B5", "B7", "N1", "frame", "gam_offset"
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
# `data` first. Any other change of a message is unexpected, but those of
# identity_m1() below.
identity_m8 <- c(
  "does not reproduce its residuals from its design and coefficients",
  "not those its coefficients were fitted with"
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
  M8 = identity_m8
)
identity_message_scope <- list(
  B7 = identity_increment2, L1 = c("performGQTest", identity_reads_data),
  L2 = c("performGQTest", identity_reads_data),
  frame = c(identity_increment2, identity_increment3, identity_increment4),
  null = c("registry student_bp", "registry szroeter"),
  gam = identity_increment2, B14 = c(identity_reads_data, identity_increment4),
  M8 = identity_increment4
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
  # fit (increment 4).
  M8 = identity_m8
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

# N2, outside the context (decision 22): White's degrees of freedom are the
# rank of its auxiliary regression with the intercept, [1, Z], minus one,
# where main took the rank of Z alone. A call of White's test may change on a
# cell where main's count was wrong: where the columns of Z span the constant,
# its df is one too many and the statistic must stay identical() to main's;
# where Z is so nearly collinear that main's decomposition without the
# intercept kept a column that lm() then aliased given the intercept (the gam
# with a smooth term), main's regression lost that column without its df
# losing it, and the statistic may change too, to the columns lm(e^2 ~ Z)
# keeps. Either way the branch must give White's test written out on the
# cell, identity_white_by_hand(): the statistic, the degrees of freedom and
# the p-value to `identity_n2_tolerance`, relative. The routes are those of
# the corpus that compute White's test: the test, its robust form, with and
# without the bootstrap, and the default battery of the runner, whose other
# tests must not change. The streaming test and the White bootstrap compute
# nothing that changes on these cells, and are not among them.
identity_white_routes <- c(
  "performWhiteTest", "performWhiteTestRobust", "performWhiteTestRobust(bootstrap = TRUE)",
  "runHeteroTests"
)
identity_n2_tolerance <- 1e-10

# White's test written out on a fit, without the package: the columns of the
# design the fit estimated, without the intercept, their squares and, for at
# most ten of them, their pairwise products, Z; the Pearson residuals; and
# lm(e^2 ~ Z), whose n R^2 is the statistic and whose rank less one the
# degrees of freedom. With them the ranks that say why main's count differed:
# of Z and of [1, Z] at the tolerance of qr(), and main's count, the number
# of columns its decomposition of Z kept, against the rank of the regression
# it fitted on them.
identity_white_by_hand <- function(m) {
  X <- stats::model.matrix(m)
  estimated <- !is.na(stats::coef(m))[colnames(X)]
  X <- X[, estimated, drop = FALSE]
  X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  p <- ncol(X)
  if (p == 0L) {
    stop("no regressors besides the intercept")
  }
  Z <- cbind(X, X^2)
  if (p > 1L && p <= 10L) {
    for (i in seq_len(p - 1L)) {
      for (j in seq.int(i + 1L, p)) Z <- cbind(Z, X[, i] * X[, j])
    }
  }
  e2 <- unname(stats::residuals(m, type = "pearson"))^2
  aux <- stats::lm(e2 ~ Z)
  statistic <- length(e2) * summary(aux)$r.squared
  df <- aux$rank - 1L
  qr_z <- qr(Z)
  kept <- sort(qr_z$pivot[seq_len(qr_z$rank)])
  list(
    statistic = c("X-squared" = statistic), parameter = c(df = df),
    p.value = stats::pchisq(statistic, df, lower.tail = FALSE),
    rank_z = qr_z$rank, rank_1z = qr(cbind(1, Z))$rank,
    main_df = length(kept), main_regression_df = stats::lm(e2 ~ Z[, kept])$rank - 1L
  )
}

# Why N2 concerns a call: "spans" or "lost" (above), from the reference of its
# cell, or NULL when it does not.
identity_n2 <- function(record, records) {
  if (!record$fn %in% identity_white_routes) {
    return(NULL)
  }
  ref <- records[[paste("reference", "white_by_hand", record$cell, sep = " | ")]]$value
  if (!is.list(ref) || isTRUE(ref$.error) || is.null(ref$rank_z)) {
    return(NULL)
  }
  kind <- if (ref$rank_z == ref$rank_1z) {
    "spans"
  } else if (ref$main_regression_df < ref$main_df) {
    "lost"
  }
  if (is.null(kind)) NULL else list(kind = kind, reference = ref)
}

# An N2 change of a call: the branch's White's test gives the statistic,
# degrees of freedom and p-value of the reference, with the statistic of
# main (on the stored design, where main has one) where Z spans the constant;
# a runner's other tests are those of main.
identity_n2_check <- function(n2, record, after, stored = NULL) {
  main_value <- if (!is.null(stored) && is.list(stored$value) && !isTRUE(stored$value$.error)) {
    stored$value
  } else {
    record$value
  }
  white_of <- function(v) {
    if (identical(record$kind, "runner")) {
      w <- v[["white"]]
      if (is.null(w) || !is.null(w$status)) NULL else w
    } else {
      v
    }
  }
  a <- white_of(main_value)
  b <- white_of(after$value)
  ref <- n2$reference
  if (is.null(a) || is.null(b) || is.null(b$statistic) || is.null(b$parameter)) {
    return(FALSE)
  }
  close <- function(x, y) {
    relative <- identity_relative(as.double(unname(x)), as.double(unname(y)))
    isTRUE(relative <= identity_n2_tolerance)
  }
  ok <- close(b$statistic, ref$statistic) &&
    identical(as.double(unname(b$parameter)), as.double(unname(ref$parameter))) &&
    close(b$p.value, ref$p.value)
  if (identical(n2$kind, "spans")) {
    ok <- ok && identical(unname(a$statistic), unname(b$statistic))
  }
  if (identical(record$kind, "runner")) {
    others <- setdiff(names(after$value), "white")
    ok <- ok && identical(main_value[others], after$value[others])
  }
  ok
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
      status = extras$status, issue = extras$issue
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
  fn
}

# The cell whose value the call must reproduce, as the test sees the cell
# (context_cell_for(), helper-model-context.R).
identity_chain_reference <- function(fn, cell) {
  if (is.null(cell$block)) {
    return(NULL)
  }
  base <- identity_base_fn(fn)
  if (base %in% context_migrated) context_cell_for(base, cell)$reference else cell$reference
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
  add <- function(kind, fn, cell, outcome) {
    id <- paste(kind, fn, cell$id, sep = " | ")
    records[[id]] <<- c(
      list(id = id, kind = kind, fn = fn, cell = cell$id, form = cell$form,
           defect = cell$defect, reference = identity_chain_reference(fn, cell),
           externals = c(identity_external_ids(fn, cell), identity_rank_reference(fn, cell))),
      outcome
    )
  }
  started <- Sys.time()
  run_cell <- function(cell, built, fns, recipe_kind, runner_kind) {
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
  verified_fns <- intersect(names(recipes), c(identity_increment2, identity_increment3))
  reference_cell <- function(id) list(id = id, form = "data_frame", defect = character())
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
  # The examples of the help pages, each expression whose value is a test.
  scratch <- dirname(normalizePath(out, mustWork = FALSE))
  for (topic in c("performWhiteTest", "performBPTest", "performKoenkerTest",
                  "performRankPermutationTest", "performHighDimensionalTest",
                  "performStudentizedBPTest", "performBPTestRobust", "performRESETTest",
                  "performGQTest", "performQuantileRegressionTest", "performGlejserTest",
                  "performParkTest", "performSzroeterTest", "performHarrisonMcCabeTest",
                  "performHartleyFmaxTest", "performSpatialHeteroTest", "performLeveneTest",
                  "performBrownForsytheTest", "performFlignerKilleenTest", "performBartlettTest",
                  "performOBrienTest")) {
    example_file <- file.path(scratch, paste0(topic, "-examples.R"))
    tools::Rd2ex(file.path(root, "man", paste0(topic, ".Rd")), example_file)
    expressions <- parse(example_file)
    unlink(example_file)
    env <- new.env(parent = globalenv())
    set.seed(1)
    for (k in seq_along(expressions)) {
      outcome <- identity_outcome(function() eval(expressions[[k]], env))
      if (inherits(outcome$value, "htest") || is.list(outcome$value) &&
            isTRUE(outcome$value$.error)) {
        add("example", topic, list(id = paste("expression", k), defect = character()), outcome)
      }
    }
  }
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
  statistic <- identity_relative(identity_numbers(a$statistic), identity_numbers(b$statistic))
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
# NULL. `n2` is identity_n2() of the call: a change to a value of White's
# test on a cell of N2 is allowed when identity_n2_check() verifies it, and
# is unexpected otherwise, whatever the other rules say.
identity_allow <- function(record, verdict, after, stored = NULL, n2 = NULL) {
  if (!is.null(n2) && identical(verdict$change, "value_change")) {
    return(if (identity_n2_check(n2, record, after, stored)) "allowed" else "unexpected")
  }
  defects <- identity_defects(record)
  # Decision 1: a value on a poly() term must be main's value on the stored
  # design, identical(), whether or not it is main's on its rebuilt one; for a
  # test that refitted the segments from the formula, main's value but for
  # rounding.
  if ("decision1" %in% defects && record$fn %in% identity_affected) {
    decided <- if (record$fn %in% identity_segment_refits && "N1" %in% defects) {
      identity_last_bits_verdict(verdict)
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
    # The nested F test written out, and Harrison-McCabe's test written out
    # with the rank of the fit, have the statistic, the degrees of freedom or
    # the sizes of the parts, and the p-value only.
    fields <- if (startsWith(external, "nested/") || startsWith(external, "byhand/")) {
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
  tolerance <- if (record$cell %in% identity_rounded_cells) 1e-10 else 0
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
  stored_id <- function(id) {
    sub("^runner \\| ", "stored_runner | ", sub("^recipe \\| ", "stored | ", id))
  }
  rows <- lapply(ids, function(id) {
    before <- main$records[[id]]
    after <- branch$records[[id]]
    stored <- main$records[[stored_id(id)]]
    verdict <- identity_classify(before, after)
    decision1 <- "decision1" %in% before$defect && before$fn %in% identity_affected
    by_segments <- decision1 && before$fn %in% identity_segment_refits &&
      "N1" %in% identity_defects(before)
    decided <- if (decision1 && !by_segments) {
      identity_stored_verdict(before, verdict, after, stored)
    }
    rounded <- if (by_segments) identity_last_bits_verdict(verdict)
    checked <- !is.null(decided)
    n2 <- identity_n2(before, main$records)
    n2_change <- !is.null(n2) && identical(verdict$change, "value_change")
    allowed <- identity_allow(before, verdict, after, stored, n2)
    # An allowed change to a value must be verified against a reference; one
    # of N2 is, by identity_n2_check(), against White's test written out.
    to_value <- identical(allowed, "allowed") &&
      verdict$change %in% c("error_to_value", "value_change")
    verified <- if (to_value && n2_change) {
      "lm(e^2 ~ Z)"
    } else if (to_value) {
      identity_verify(before, main$records, branch$records)
    } else {
      NA
    }
    if (to_value && is.na(verified)) {
      allowed <- "unexpected"
      verdict$detail <- paste("not verified against a reference:", verdict$detail)
    }
    data.frame(
      id = id, kind = before$kind, fn = before$fn, cell = before$cell,
      # The defects of the cell that concern the call (identity_defects()),
      # and N2 where White's count of the cell was wrong.
      defect = paste(c(identity_defects(before), if (n2_change) "N2"), collapse = " "),
      n2 = if (n2_change) n2$kind else NA_character_,
      change = verdict$change,
      verdict = allowed,
      verified_by = if (to_value) verified else NA_character_,
      statistic_relative = if (is.null(verdict$statistic)) NA_real_ else verdict$statistic,
      everything_relative = if (is.null(verdict$everything)) NA_real_ else verdict$everything,
      stored_checked = checked,
      stored_identical = checked && !identical(decided, "unexpected"),
      last_bits_checked = !is.null(rounded),
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
    template <- paste0(
      "\nN2: %d calls of White's test change, %d allowed and verified against lm(e^2 ~ Z) ",
      "to %.0e: %d on %d cells whose auxiliary columns span the constant, the statistic ",
      "identical() to main's, and %d on %d cells whose auxiliary regression lost a column ",
      "to its intercept on main, the statistic within %.2g relative of main's\n"
    )
    spans <- n2[n2$n2 == "spans", , drop = FALSE]
    lost <- n2[n2$n2 == "lost", , drop = FALSE]
    cat(sprintf(template, nrow(n2), sum(n2$verdict == "allowed"), identity_n2_tolerance,
                nrow(spans), length(unique(spans$cell)), nrow(lost), length(unique(lost$cell)),
                if (nrow(lost) > 0L) max(lost$statistic_relative) else 0))
  }
  segments <- calls[calls$last_bits_checked, , drop = FALSE]
  if (nrow(segments) > 0L) {
    rounded <- segments[segments$change == "value_change", , drop = FALSE]
    largest <- if (nrow(rounded) > 0L) max(rounded$everything_relative) else 0
    template <- paste0(
      "Decision 1, segments refitted from the formula before (N1): %d poly() calls with ",
      "a value, %d identical() to main, %d differing by at most %.2g relative on any number\n"
    )
    cat(sprintf(template, nrow(segments), sum(segments$change == "identical"), nrow(rounded),
                largest))
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
    quit(status = 1L)
  }
  invisible(calls)
}

if (identical(args[1L], "run")) {
  identity_run(args[2L], args[3L], root = if (length(args) > 3L) args[4L] else ".")
} else {
  identity_compare(args[2L], args[3L], if (length(args) > 3L) args[4L] else NULL)
}
