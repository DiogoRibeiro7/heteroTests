# The return contract: every exported perform*Test() returns an htest object
# with the fields documented on ?test_results, and no field that page does not
# document. One recipe per exported test, plus the settings that add fields,
# so that a new export, or a new field, fails here until it is documented.

contract_data <- function() {
  set.seed(20261015)
  n <- 120
  d <- data.frame(
    x1 = runif(n, 0.5, 2), x2 = rnorm(n),
    group = factor(rep(letters[1:4], length.out = n)),
    id = rep(1:12, each = 10), time = rep(1:10, 12)
  )
  d$y <- 1 + d$x1 - d$x2 + rnorm(n, sd = d$x1)
  list(d = d, m = lm(y ~ x1 + x2, data = d))
}

contract_recipes <- list(
  performWhiteTest = function(m, d) performWhiteTest(m, d),
  performWhiteTestStreaming = function(m, d) performWhiteTestStreaming(m, d, progress = FALSE),
  performWhiteTestRobust = function(m, d) performWhiteTestRobust(m, d),
  performBPTest = function(m, d) performBPTest(m, d),
  performBPTestStreaming = function(m, d) performBPTestStreaming(m, d, progress = FALSE),
  performBreuschPaganTest = function(m, d) performBreuschPaganTest(m, d),
  performBPTestRobust = function(m, d) performBPTestRobust(m, d),
  performGQTest = function(m, d) performGQTest(m, d, order_by = "x1"),
  performKoenkerTest = function(m, d) performKoenkerTest(m, d),
  performKoenkerTestStreaming = function(m, d) performKoenkerTestStreaming(m, d, progress = FALSE),
  performParkTest = function(m, d) performParkTest(m, d, "x1"),
  performStudentizedBPTest = function(m, d) performStudentizedBPTest(m, d),
  performWhiteTestBootstrap = function(m, d) performWhiteTestBootstrap(m, d, B = 19),
  performSzroeterTest = function(m, d) performSzroeterTest(m, d, order_by = "x1"),
  performHarrisonMcCabeTest = function(m, d) performHarrisonMcCabeTest(m, d, order_by = "x1"),
  performWildBootstrapTest = function(m, d) performWildBootstrapTest(m, d, B = 19, progress = FALSE),
  performQuantileRegressionTest = function(m, d) {
    testthat::skip_if_not_installed("quantreg")
    performQuantileRegressionTest(m, d)
  },
  performRankPermutationTest = function(m, d) {
    performRankPermutationTest(m, d, B = 19, progress = FALSE)
  },
  performHighDimensionalTest = function(m, d) performHighDimensionalTest(m, d),
  performSpatialHeteroTest = function(m, d) {
    testthat::skip_if_not_installed("spdep")
    set.seed(1)
    coords <- cbind(runif(nrow(d)), runif(nrow(d)))
    lw <- suppressWarnings(spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(coords, k = 4))))
    performSpatialHeteroTest(m, d, listw = lw, permutations = 19)
  },
  performSpearmanTest = function(m, d) performSpearmanTest(m, d),
  performLeveneTest = function(m, d) performLeveneTest(m, d, "group"),
  performGlejserTest = function(m, d) performGlejserTest(m, d, "x1"),
  performBrownForsytheTest = function(m, d) performBrownForsytheTest(m, d, "group"),
  performFlignerKilleenTest = function(m, d) performFlignerKilleenTest(m, d, "group"),
  performBartlettTest = function(m, d) performBartlettTest(m, d, "group"),
  performHarveyTest = function(m, d) performHarveyTest(m, d),
  performArchLMTest = function(m, d) performArchLMTest(m, d, lags = 2),
  performMcLeodLiTest = function(m, d) performMcLeodLiTest(m, d, lags = 5),
  performHartleyFmaxTest = function(m, d) performHartleyFmaxTest(m, d, "group"),
  performOBrienTest = function(m, d) performOBrienTest(m, d, "group"),
  performCookWeisbergTest = function(m, d) performCookWeisbergTest(m, d),
  performNCVTest = function(m, d) performNCVTest(m, d),
  performSpreadLevelTest = function(m, d) performSpreadLevelTest(m, d),
  performPesaranTest = function(m, d) performPesaranTest(m, d, "id", "time"),
  performBPRandomEffectsTest = function(m, d) performBPRandomEffectsTest(m, d, "id"),
  performDavidianCarrollTest = function(m, d) performDavidianCarrollTest(m, d),
  performVarianceFormTest = function(m, d) performVarianceFormTest(m, d),
  performRESETTest = function(m, d) performRESETTest(m),
  performBoxMTest = function(m, d) performBoxMTest(d[, c("y", "x2")], d$group)
)

# Settings that add a field or switch the statistic.
contract_variants <- list(
  performWhiteTestRobust = function(m, d) performWhiteTestRobust(m, d, bootstrap = TRUE, B = 19),
  performBPTestRobust = function(m, d) performBPTestRobust(m, d, bootstrap = TRUE, B = 19),
  performWhiteTest = function(m, d) performWhiteTest(m, d, cross_products = FALSE),
  performHarveyTest = function(m, d) performHarveyTest(m, d, studentize = TRUE),
  performGlejserTest = function(m, d) performGlejserTest(m, d, "x1", robust = TRUE),
  performCookWeisbergTest = function(m, d) performCookWeisbergTest(m, d, var_formula = ~x1),
  performWildBootstrapTest = function(m, d) {
    performWildBootstrapTest(m, d, B = 19, distribution = "mammen", progress = FALSE)
  }
)

# The fields ?test_results documents, with the tests allowed to return the
# ones only some tests add.
contract_core <- c("statistic", "parameter", "p.value", "method", "data.name")
contract_optional <- c("alternative", "estimate")
contract_streaming <- c(
  "performWhiteTestStreaming", "performBPTestStreaming", "performKoenkerTestStreaming"
)
contract_added <- list(
  p.value_bootstrap = c("performWhiteTestRobust", "performBPTestRobust"),
  robust_details = c("performWhiteTestRobust", "performBPTestRobust"),
  boot_statistics = "performWhiteTestBootstrap",
  bootstrap = "performWildBootstrapTest",
  permutation = "performRankPermutationTest",
  projection = "performHighDimensionalTest",
  variance_function = "performVarianceFormTest",
  quantiles = "performQuantileRegressionTest",
  se_type = "performQuantileRegressionTest",
  iid = "performQuantileRegressionTest",
  chunks_processed = contract_streaming,
  chunk_size = contract_streaming,
  r_squared = contract_streaming,
  res = "performSpatialHeteroTest"
)

contract_run <- function(recipe, m, d) {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  suppressMessages(suppressWarnings(recipe(m, d)))
}

is_named <- function(x) !is.null(names(x)) && all(nzchar(names(x)))

check_contract <- function(fn, r) {
  expect_s3_class(r, "htest")
  fields <- names(r)
  expect_true(all(c("statistic", "p.value", "method", "data.name") %in% fields), info = fn)

  added <- names(contract_added)[vapply(contract_added, function(f) fn %in% f, logical(1))]
  allowed <- c(contract_core, contract_optional, added)
  expect_identical(
    setdiff(fields, allowed), character(),
    info = paste(fn, "returns undocumented fields")
  )

  expect_true(is.numeric(r$statistic) && length(r$statistic) == 1L, info = fn)
  expect_true(is_named(r$statistic), info = paste(fn, "statistic is not named"))
  expect_true(is.finite(r$statistic), info = fn)

  if (!is.null(r$parameter)) {
    expect_true(is.numeric(r$parameter) && length(r$parameter) >= 1L, info = fn)
    expect_true(is_named(r$parameter), info = paste(fn, "parameter is not named"))
    expect_true(all(is.finite(r$parameter)), info = fn)
  }

  expect_true(is.numeric(r$p.value) && length(r$p.value) == 1L, info = fn)
  expect_true(r$p.value >= 0 && r$p.value <= 1, info = fn)
  expect_true(is.character(r$method) && length(r$method) == 1L && nzchar(r$method), info = fn)
  expect_true(is.character(r$data.name) && length(r$data.name) == 1L, info = fn)

  if (!is.null(r$alternative)) {
    expect_true(is.character(r$alternative) && length(r$alternative) == 1L, info = fn)
  }
  if (!is.null(r$estimate)) {
    expect_true(is.numeric(r$estimate), info = fn)
    labelled <- if (is.matrix(r$estimate)) {
      !is.null(rownames(r$estimate)) && !is.null(colnames(r$estimate))
    } else {
      is_named(r$estimate)
    }
    expect_true(labelled, info = paste(fn, "estimate is not labelled"))
  }
  if (!is.null(r$p.value_bootstrap)) {
    expect_true(length(r$p.value_bootstrap) == 1L, info = fn)
    p_boot <- r$p.value_bootstrap
    expect_true(is.na(p_boot) || (p_boot >= 0 && p_boot <= 1), info = fn)
  }
}

test_that("every exported test has a recipe here", {
  exported <- grep("^perform.*Test", getNamespaceExports("heteroTests"), value = TRUE)
  expect_setequal(names(contract_recipes), exported)
})

test_that("every exported test returns the documented fields", {
  s <- contract_data()
  for (fn in names(contract_recipes)) {
    r <- tryCatch(contract_run(contract_recipes[[fn]], s$m, s$d), skip = function(e) NULL)
    if (is.null(r)) next
    check_contract(fn, r)
  }
})

test_that("the settings that add fields keep to the contract", {
  s <- contract_data()
  for (fn in names(contract_variants)) {
    check_contract(fn, contract_run(contract_variants[[fn]], s$m, s$d))
  }
})

test_that("?test_results documents exactly the fields the contract allows", {
  root <- skip_if_not_source_tree()
  rd <- readLines(file.path(root, "man", "test_results.Rd"), warn = FALSE)
  # The label of each \item, up to the "}{" that opens its description.
  labels <- regmatches(rd, regexpr("^\\s*\\\\item\\{.*?\\}\\{", rd, perl = TRUE))
  code <- gregexpr("(?<=\\\\code\\{)[A-Za-z_.]+(?=\\})", labels, perl = TRUE)
  documented <- unique(unlist(regmatches(labels, code)))
  expect_setequal(documented, c(contract_core, contract_optional, names(contract_added)))
})
