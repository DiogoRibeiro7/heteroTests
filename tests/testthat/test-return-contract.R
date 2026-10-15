# The return contract: every exported perform*Test() returns an htest object
# with the fields documented on ?test_results, and no field that page does not
# document. One recipe per exported test, plus the settings that add fields,
# so that a new export, or a new field, fails here until it is documented.
# The recipes are in helper-test-recipes.R.

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
