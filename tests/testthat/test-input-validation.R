# Uniform input validation: every exported test, called as in
# helper-test-recipes.R, meets the same bad input with the same error. The
# fixed set of bad inputs is a glm, a model with an NA coefficient, a data
# frame that lacks rows the model was fitted to, and one that lacks a
# variable of the model.
#
# Where a test still returns a value for one of these inputs, refusing it
# would change what the call returns, so it is listed below as pending, with
# the decision it waits for in ROADMAP.md, and is not asserted here.

validation_inputs <- function() {
  s <- contract_data()
  d <- s$d
  d$x1_twice <- 2 * d$x1
  list(
    glm = list(m = glm(y ~ x1 + x2, data = d), d = d),
    na_coefficient = list(m = lm(y ~ x1 + x2 + x1_twice, data = d), d = d),
    rows_missing = list(m = s$m, d = s$d[1:60, ]),
    variable_missing = list(m = s$m, d = s$d[, setdiff(names(s$d), "x2")])
  )
}

validation_messages <- c(
  glm = "needs a model fitted by stats::lm\\(\\)",
  na_coefficient = "Model coefficients must be finite",
  rows_missing = paste0(
    "requires `data` (to contain the rows used to fit the model|",
    "with [0-9]+ observations to match the fitted model)"
  ),
  variable_missing = "The following variables are missing from"
)

# A glm is refused only by the tests that cannot use one; the others accept
# it, and whether every test should refuse it is pending.
validation_glm_refused <- c(
  "performWhiteTestStreaming", "performHarrisonMcCabeTest", "performVarianceFormTest"
)

# Tests that return a value for the input today (pending).
validation_pending <- list(
  na_coefficient = c(
    "performSpreadLevelTest", "performPesaranTest", "performBPRandomEffectsTest",
    "performDavidianCarrollTest", "performRESETTest"
  ),
  rows_missing = c("performVarianceFormTest", "performPesaranTest"),
  variable_missing = c(
    "performPesaranTest", "performBPRandomEffectsTest", "performVarianceFormTest"
  )
)

validation_outcome <- function(recipe, m, d) {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  tryCatch(
    {
      suppressMessages(suppressWarnings(recipe(m, d)))
      "value"
    },
    error = function(e) conditionMessage(e),
    skip = function(e) NULL
  )
}

test_that("every exported test meets each bad input with the same error", {
  inputs <- validation_inputs()
  tests <- setdiff(names(contract_recipes), "performBoxMTest")
  for (input in names(inputs)) {
    checked <- if (input == "glm") {
      validation_glm_refused
    } else {
      setdiff(tests, validation_pending[[input]])
    }
    for (fn in checked) {
      outcome <- validation_outcome(contract_recipes[[fn]], inputs[[input]]$m, inputs[[input]]$d)
      if (is.null(outcome)) next
      expect_match(outcome, validation_messages[[input]], info = paste(fn, "on", input))
    }
  }
})

test_that("a model-only test checks the data it is given against the model", {
  s <- contract_data()
  m <- s$m
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  # The model frame holds the terms, not the variables, and is accepted.
  logged <- lm(log(y + 10) ~ x1 + x2, data = s$d)
  expect_identical(
    performSpearmanTest(logged, stats::model.frame(logged)),
    performSpearmanTest(logged)
  )
  # So are the data with rows the fit did not use, in any order.
  extra <- rbind(s$d, s$d[1:5, ])
  rownames(extra) <- c(rownames(s$d), paste0("extra", 1:5))
  expect_identical(performHarveyTest(m, extra[sample(nrow(extra)), ]), performHarveyTest(m))
  expect_error(
    performArchLMTest(m, s$d[-7, ], lags = 2),
    "ARCH LM requires `data` to contain the rows used to fit the model. Missing rows: 7"
  )
})
