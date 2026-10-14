# 0.14.0 folds the three *Enhanced functions into their base names behind
# `enhanced = TRUE`. The base names keep their defaults, so no existing call
# changes its output, and the old names warn and return what the new form
# returns.

test_that("plotResidualsFittedEnhanced() warns and draws plotResidualsFitted(enhanced = TRUE)", {
  skip_if_not_installed("ggplot2")
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  expect_warning(
    old <- plotResidualsFittedEnhanced(m),
    "'plotResidualsFittedEnhanced' is deprecated.*plotResidualsFitted\\(model, enhanced = TRUE\\)",
    class = "deprecatedWarning"
  )
  new <- plotResidualsFitted(m, enhanced = TRUE)
  expect_identical(old$data, new$data)
  expect_identical(old$labels, new$labels)
  expect_identical(length(old$layers), length(new$layers))
  expect_identical(plotResidualsFitted(m)$labels$title, "Residuals vs Fitted")
})

test_that("plotDiagnosticSuiteEnhanced() warns and returns plotDiagnosticSuite(enhanced = TRUE)", {
  skip_if_not_installed("ggplot2")
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  expect_warning(
    old <- plotDiagnosticSuiteEnhanced(m),
    "'plotDiagnosticSuiteEnhanced' is deprecated",
    class = "deprecatedWarning"
  )
  new <- plotDiagnosticSuite(m, enhanced = TRUE)
  expect_identical(names(old), names(new))
  expect_identical(old$residuals_fitted$labels, new$residuals_fitted$labels)
  expect_identical(plotDiagnosticSuite(m)$residuals_fitted$labels$title, "Residuals vs Fitted")
})

test_that("checkModelEnhanced() warns and runs checkModel(enhanced = TRUE)", {
  m <- lm(mpg ~ wt, data = mtcars[1:6, ])
  expect_warning(
    expect_warning(
      out <- checkModelEnhanced(m),
      "Very few residual degrees of freedom"
    ),
    "'checkModelEnhanced' is deprecated",
    class = "deprecatedWarning"
  )
  expect_identical(out, m)
  expect_warning(checkModel(m, enhanced = TRUE), "Very few residual degrees of freedom")
  expect_silent(checkModel(m))
})
