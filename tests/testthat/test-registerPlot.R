
library(testthat)
library(heteroTests)

skip_if_not_installed("ggplot2")

# Custom plot returns simple ggplot
custom_fun <- function(model) {
  ggplot2::ggplot(data.frame(fit=fitted(model), res=residuals(model)),
                  ggplot2::aes(fit, res)) +
    ggplot2::geom_point()
}

registerPlot("custom_plot", custom_fun)

test_that("registerPlot allows custom plotting", {
  model <- lm(y ~ x1 + x2, data = data_heterosced)
  plots <- runDiagnosticPlots(model, plots = c("custom_plot"))
  expect_named(plots, "custom_plot")
  expect_s3_class(plots[[1]], "ggplot")
})

# Clean up so other tests are unaffected
rm(list="custom_plot", envir = get(".plot_registry", asNamespace("heteroTests")))
