.register_autoplot_methods <- function(pkgname) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return()
  }
  pkg_ns <- getNamespace(pkgname)
  base::registerS3method(
    "autoplot",
    "hetero_test_suite",
    get("autoplot.hetero_test_suite", envir = pkg_ns),
    envir = asNamespace("ggplot2")
  )
  base::registerS3method(
    "autoplot",
    "hetero_grouped_suite",
    get("autoplot.hetero_grouped_suite", envir = pkg_ns),
    envir = asNamespace("ggplot2")
  )
}

.onLoad <- function(libname, pkgname) {
  .register_autoplot_methods(pkgname)
  setHook(
    packageEvent("ggplot2", "onLoad"),
    function(...) .register_autoplot_methods(pkgname)
  )
}

# Column names referenced inside ggplot2 aes() and data.frame subsetting are
# resolved at evaluation time, so R CMD check reports them as undefined
# globals. Declaring them here keeps the check output focused on real problems.
utils::globalVariables(c(
  ".data",
  ".highlight",
  "abs_resid",
  "diagnostic",
  "effect_size",
  "influential",
  "model",
  "p.value",
  "power",
  "res_sqrt",
  "resid",
  "sigma_func_name"
))
