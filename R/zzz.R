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
