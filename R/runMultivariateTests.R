#' Run multivariate heteroscedasticity tests
#'
#' Currently only Box's M test is supported. Additional tests can be
#' registered via `registerDiagnostic()` with custom names.
#'
#' @param data A numeric data frame or matrix.
#' @param group A factor of the same length as the number of rows in `data`.
#' @param tests Character vector of test names. Defaults to `"box_m"`.
#' @return A named list of `htest` objects.
#' @examples
#' data(iris)
#' runMultivariateTests(iris[, 1:4], iris$Species)
#' @export
runMultivariateTests <- function(data, group, tests = c("box_m")) {
  available <- as.list(.diagnostic_registry)
  invalid <- setdiff(tests, names(available))
  if (length(invalid) > 0) {
    stop("Unknown tests: ", paste(invalid, collapse = ", "))
  }
  lapply(tests, function(t) available[[t]](data, group))
}
