# Evaluates a call of a name deprecated in 0.14.0 for what it returns, without
# its deprecation warning. Any other warning still reaches the test.
without_deprecation <- function(expr) {
  withCallingHandlers(
    expr,
    deprecatedWarning = function(w) invokeRestart("muffleWarning")
  )
}
