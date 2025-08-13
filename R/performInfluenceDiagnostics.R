#' Detect influential observations via Cook's distance
#'
#' @param model Fitted model of class `lm`.
#' @param cutoff Threshold for Cook's distance. Defaults to `4/(n - p)`.
#' @return A list with Cook's distances and the indices deemed influential.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performInfluenceDiagnostics(m)
performInfluenceDiagnostics <- function(model, cutoff = NULL) {
  checkModel(model)
  n <- nobs(model)
  p <- length(coef(model))
  if (is.null(cutoff)) {
    cutoff <- 4 / (n - p)
  }
  cd <- cooks.distance(model)
  influential <- which(cd > cutoff)
  list(cooks_distance = cd, influential = influential, cutoff = cutoff)
}
