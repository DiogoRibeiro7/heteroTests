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
  # The distances of the observations of the fit, padded with NA at the rows
  # a fit with na.action = na.exclude dropped, as cooks.distance() of an lm
  # pads them: that of an mgcv::gam() fit with na.exclude recycled its padded
  # residuals against its leverages, with a warning, and gave other distances
  # (B13, from the review of increment 9 of the model context).
  cd <- stats::naresid(model$na.action, cooks.distance(rmodel_as_na_omit(model)))
  influential <- which(cd > cutoff)
  list(cooks_distance = cd, influential = influential, cutoff = cutoff)
}
