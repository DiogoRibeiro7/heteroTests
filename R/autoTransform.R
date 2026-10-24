#' Automatic transformation helper
#'
#' Chooses between no transformation, log, square root and Box--Cox
#' based on the Breusch-Pagan test statistic.
#'
#' @param model A fitted model of class `lm`.
#' @return A list with elements `model` (the transformed fit) and
#'   `method` indicating the chosen transformation.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' autoTransform(m)
autoTransform <- function(model) {
  checkModel(model)
  data <- model.frame(model)
  y <- model.response(data)
  terms_obj <- stats::terms(model)
  predictor_terms <- attr(terms_obj, "term.labels")
  has_intercept <- attr(terms_obj, "intercept") == 1L
  xform <- list(
    none = y,
    log = log(y),
    sqrt = sqrt(y)
  )
  if (all(y > 0)) {
    bc <- MASS::boxcox(formula(model), data = data, plotit = FALSE)
    lam <- bc$x[which.max(bc$y)]
    xform$boxcox <- if (abs(lam) < 1e-8) log(y) else (y^lam - 1) / lam
  }
  best <- NULL
  best_p <- -Inf
  # performBPTest() warns on each candidate fit with a term of more levels
  # than observations per level (rwarn_small_groups(), R/model_context.R);
  # the warning is given once, that of the fit chosen, or of the first
  # candidate that gave one when the chosen fit gave none.
  small_groups <- list()
  chosen_warning <- NULL
  for (nm in names(xform)) {
    df <- data
    df$ytrans <- xform[[nm]]
    df <- df[is.finite(df$ytrans), , drop = FALSE]
    formula_trans <- stats::reformulate(
      predictor_terms,
      response = "ytrans",
      intercept = has_intercept
    )
    fit <- lm(formula_trans, data = df)
    run <- rcache_capture_warnings(performBPTest(fit, df))
    p <- run$value$p.value
    small_groups <- c(small_groups, run$warnings)
    if (p > best_p) {
      best_p <- p
      best <- list(model = fit, method = nm, lambda = if (nm == "boxcox") lam else NA)
      chosen_warning <- run$warnings
    }
  }
  if (length(small_groups) > 0L) {
    once <- if (length(chosen_warning) > 0L) chosen_warning[1L] else small_groups[1L]
    rcache_resignal(list(warnings = once))
  }
  best
}
