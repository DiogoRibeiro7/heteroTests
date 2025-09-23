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
    p <- performBPTest(fit, df)$p.value
    if (p > best_p) {
      best_p <- p
      best <- list(model = fit, method = nm, lambda = if (nm == "boxcox") lam else NA)
    }
  }
  best
}
