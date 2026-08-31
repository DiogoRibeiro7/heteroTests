#' Effect size calculations for heteroscedasticity diagnostics
#'
#' Converts chi-squared statistics produced by heteroscedasticity tests into
#' interpretable effect sizes such as Cramer's V, the phi coefficient, or an
#' eta-squared analogue. The helper also provides qualitative magnitude
#' descriptors and a brief interpretation string that can be surfaced to users.
#'
#' @param test_result Result object from a heteroscedasticity test (typically
#'   of class \code{htest}).
#' @param model Fitted model supplied to the diagnostic.
#' @param data Data frame containing the variables referenced in `model`.
#' @param type Effect size metric to compute. Supported options are
#'   `"cramers_v"`, `"phi"`, and `"eta_squared"`.
#'
#' @return A named list with elements `effect_size`, `magnitude`,
#'   `practical_significance`, `interpretation`, and `type`.
#'
#' @examples
#' data(mtcars)
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' result <- performWhiteTest(model, mtcars)
#' rcalculateEffectSize(result, model, mtcars)
#'
#' @export
rcalculateEffectSize <- function(test_result, model, data,
                                 type = c("cramers_v", "phi", "eta_squared")) {
  rvalidateModelInputs(model, test_name = "Effect size", min_obs = 5L)
  rvalidateDataInputs(data, min_obs = 5L)

  type <- match.arg(type)

  statistic <- test_result$statistic
  if (is.null(statistic) || length(statistic) == 0L) {
    stop("`test_result` must contain a `statistic` element.", call. = FALSE)
  }
  chi_sq <- as.numeric(statistic[[1L]])
  if (!is.finite(chi_sq)) {
    stop("Chi-squared statistic must be finite to compute effect sizes.", call. = FALSE)
  }

  df <- NA_real_
  if (!is.null(test_result$parameter)) {
    df <- suppressWarnings(as.numeric(test_result$parameter[[1L]]))
  }
  if (is.na(df) || df <= 0) {
    df <- max(1, length(stats::coef(model)) - 1L)
  }

  n <- nrow(data)
  if (is.na(n) || n <= 0) {
    stop("`data` must contain at least one observation.", call. = FALSE)
  }

  effect_size <- switch(type,
    cramers_v = {
      denom <- n * max(1, df)
      sqrt(max(0, chi_sq / denom))
    },
    phi = sqrt(max(0, chi_sq / max(1, n))),
    eta_squared = {
      denom <- chi_sq + n * max(1, df)
      if (denom == 0) {
        0
      } else {
        max(0, chi_sq / denom)
      }
    }
  )

  thresholds <- switch(type,
    cramers_v = c(small = 0.1, medium = 0.3, large = 0.5),
    phi = c(small = 0.1, medium = 0.3, large = 0.5),
    eta_squared = c(small = 0.01, medium = 0.06, large = 0.14)
  )

  magnitude <- if (!is.finite(effect_size)) {
    "unknown"
  } else if (effect_size < thresholds[["small"]]) {
    "negligible"
  } else if (effect_size < thresholds[["medium"]]) {
    "small"
  } else if (effect_size < thresholds[["large"]]) {
    "medium"
  } else {
    "large"
  }

  practical_significance <- is.finite(effect_size) && effect_size >= thresholds[["medium"]]
  interpretation <- if (!is.finite(effect_size)) {
    "Effect size could not be determined."
  } else {
    sprintf(
      "Effect size %.3f (%s) suggests %s practical impact.",
      effect_size,
      type,
      if (practical_significance) "meaningful" else "limited"
    )
  }

  list(
    effect_size = effect_size,
    magnitude = magnitude,
    practical_significance = practical_significance,
    interpretation = interpretation,
    type = type
  )
}
