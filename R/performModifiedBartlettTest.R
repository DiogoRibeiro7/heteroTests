#' Bartlett compatibility alias
#'
#' Retains the historical `performModifiedBartlettTest()` entry point while
#' making explicit that the correction previously described as a separate
#' "modified Bartlett" procedure is the standard finite-sample correction
#' already used by Bartlett's chi-squared test.
#'
#' @param model A fitted [stats::lm] object.
#' @param data A [base::data.frame] used to fit `model` and containing `group`.
#' @param group Character scalar naming the grouping variable.
#'
#' @return An object of class \code{htest} exactly as returned by
#'   [performBartlettTest()].
#'
#' @details
#' The pre-0.7.1 implementation manually evaluated the standard corrected
#' Bartlett statistic. That correction is already part of Bartlett's classical
#' chi-squared test; it does not define a second inferential procedure.
#'
#' This function therefore delegates directly to [performBartlettTest()] so the
#' two names cannot drift statistically while preserving compatibility for
#' existing code. API consolidation is intentionally deferred until after the
#' package-wide validation passes.
#'
#' @references
#' Bartlett, M. S. (1937). Properties of sufficiency and statistical tests.
#' *Proceedings of the Royal Society of London A, 160*(901), 268-282.
#' <https://doi.org/10.1098/rspa.1937.0109>
#'
#' @section Validation:
#' Pass B asserts exact equality with [performBartlettTest()] and
#' [stats::bartlett.test()] across balanced and unbalanced group designs.
#'
#' @examples
#' data(mtcars)
#' mtcars$cyl <- factor(mtcars$cyl)
#' mod <- lm(mpg ~ wt, data = mtcars)
#' performModifiedBartlettTest(mod, mtcars, "cyl")
#'
#' @seealso
#' [performBartlettTest()] is the canonical entry point. Use
#' [performLeveneTest()] or [performBrownForsytheTest()] when normality is
#' questionable.
performModifiedBartlettTest <- function(model, data, group) {
  performBartlettTest(model, data, group)
}
