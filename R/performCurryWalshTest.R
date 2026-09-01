#' Withdrawn Curry-Walsh spatial pseudo-test
#'
#' `performCurryWalshTest()` previously computed Moran's I on squared residuals
#' with inverse-distance weights and referred the raw statistic to a standard
#' normal distribution. Moran's I is not standardised, so the resulting test
#' never rejected.
#'
#' The function is retained so existing callers receive an explicit migration
#' error rather than silently obtaining a test with no power.
#'
#' @param model A fitted [stats::lm] object. Retained only for backward-compatible
#'   argument matching.
#' @param coords Two-column coordinate matrix. Retained only for
#'   backward-compatible argument matching.
#' @param ... Further arguments, ignored.
#'
#' @return This function does not return a test result. It signals an error with
#'   migration guidance.
#'
#' @details
#' Under the null, Moran's I has expectation \eqn{-1/(n-1)} and a variance
#' determined by the weights matrix; inference requires the standardised
#' \eqn{z = (I - E[I]) / \sqrt{\mathrm{Var}(I)}}, or a permutation reference.
#' The former implementation used `2 * (1 - pnorm(abs(I)))` on the raw statistic.
#' Because I is bounded near \eqn{\pm 1} it essentially never reaches 1.96: in
#' simulation the test rejected in 0.0% of samples with strong heteroscedasticity,
#' and on clustered variance it returned `I = 0.157` with `p = 0.875`.
#'
#' [performSpatialHeteroTest()] already provides this diagnostic correctly. It
#' applies `spdep::moran.mc()` to the squared residuals, which supplies a
#' permutation reference distribution instead of an unstandardised normal
#' approximation.
#'
#' @references
#' Moran, P. A. P. (1950). Notes on continuous stochastic phenomena.
#' *Biometrika, 37*(1/2), 17--23. <https://doi.org/10.2307/2332142>
#'
#' Anselin, L. (1995). Local indicators of spatial association -- LISA.
#' *Geographical Analysis, 27*(2), 93--115.
#'
#' @examples
#' \dontrun{
#' # Withdrawn: this signals an error with migration guidance.
#' performCurryWalshTest(model, coords)
#' }
#'
#' @seealso
#' [performSpatialHeteroTest()] for the corrected spatial diagnostic.
#'
#' @export
performCurryWalshTest <- function(model, coords, ...) {
  .Deprecated(msg = paste(
    "performCurryWalshTest() has been withdrawn: it referred an unstandardised",
    "Moran's I to a standard normal distribution, so it never rejected."
  ))
  stop(
    paste0(
      "No inferential result is returned. Moran's I must be centred on its null ",
      "expectation and scaled by its null standard deviation, or referred to a ",
      "permutation distribution. Use performSpatialHeteroTest(), which applies ",
      "spdep::moran.mc() to the squared residuals."
    ),
    call. = FALSE
  )
}
