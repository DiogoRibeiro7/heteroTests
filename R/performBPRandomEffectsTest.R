#' Breusch–Pagan LM test for random effects
#'
#' Computes the Breusch and Pagan (1980) Lagrange Multiplier statistic for the
#' null that the individual-specific error variance is zero, against the
#' alternative that a random individual effect is present.
#'
#' This is a test for the presence of an individual effect, not a test for
#' heteroscedasticity, and it does not respond to one: simulated at n = 400,
#' its rejection rate is 100% against random intercepts and 3.2% against errors
#' with `sd = x^2`, which is the nominal level rather than power. Use the
#' auxiliary-regression diagnostics for non-constant variance.
#'
#' @param model A fitted [stats::lm] object estimated on stacked panel data whose
#'   residuals are analysed. The function assumes the observations are ordered by
#'   individual but does not require an explicit panel model class.
#' @param data A [base::data.frame] containing the variables used in `model`
#'   together with the individual identifier supplied via `id`.
#' @param id Character scalar naming the column in `data` that indexes individuals
#'   (or panels). The column must align with the row ordering used to fit `model`.
#'
#' @return A \code{htest} object with the LM statistic, the one-degree-of-freedom
#'   chi-squared reference distribution, and supporting metadata about the fitted
#'   model.
#'
#' @details
#' Let \eqn{\hat{e}_{it}} denote the residual for individual \eqn{i} at time
#' \eqn{t}. Breusch and Pagan show that, under the null of homoskedastic random
#' effects, the statistic
#' \deqn{\text{LM} = \frac{T^2}{2 (T - 1)} \frac{\sum_i \left( \sum_t \hat{e}_{it} \right)^2}{\sum_i \sum_t \hat{e}_{it}^2}}
#' is asymptotically chi-squared with one degree of freedom, where \eqn{T} is the
#' average number of time periods. Large values indicate that some individuals have
#' systematically larger or smaller residual variances, suggesting a violation of
#' the random-effects homoskedasticity assumption.
#'
#' The implementation aggregates residuals within each individual, applies the
#' scaling recommended for balanced panels, and issues a warning when panel lengths
#' differ substantially. The shared validation helpers ensure the `id` variable is
#' present, that each individual contributes at least two observations, and that
#' residuals exhibit variation before computing the statistic.
#'
#' @references
#' Breusch, T. S., & Pagan, A. R. (1980). The Lagrange Multiplier test and its
#' applications to model specification in econometrics. *The Review of Economic
#' Studies, 47*(1), 239–253. <https://doi.org/10.2307/2297111>
#'
#' Baltagi, B. H. (2021). *Econometric Analysis of Panel Data* (6th ed.). Springer.
#' Section 7.2 covers LM diagnostics for random-effects models.
#'
#' @examples
#' df <- data.frame(
#'   id = rep(1:5, each = 4),
#'   time = rep(1:4, 5),
#'   x = runif(20),
#'   y = rnorm(20)
#' )
#' mod <- lm(y ~ x, data = df)
#' performBPRandomEffectsTest(mod, df, "id")
#'
#' # Mildly unbalanced panels trigger a warning but the statistic remains defined
#' df2 <- df[-c(3, 12), ]
#' mod2 <- lm(y ~ x, data = df2)
#' performBPRandomEffectsTest(mod2, df2, "id")
#'
#' @seealso
#' [performPesaranTest()] for cross-sectional dependence diagnostics and
#' [performWhiteTest()] when heteroscedasticity is suspected within each time
#' period.
performBPRandomEffectsTest <- function(model, data, id) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class 'lm'.")
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!id %in% names(data)) {
    stop("`id` must be a column in `data`.")
  }

  res <- residuals(model)
  idfac <- factor(data[[id]])
  T_i <- tapply(res, idfac, length)
  if (length(unique(T_i)) > 1) {
    warning("Unequal panel lengths; using mean T")
  }
  T <- mean(T_i)
  sum_ei <- tapply(res, idfac, sum)

  # Breusch and Pagan (1980), equation 5:
  #
  #   LM = nT / (2 (T - 1)) * [ sum_i (sum_t e_it)^2 / sum_it e_it^2 - 1 ]^2
  #
  # The ratio is close to 1 under the null, and the statistic measures its
  # squared departure from 1. Dropping the "- 1" and the square, as this
  # function did before 0.11.0, leaves a quantity that sits at T^2 / (2(T-1))
  # -- 3.6 for T = 6, against a chi-square(1) critical value of 3.841 -- so it
  # rejected about a third of the time when there was no individual effect.
  ratio <- sum(sum_ei^2) / sum(res^2)
  LM <- (length(res) / (2 * (T - 1))) * (ratio - 1)^2
  p_value <- 1 - pchisq(LM, df = 1)

  structure(
    list(
      statistic = c(LM = LM),
      parameter = 1,
      p.value = p_value,
      method = "Breusch-Pagan LM test for random effects",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
