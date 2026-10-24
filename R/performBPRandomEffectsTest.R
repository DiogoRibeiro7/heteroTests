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
#'   residuals are analysed, the pooled `lm()` of the panel; no panel model
#'   class is needed, and the observations may be in any order.
#' @param data The data frame `model` was fitted to, holding the individual
#'   identifier named by `id`, or `NULL`, the default. The residuals and weights
#'   are read from the fit itself, and `id` from the rows of `data` that hold
#'   the observations of the fit. Without `data` it is read from the model
#'   frame of `model`, which holds it only when it is a variable of the model
#'   under its own name (`id`, not `factor(id)`); an identifier the model does
#'   not use needs `data`. Since 0.16.0, data that are given are matched to the
#'   observations of the fit: rows with row names of their own by name, in any
#'   order; rows that only number themselves (a tibble, a data.table, a data
#'   frame whose row names were reset) by the row numbers of the fit, or in the
#'   order of the fit when they hold one row per observation. The rows the fit
#'   dropped may be there or not. Data whose rows cannot be shown to hold the
#'   observations of the fit are refused; see [test_results].
#' @param id Character scalar naming the column in `data` that indexes individuals
#'   (or panels).
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
performBPRandomEffectsTest <- function(model, data = NULL, id) {
  test_label <- "Breusch-Pagan random effects test"
  # A model the tests do not take, and `data` that is not a data frame, are
  # refused first, with the messages of before 0.16.0.
  rrefuse_non_gaussian_glm(model, "performBPRandomEffectsTest")
  if (!is.null(data) && !is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  # The residuals and weights come from the fit; `data`, when given, is matched
  # to its observations and checked (R/model_context.R). `id` is read from the
  # rows of `data` that hold the observations, so data taken in the order of
  # the fit must tell those rows apart when it is not a column of the model
  # frame.
  ctx <- rmodel_context(
    model, data, fn = "performBPRandomEffectsTest", test_label = test_label,
    reads = if (!missing(id) && is.character(id)) id else character()
  )
  if (!is.null(data) && !id %in% names(data)) {
    stop("`id` must be a column in `data`.")
  }
  rrefuse_nonfinite_coefficients(model, test_label, allow_aliased = TRUE)
  rvalidate_model_context(ctx)

  # The units, one per observation in the order of the fit: from the
  # confirmed rows of `data`, or without `data` from the model frame when it
  # holds them under their own name; and the Pearson residuals of the fit,
  # which refuse a zero weight. Before 0.16.0 the i-th row of `data` was read
  # with the i-th residual, and data with row names of their own in another
  # order than the fit, with rows after those of the fit, or of a fit that
  # dropped rows, were refused. A fit with na.exclude is read as before
  # (rcontext_columns_at_fit()). A unit missing at an observation of the fit
  # leaves it out of the sums by unit, as before (decision 8 of the design of
  # the model context: each test keeps its behaviour).
  read <- rcontext_columns_at_fit(
    ctx, data, c(id = id), "performBPRandomEffectsTest", test_label
  )
  res <- read$residuals
  idfac <- factor(read$columns$id)
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
  # lower.tail = FALSE rather than 1 - pchisq(): the complement underflows
  # to exactly zero for large statistics, which loses the p-value's
  # magnitude. At LM = 182.9 the subtraction gives 0 where the upper tail
  # gives 1.1e-41, the value plm::plmtest() reports.
  p_value <- stats::pchisq(LM, df = 1, lower.tail = FALSE)

  structure(
    list(
      statistic = c(LM = LM),
      parameter = c(df = 1),
      p.value = p_value,
      method = "Breusch-Pagan LM test for random effects",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
