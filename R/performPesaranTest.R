#' Perform Pesaran's CD test for cross-sectional dependence
#'
#' Implements the Pesaran (2004, 2015) cross-sectional dependence (CD) statistic
#' by averaging pairwise residual correlations across individuals within each
#' period of a panel dataset.
#'
#' @param model A fitted [stats::lm] object estimated on panel data, the pooled
#'   `lm()` of the panel.
#' @param data The data frame `model` was fitted to, holding the panel
#'   identifiers named by `id` and `time`, or `NULL`, the default. The
#'   residuals and weights are read from the fit itself, and `id` and `time`
#'   from the rows of `data` that hold the observations of the fit. Without
#'   `data` they are read from the model frame of `model`, which holds them
#'   only when they are variables of the model under their own names (`id`,
#'   not `factor(id)`); identifiers the model does not use need `data`. Since
#'   0.16.0, data that are given are matched to the observations of the fit:
#'   rows with row names of their own by name, in any order; rows that only
#'   number themselves (a tibble, a data.table, a data frame whose row names
#'   were reset) by the row numbers of the fit, or in the order of the fit when
#'   they hold one row per observation. The rows the fit dropped may be there
#'   or not. Data whose rows cannot be shown to hold the observations of the
#'   fit are refused; see [test_results].
#' @param id Character scalar naming the column that identifies individuals (the
#'   cross-sectional dimension).
#' @param time Character scalar naming the column that indexes time periods.
#'   Each pair of `id` and `time` must hold one observation of the fit: since
#'   0.16.0 a pair that holds more is refused.
#'
#' @return A \code{htest} object containing the standardised CD statistic and
#'   its two-sided p-value under the asymptotic standard normal reference
#'   distribution.
#'
#' @details
#' Residuals are reshaped into an \eqn{T \times N} matrix, where \eqn{T} and
#' \eqn{N} denote the number of time periods and individuals respectively. The CD
#' statistic is computed as
#' \deqn{\text{CD} = \sqrt{\frac{2T}{N (N - 1)}} \sum_{i = 1}^{N - 1}
#'   \sum_{j = i + 1}^{N} \hat{\rho}_{ij},}
#' where \eqn{\hat{\rho}_{ij}} is the sample correlation between residuals of
#' individuals \eqn{i} and \eqn{j}. Under the null hypothesis of cross-sectional
#' independence the statistic converges to the standard normal distribution as
#' both dimensions grow. Substantial positive (negative) values indicate pervasive
#' positive (negative) correlation across panels, motivating cluster-robust or
#' spatial corrections to inference. For small samples users should interpret the
#' statistic cautiously or consider small-sample adjustments available in the
#' literature.
#'
#' The statistic needs a balanced panel, every unit observed once in every
#' period: a pair of `id` and `time` that holds two observations of the fit is
#' refused (since 0.16.0), and on an unbalanced panel, as that of a fit that
#' dropped some rows of some units, it is not defined yet: the test stops, or,
#' when the number of periods of a unit divides that of the panel, returns a
#' wrong value computed on its residuals recycled over the periods it lacks. A
#' fit that drops every row of a unit keeps a balanced panel. The statistic of
#' an unbalanced panel is a separate change, on the roadmap.
#'
#' @references
#' Pesaran, M. H. (2004). General diagnostic tests for cross section dependence in
#' panels. *CESifo Working Paper Series No. 1229*.
#'
#' Pesaran, M. H. (2015). Testing weak cross-sectional dependence in large
#' panels. *Econometric Reviews, 34*(6-10), 1089–1117.
#' <https://doi.org/10.1080/07474938.2014.956623>
#'
#' @examples
#' df <- data.frame(
#'   id = rep(1:3, each = 5),
#'   time = rep(1:5, 3),
#'   x = runif(15),
#'   y = rnorm(15)
#' )
#' mod <- lm(y ~ x, data = df)
#' performPesaranTest(mod, df, "id", "time")
#'
#' # Panels with strong cross-sectional correlation yield large statistics
#' set.seed(789)
#' common_shock <- rnorm(5, sd = 0.5)
#' df$y <- rep(common_shock, each = 3) + rnorm(15, sd = 0.2)
#' mod_cs <- lm(y ~ x, data = df)
#' performPesaranTest(mod_cs, df, "id", "time")
#'
#' @seealso
#' [performBPRandomEffectsTest()] for related random-effects diagnostics and
#' [performSpatialHeteroTest()] when spatial dependence is suspected.
performPesaranTest <- function(model, data = NULL, id, time) {
  test_label <- "Pesaran CD test"
  # A model the tests do not take, and `data` that is not a data frame, are
  # refused first, with the messages of before 0.16.0.
  rrefuse_non_gaussian_glm(model, "performPesaranTest")
  if (!is.null(data) && !is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  # The residuals and weights come from the fit; `data`, when given, is matched
  # to its observations and checked (R/model_context.R). `id` and `time` are
  # read from the rows of `data` that hold the observations, so data taken in
  # the order of the fit must tell those rows apart when they are not columns
  # of the model frame.
  ctx <- rmodel_context(
    model, data, fn = "performPesaranTest", test_label = test_label,
    reads = c(
      if (!missing(id) && is.character(id)) id,
      if (!missing(time) && is.character(time)) time
    )
  )
  if (!is.null(data) && !all(c(id, time) %in% names(data))) {
    stop("`id` and `time` must be columns in `data`.")
  }
  rrefuse_nonfinite_coefficients(model, test_label, allow_aliased = TRUE)
  rvalidate_model_context(ctx)

  # The units and the periods, one per observation in the order of the fit:
  # from the confirmed rows of `data`, or without `data` from the model frame
  # when it holds them under their own names; and the Pearson residuals of the
  # fit, which refuse a zero weight. Before 0.16.0 the i-th row of `data` was
  # read with the i-th residual, and data with row names of their own in
  # another order than the fit, with rows after those of the fit, or of a fit
  # that dropped rows, were refused. A fit with na.exclude is read as before
  # (rcontext_columns_at_fit()).
  read <- rcontext_columns_at_fit(
    ctx, data, c(id = id, time = time), "performPesaranTest", test_label
  )
  if (!read$padded) {
    # A unit or a period missing at an observation of the fit, on which the
    # statistic stopped before, is refused with a message that names it; and
    # so is a pair of `id` and `time` that holds two observations of the fit,
    # which put one of them in the cell of another period (decision 13 of the
    # design of the model context).
    rstop_missing_variable(ctx, read$columns$id, id, "id", test_label)
    rstop_missing_variable(ctx, read$columns$time, time, "time", test_label)
    rstop_repeated_panel_cells(read$columns$id, read$columns$time, id, time,
                               "performPesaranTest")
  }
  df <- data.frame(id = read$columns$id, time = read$columns$time, res = read$residuals)
  df <- df[order(df$time, df$id), ]
  ids <- unique(df$id)
  times <- unique(df$time)
  N <- length(ids)
  T <- length(times)
  # Each unit fills a column of periods. A unit observed in fewer periods than
  # the panel holds, as in an unbalanced panel or the panel of a fit that
  # dropped rows, does not fit its column: the assignment stops ("number of
  # items to replace is not a multiple of replacement length"), or, when its
  # number of periods divides that of the panel, recycles its residuals. The
  # statistic of an unbalanced panel is a separate change (N12 of the design
  # of the model context).
  mat <- matrix(NA, nrow = T, ncol = N)
  for (i in seq_along(ids)) {
    mat[, i] <- df$res[df$id == ids[i]]
  }
  rho <- stats::cor(mat)
  cor_vals <- rho[upper.tri(rho)]

  # Pesaran (2004, 2015):
  #
  #   CD = sqrt( 2T / (N (N - 1)) ) * sum_{i<j} rho_ij
  #
  # which is asymptotically standard normal under cross-sectional
  # independence. Before 0.11.0 this read sqrt(N (N - 1) / (2T)) times the
  # *mean* correlation, which is the same quantity divided by T: the statistic
  # came out T times too small -- measured standard deviation 0.133 against
  # 1.067 for the published form at T = 8 -- so the test never rejected.
  pairs_used <- sum(!is.na(cor_vals))
  if (pairs_used == 0L) {
    stop("No usable pairwise residual correlations.", call. = FALSE)
  }
  CD <- sqrt(2 * T / (N * (N - 1))) * sum(cor_vals, na.rm = TRUE)
  # As above, the upper tail rather than its complement, so a large |CD|
  # does not collapse to a p-value of exactly zero.
  p_value <- 2 * stats::pnorm(abs(CD), lower.tail = FALSE)

  structure(
    list(
      statistic = c(z = CD),
      parameter = NULL,
      p.value = p_value,
      method = "Pesaran CD test for cross-sectional dependence",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
