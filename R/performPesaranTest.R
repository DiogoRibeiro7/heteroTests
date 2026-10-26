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
#'   0.16.0 a pair that holds more is refused. The units need not be observed
#'   at the same periods (Details).
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
#' Each pair of `id` and `time` must hold one observation of the fit: since
#' 0.16.0 a pair that holds two is refused. On an unbalanced panel, whose units
#' are not all observed at the same periods, as that of a fit that dropped
#' some rows of some units, the statistic is computed, since 0.16.0, from the
#' periods each pair of units shares (Pesaran 2004, Section 9, equation 31),
#' as `plm::pcdtest()` computes it:
#' \deqn{\text{CD} = \sqrt{\frac{1}{P}} \sum_{i < j} \sqrt{T_{ij}}\,
#'   \hat{\rho}_{ij},}
#' where \eqn{T_{ij}} is the number of periods at which units \eqn{i} and
#' \eqn{j} are both observed, \eqn{\hat{\rho}_{ij}} the correlation of their
#' residuals over those periods, each centred at its mean over them, and
#' \eqn{P} the number of pairs that share at least two periods, the pairs of
#' the sum; \eqn{P = N (N - 1) / 2} when every pair does, and on a balanced
#' panel, where \eqn{T_{ij} = T}, the statistic is the one above. A pair that
#' shares fewer than two periods has no correlation and is left out, with a
#' warning, as `plm::pcdtest()` leaves it out; a panel in which no pair shares
#' two periods is refused. Pesaran gives the statistic its standard normal
#' limit for \eqn{T_{ij} > 3}. Before 0.16.0 the test stopped on an unbalanced
#' panel ("number of items to replace is not a multiple of replacement
#' length"), or, when the number of periods of a unit divided that of the
#' panel, returned a wrong value computed on its residuals recycled over the
#' periods it lacks. A fit with `na.action = na.exclude` is read as before,
#' by position, until a later step: its panel must be balanced in `data`.
#'
#' @references
#' Pesaran, M. H. (2004). General diagnostic tests for cross section dependence in
#' panels. *CESifo Working Paper Series No. 1229* (also *Cambridge Working
#' Papers in Economics* 0435). Section 9, "CD tests in the case of unbalanced
#' panels", equation 31.
#'
#' Pesaran, M. H. (2015). Testing weak cross-sectional dependence in large
#' panels. *Econometric Reviews, 34*(6-10), 1089–1117.
#' <https://doi.org/10.1080/07474938.2014.956623>
#'
#' Croissant, Y. and Millo, G. (2008). Panel data econometrics in R: the plm
#' package. *Journal of Statistical Software, 27*(2), 1–43.
#' <https://doi.org/10.18637/jss.v027.i02>. The statistic of an unbalanced
#' panel is that of `plm::pcdtest(test = "cd")` (plm 2.6-6), whose help page
#' says its tests are "suitable also for unbalanced panels".
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
  # A unit or a period missing at an observation, on which the statistic
  # stopped before, is refused with a message that names it; and a pair of
  # `id` and `time` that holds two observations, which put one of them in the
  # cell of another period, is refused (decision 13 of the design of the
  # model context), on every route, a fit with na.exclude included.
  rstop_missing_variable(ctx, read$columns$id, id, "id", test_label, ids = read$ids)
  rstop_missing_variable(ctx, read$columns$time, time, "time", test_label, ids = read$ids)
  rstop_repeated_panel_cells(read$columns$id, read$columns$time, id, time,
                             "performPesaranTest")
  # A fit with na.exclude is read by position until the step of the model
  # context that moves na.exclude (decision 29): its residuals are padded with
  # NA at the rows it dropped, and the panel of the rows of `data` it reads
  # must be balanced, as since the fifth step (N12, decision 28).
  if (read$padded) {
    rstop_unbalanced_panel(read$columns$id, read$columns$time, id, "performPesaranTest")
  }
  df <- data.frame(id = read$columns$id, time = read$columns$time, res = read$residuals)
  df <- df[order(df$time, df$id), ]
  ids <- unique(df$id)
  times <- unique(df$time)
  N <- length(ids)
  T <- length(times)
  if (nrow(df) < N * T) {
    # An unbalanced panel (N12): without repeated pairs, some unit lacks some
    # period. The statistic is computed from the periods each pair of units
    # shares, as plm::pcdtest() computes it (decided by the author on
    # 2026-10-24). Before 0.16.0 the matrix below stopped on such a panel, or
    # recycled the residuals of a unit over the periods it lacks; the fifth
    # step of the model context refused it until now.
    CD <- rpesaran_cd_unbalanced(read$residuals, read$columns$id, read$columns$time,
                                 "performPesaranTest")
  } else {
    # Each unit fills a column of periods, in the order of the periods.
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
    # *mean* correlation, which is the same quantity divided by T: the
    # statistic came out T times too small -- measured standard deviation
    # 0.133 against 1.067 for the published form at T = 8 -- so the test never
    # rejected.
    pairs_used <- sum(!is.na(cor_vals))
    if (pairs_used == 0L) {
      stop("No usable pairwise residual correlations.", call. = FALSE)
    }
    CD <- sqrt(2 * T / (N * (N - 1))) * sum(cor_vals, na.rm = TRUE)
  }
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

#' Pesaran's CD statistic of an unbalanced panel
#'
#' The statistic of an unbalanced panel of Pesaran (2004, Section 9, equation
#' 31), computed as `plm::pcdtest(test = "cd")` computes it (plm 2.6-6, its
#' internal `pcdres()`): the residuals of each pair of units are correlated
#' over the periods both are observed at, each centred at its mean over those
#' periods (`stats::cor(use = "pairwise.complete.obs")`), and the correlation
#' of a pair is weighted by the square root of the number of those periods,
#' `T_ij`:
#'
#'   CD = sqrt(1 / P) * sum_{i<j} sqrt(T_ij) rho_ij,
#'
#' over the P pairs that share at least two periods. P is N (N - 1) / 2 when
#' every pair does, and on a balanced panel, where T_ij = T, this is the
#' statistic [performPesaranTest()] computes on its matrix of periods by
#' units. A pair that shares one period or none has no correlation: plm leaves
#' it out of the sum and of P, with a warning, and so does this function; a
#' panel in which no pair shares two periods is refused. A correlation that is
#' not defined because the residuals of a unit are constant over the periods
#' of a pair adds nothing to the sum and stays in P, as on a balanced panel,
#' with a warning; plm returns `NA` there, on a balanced panel too.
#'
#' @param residuals The residuals, one per observation, without missing
#'   values.
#' @param id_values,time_values The units and the periods, one per
#'   observation, without missing values or repeated pairs.
#' @param fn Character scalar naming the calling test.
#' @return The statistic.
#' @keywords internal
#' @noRd
rpesaran_cd_unbalanced <- function(residuals, id_values, time_values, fn) {
  units <- sort(unique(id_values))
  periods <- sort(unique(time_values))
  n_units <- length(units)
  # The residuals in a matrix of periods by units, NA where a unit is not
  # observed, and the number of periods each pair of units shares.
  wide <- matrix(NA_real_, nrow = length(periods), ncol = n_units)
  wide[cbind(match(time_values, periods), match(id_values, units))] <- residuals
  shared <- crossprod(!is.na(wide))
  pairs <- upper.tri(shared)
  kept <- pairs & shared >= 2
  n_pairs <- sum(pairs)
  n_kept <- sum(kept)
  if (n_kept == 0L) {
    rstop_model_context(sprintf(
      paste0(
        "%s() needs a pair of units observed together at two periods or more, and no pair ",
        "of the %d units of the fit shares more than one period. The residuals of two units ",
        "are correlated over the periods both are observed at."
      ),
      fn, n_units
    ))
  }
  if (n_kept < n_pairs) {
    warning(sprintf(
      paste0(
        "%s(): %d of the %d pairs of units share fewer than two periods and are left out ",
        "of the statistic, as plm::pcdtest() leaves them out; it is computed on the ",
        "other %d."
      ),
      fn, n_pairs - n_kept, n_pairs, n_kept
    ), call. = FALSE)
  }
  # cor() warns "the standard deviation is zero" for each pair it cannot
  # correlate, those that share fewer than two periods among them; the pairs
  # of the statistic whose correlation is not defined are counted below.
  rho <- withCallingHandlers(
    stats::cor(wide, use = "pairwise.complete.obs"),
    warning = function(w) invokeRestart("muffleWarning")
  )
  terms <- sqrt(shared[kept]) * rho[kept]
  undefined <- sum(is.na(terms))
  if (undefined == n_kept) {
    stop("No usable pairwise residual correlations.", call. = FALSE)
  }
  if (undefined > 0L) {
    warning(sprintf(
      paste0(
        "%s(): the residuals of a unit are constant over the periods it shares with ",
        "another in %d of the %d pairs of the statistic, whose correlation is not ",
        "defined; they add nothing to its sum."
      ),
      fn, undefined, n_kept
    ), call. = FALSE)
  }
  sqrt(1 / n_kept) * sum(terms, na.rm = TRUE)
}
