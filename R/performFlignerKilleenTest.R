#' Perform Fligner–Killeen test for homogeneity of variances
#'
#' Conducts the median-centred Fligner–Killeen test of homogeneity of
#' variances on the Pearson residuals of the fit, with [stats::fligner.test()]:
#' it ranks the absolute deviations of the residuals from their group medians
#' and scores the ranks by quantiles of the normal distribution, which makes it
#' robust to outliers and to errors that are not normal.
#'
#' @param model A fitted [stats::lm] object.
#' @param data The data frame `model` was fitted to, or `NULL`, the default. The
#'   residuals and weights are read from the fit itself, and `group` from the
#'   rows of `data` that hold the observations of the fit; the observations
#'   whose group is missing are left out, with a warning. Without `data` it is
#'   read from the model frame of `model`, which holds it only when it is a
#'   variable of the model under its own name (`x`, not `log(x)`); a grouping
#'   variable the model does not use needs `data`. Since 0.16.0, data that are
#'   given are matched to the observations of the fit: rows with row names of
#'   their own by name, in any order; rows that only number themselves
#'   (a tibble, a data.table, a data frame whose row names were reset) by the
#'   row numbers of the fit, or in the order of the fit when they hold one row
#'   per observation. The rows the fit dropped may be there or not. Data whose
#'   rows cannot be shown to hold the observations of the fit are refused; see
#'   [test_results].
#' @param group Character scalar naming the grouping variable.
#'
#' @return An object of class \code{htest} with the chi-squared statistic and
#'   p-value.
#'
#' @details
#' The test is the median-centred version of the test of Fligner and Killeen
#' (1976), the "F-K:med \eqn{X^2}" of Conover, Johnson and Johnson (1981), whose
#' simulation study found it among the tests of homogeneity of variances most
#' robust to departures from normality; it is the test [stats::fligner.test()]
#' computes. The residuals of each group are centred at the median of the
#' group, the absolute values of the centred residuals are ranked over all
#' \eqn{n} observations, and the rank \eqn{r_i} is given the score
#' \eqn{a_i = \Phi^{-1}\{(1 + r_i / (n + 1)) / 2\}}. With \eqn{\bar a_j} the mean
#' score of group \eqn{j}, of size \eqn{n_j}, \eqn{\bar a} the mean of all the
#' scores and \eqn{s_a^2 = \sum_i (a_i - \bar a)^2 / (n - 1)}, the statistic
#' \deqn{X^2 = \sum_j n_j (\bar a_j - \bar a)^2 / s_a^2}
#' is referred to a chi-squared distribution with \eqn{k - 1} degrees of
#' freedom, for \eqn{k} groups. No logarithm is taken. The shared validation
#' helpers verify model compatibility and the minimum sample and group sizes,
#' and the observations whose group is missing are left out, before the
#' statistic is computed.
#'
#' Tied values are given the mean of their ranks. Since 0.16.0 the deviations
#' are those of the residuals
#' \eqn{r_i = \sqrt{w_i}\,(y_i - x_i^\top \hat\beta - o_i)} computed
#' observation by observation, as the Gaussian `glm` computes them, and not
#' the residuals an `lm` stores, which its QR decomposition rounds differently
#' from one observation to the next. For an `mgcv::gam()` fit \eqn{x_i} is the
#' row of its linear predictor matrix. Observations with the same values of
#' the model's variables, weight and offset, such as repeated values of an
#' integer response or of a response recorded with few digits, are then given
#' the same residual, as in exact arithmetic, where their residuals agree to
#' within rounding, even when a basis such as `poly()` gives them design rows
#' that differ in their last bits; a term of the row position, such as
#' `seq_along(x)`, keeps them apart. This needs the model's variables: it is
#' done when `data` is given, or when the model frame holds them, as it holds
#' a variable the model uses under its own name (`x`, not `poly(x, 3)`). And
#' absolute deviations closer than \eqn{8 \epsilon s_j}, with \eqn{\epsilon}
#' = `.Machine$double.eps` and \eqn{s_j} the largest \eqn{|r_i|} of group
#' \eqn{j} (the smaller of the two for deviations of different groups), are
#' ranked as tied: in a group of even size the two residuals in the middle lie
#' at the same distance from the median of the group, half their difference,
#' a tie that the centring breaks by up to \eqn{\epsilon s_j}. On data with
#' repeated observations the statistic can differ much from before: on
#' `lm(extra ~ group, data = sleep)` it was 0.154 and is 0.213, the value of
#' exact ties. On continuous data it moves by much less (see `NEWS`). Where
#' neither changes a rank, the value is that of [stats::fligner.test()] on
#' the residuals of the fit, as before. Two limits remain. A tie between
#' different observations that exact decimal arithmetic would give, as when
#' two pairs of observations of a regressor recorded with few digits differ by
#' the same amounts, is restored only when rounding leaves it within the
#' tolerance, and two forms of one model can then differ by several percent:
#' on 70 observations that repeat 30 values of \eqn{x} recorded with one
#' decimal, `y ~ poly(x, 3) + g` gives 1.21547, and
#' `y ~ x + I(x^2) + I(x^3) + g` and `y ~ poly(x, 3, raw = TRUE) + g` give
#' 1.30280, since `poly()` breaks in its last bits the ties between repeated
#' values of \eqn{x} in different groups. And in a group whose residuals are
#' far from zero, \eqn{10^8} times their spread, the tolerance may also tie
#' two deviations that differ by a few units in the last place of the
#' residuals.
#'
#' @references
#' Conover, W. J., Johnson, M. E., & Johnson, M. M. (1981). A comparative study
#' of tests for homogeneity of variances, with applications to the outer
#' continental shelf bidding data. *Technometrics, 23*(4), 351–361.
#' <https://doi.org/10.1080/00401706.1981.10487680>
#'
#' Fligner, M. A., & Killeen, T. J. (1976). Distribution-free two-sample tests for
#' scale. *Journal of the American Statistical Association, 71*(353), 210–213.
#' <https://doi.org/10.1080/01621459.1976.10481517>
#'
#' @examples
#' data(mtcars)
#' mtcars$cyl <- factor(mtcars$cyl)
#' mod <- lm(mpg ~ wt, data = mtcars)
#' performFlignerKilleenTest(mod, mtcars, "cyl")
#'
#' # Rank-based alternative to Bartlett's classical test
#' performBartlettTest(mod, mtcars, "cyl")
#'
#' @seealso
#' [performLeveneTest()] and [performBrownForsytheTest()] for other robust
#' variance-equality diagnostics.
performFlignerKilleenTest <- function(model, data = NULL, group) {
  test_label <- "Fligner-Killeen test"
  # The residuals and weights come from the fit; `data`, when given, is matched
  # to its observations and checked (R/model_context.R). `group` is read from
  # the rows of `data` that hold the observations, so data taken in the order
  # of the fit must tell those rows apart when it is not a column of the model
  # frame.
  ctx <- rmodel_context(
    model, data, fn = "performFlignerKilleenTest", test_label = test_label,
    reads = if (!missing(group) && length(group) == 1L) as.character(group) else character()
  )

  rvalidateModelInputs(model, test_name = "Fligner-Killeen", min_obs = 6L, allow_aliased = TRUE)
  rvalidate_model_context(ctx)

  # An NA or empty name is refused here, before it reaches the checks of
  # `data` (rcontext_group_rows()), as by performOBrienTest().
  if (missing(group) || length(group) != 1L || is.na(group) || !nzchar(as.character(group))) {
    stop("`group` must be a single column name.", call. = FALSE)
  }

  group <- as.character(group)

  handle_validation_result <- function(result, warn_patterns = character()) {
    if (length(result$warnings) > 0) {
      for (msg in unique(result$warnings)) {
        warning(msg, call. = FALSE)
      }
    }
    if (!isTRUE(result$passed)) {
      messages <- unique(result$messages)
      warn_idx <- rep(FALSE, length(messages))
      if (length(warn_patterns) > 0 && length(messages) > 0) {
        warn_idx <- vapply(
          messages,
          function(msg) any(grepl(warn_patterns, msg, perl = TRUE)),
          logical(1)
        )
      }
      if (any(warn_idx)) {
        for (msg in messages[warn_idx]) {
          warning(msg, call. = FALSE)
        }
      }
      remaining <- messages[!warn_idx]
      if (length(remaining) > 0) {
        stop(paste(remaining, collapse = "\n"), call. = FALSE)
      }
    }
    invisible(result)
  }

  # The Pearson residuals of the fit and the rows of `data` that hold its
  # observations, in the order of the fit, or the model frame without `data`;
  # the observations whose group is missing are left out, with a warning, as
  # before 0.16.0 (rcontext_group_rows()).
  kept <- rcontext_group_rows(ctx, data, group)
  working_data <- kept$data
  residuals <- kept$residuals

  if (length(residuals) == 0 || nrow(working_data) == 0) {
    .std_error(
      "rassumption_violation",
      assumption = "Fligner-Killeen test requires observations after removing missing data"
    )
  }

  validation_frame <- working_data[, group, drop = FALSE]
  requirements <- rvalidateTestRequirements(
    "fligner_killeen",
    model = model,
    data = validation_frame,
    group_var = group,
    min_group_size = 3L
  )
  handle_validation_result(requirements)

  group_vector <- validation_frame[[group]]
  grp <- if (is.factor(group_vector)) {
    base::droplevels(group_vector)
  } else {
    base::droplevels(factor(group_vector))
  }

  if (length(grp) != length(residuals)) {
    stop(
      sprintf(
        "%s detected a mismatch between residuals and grouping variable after cleaning (residuals = %d, groups = %d).",
        test_label,
        length(residuals),
        length(grp)
      ),
      call. = FALSE
    )
  }

  if (length(levels(grp)) < 2L) {
    .std_error(
      "rassumption_violation",
      assumption = "Fligner-Killeen test requires at least two groups"
    )
  }

  finite_res <- residuals[is.finite(residuals)]
  residual_variance <- if (length(finite_res) > 1L) stats::var(finite_res) else NA_real_
  range_span <- if (length(finite_res) > 0L) diff(range(finite_res)) else NA_real_
  scale_reference <- if (length(finite_res) > 0L) {
    max(1, max(abs(finite_res), na.rm = TRUE))
  } else {
    1
  }

  insufficient_variation <- (
    length(finite_res) < 2L ||
      length(unique(finite_res)) < 2L ||
      is.na(residual_variance) ||
      residual_variance <= .Machine$double.eps ||
      (!is.na(range_span) && range_span <= sqrt(.Machine$double.eps) * scale_reference)
  )

  if (insufficient_variation) {
    .std_error(
      "rassumption_violation",
      assumption = "Fligner-Killeen test requires variation in residuals for ranking"
    )
  }

  # The deviations are ranked on residuals computed observation by
  # observation, so that repeated observations get the same residual
  # (rfligner_residuals()), with the ties that rounding broke at the centring
  # made exact again (rfligner_rounding_ties()). Where that changes no rank,
  # the residuals of the fit are passed on as before.
  ranked <- rfligner_ranked_residuals(ctx, data, group, residuals, grp)
  fl_res <- stats::fligner.test(ranked, grp)

  structure(
    list(
      statistic = c("X-squared" = unname(fl_res$statistic)),
      parameter = c(df = unname(fl_res$parameter)),
      p.value = fl_res$p.value,
      method = "Fligner-Killeen test for homogeneity of variances",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}

# The residuals Fligner-Killeen's test ranks.
#
# The test ranks the absolute deviations of the residuals from their group
# medians, and ties in exact arithmetic are broken by rounding in two places.
# The residuals of an lm come from its QR decomposition, y - Q Q'y, whose
# rounding differs from one observation to the next: two observations with
# the same response and design row, such as repeated values of an integer
# response, get residuals that differ in their last bits (in 20 fits of
# rpois(n, 3) on four groups, by up to 17 times eps * max|r| at 60
# observations and 6,900 times at 6,000), and the test ranked them apart. The
# Gaussian glm computes y - X b, the same for both. And the centring breaks
# the tie of the two residuals in the middle of a group of even size. So the
# test ranks residuals computed as y - X b (rfligner_residuals()), with the
# ties that rounding broke at the centring made exact again
# (rfligner_rounding_ties()). Where that changes no rank of the residuals of
# the fit, those are returned, so that the value is that of before 0.16.0;
# otherwise the deviations with their ties, or the residuals of y - X b.
rfligner_ranked_residuals <- function(ctx, data, group, residuals, grp) {
  own <- rfligner_residuals(ctx, data, group, residuals)
  if (is.null(own)) {
    own <- as.vector(residuals)
  }
  tied <- rfligner_rounding_ties(own, grp)
  candidate <- if (is.null(tied)) own else tied
  before <- rank(abs(rfligner_centred(residuals, grp)))
  after <- rank(abs(rfligner_centred(candidate, grp)))
  if (identical(before, after)) residuals else candidate
}

# Deviations from the group medians, as stats::fligner.test() computes them.
rfligner_centred <- function(x, g) {
  x <- as.vector(x)
  as.vector(x - tapply(x, g, stats::median)[g])
}

# The Pearson residuals sqrt(w) (y - (X b + o)) of the observations the test
# keeps, with X b summed column by column in the same order for every row
# (rowSums(), not the BLAS of %*%, whose paths may round some rows
# otherwise). X is the design the context stores, model.matrix() of the fit,
# which for an mgcv::gam() fit is its linear predictor matrix, smooth terms
# included; the aliased columns of a rank-deficient fit are left out with
# their coefficients. Observations with the same values of the model's
# variables, the same weight and the same offset have the same residual in
# exact arithmetic, but not always the same design row: poly() builds its
# basis with a QR decomposition of its own, which can give two such rows that
# differ in their last bits. So each set of them is given one value, the
# residual of its first member (rfligner_same_observations()). NULL when the
# residuals cannot be computed, or when they are not the residuals of the fit
# up to rounding, which the test then ranks as before.
rfligner_residuals <- function(ctx, data, group, kept) {
  coefs <- stats::coef(ctx$model)
  design <- ctx$design
  response <- ctx$response
  if (is.null(design) || is.null(response) || !is.numeric(coefs) ||
        NCOL(design) != length(coefs) || NROW(design) != length(response)) {
    return(NULL)
  }
  used <- !is.na(coefs)
  x <- design[, used, drop = FALSE]
  contributions <- x * rep(unname(coefs[used]), each = nrow(x))
  fitted_mean <- rowSums(contributions)
  # The size of the numbers each residual is computed from, the scale of its
  # rounding.
  size <- abs(as.vector(response)) + rowSums(abs(contributions))
  if (!is.null(ctx$offset)) {
    fitted_mean <- fitted_mean + ctx$offset
    size <- size + abs(ctx$offset)
  }
  own <- as.vector(response) - fitted_mean
  if (!is.null(ctx$weights)) {
    own <- sqrt(ctx$weights) * own
    fitted_mean <- sqrt(ctx$weights) * fitted_mean
    size <- sqrt(ctx$weights) * size
  }
  # An observation takes the residual of the first with the same values of
  # the model's variables only when the two already agree to within rounding,
  # 64 * eps of the larger size: that restores a tie that rounding broke and
  # imposes none. A term that depends on the position of the row, such as
  # seq_along(x) or cumsum(x), gives such observations residuals that differ
  # by much more, and they are left apart. The gaps poly() opens between
  # repeated observations reached 2.7 * eps of that size (on iris with every
  # row twice and poly(Petal.Length, 3)).
  first <- rfligner_same_observations(ctx, data)
  if (length(first) == length(own)) {
    apart <- abs(own - own[first]) > 64 * .Machine$double.eps * pmax(size, size[first])
    first[apart] <- which(apart)
    own <- own[first]
  }
  if (length(own) != length(kept)) {
    # The observations whose group, or a variable of the model, is missing in
    # the rows that hold them, which rcontext_group_rows() left out.
    rows <- if (is.null(data)) ctx$frame else data[ctx$rows, , drop = FALSE]
    variables <- intersect(unique(c(all.vars(stats::terms(ctx$model)), group)), names(rows))
    keep <- stats::complete.cases(rows[, variables, drop = FALSE])
    if (length(keep) != length(own) || sum(keep) != length(kept)) {
      return(NULL)
    }
    own <- own[keep]
    fitted_mean <- fitted_mean[keep]
  }
  kept <- as.vector(kept)
  scale <- max(abs(kept), abs(fitted_mean))
  if (!all(is.finite(own)) || max(abs(own - kept)) > sqrt(.Machine$double.eps) * scale) {
    return(NULL)
  }
  own
}

# For each observation of the fit, the first observation with the same values
# of the model's variables (the response and every variable of the formula),
# the same weight and the same offset, or NULL. The variables are read from
# the rows of `data` that the context matched to the observations and
# checked against the model frame, or, without `data`, from the model frame,
# which holds a variable under its own name only when the model uses it so
# (`x`, not `poly(x, 3)`); when a variable is held by neither, the columns of
# the model frame stand for it, as computed, so that two observations whose
# computed terms differ in their last bits are taken as different. Values are
# compared exactly.
rfligner_same_observations <- function(ctx, data) {
  frame <- ctx$frame
  n <- NROW(frame)
  rows <- if (!is.null(data) && length(ctx$rows) == n) data[ctx$rows, , drop = FALSE]
  columns <- list()
  held <- TRUE
  for (variable in all.vars(stats::terms(ctx$model))) {
    if (!is.null(rows) && variable %in% names(rows)) {
      columns <- c(columns, list(rows[[variable]]))
    } else if (variable %in% names(frame)) {
      columns <- c(columns, list(frame[[variable]]))
    } else {
      held <- FALSE
    }
  }
  if (!held) {
    columns <- c(columns, as.list(frame))
  }
  columns <- Filter(Negate(is.null), c(columns, list(ctx$weights, ctx$offset)))
  # Matrix columns, such as a matrix response or a poly() basis in the model
  # frame, are compared column by column.
  columns <- unlist(lapply(columns, function(v) {
    if (is.matrix(v)) lapply(seq_len(ncol(v)), function(j) v[, j]) else list(v)
  }), recursive = FALSE)
  if (length(columns) == 0L || !all(vapply(columns, NROW, integer(1)) == n)) {
    return(NULL)
  }
  codes <- lapply(columns, function(v) match(v, unique(v)))
  key <- do.call(paste, c(codes, sep = "\r"))
  match(key, key)
}

# The ties that rounding broke when the residuals were centred at their group
# medians, made exact again, or NULL when there are none.
#
# stats::fligner.test() ranks |x - m_j|, with m_j the median of group j, and
# averages the ranks of values that are exactly equal. In a group of even
# size the two residuals in the middle, a <= b, lie at the same distance from
# m_j = (a + b) / 2 in exact arithmetic, but m_j and the two differences are
# rounded, so that one of the two often ranks below the other, which decided
# about 1e-6 of the statistic (found comparing a Gaussian glm with the same
# lm). With eps = .Machine$double.eps, m_j is rounded by at most eps / 2 *
# |m_j| and each difference by at most eps / 2 of its size, which opens a gap
# of at most eps * |m_j| + eps * (b - a) / 2 = eps * max(|a|, |b|) between the
# two, and of at most about 3 * eps * s_j between two deviations of group j
# that are equal in exact arithmetic, with s_j the largest absolute residual
# of the group. Deviations closer than 8 * eps * s_j are taken as tied, and
# two deviations of different groups closer than 8 eps times the smaller of
# their two s_j. The scale is that of the residuals of the group, not of
# their deviations, since the rounding of m_j is relative to its own size, and
# not that of all the residuals, so that a group far from zero does not widen
# the tolerance of the others.
#
# Each run of such deviations, in increasing order, is set to its smallest
# member, with its sign. The medians of the result are then exactly zero in
# every group: in a group of odd size the middle deviation is zero, and in one
# of even size the two in the middle are tied with opposite signs. So
# stats::fligner.test() centres it at zero, which leaves it unchanged, and
# ranks it with the tie exact. Should a median not be zero, the residuals are
# left as they are (NULL).
rfligner_rounding_ties <- function(x, g) {
  x <- as.vector(x)
  centred <- rfligner_centred(x, g)
  magnitude <- abs(centred)
  scale <- as.vector(tapply(abs(x), g, max)[g])
  ord <- order(magnitude)
  sorted <- magnitude[ord]
  gaps <- diff(sorted)
  at <- scale[ord]
  tolerance <- 8 * .Machine$double.eps * pmin(at[-1L], at[-length(at)])
  if (!any(gaps > 0 & gaps <= tolerance)) {
    return(NULL)
  }
  run <- cumsum(c(TRUE, gaps > tolerance))
  magnitude[ord] <- sorted[match(run, run)]
  tied <- sign(centred) * magnitude
  if (!all(tapply(tied, g, stats::median) == 0)) {
    return(NULL)
  }
  tied
}
