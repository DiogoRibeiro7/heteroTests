# man/performBoxMTest.Rd is written by hand, without a roxygen header, and
# roxygen2 does not overwrite it; the block below says what it says.

#' Box's M Test for Equality of Covariance Matrices
#'
#' Test whether multiple groups have equal covariance matrices, by Box's (1949)
#' chi-square approximation to his M statistic.
#'
#' @details
#' `performBoxMTest(data, group)` is the one test that does not take
#' `(model, data, ...)`: it tests the covariance matrices of the data
#' themselves, not the residuals of a fitted model.
#'
#' With \eqn{g} groups of \eqn{n_i} observations of \eqn{p} variables,
#' \eqn{\nu_i = n_i - 1}, \eqn{\nu = \sum_i \nu_i}, \eqn{S_i} the covariance
#' matrix of group \eqn{i} and \eqn{S} the pooled one, Box's statistic is
#' \deqn{M = \nu \log|S| - \sum_i \nu_i \log|S_i|,}
#' his correction factor is
#' \deqn{c_1 = \left(\sum_i \frac{1}{\nu_i} - \frac{1}{\nu}\right)
#'   \frac{2p^2 + 3p - 1}{6 (p + 1) (g - 1)},}
#' and \eqn{M (1 - c_1)} is referred to a chi-square distribution with
#' \eqn{f_1 = (g - 1) p (p + 1) / 2} degrees of freedom: Box's (1949)
#' chi-square approximation. The test assumes that each group is multivariate
#' normal. It gives the values of `biotools::boxM()`, `heplots::boxM()` and
#' `rstatix::box_m()`: 140.9430 on 20 degrees of freedom for the example on
#' `iris`.
#'
#' Before 0.16.0 the correction factor was \eqn{(g - 1) c_1}: the same for two
#' groups, whose values do not change, and too large for three or more, which
#' made the statistic too small and the test conservative. The example on
#' `iris` gave 135.2229.
#'
#' The groups are the levels of `group` that have observations: a level with
#' none, as `subset()` leaves on a factor, is dropped (since 0.16.0; before, it
#' made the result `NA`). There must be at least two of them, and each group
#' needs more observations than there are variables, \eqn{n_i > p}: a group
#' of \eqn{n_i \le p} observations has a singular covariance matrix (a group of
#' one has none), whose log-determinant is \eqn{-\infty}, and the statistic is
#' not defined, whatever the data. Since 0.16.0 the test refuses such data with
#' an error that names the groups; before, rounding made the statistic a finite
#' number (133.74, p = 7.7e-19, for a group of three observations of four
#' variables beside two species of `iris`), a group of one gave `NA` and a
#' single group `NaN`. With more observations than variables a group's
#' covariance matrix is still singular when a variable is constant in the
#' group, or a linear combination of the others in it; the test refuses that
#' too, naming the group and saying which, where it returned `Inf` (p = 0) or
#' `NaN` with warnings. A group is checked by the rank of its centred data at
#' the tolerance of `qr()`. A missing value in `data` or in `group` still
#' gives `NA`.
#'
#' @section Size:
#' In `inst/validation/box-m-approximations-size.R`, at 5000 replications per
#' cell and the 5% level, with \eqn{g} = 2, 3 or 5 groups of 10 to 100
#' observations of \eqn{p} = 1 to 10 variables, the test held its level under
#' Gaussian data in 60 of 67 cells, inside the release gate's interval, 0.042
#' to 0.058.
#'
#' * **It is liberal when the groups are small for the number of variables.**
#'   With groups of 10 and 4 or 6 variables it rejected a true null hypothesis
#'   0.055 to 0.106 of the time, outside the gate in four of those six cells
#'   (0.062 to 0.106, the most with five groups of 10 and six variables), and
#'   with groups of 20 and 10 variables 0.071 to 0.087. Box's F
#'   approximation, which the package does not compute, was closer to the
#'   level in each of those nine cells (0.051 to 0.070); in the other Gaussian
#'   cells the two differed by at most 0.005.
#' * **It is not robust to heavy tails.** With independent \eqn{t_5}
#'   coordinates, three groups of 20, 50 or 100 and two or four variables, it
#'   rejected 0.26 to 0.48 of the time (0.18 to 0.46 with the factor of
#'   0.15.0), more with larger groups: the test then detects the tails as well
#'   as unequal covariance matrices.
#' * **It is conservative with light tails.** On the data of the full sweep
#'   (`inst/validation/full-sweep-size-power.R`), 150 observations of
#'   `(y, x)` with `x` uniform and grouped by `cut(x, 3)`, it rejected 0.017
#'   of the time, as Box's F did. The sweep's 0.012, at 400 replications, was
#'   measured with the factor of 0.15.0, which gives 0.016 in that design at
#'   5000 replications: the low size comes from the uniform, light-tailed data,
#'   not from the approximation.
#'
#' When the groups are small for the number of variables, or the data may have
#' heavy tails, a rejection is weak evidence of unequal covariance matrices.
#'
#' @section Validation:
#' `tests/testthat/test-boxm.R` checks the statistic against Box's
#' approximation written out for two to five groups and one to four variables,
#' and against the values of `biotools::boxM()` 4.3 and `heplots::boxM()`
#' 1.8.6 on `iris` and on `mtcars`, and that two groups give the values of
#' 0.15.0. `inst/validation/box-m-approximations-size.R` compares the
#' chi-square and the F approximation with `biotools`, `heplots` and
#' `DFA.CANCOR` on ten data sets, and measures the sizes above.
#'
#' @param data A numeric data frame or matrix.
#' @param group A factor indicating group membership. At least two groups must
#'   have observations, and each of them more observations than `data` has
#'   variables.
#'
#' @return An object of class \code{htest}: `statistic`, named `M`, is Box's
#'   corrected statistic \eqn{M (1 - c_1)}; `parameter`, named `df`, its
#'   degrees of freedom \eqn{f_1}; `p.value` the upper tail of the chi-square
#'   distribution; and `method` and `data.name`.
#'
#' @references
#' Box, G. E. P. (1949). A general distribution theory for a class of likelihood
#' criteria. *Biometrika, 36*(3/4), 317–346. \doi{10.1093/biomet/36.3-4.317}
#'
#' @examples
#' data(iris)
#' performBoxMTest(iris[,1:4], iris$Species)
#'
#' # Two species: with two groups the values are those of 0.15.0
#' two <- droplevels(subset(iris, Species != "virginica"))
#' performBoxMTest(two[, 1:4], two$Species)
#'
#' @seealso
#' [runMultivariateTests()] orchestrates a suite of multivariate diagnostics
#' including Box's M test.
#' @export
performBoxMTest <- function(data, group) {
  data_name <- paste(
    rexpression_label(substitute(data), "data"), "by",
    rexpression_label(substitute(group), "group")
  )
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("data must be a data.frame or matrix")
  }
  # The groups are the levels observed in the rows tested. A level with no
  # observation, as subset() leaves on a factor, gave a covariance matrix of
  # NAs and an NA result before 0.16.0; it is dropped, so the test is that of
  # the groups observed.
  group <- droplevels(as.factor(group))
  k <- ncol(data)
  groups <- levels(group)
  g <- length(groups)
  n <- nrow(data)
  ns <- as.numeric(table(group))
  rboxm_refuse_groups(groups, ns, k)
  xs <- lapply(groups, function(gr) as.matrix(data[group == gr, , drop = FALSE]))
  covmats <- lapply(xs, cov)
  pooled <- Reduce(`+`, Map(function(S, n) (n - 1) * S, covmats, ns)) / (n - g)
  logdet_groups <- suppressWarnings(vapply(covmats, function(S) log(det(S)), 0))
  logdet_pooled <- suppressWarnings(log(det(pooled)))
  rboxm_refuse_singular(groups, xs, logdet_groups, logdet_pooled)
  M <- (n - g) * logdet_pooled - sum((ns - 1) * logdet_groups)
  # Box's (1949) correction factor c1, with nu_i = n_i - 1 and nu = n - g:
  # (sum 1 / nu_i - 1 / nu) (2 k^2 + 3 k - 1) / (6 (k + 1) (g - 1)). Before
  # 0.16.0 the factor (g - 1) stood in the numerator too, where it cancelled
  # the one of the denominator, so the statistic was M (1 - (g - 1) c1):
  # Box's for two groups, and too small for three or more.
  q <- (2 * k^2 + 3 * k - 1) / (6 * (k + 1) * (g - 1)) *
    (sum(1 / (ns - 1)) - 1 / (n - g))
  stat <- M * (1 - q)
  df <- (g - 1) * k * (k + 1) / 2
  pval <- pchisq(stat, df, lower.tail = FALSE)
  structure(
    list(
      statistic = c(M = stat), parameter = c(df = df),
      p.value = pval, method = "Box's M Test for Equality of Covariance Matrices",
      data.name = data_name
    ),
    class = "htest"
  )
}

#' Refuse a group whose covariance matrix is singular
#'
#' With more observations than variables a group's covariance matrix can
#' still be singular: a variable that is constant in the group, or that is a
#' linear combination of others in it. Its log-determinant is then
#' \eqn{-\infty}, or `NaN` when rounding leaves the determinant negative, and
#' the statistic is not defined, whatever the data: before 0.16.0 the test
#' returned `Inf` on 20 degrees of freedom (p = 0) for `iris` with
#' `Petal.Width` set to 0.2 in the setosa rows, and `NaN`, with warnings, with
#' a fifth column the sum of the first two. A group is refused when the rank of
#' its centred data, from [qr()] at its default tolerance, is below the number
#' of variables, or when the log-determinant of its covariance matrix, or of
#' the pooled one, is not finite. Data with a missing value give `NA` as
#' before, and are not checked here.
#'
#' @param groups The levels of the group that have observations.
#' @param xs The data of each group, as matrices.
#' @param logdet_groups,logdet_pooled The log-determinants of the covariance
#'   matrices of the groups and of the pooled one.
#' @return `NULL`, invisibly; called for the error.
#' @keywords internal
#' @noRd
rboxm_refuse_singular <- function(groups, xs, logdet_groups, logdet_pooled) {
  if (any(vapply(xs, anyNA, logical(1)))) {
    return(invisible(NULL))
  }
  p <- ncol(xs[[1L]])
  variables <- colnames(xs[[1L]])
  if (is.null(variables)) {
    variables <- paste0("V", seq_len(p))
  }
  for (i in seq_along(groups)) {
    x <- xs[[i]]
    rank <- qr(sweep(x, 2L, colMeans(x)))$rank
    if (rank >= p && is.finite(logdet_groups[[i]])) {
      next
    }
    if (rank >= p) {
      # Full rank, but det() under- or overflows on the scale of the data.
      stop(
        sprintf(
          paste0(
            "Box's M test could not compute the log-determinant of the covariance matrix ",
            "of group '%s': its variables have full rank, and their scale makes the ",
            "determinant under- or overflow. Rescale the variables."
          ),
          groups[[i]]
        ),
        call. = FALSE
      )
    }
    constant <- variables[apply(x, 2L, function(v) diff(range(v)) == 0)]
    why <- if (length(constant) > 0L) {
      sprintf(
        "%s %s %s constant in it",
        if (length(constant) > 1L) "the variables" else "the variable",
        paste0("'", constant, "'", collapse = ", "),
        if (length(constant) > 1L) "are" else "is"
      )
    } else {
      sprintf(
        "a variable is a linear combination of the others in it (its %d variables have rank %d)",
        as.integer(p), as.integer(rank)
      )
    }
    stop(
      sprintf(
        paste0(
          "Box's M test needs a nonsingular covariance matrix in each group, and that of ",
          "group '%s' is singular: %s. Its log-determinant is -Inf and the statistic is not ",
          "defined, whatever the data. Leave out the variable, or the combination of ",
          "variables, that does not vary in the group."
        ),
        groups[[i]], why
      ),
      call. = FALSE
    )
  }
  # Nonsingular groups make the pooled matrix nonsingular too: only its scale
  # can leave its log-determinant not finite.
  if (!is.finite(logdet_pooled)) {
    stop(
      paste0(
        "Box's M test could not compute the log-determinant of the pooled covariance ",
        "matrix: the scale of the variables makes the determinant under- or overflow. ",
        "Rescale the variables."
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Refuse groups on which Box's M is not defined
#'
#' Box's M compares the covariance matrices of two groups or more, and needs
#' each of them to be nonsingular. A group of \eqn{n_i \le p} observations of
#' \eqn{p} variables has a singular covariance matrix (of rank at most
#' \eqn{n_i - 1}), or none at all for one observation, so its log-determinant
#' is \eqn{-\infty} and the statistic is not defined, whatever the data.
#' Before 0.16.0 rounding made it a finite number, 133.74 (p = 7.7e-19) for a
#' group of three observations of four variables, a group of one gave `NA`,
#' and one group with observations gave `NaN` on 0 degrees of freedom. All
#' are refused, with the plain error of a test's own refusal.
#'
#' @param groups The levels of the group that have observations.
#' @param ns Their numbers of observations.
#' @param p The number of variables.
#' @return `NULL`, invisibly; called for the error.
#' @keywords internal
#' @noRd
rboxm_refuse_groups <- function(groups, ns, p) {
  if (length(groups) < 2L) {
    stop(
      sprintf(
        paste0(
          "Box's M test compares the covariance matrices of two groups or more, ",
          "and `group` has %s with observations%s."
        ),
        if (length(groups) == 1L) "one level" else "no level",
        if (length(groups) == 1L) sprintf(" ('%s')", groups) else ""
      ),
      call. = FALSE
    )
  }
  small <- ns <= p
  if (!any(small)) {
    return(invisible(NULL))
  }
  observations <- function(m) {
    sprintf("%d observation%s", as.integer(m), if (m == 1) "" else "s")
  }
  named <- sprintf("'%s' (%s)", groups[small], vapply(ns[small], observations, ""))
  listed <- if (length(named) > 1L) {
    paste(paste(named[-length(named)], collapse = ", "), "and", named[length(named)])
  } else {
    named
  }
  stop(
    sprintf(
      paste0(
        "Box's M test needs more observations than variables in each group: with %d ",
        "variable%s, group%s %s %s no more. The covariance matrix of such a group is ",
        "singular%s, so its log-determinant is -Inf and the statistic is not defined, ",
        "whatever the data. Each group needs at least %d observations; use more data, ",
        "fewer variables or fewer groups."
      ),
      as.integer(p), if (p == 1) "" else "s", if (sum(small) > 1L) "s" else "",
      listed, if (sum(small) > 1L) "have" else "has",
      if (any(ns[small] == 1)) " (a group of one observation has none)" else "",
      as.integer(p) + 1L
    ),
    call. = FALSE
  )
}
