#' Perform Pesaran's CD test for cross-sectional dependence
#'
#' Implements the Pesaran (2004, 2015) cross-sectional dependence (CD) statistic
#' by averaging pairwise residual correlations across individuals within each
#' period of a panel dataset.
#'
#' @param model A fitted [stats::lm] object estimated on panel data. The residuals
#'   must be ordered consistently with `data`.
#' @param data A [base::data.frame] containing the variables used in `model` and
#'   the panel identifiers supplied via `id` and `time`.
#' @param id Character scalar naming the column that identifies individuals (the
#'   cross-sectional dimension).
#' @param time Character scalar naming the column that indexes time periods.
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
performPesaranTest <- function(model, data, id, time) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class 'lm'.")
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!all(c(id, time) %in% names(data))) {
    stop("`id` and `time` must be columns in `data`.")
  }

  res <- residuals(model)
  df <- data.frame(id = data[[id]], time = data[[time]], res = res)
  df <- df[order(df$time, df$id), ]
  ids <- unique(df$id)
  times <- unique(df$time)
  N <- length(ids)
  T <- length(times)
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
  p_value <- 2 * (1 - pnorm(abs(CD)))

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
