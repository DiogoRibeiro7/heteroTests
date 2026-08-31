#' Test-specific sample size requirements
#'
#' Central catalogue of the minimum sample sizes required by heteroscedasticity
#' diagnostics supported in the package. Each entry either provides a
#' requirements list with human-readable rationale or a function that computes
#' the minimum number of observations from contextual arguments (e.g. lag
#' lengths for dynamic tests).
#'
#' Static entries are named lists with some combination of the following
#' components:
#' \describe{
#'   \item{`min_obs`}{Overall minimum observation count.}
#'   \item{`min_obs_per_group`}{Minimum number of observations that each group
#'     should contribute.}
#'   \item{`reason`}{Short explanation describing why the requirement exists.}
#' }
#'
#' Dynamic entries are functions that receive additional arguments supplied to
#' [rvalidateSampleSize()] and should return either a numeric minimum or a list
#' containing `min_obs` and, optionally, `reason`.
#'
#' @format A named list keyed by lowercase test identifiers.
#' @examples
#' heteroTests:::rTEST_REQUIREMENTS$white$min_obs
#'
#' # Dynamic entry: ARCH LM depends on the lag order
#' heteroTests:::rTEST_REQUIREMENTS$arch_lm(lags = 3)
#' @keywords internal
rTEST_REQUIREMENTS <- list(
  white = list(min_obs = 20L, reason = "Auxiliary regression needs sufficient df"),
  breusch_pagan = list(min_obs = 15L, reason = "Asymptotic properties require adequate sample"),
  goldfeld_quandt = list(min_obs = 30L, reason = "Data splitting requires sufficient observations"),
  koenker = list(min_obs = 15L, reason = "Studentized test needs stability"),
  park = list(min_obs = 10L, reason = "Log transformation requires minimum sample"),
  glejser = list(min_obs = 12L, reason = "Auxiliary regression requires enough observations"),
  harvey = list(min_obs = 15L, reason = "Log-variance regression requires residual degrees of freedom"),
  spearman = list(min_obs = 10L, reason = "Correlation test requires adequate sample size"),
  levene = list(min_obs_per_group = 5L, reason = "Group ANOVA needs sufficient observations"),
  brown_forsythe = list(min_obs_per_group = 5L, reason = "Median-based variance test needs adequate group sizes"),
  bartlett = list(min_obs_per_group = 3L, reason = "Variance estimation minimum"),
  fligner_killeen = list(min_obs_per_group = 3L, reason = "Rank-based variance test requires multiple observations per group"),
  hartley_fmax = list(min_obs_per_group = 2L, reason = "Variance ratios require at least two observations per group"),
  cameron_trivedi = list(min_obs = 15L, reason = "Variance decomposition regression requires residual degrees of freedom"),
  ordered_lm = list(min_obs = 3L, reason = "Ordered LM test requires at least three ordered observations"),
  studentized_bp = list(min_obs = 15L, reason = "Studentized auxiliary regression requires adequate sample"),
  szroeter = list(min_obs = 15L, reason = "Rank-based statistic requires enough ordered observations"),
  bootstrap_tests = list(min_obs = 50L, reason = "Bootstrap needs adequate base sample"),
  wild_bootstrap = list(min_obs = 30L, reason = "Wild bootstrap multipliers require stable auxiliary regression"),
  hc_covariance = list(min_obs = 20L, reason = "HC adjustments need residual degrees of freedom"),
  quantile_regression = list(min_obs = 40L, reason = "Quantile comparisons require sufficient data per quantile"),
  rank_permutation = list(min_obs = 25L, reason = "Permutation reference distribution needs adequate sample"),
  high_dimensional = list(min_obs = 25L, reason = "Principal component projection requires ample observations"),
  spatial_hetero = list(min_obs = 20L, reason = "Spatial statistics need enough observations to form neighbours"),
  arch_lm = function(lags) {
    if (missing(lags)) {
      stop("`lags` must be supplied to evaluate ARCH LM sample size requirements.", call. = FALSE)
    }
    if (!is.numeric(lags) || length(lags) != 1L || is.na(lags) || lags < 1) {
      stop("`lags` must be a positive integer.", call. = FALSE)
    }
    as.integer(2 * as.integer(lags) + 5L)
  },
  mcleod_li = function(lags) {
    if (missing(lags)) {
      stop("`lags` must be supplied to evaluate McLeod-Li sample size requirements.", call. = FALSE)
    }
    if (!is.numeric(lags) || length(lags) != 1L || is.na(lags) || lags < 1) {
      stop("`lags` must be a positive integer.", call. = FALSE)
    }
    as.integer(2 * as.integer(lags) + 5L)
  }
)
