#' Machine-learning residual analysis
#'
#' Fits a generalized additive model (GAM) using the same formula as the input
#' linear model and compares residual variance.
#'
#' @param model A fitted `lm` model or a formula.
#' @param data Data frame used if `model` is a formula.
#' @return A list containing the GAM model, residuals from both models and the
#'   reduction in RMSE when using the GAM.
#' @seealso \code{\link{HeteroDiagnostic}}, \code{\link{runDiagnostics}},
#'   \code{\link{runHeteroTests}}
#' @seealso \code{\link{runDiagnostics}} to compute the individual statistics
#'   used here.
#' @examples
#' data(mtcars)
#' res <- analyzeMLResiduals(mpg ~ wt + qsec, mtcars)
#' res$rmse_reduction
#' @export
analyzeMLResiduals <- function(model, data = NULL) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' required for analyzeMLResiduals")
  }
  if (inherits(model, "formula")) {
    if (is.null(data)) stop("`data` must be supplied when `model` is a formula")
    checkData(data)
    lm_model <- lm(model, data = data)
  } else {
    checkModel(model)
    lm_model <- model
    if (is.null(data)) {
      data <- model.frame(model)
    } else {
      checkData(data)
    }
  }
  gam_model <- mgcv::gam(formula(lm_model), data = data)
  lm_res <- resid(lm_model)
  gam_res <- resid(gam_model)
  rmse <- function(x) sqrt(mean(x^2))
  lm_rmse <- rmse(lm_res)
  gam_rmse <- rmse(gam_res)
  list(
    gam_model = gam_model,
    lm_residuals = lm_res,
    gam_residuals = gam_res,
    rmse_reduction = lm_rmse - gam_rmse
  )
}

#' Compare diagnostics across multiple models
#'
#' Runs `runDiagnostics` for each supplied model and returns a data frame of
#' selected statistics for comparison.
#'
#' @param models A list of fitted models or formulas.
#' @param data Optional data frame used when models are formulas.
#' @param tests Heteroscedasticity tests to run.
#' @return A data frame with one row per model and columns for each test statistic.
#' @examples
#' data(mtcars)
#' m1 <- lm(mpg ~ wt + qsec, mtcars)
#' m2 <- lm(mpg ~ wt + hp, mtcars)
#' compareModelDiagnostics(list(m1, m2))
#' @export
compareModelDiagnostics <- function(models, data = NULL,
                                    tests = c("white", "breusch_pagan")) {
  res_list <- lapply(models, function(m) {
    if (inherits(m, "formula")) {
      if (is.null(data)) stop("`data` must be supplied when models include formulas")
      checkData(data)
      m <- lm(m, data = data)
    } else {
      checkModel(m)
    }
    diags <- runDiagnostics(m, data, tests = tests)
    stats <- sapply(diags[tests], function(d) d$statistic)
    stats
  })
  df <- do.call(rbind, res_list)
  df <- as.data.frame(df)
  rownames(df) <- paste0("Model", seq_along(models))
  df
}

#' Summarise heteroscedasticity test results
#'
#' Runs selected diagnostics for a single model and returns a tidy
#' data frame of test statistics and p-values.
#'
#' @param model A fitted `lm` model or a formula.
#' @param data Data frame used if `model` is a formula.
#' @param tests Character vector of heteroscedasticity tests.
#' @return Data frame with columns `test`, `statistic`, and `p.value`.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, mtcars)
#' compareTestResults(m)
#' @export
compareTestResults <- function(model, data = NULL,
                               tests = c("white", "breusch_pagan")) {
  if (inherits(model, "formula")) {
    if (is.null(data)) stop("`data` must be supplied when `model` is a formula")
    checkData(data)
    model <- lm(model, data = data)
  } else {
    checkModel(model)
    if (!is.null(data)) checkData(data)
  }
  res <- runHeteroTests(model, data, tests)
  df <- data.frame(
    test = names(res),
    statistic = sapply(res, function(x) x$statistic),
    p.value = sapply(res, function(x) x$p.value),
    row.names = NULL
  )
  df
}
