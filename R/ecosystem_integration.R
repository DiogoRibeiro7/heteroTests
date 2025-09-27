#' Ecosystem integration helpers
#'
#' These utilities extend the package to work smoothly with broader R
#' ecosystem tools such as tidymodels workflows, parsnip model objects,
#' `survey` designs, data.table-backed pipelines and broom tidiers. They are
#' exported only where user interaction is expected; internal helpers remain
#' undocumented.
#'
#' @keywords internal
".ht_null" <- NULL

`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

# internal ---------------------------------------------------------------

.ht_prepare_input_data <- function(data, context = "diagnostic") {
  if (is.null(data)) {
    return(list(
      data = NULL,
      grouped = FALSE,
      group_splits = NULL,
      group_keys = NULL,
      source = "unknown"
    ))
  }

  allowed_classes <- c("data.frame", "tbl_df", "data.table", "grouped_df", "dtplyr_step", "survey.design")
  if (!any(vapply(allowed_classes, inherits, logical(1), x = data))) {
    stop("`data` must be a data.frame or a compatible tibble/data.table object.", call. = FALSE)
  }

  if (inherits(data, "dtplyr_step")) {
    if (!requireNamespace("dtplyr", quietly = TRUE) || !requireNamespace("dplyr", quietly = TRUE)) {
      stop(
        sprintf(
          "dtplyr input supplied to %s but packages `dtplyr` and `dplyr` are not installed.",
          context
        ),
        call. = FALSE
      )
    }
    data <- dplyr::collect(data)
  }

  source <- class(data)[1]

  if (inherits(data, "grouped_df")) {
    if (!requireNamespace("dplyr", quietly = TRUE)) {
      stop(
        sprintf(
          "Grouped tibble supplied to %s but package `dplyr` is not installed.",
          context
        ),
        call. = FALSE
      )
    }
    group_keys <- dplyr::group_keys(data)
    splits <- dplyr::group_split(data, .keep = TRUE)
    splits <- lapply(splits, function(df) {
      df <- dplyr::ungroup(df)
      as.data.frame(df)
    })
    data <- as.data.frame(dplyr::ungroup(data))
    grouped <- TRUE
  } else {
    group_keys <- NULL
    splits <- NULL
    grouped <- FALSE
  }

  if (inherits(data, "data.table")) {
    data <- as.data.frame(data)
  }

  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  list(
    data = data,
    grouped = grouped,
    group_splits = splits,
    group_keys = group_keys,
    source = source
  )
}

.ht_extract_formula <- function(model) {
  if (inherits(model, "formula")) {
    return(model)
  }
  fm <- tryCatch(stats::formula(model), error = function(e) NULL)
  if (!is.null(fm)) {
    return(fm)
  }
  call <- tryCatch(model$call, error = function(e) NULL)
  if (!is.null(call) && "formula" %in% names(call)) {
    return(eval(call$formula, envir = parent.frame()))
  }
  NULL
}

.ht_fit_from_formula <- function(formula, data, weights = NULL, engine = c("lm", "glm")) {
  engine <- match.arg(engine)
  if (identical(engine, "glm")) {
    stats::glm(formula, data = data, weights = weights)
  } else {
    stats::lm(formula, data = data, weights = weights)
  }
}

.ht_prepare_model <- function(model, data = NULL, context = "diagnostic", allow_grouped = TRUE) {
  if (inherits(data, "survey.design")) {
    if (!requireNamespace("survey", quietly = TRUE)) {
      stop("`survey` design supplied but the survey package is not installed.", call. = FALSE)
    }
    if (!inherits(model, "formula")) {
      stop("When supplying a survey design you must provide the model as a formula.", call. = FALSE)
    }
    fit <- survey::svyglm(model, design = data)
    mf <- stats::model.frame(fit)
    prep <- .ht_prepare_input_data(as.data.frame(mf), context)
    return(list(
      model = fit,
      data = prep$data,
      grouped = prep$grouped,
      group_splits = prep$group_splits,
      group_keys = prep$group_keys,
      fit_factory = function(df) survey::svyglm(model, design = survey::svydesign(ids = ~1, data = df, weights = stats::weights(fit))),
      formula = model,
      engine = "svyglm",
      source = "survey"
    ))
  }

  if (inherits(model, "workflow")) {
    if (!requireNamespace("workflows", quietly = TRUE)) {
      stop("tidymodels workflow supplied but the workflows package is not installed.", call. = FALSE)
    }
    parsnip_fit <- workflows::extract_fit_parsnip(model)
    mold_data <- tryCatch(workflows::extract_mold(model)$predictors, error = function(e) NULL)
    # fall back to `data` argument if mold extraction fails
    data <- data %||% mold_data
    return(.ht_prepare_model(parsnip_fit, data = data, context = context, allow_grouped = allow_grouped))
  }

  if (inherits(model, "model_fit")) {
    engine_fit <- model$fit
    if (inherits(engine_fit, c("lm", "glm"))) {
      data <- data %||% tryCatch(engine_fit$model, error = function(e) NULL)
      prep <- .ht_prepare_input_data(data, context)
      formula <- .ht_extract_formula(engine_fit)
      return(list(
        model = engine_fit,
        data = prep$data,
        grouped = prep$grouped && allow_grouped,
        group_splits = if (allow_grouped) prep$group_splits else NULL,
        group_keys = if (allow_grouped) prep$group_keys else NULL,
        fit_factory = if (!is.null(formula) && allow_grouped) {
          function(df) .ht_fit_from_formula(formula, df, weights = tryCatch(stats::weights(engine_fit), error = function(e) NULL), engine = if (inherits(engine_fit, "glm")) "glm" else "lm")
        } else {
          NULL
        },
        formula = formula,
        engine = class(engine_fit)[1],
        source = "parsnip"
      ))
    }
    stop("Unsupported parsnip engine for heteroscedasticity diagnostics.", call. = FALSE)
  }

  if (inherits(model, "formula")) {
    if (is.null(data)) {
      stop("`data` must be supplied when `model` is a formula.", call. = FALSE)
    }
    prep <- .ht_prepare_input_data(data, context)
    fit <- stats::lm(model, data = prep$data)
    return(list(
      model = fit,
      data = prep$data,
      grouped = prep$grouped && allow_grouped,
      group_splits = if (allow_grouped) prep$group_splits else NULL,
      group_keys = if (allow_grouped) prep$group_keys else NULL,
      fit_factory = if (allow_grouped) function(df) stats::lm(model, data = df) else NULL,
      formula = model,
      engine = "lm",
      source = "formula"
    ))
  }

  if (inherits(model, c("lm", "glm"))) {
    base_data <- data %||% tryCatch(model.frame(model), error = function(e) NULL)
    prep <- .ht_prepare_input_data(base_data, context)
    formula <- .ht_extract_formula(model)
    weights <- tryCatch(stats::weights(model), error = function(e) NULL)
    fit_factory <- if (!is.null(formula) && allow_grouped) {
      function(df) .ht_fit_from_formula(formula, df, weights = weights, engine = if (inherits(model, "glm")) "glm" else "lm")
    } else {
      NULL
    }
    return(list(
      model = model,
      data = prep$data,
      grouped = prep$grouped && allow_grouped,
      group_splits = if (allow_grouped) prep$group_splits else NULL,
      group_keys = if (allow_grouped) prep$group_keys else NULL,
      fit_factory = fit_factory,
      formula = formula,
      engine = class(model)[1],
      source = "model"
    ))
  }

  stop("Unsupported model type for heteroscedasticity diagnostics; supply an 'lm'/'glm', formula, tidymodels workflow or parsnip model.", call. = FALSE)
}

.ht_decorate_result <- function(result, diagnostic_name, model, data, extras = list()) {
  if (!inherits(result, "htest")) {
    return(result)
  }

  if (!inherits(result, "hetero_test")) {
    class(result) <- c("hetero_test", class(result))
  }

  attr(result, "diagnostic") <- diagnostic_name %||% attr(result, "diagnostic") %||% NA_character_
  attr(result, "nobs") <- attr(result, "nobs") %||% tryCatch(stats::nobs(model), error = function(e) if (!is.null(data)) nrow(data) else NA_integer_)
  attr(result, "model") <- attr(result, "model") %||% model
  attr(result, "data_source") <- attr(result, "data_source") %||% if (!is.null(data)) class(data)[1] else NA_character_
  attr(result, "extras") <- utils::modifyList(attr(result, "extras") %||% list(), extras)
  result
}

.ht_failure_result <- function(diagnostic_name, model, data, message,
                              suggestions = character(), issue = NULL) {
  base <- structure(
    list(
      statistic = c(NA_real_),
      parameter = NA_real_,
      p.value = NA_real_,
      method = sprintf("%s (failed)", diagnostic_name),
      data.name = tryCatch(
        deparse(stats::formula(model)),
        error = function(e) "model"
      )
    ),
    class = "htest"
  )
  .ht_decorate_result(
    base,
    diagnostic_name = diagnostic_name,
    model = model,
    data = data,
    extras = list(
      status = "error",
      message = message,
      suggestions = suggestions,
      issue = issue
    )
  )
}

ht_tidy_single <- function(x) {
  statistic <- x$statistic
  parameter <- x$parameter %||% NA_real_
  estimate <- x$estimate %||% NA_real_
  method <- x$method %||% NA_character_
  alternative <- x$alternative %||% NA_character_
  extras <- attr(x, "extras") %||% list()
  status <- extras$status %||% "ok"
  failure_message <- extras$message %||% NA_character_
  suggestion_text <- extras$suggestions %||% NA_character_
  data.frame(
    diagnostic = attr(x, "diagnostic") %||% names(statistic),
    statistic = unname(statistic),
    parameter = unname(parameter),
    p.value = x$p.value %||% NA_real_,
    estimate = if (length(estimate) > 0) unname(estimate) else NA_real_,
    alternative = alternative,
    method = method,
    nobs = attr(x, "nobs") %||% NA_integer_,
    status = status,
    message = failure_message,
    suggestions = if (length(suggestion_text) == 0L) {
      NA_character_
    } else if (length(suggestion_text) > 1L) {
      paste(unique(suggestion_text), collapse = "\n")
    } else {
      suggestion_text
    },
    stringsAsFactors = FALSE
  )
}

# broom methods ----------------------------------------------------------

#' @title Tidy heteroscedasticity test results
#' @name tidy.hetero_test
#' @description Methods that integrate heteroscedasticity diagnostics with the
#'   broom generics provided by the `generics` package (and re-exported by
#'   broom). They produce tidy data frames suited for downstream analysis.
#' @param x A heteroscedasticity diagnostic result or collection.
#' @param ... Additional arguments passed to methods.
#' @param data Optional data frame supplied to [generics::augment()].
#' @return A data frame (for `tidy` and `glance`) or an augmented data frame (for
#'   `augment`).
#' @examples
#' data(mtcars)
#' fit <- lm(mpg ~ wt + qsec, data = mtcars)
#' res <- runHeteroTests(fit, mtcars, tests = "white")
#' generics::tidy(res[[1]])
#' generics::tidy(res)
#' @export
#' @method tidy hetero_test
#' @importFrom generics tidy
#' @seealso [generics::tidy()], [generics::glance()], [generics::augment()]
tidy.hetero_test <- function(x, ...) {
  ht_tidy_single(x)
}

#' @export
#' @method glance hetero_test
#' @importFrom generics glance
glance.hetero_test <- function(x, ...) {
  data.frame(
    diagnostic = attr(x, "diagnostic") %||% NA_character_,
    statistic = unname(x$statistic),
    parameter = unname(x$parameter %||% NA_real_),
    p.value = x$p.value %||% NA_real_,
    method = x$method %||% NA_character_,
    nobs = attr(x, "nobs") %||% NA_integer_,
    stringsAsFactors = FALSE
  )
}

#' @export
#' @method augment hetero_test
#' @importFrom generics augment
augment.hetero_test <- function(x, data = NULL, ...) {
  model <- attr(x, "model")
  if (is.null(model)) {
    stop("Augmenting heteroscedasticity diagnostics requires the fitted model to be available.", call. = FALSE)
  }
  data <- data %||% tryCatch(model$model, error = function(e) NULL)
  if (is.null(data)) {
    stop("Unable to recover the data used to fit the model. Supply it explicitly via `data=`.", call. = FALSE)
  }
  df <- as.data.frame(data)
  df$.fitted <- tryCatch(stats::fitted(model), error = function(e) rep(NA_real_, nrow(df)))
  df$.resid <- tryCatch(stats::residuals(model), error = function(e) rep(NA_real_, nrow(df)))
  df$.std.resid <- tryCatch(stats::rstandard(model), error = function(e) rep(NA_real_, nrow(df)))
  df$.diagnostic <- attr(x, "diagnostic") %||% NA_character_
  df
}

# collections ------------------------------------------------------------

#' @export
#' @method tidy hetero_test_suite
tidy.hetero_test_suite <- function(x, ...) {
  pieces <- lapply(x, function(res) {
    if (inherits(res, "hetero_test")) {
      ht_tidy_single(res)
    } else {
      NULL
    }
  })
  pieces <- Filter(Negate(is.null), pieces)
  if (length(pieces) == 0) {
    return(data.frame())
  }
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

#' @export
#' @method glance hetero_test_suite
glance.hetero_test_suite <- function(x, ...) {
  tidy_res <- tidy.hetero_test_suite(x)
  if (nrow(tidy_res) == 0) {
    return(data.frame(n_diagnostics = 0L, any_significant = FALSE))
  }
  data.frame(
    n_diagnostics = nrow(tidy_res),
    any_significant = any(tidy_res$p.value < 0.05, na.rm = TRUE),
    min_p.value = min(tidy_res$p.value, na.rm = TRUE),
    max_p.value = max(tidy_res$p.value, na.rm = TRUE)
  )
}

#' @export
#' @method augment hetero_test_suite
augment.hetero_test_suite <- function(x, data = NULL, ...) {
  pieces <- lapply(x, function(res) {
    if (inherits(res, "hetero_test")) {
      augment.hetero_test(res, data = data, ...)
    } else {
      NULL
    }
  })
  pieces <- Filter(Negate(is.null), pieces)
  if (length(pieces) == 0) {
    return(data.frame())
  }
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

# grouped ---------------------------------------------------------------

#' @export
#' @method tidy hetero_grouped_suite
tidy.hetero_grouped_suite <- function(x, ...) {
  keys <- attr(x, "group_keys")
  if (is.null(keys)) {
    return(data.frame())
  }
  group_frames <- vector("list", length(x))
  for (i in seq_along(x)) {
    diag_res <- x[[i]]
    if (!inherits(diag_res, "hetero_test_suite")) {
      next
    }
    tidy_res <- tidy.hetero_test_suite(diag_res)
    if (nrow(tidy_res) == 0) {
      next
    }
    key_row <- keys[rep(i, nrow(tidy_res)), , drop = FALSE]
    group_frames[[i]] <- cbind(as.data.frame(key_row), tidy_res, row.names = NULL)
  }
  group_frames <- Filter(Negate(is.null), group_frames)
  if (length(group_frames) == 0) {
    return(data.frame())
  }
  out <- do.call(rbind, group_frames)
  rownames(out) <- NULL
  out
}

# public interface ------------------------------------------------------

#' Run heteroscedasticity diagnostics on survey designs
#'
#' Provides a convenience wrapper that fits a survey-weighted linear model via
#' [survey::svyglm()] before running [runHeteroTests()]. The fitted model and
#' diagnostics share the same interface as the unweighted tools.
#'
#' @param formula Model formula specifying the mean structure.
#' @param design A [survey::survey.design] object describing the complex survey
#'   design, including weights and (optionally) strata and clusters.
#' @param tests Character vector of diagnostic names forwarded to
#'   [runHeteroTests()]. Defaults to the White and Breusch-Pagan tests.
#' @param ... Additional arguments passed to [runHeteroTests()].
#' @return A [`hetero_test_suite`] object containing the diagnostic results.
#' @export
#' @examples
#' 
#' if (requireNamespace("survey", quietly = TRUE)) {
#'   library(survey)
#'   data(api)
#'   dstrata <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat)
#'   res <- runSurveyHeteroTests(api00 ~ api99 + ell, dstrata, tests = "white")
#'   generics::tidy(res)
#' }
runSurveyHeteroTests <- function(formula, design, tests = c("white", "breusch_pagan"), ...) {
  if (!requireNamespace("survey", quietly = TRUE)) {
    stop("The survey package must be installed to analyse survey designs.", call. = FALSE)
  }
  if (!inherits(formula, "formula")) {
    stop("`formula` must be a model formula when analysing survey designs.", call. = FALSE)
  }
  if (!inherits(design, "survey.design")) {
    stop("`design` must be a survey::svydesign() object describing the survey structure.", call. = FALSE)
  }
  data <- as.data.frame(design$variables)
  weights <- tryCatch(stats::weights(design), error = function(e) NULL)
  if (is.null(weights)) {
    weights <- tryCatch(design$prob, error = function(e) NULL)
    if (!is.null(weights)) {
      weights <- 1 / weights
    }
  }
  if (!is.null(weights)) {
    weights <- as.numeric(weights)
  }
  fit <- tryCatch(
    stats::lm(formula, data = data, weights = weights),
    error = function(e) stats::lm(formula, data = data)
  )
  runHeteroTests(fit, data = data, tests = tests, use_cache = FALSE, ...)
}

