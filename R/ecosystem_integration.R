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

# Single canonical definition shared with intelligent_recommendations.R; both
# copies must stay identical so package collation order cannot change behaviour.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) {
    return(y)
  }
  if (is.atomic(x) && length(x) == 1L && is.na(x)) {
    return(y)
  }
  x
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

# Refit a fitted lm/glm on another data frame by evaluating its own call again,
# so the family, weights, offset, subset and na.action are the ones the model
# was fitted with. Passing those to stats::lm() as local variables does not
# work: lm() looks them up in `data` and then in the environment of the formula,
# never in the function that calls it.
.ht_refit_model <- function(model, data) {
  call <- tryCatch(stats::getCall(model), error = function(e) NULL)
  formula <- .ht_extract_formula(model)
  if (!is.call(call) || is.null(formula)) {
    stop(
      "The fitted model does not record the call that produced it, so it ",
      "cannot be refitted on other data.",
      call. = FALSE
    )
  }
  call$formula <- formula
  call$data <- quote(.ht_refit_data)
  enclosure <- environment(formula)
  if (!is.environment(enclosure)) {
    enclosure <- globalenv()
  }
  env <- new.env(parent = enclosure)
  env$.ht_refit_data <- data
  eval(call, env)
}

# The same, for one group of a grouped analysis. A model whose weights, offset
# or subset are vectors in the workspace cannot be split by group, and lm()
# reports that as a length mismatch; say what it means.
.ht_refit_group <- function(model, data) {
  tryCatch(
    .ht_refit_model(model, data),
    error = function(e) {
      stop(
        "The model could not be refitted within a group: ",
        conditionMessage(e), "\n",
        "Grouped diagnostics evaluate the model's own call on each group, so ",
        "every variable it uses, including weights, offset and subset, must ",
        "be a column of `data`.",
        call. = FALSE
      )
    }
  )
}

# What a survey design gets: an ordinary least-squares fit to its data. The
# diagnostics have no design-based version, and the alternatives that look
# like one are worse. Sampling weights are not precision weights, so a fit by
# lm(weights = ) or survey::svyglm(), whose residuals() carry the square roots
# of the weights, makes every test reject a homoscedastic model whenever the
# weights vary with the regressors. inst/validation/survey-designs-size.R has
# the measurements.
.ht_survey_ols <- function(formula, design, context) {
  data <- as.data.frame(design$variables)
  ignored <- .ht_survey_ignored_features(design)
  if (length(ignored) > 0L) {
    warning(
      context, " does not use the ", paste(ignored, collapse = " or "),
      " of the survey design. The model is fitted by ordinary least squares ",
      "and the tests treat the observations as an independent sample, so ",
      "they hold their level only if the design can be ignored given the ",
      "regressors. See ?runSurveyHeteroTests.",
      call. = FALSE
    )
  }
  list(model = stats::lm(formula, data = data), data = data)
}

# The parts of a design the classical tests cannot see: unequal weights, and
# sampling units that hold more than one observation.
.ht_survey_ignored_features <- function(design) {
  ignored <- character()
  probabilities <- tryCatch(as.numeric(design$prob), error = function(e) numeric())
  probabilities <- probabilities[is.finite(probabilities)]
  spread <- if (length(probabilities) > 1L) diff(range(probabilities)) else 0
  if (spread > 1e-8 * max(probabilities, 0)) {
    ignored <- c(ignored, "sampling weights")
  }
  units <- tryCatch(design$cluster[[1L]], error = function(e) NULL)
  if (!is.null(units) && anyDuplicated(units) > 0L) {
    ignored <- c(ignored, "clusters")
  }
  ignored
}

.ht_prepare_model <- function(model, data = NULL, context = "diagnostic", allow_grouped = TRUE) {
  if (inherits(data, "survey.design")) {
    if (!inherits(model, "formula")) {
      stop(
        "When supplying a survey design you must provide the model as a formula.",
        call. = FALSE
      )
    }
    fit <- .ht_survey_ols(model, data, context)
    return(list(
      model = fit$model,
      data = fit$data,
      grouped = FALSE,
      group_splits = NULL,
      group_keys = NULL,
      fit_factory = NULL,
      formula = model,
      engine = "lm",
      source = "survey"
    ))
  }

  if (inherits(model, "workflow")) {
    if (!requireNamespace("workflows", quietly = TRUE)) {
      stop("tidymodels workflow supplied but the workflows package is not installed.", call. = FALSE)
    }
    parsnip_fit <- workflows::extract_fit_parsnip(model)
    # The engine's fit names the variables as the workflow's preprocessing
    # made them, its response `..y` among them, so the data the workflow was
    # fitted to do not hold them: they are mapped to them by that
    # preprocessing, and once their rows are confirmed the model frame of the
    # engine's fit is tested, as without `data` (N5, .ht_workflow_data()).
    if (!is.null(data)) {
      data <- .ht_workflow_data(model, parsnip_fit$fit, data, context)
    }
    # Without `data`, the data of the fit are the model frame of the engine's
    # fit (below), which holds its response and its predictors. Before 0.16.0
    # they were the predictors of the workflow's mold, which lack the
    # response: every test refused them, and runHeteroTests() returned the
    # Cook-Weisberg statistic, which ignored them, under the names asked for.
    return(.ht_prepare_model(parsnip_fit, data = data, context = context, allow_grouped = allow_grouped))
  }

  if (inherits(model, "model_fit")) {
    engine_fit <- model$fit
    if (inherits(engine_fit, c("lm", "glm"))) {
      # A fit of fit_xy() names its response `..y` too (N5,
      # .ht_parsnip_data()).
      if (!is.null(data)) {
        data <- .ht_parsnip_data(model, engine_fit, data, context)
      }
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
          function(df) .ht_refit_group(engine_fit, df)
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
    fit_factory <- if (!is.null(formula) && allow_grouped) {
      function(df) .ht_refit_group(model, df)
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

# The variables of the engine's fit of a workflow or a parsnip fit that `data`
# lacks, or NULL when its terms cannot be read.
.ht_engine_lacks <- function(engine_fit, data) {
  engine_terms <- tryCatch(stats::terms(engine_fit), error = function(e) NULL)
  if (is.null(engine_terms)) {
    return(NULL)
  }
  setdiff(all.vars(engine_terms), names(data))
}

# A lazy dtplyr table collected into a data frame, as .ht_prepare_input_data()
# collects it, so that its columns are those of the data and not of the lazy
# object (from the review of increment 10: `names()` of a lazy table are those
# of its parts, "parent", "vars", ...).
.ht_collect <- function(data, context) {
  if (!inherits(data, "dtplyr_step")) {
    return(data)
  }
  if (!requireNamespace("dtplyr", quietly = TRUE) || !requireNamespace("dplyr", quietly = TRUE)) {
    stop(
      sprintf(
        "dtplyr input supplied to %s but packages `dtplyr` and `dplyr` are not installed.",
        context
      ),
      call. = FALSE
    )
  }
  dplyr::collect(data)
}

# The refusal of `data` that does not hold the variables of the engine's fit
# of a workflow and cannot be mapped to them, `why` saying why.
.ht_stop_engine_data <- function(lacking, why) {
  stop(
    "A tidymodels workflow is tested on the fit of its engine, whose variables are ",
    "those the workflow's preprocessing made (it names the response `..y`), and `data` ",
    "lacks ", paste(lacking, collapse = ", "), "; ", why, " Pass `data = NULL`, which ",
    "tests the model frame of that fit.",
    call. = FALSE
  )
}

#' The data of a tidymodels workflow, mapped to the variables of its engine
#'
#' A workflow is tested on the fit of its engine, which was fitted to the data
#' its preprocessing made: the predictors of its mold, under the names it gave
#' them (`log(x1)`, the dummies of a factor, a column a recipe transformed),
#' and the response as `..y`. The data the workflow was fitted to hold none of
#' those, and before 0.17.0 they were refused ("`data` lacks ..y. Pass
#' `data = NULL`"; N5 of the design of the model context). They are mapped to
#' the variables of the engine by the workflow's own preprocessing,
#' `hardhat::forge()` with its blueprint and the outcome: the predictors, and
#' as `..y` the outcome the preprocessing makes, transformed by a recipe or a
#' formula as it was at the fit (decision 35 of the design); and the case
#' weights of a workflow with them, under the name the engine's call gives
#' them, so that the alignment compares them as it compares the weights of a
#' fit for the tests.
#'
#' @param workflow The fitted workflow.
#' @param engine_fit The fit of its engine, an `lm` or a `glm`.
#' @param data A data frame, not grouped.
#' @param lacking The variables of the engine `data` lacks, for the messages.
#' @return A list: `data`, the mapped data frame; `source`, the row of `data`
#'   each of its rows was made from, or `NULL` when the preprocessing dropped
#'   rows of `data` (a step it applies to new data, such as `step_filter()`
#'   with `skip = FALSE`); and `fitted_rows`, the number of rows the
#'   preprocessing kept of the data the workflow was fitted to.
#' @keywords internal
#' @noRd
.ht_workflow_mapped <- function(workflow, engine_fit, data, lacking) {
  if (!requireNamespace("hardhat", quietly = TRUE)) {
    .ht_stop_engine_data(lacking, paste(
      "the package hardhat, whose hardhat::forge() maps `data` to them by the workflow's",
      "preprocessing, is not installed."
    ))
  }
  mold <- tryCatch(workflows::extract_mold(workflow), error = function(e) NULL)
  if (is.null(mold)) {
    .ht_stop_engine_data(
      lacking, "the workflow's preprocessing cannot be read, so `data` cannot be mapped to them."
    )
  }
  forged <- tryCatch(
    suppressWarnings(hardhat::forge(data, mold$blueprint, outcomes = TRUE)),
    error = function(e) e
  )
  if (inherits(forged, "error")) {
    .ht_stop_engine_data(lacking, sprintf(
      "the workflow's preprocessing cannot map `data` to them (%s).",
      gsub("\\s+", " ", conditionMessage(forged))
    ))
  }
  outcomes <- forged$outcomes
  if (NCOL(outcomes) != 1L) {
    .ht_stop_engine_data(lacking, "the workflow has more than one outcome.")
  }
  mapped <- as.data.frame(forged$predictors)
  mapped[["(Intercept)"]] <- NULL
  mapped[["..y"]] <- outcomes[[1L]]
  kept_all <- nrow(mapped) == nrow(data)
  # The case weights, under the name of the engine's call (`weights`).
  weights_name <- all.vars(engine_fit$call$weights)
  case_weights <- tryCatch(all.vars(workflow$pre$actions$case_weights$col),
                           error = function(e) character())
  if (kept_all && length(weights_name) == 1L && length(case_weights) == 1L &&
        case_weights %in% names(data) && !weights_name %in% names(mapped)) {
    mapped[[weights_name]] <- as.double(unclass(data[[case_weights]]))
  }
  list(data = mapped, source = if (kept_all) seq_len(nrow(data)),
       fitted_rows = NROW(mold$predictors))
}

#' The data of a parsnip fit made with fit_xy(), mapped to its engine
#'
#' [parsnip::fit_xy()] fits the engine to the columns of `x` and to the
#' outcome as `..y`, which the data the fit was made from do not hold; before
#' 0.17.0 every test refused them (N5 of the design of the model context). The
#' outcome is mapped to `..y` from the column the fit records
#' (`preproc$y_var`, when `y` was given as a data frame). An outcome given as
#' a vector is not recorded, and such data are refused with a message that
#' says so.
#'
#' @param model_fit The parsnip fit.
#' @param data A data frame, not grouped.
#' @return As for `.ht_workflow_mapped()`.
#' @keywords internal
#' @noRd
.ht_parsnip_mapped <- function(model_fit, data) {
  y_var <- model_fit$preproc$y_var
  if (length(y_var) != 1L || !is.character(y_var) || !y_var %in% names(data)) {
    stop(
      "A parsnip fit made with fit_xy() is tested on the fit of its engine, which names ",
      "the response `..y`, and `data` lacks it; ",
      if (length(y_var) == 1L && is.character(y_var)) {
        sprintf("`data` lacks the outcome `%s` of the fit too.", y_var)
      } else {
        "the fit does not record the column of its outcome, which was given as a vector."
      },
      " Pass `data = NULL`, which tests the model frame of that fit.",
      call. = FALSE
    )
  }
  mapped <- as.data.frame(data)
  mapped[["..y"]] <- data[[y_var]]
  list(data = mapped, source = seq_len(nrow(data)), fitted_rows = NULL)
}

#' The data of a workflow or a parsnip fit, mapped and matched to its engine
#'
#' The data the workflow or the parsnip fit was made from, mapped to the
#' variables of the fit of its engine (`.ht_workflow_mapped()`,
#' `.ht_parsnip_mapped()`), and their rows matched to its observations by the
#' alignment of the model context ([rmodel_context()]), which compares every
#' column of its model frame. A preprocessing that dropped rows of the data at
#' the fit with a step it skips on new data, as `step_naomit()` and
#' `step_filter()` do by default, numbers the rows it kept afresh: the mapped
#' rows complete in the variables of the engine are taken in their order when
#' they are as many as the rows it kept, and confirmed in turn; otherwise the
#' data are refused, with a message that says so. A refusal of the rows says
#' what was matched (`lead`), what the alignment found and what to pass, and
#' has the class of the guard, `heteroTests_data_rows_error`, final in the
#' runners: data in another order than the fit, data that lack rows of it
#' ("Missing rows") and data whose rows are not those of the fit alike.
#'
#' @param object The workflow or the parsnip fit.
#' @param engine_fit The fit of its engine.
#' @param data The data, collected; `NULL` is returned for data that hold the
#'   variables of the engine already, as its model frame does.
#' @param context Character scalar naming the calling function, as
#'   `"runHeteroTests()"`.
#' @param beyond Does the caller read columns of `data` beyond the model frame
#'   (`augment()`, which binds them)? See [rmodel_context()].
#' @return `NULL` when `data` hold the variables of the engine; otherwise a
#'   list with `ctx`, the context of the engine's fit matched to the mapped
#'   rows, and `source`, the row of `data` of each mapped row (or `NULL`).
#' @keywords internal
#' @noRd
.ht_engine_match <- function(object, engine_fit, data, context, beyond = FALSE) {
  lacking <- .ht_engine_lacks(engine_fit, data)
  if (length(lacking) == 0L) {
    return(NULL)
  }
  workflow <- inherits(object, "workflow")
  if (!workflow && !identical(lacking, "..y")) {
    return(NULL)
  }
  if (!is.data.frame(data) || inherits(data, "grouped_df")) {
    if (!workflow) {
      return(NULL)
    }
    .ht_stop_engine_data(lacking, "only an ungrouped data frame is mapped to them.")
  }
  mapped <- if (workflow) {
    .ht_workflow_mapped(object, engine_fit, data, lacking)
  } else {
    .ht_parsnip_mapped(object, data)
  }
  lead <- if (workflow) {
    paste(
      "A tidymodels workflow is tested on the fit of its engine, which was fitted to the",
      "data the workflow's preprocessing made, numbered in their order, and `data`,",
      "mapped by that preprocessing (hardhat::forge()), does not hold its observations:"
    )
  } else {
    paste(
      "A parsnip fit made with fit_xy() is tested on the fit of its engine, and `data`,",
      "its outcome taken as the response `..y`, does not hold its observations:"
    )
  }
  advice <- if (workflow) {
    paste(
      "Pass the data the workflow was fitted to, in the order it was fitted in, or",
      "`data = NULL`, which tests the model frame of that fit."
    )
  } else {
    "Pass the data the fit was made from, or `data = NULL`, which tests the model frame of that fit."
  }
  fn <- sub("\\(\\)$", "", context)
  attempt <- function(rows) {
    tryCatch(
      rmodel_context(engine_fit, rows, fn = fn, test_label = context, beyond = beyond),
      error = function(e) e
    )
  }
  refused_rows <- function(e) {
    inherits(e, "heteroTests_data_rows_error") ||
      inherits(e, "error") && grepl("requires `data`", conditionMessage(e), fixed = TRUE)
  }
  ctx <- attempt(mapped$data)
  fitted_rows <- mapped$fitted_rows
  if (refused_rows(ctx) && !is.null(fitted_rows) && nrow(mapped$data) > fitted_rows) {
    variables <- intersect(all.vars(stats::terms(engine_fit)), names(mapped$data))
    complete <- stats::complete.cases(mapped$data[, variables, drop = FALSE])
    if (sum(complete) != fitted_rows) {
      stop(structure(
        class = c("heteroTests_data_rows_error", "error", "condition"),
        list(
          message = paste(
            sprintf(
              paste(
                "A tidymodels workflow is tested on the fit of its engine, and its preprocessing",
                "kept %d rows of the data it was fitted to, numbered afresh, where it maps the %d",
                "rows of `data` without dropping any: a step it skips on new data, such as",
                "step_naomit() or step_filter() with skip = TRUE, dropped rows at the fit only,",
                "so the rows of `data` cannot be matched to the observations of that fit."
              ),
              fitted_rows, nrow(mapped$data)
            ),
            "Pass `data = NULL`, which tests the model frame of that fit."
          ),
          call = NULL
        )
      ))
    }
    kept <- mapped$data[complete, , drop = FALSE]
    rownames(kept) <- NULL
    retry <- attempt(kept)
    if (!inherits(retry, "error")) {
      ctx <- retry
      mapped$source <- mapped$source[complete]
    }
  }
  if (inherits(ctx, "error")) {
    if (!refused_rows(ctx)) {
      stop(ctx)
    }
    found <- sub(" (Pass|Refit) .*$", "", conditionMessage(ctx))
    stop(structure(
      class = c("heteroTests_data_rows_error", "error", "condition"),
      list(message = paste(lead, found, advice), call = NULL)
    ))
  }
  list(ctx = ctx, source = mapped$source)
}

#' The data of a tidymodels workflow, for the runners
#'
#' The data a workflow was fitted to, mapped to the variables of its engine
#' and matched to its observations (`.ht_engine_match()`); once they are, the
#' runners test the model frame of the engine's fit, as without `data`, so
#' they give its value (N5 of the design of the model context). Data that
#' hold the variables of the engine already, such as its model frame, are
#' left as they are; a lazy dtplyr table is collected first.
#'
#' @param workflow The fitted workflow, or a parsnip fit.
#' @param engine_fit The fit of its engine.
#' @param data The `data` argument of the runner, not `NULL`.
#' @param context Character scalar naming the runner, as `"runHeteroTests()"`.
#' @return `data`, or `NULL` when it was mapped and matched, for the model
#'   frame of that fit.
#' @keywords internal
#' @noRd
.ht_workflow_data <- function(workflow, engine_fit, data, context) {
  data <- .ht_collect(data, context)
  if (is.null(.ht_engine_match(workflow, engine_fit, data, context))) {
    return(data)
  }
  NULL
}
.ht_parsnip_data <- .ht_workflow_data

# The tidymodels object a suite of the runners was computed from, a workflow
# or a parsnip fit, kept on each of its results, so that augment() can map
# the data of that object to the variables of its engine as the runners map
# them (from the review of increment 10).
rsuite_mark_tidymodels <- function(suite, object) {
  if (!inherits(object, c("workflow", "model_fit"))) {
    return(suite)
  }
  for (i in seq_along(suite)) {
    if (inherits(suite[[i]], "hetero_test")) {
      attr(suite[[i]], "tidymodels") <- object
    }
  }
  suite
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

# One row per test. A result can carry two parameters (the degrees of freedom
# of an F statistic, or a degree of freedom and a number of replications) and
# several estimates. Passing those vectors to data.frame() recycled the row, so
# such a test appeared twice in the tidy output and twice in every table and
# plot built from it. The second parameter gets its own column, and an estimate
# is reported only when there is exactly one.
ht_tidy_single <- function(x) {
  element <- function(value, i) {
    if (length(value) >= i) unname(value[[i]]) else NA_real_
  }
  estimate <- x$estimate
  method <- x$method %||% NA_character_
  alternative <- x$alternative %||% NA_character_
  extras <- attr(x, "extras") %||% list()
  status <- extras$status %||% "ok"
  failure_message <- extras$message %||% NA_character_
  suggestion_text <- extras$suggestions %||% NA_character_
  data.frame(
    diagnostic = attr(x, "diagnostic") %||% names(x$statistic)[1L] %||% NA_character_,
    statistic = element(x$statistic, 1L),
    parameter = element(x$parameter, 1L),
    parameter2 = element(x$parameter, 2L),
    p.value = element(x$p.value, 1L),
    estimate = if (length(estimate) == 1L) unname(estimate[[1L]]) else NA_real_,
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
  tidied <- ht_tidy_single(x)
  data.frame(
    diagnostic = attr(x, "diagnostic") %||% NA_character_,
    statistic = tidied$statistic,
    parameter = tidied$parameter,
    parameter2 = tidied$parameter2,
    p.value = tidied$p.value,
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
  given <- !is.null(data)
  data <- data %||% tryCatch(model$model, error = function(e) NULL)
  if (is.null(data)) {
    stop("Unable to recover the data used to fit the model. Supply it explicitly via `data=`.", call. = FALSE)
  }
  fitted_values <- tryCatch(stats::fitted(model), error = function(e) NULL)
  residual_values <- tryCatch(stats::residuals(model), error = function(e) NULL)
  standardised <- tryCatch(stats::rstandard(model), error = function(e) NULL)
  bound <- NULL
  if (given && !is.null(fitted_values)) {
    # The rows of `data` matched to the observations of the fit as the tests
    # match them, and bound as broom binds them (decision 34 of the design of
    # the model context, raugment_bind()).
    bound <- raugment_bind(.ht_collect(data, "augment()"), model, attr(x, "tidymodels"))
    df <- bound$df
  } else {
    df <- as.data.frame(data)
    # One row per value of fitted(): one per observation of the fit, or, for a
    # fit with na.action = na.exclude, whose fitted values, residuals and
    # standardised residuals are padded with NA at the rows it dropped, one
    # per row of the data it was fitted to. The model frame, without `data`,
    # holds the rows of the observations only, and such a fit pads them with
    # NA rows there, so that the result is the result of the same fit with
    # na.omit padded as naresid() pads it (B13, increment 9 of the model
    # context); before, the columns did not fit the rows ("replacement has
    # 120 rows, data has 117").
    if (!is.null(fitted_values)) {
      df <- raugment_rows(df, model, length(fitted_values))
    }
  }
  at <- function(values) {
    if (is.null(values)) {
      return(rep(NA_real_, nrow(df)))
    }
    if (is.null(bound$positions)) values else unname(values)[bound$positions]
  }
  df$.fitted <- at(fitted_values)
  df$.resid <- at(residual_values)
  df$.std.resid <- at(standardised)
  df$.diagnostic <- attr(x, "diagnostic") %||% NA_character_
  df
}

#' The rows of `data` augment() binds, as broom binds them
#'
#' `generics::augment()` of the results of the runner binds the columns of the
#' fit, its fitted values, residuals and standardised residuals, to the rows of
#' `data`. Until increment 10 of the model context it bound them row by row:
#' named data in another order than the fit had the values of other
#' observations bound to them, and data of another number of rows were refused
#' (since increment 9), on a fit with na.exclude bound row by row too. The
#' rows of `data` are now matched to the observations of the fit as the tests
#' match them ([rcontext_data_match()]), and bound as broom binds them
#' (decision 34 of the design, decided by the author on the review of
#' increment 10), in the order of `data`:
#'
#' * on a fit without na.exclude, the rows that hold an observation, each with
#'   its own values: the full data of a fit that dropped rows give the rows it
#'   used, 117 of 120, and the rows of `data` the fit did not use are left out;
#' * on a fit with na.exclude, those rows and the rows the fit dropped, with
#'   `NA` there, every row of its full data: the result of the same fit with
#'   na.omit, padded as naresid() pads it (decision 32); given the rows of its
#'   observations only, which lack the rows it dropped, those rows in the
#'   order of the fit with a row of `NA` at each row it dropped, named as
#'   naresid() names it (increment 9).
#'
#' The data of a workflow, or of a parsnip fit of fit_xy(), are mapped to the
#' variables of the fit of its engine as the runners map them
#' (`.ht_engine_match()`), from the object the runner kept on its results.
#' What the alignment refuses is refused, naming augment().
#'
#' @param data The `data` argument of augment(), collected.
#' @param model The model of the result.
#' @param object The workflow or parsnip fit the runner was given, or `NULL`.
#' @return A list: `df`, the rows of `data` to bind to, a data frame; and
#'   `positions`, for each of its rows, the position of its values in
#'   `fitted()` and `residuals()` of the fit (`NA` for a row the fit dropped),
#'   or `NULL` when they are bound as they are, `data` holding the rows of the
#'   fit in its order.
#' @keywords internal
#' @noRd
raugment_bind <- function(data, model, object = NULL) {
  df <- as.data.frame(data)
  engine <- if (inherits(object, c("workflow", "model_fit"))) {
    .ht_engine_match(object, model, data, "augment()", beyond = TRUE)
  }
  if (is.null(engine)) {
    ctx <- rcontext_data_match(model, df, fn = "augment")
    obs_rows <- ctx$rows
    alignment <- ctx$alignment
  } else {
    if (is.null(engine$source)) {
      stop(
        paste(
          "augment() cannot tell which rows of `data` hold the observations of the fit:",
          "the workflow's preprocessing drops rows of `data` with a step it applies to new",
          "data, such as step_filter() with skip = FALSE. Pass `data = NULL`, which binds",
          "the model frame of the fit."
        ),
        call. = FALSE
      )
    }
    obs_rows <- engine$source[engine$ctx$rows]
    alignment <- engine$ctx$alignment
  }
  n_obs <- length(obs_rows)
  omitted <- model$na.action
  exclude <- inherits(omitted, "exclude")
  n_fit <- n_obs + if (exclude) length(omitted) else 0L
  observed <- if (exclude) seq_len(n_fit)[-omitted] else seq_len(n_obs)
  positions <- rep(NA_integer_, nrow(df))
  positions[obs_rows] <- observed
  if (exclude) {
    dropped <- rep(NA_integer_, length(omitted))
    if (alignment %in% c("row_names", "row_numbers")) {
      free <- which(is.na(positions))
      dropped <- free[match(as.character(names(omitted)), rownames(df)[free])]
    }
    if (all(is.na(dropped))) {
      # `data` lack the rows the fit dropped: the rows of its observations in
      # the order of the fit, padded as naresid() pads (increment 9).
      rows <- if (identical(obs_rows, seq_len(nrow(df)))) df else df[obs_rows, , drop = FALSE]
      return(list(df = raugment_rows(rows, model, n_fit), positions = NULL))
    }
    positions[dropped[!is.na(dropped)]] <- as.integer(omitted)[!is.na(dropped)]
  }
  rows <- which(!is.na(positions))
  if (identical(rows, seq_len(nrow(df))) && identical(positions, seq_len(n_fit))) {
    return(list(df = df, positions = NULL))
  }
  list(df = df[rows, , drop = FALSE], positions = positions[rows])
}

# The rows augment.hetero_test() binds the per-observation columns of the fit
# to, `n` of them, from the model frame without `data`, or from the rows of
# the observations of a fit with na.exclude matched in `data`
# (raugment_bind()): those rows when they are `n`; for a fit with na.exclude,
# those rows with a row of NA at each row the fit dropped, named as naresid()
# names it; otherwise a refusal that says what augment() needs.
raugment_rows <- function(df, model, n) {
  if (nrow(df) == n) {
    return(df)
  }
  omitted <- model$na.action
  if (inherits(omitted, "exclude") && nrow(df) + length(omitted) == n) {
    keep <- rep.int(NA_integer_, n)
    keep[-omitted] <- seq_len(nrow(df))
    names_kept <- rownames(df)
    df <- df[keep, , drop = FALSE]
    row_names <- character(n)
    row_names[-omitted] <- names_kept
    row_names[omitted] <- names(omitted)
    rownames(df) <- make.unique(row_names)
    return(df)
  }
  stop(
    sprintf(
      paste(
        "augment() binds one row of `data` to each fitted value of the model, %d of them,",
        "and `data` has %d rows. Pass the data the model was fitted to, or `data = NULL`",
        "for its model frame."
      ),
      n, nrow(df)
    ),
    call. = FALSE
  )
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

#' Run heteroscedasticity diagnostics on the data of a survey design
#'
#' A convenience wrapper that takes the data out of a
#' [survey::svydesign()] object, fits the model by ordinary least squares and
#' runs [runHeteroTests()]. **It does not use the sampling weights, strata or
#' clusters of the design**, and warns when the design has unequal weights or
#' clusters.
#'
#' The tests are the ones [runHeteroTests()] runs on any `lm`: they treat the
#' observations as an independent sample with equal weights. They hold their
#' level on survey data when the design can be ignored given the regressors,
#' that is when the chance of being sampled depends only on variables in the
#' model, and the sample has no clusters. When selection depends on the
#' response, or observations are clustered, they reject a model with constant
#' error variance far more often than their nominal level.
#'
#' The package has no design-based test of constant variance. Sampling weights
#' are not precision weights: they say how many population units an
#' observation stands for and nothing about its error variance. Passing them
#' to `lm(weights = )`, or testing a [survey::svyglm()] fit, makes the tests
#' examine residuals multiplied by the square roots of the weights, and a
#' homoscedastic model is then rejected whenever the weights vary with the
#' regressors. `svyglm` fits are refused for that reason.
#'
#' `inst/validation/survey-designs-size.R` measures all of this, together with
#' two design-based tests that were tried and not adopted because they reject
#' too often at the sample sizes examined.
#'
#' @param formula Model formula specifying the mean structure.
#' @param design A [survey::svydesign()] object. Only its data are used.
#' @param tests Character vector of diagnostic names forwarded to
#'   [runHeteroTests()]. Defaults to the White and Koenker tests, the default
#'   of [runHeteroTests()]; it was the White and the classical Breusch-Pagan
#'   tests up to 0.14.0.
#' @param ... Additional arguments passed to [runHeteroTests()].
#' @return A [`hetero_test_suite`] object containing the diagnostic results.
#'   Since 0.16.0 it carries `attr(, "model_origin") = "survey"`: the model is
#'   fitted here, so `summary()` shows its formula rather than its call.
#' @section Earlier versions:
#' Up to 0.12.0 this page described a survey-weighted fit. The weights did not
#' reach the fit, which was by ordinary least squares, as it is now, so the
#' reported values are unchanged.
#' @seealso [runHeteroTests()]
#' @export
#' @examples
#'
#' if (requireNamespace("survey", quietly = TRUE)) {
#'   library(survey)
#'   data(api)
#'   dstrata <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat)
#'   # Warns: the weights of this design are unequal and are not used.
#'   res <- runSurveyHeteroTests(api00 ~ api99 + ell, dstrata, tests = "white")
#'   generics::tidy(res)
#' }
runSurveyHeteroTests <- function(formula, design, tests = c("white", "koenker"), ...) {
  if (!requireNamespace("survey", quietly = TRUE)) {
    stop("The survey package must be installed to analyse survey designs.", call. = FALSE)
  }
  if (!inherits(formula, "formula")) {
    stop("`formula` must be a model formula when analysing survey designs.", call. = FALSE)
  }
  if (!inherits(design, "survey.design")) {
    stop("`design` must be a survey::svydesign() object describing the survey structure.", call. = FALSE)
  }
  fit <- .ht_survey_ols(formula, design, "runSurveyHeteroTests()")
  # The model is fitted here, so the suite says so (rsuite_mark_origin()).
  rsuite_mark_origin(
    runHeteroTests(fit$model, data = fit$data, tests = tests, use_cache = FALSE, ...),
    "survey"
  )
}
