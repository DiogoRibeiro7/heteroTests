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

#' What augment() and the runners need of a tidymodels object
#'
#' The data of a workflow, or of a parsnip fit of fit_xy(), are mapped to the
#' variables of the fit of its engine by what this record holds: for a
#' workflow, the blueprint of its preprocessing (`hardhat::forge()` maps new
#' data by it), the number of rows its preprocessing kept of the data it was
#' fitted to, and the column of its case weights; for a parsnip fit, the
#' column of its outcome. The runners build it from the object they are given,
#' and keep it on each result for augment() (rsuite_mark_tidymodels()); before
#' the second review of increment 10 they kept the object itself, whose fit
#' and mold hold the data, so that the results of `runHeteroTests(wf)` with
#' three tests on 5,000 rows took 2,794 KB serialized, where they took 1,522 KB
#' without it.
#'
#' The expressions of the steps of a recipe keep the environment they were
#' written in, and a recipe written inside a function keeps its frame, every
#' object of it: 49,347 KB serialized for a frame that held a 50 MB data frame
#' (the third review of increment 10). The expressions of the blueprint are
#' given the shared environment after such frames, and the variables they
#' read from the frames written in as values (`.ht_prune_environments()`).
#'
#' @param object A workflow or a parsnip fit.
#' @return A list of class `heteroTests_tidymodels`, or `NULL` for another
#'   object.
#' @keywords internal
#' @noRd
rtidymodels_map <- function(object) {
  if (inherits(object, "workflow")) {
    mold <- tryCatch(workflows::extract_mold(object), error = function(e) NULL)
    case_weights <- tryCatch(all.vars(object$pre$actions$case_weights$col),
      error = function(e) character()
    )
    return(structure(
      list(
        kind = "workflow", blueprint = .ht_prune_environments(mold$blueprint),
        fitted_rows = if (!is.null(mold)) NROW(mold$predictors),
        case_weights = case_weights
      ),
      class = "heteroTests_tidymodels"
    ))
  }
  if (inherits(object, "model_fit")) {
    return(structure(list(kind = "model_fit", y_var = object$preproc$y_var),
      class = "heteroTests_tidymodels"
    ))
  }
  NULL
}

# An environment shared by every object that uses it, which serializing an
# object refers to without its contents: the global environment, the base and
# empty environments, a namespace and an attached package.
.ht_shared_env <- function(env) {
  identical(env, globalenv()) || identical(env, baseenv()) || identical(env, emptyenv()) ||
    isNamespace(env) || !is.null(attr(env, "name", exact = TRUE))
}

# The frames of `env` and its parents before the first shared environment
# (.ht_shared_env()), and that environment.
.ht_frames <- function(env) {
  frames <- list()
  while (!.ht_shared_env(env)) {
    frames[[length(frames) + 1L]] <- env
    env <- parent.env(env)
  }
  list(frames = frames, shared = env)
}

# The value `name` is bound to in the first of `frames` that binds it, as a
# one-element list, or NULL when none does; a function only, skipping the
# other bindings, for the name of a function called (`fun`), as R finds it.
.ht_frame_value <- function(name, frames, fun = FALSE) {
  for (frame in frames) {
    if (exists(name, envir = frame, inherits = FALSE)) {
      value <- tryCatch(list(get(name, envir = frame, inherits = FALSE)),
        error = function(e) NULL
      )
      if (!is.null(value) && (!fun || is.function(value[[1L]]))) {
        return(value)
      }
    }
  }
  NULL
}

# `expr` with each variable it reads from `frames` written in as its value,
# so that it reads the same values once evaluated in the shared environment
# after them: a name of a column of the data (`columns`), which the data mask
# of a step finds first, the names an anonymous function binds (`bound`), the
# right side of `$`, `@`, `::` and `:::`, and a name no frame binds are left
# as they are; `.env$name` is the value of `name`; a function a frame binds is
# written in pruned in turn (.ht_prune_environments()).
.ht_inline <- function(expr, frames, columns, bound, depth) {
  if (is.symbol(expr)) {
    name <- as.character(expr)
    if (!nzchar(name) || name %in% columns || name %in% bound) {
      return(expr)
    }
    value <- .ht_frame_value(name, frames)
    if (is.null(value)) {
      return(expr)
    }
    return(.ht_prune_environments(value[[1L]], columns, depth + 1L))
  }
  if (!is.call(expr)) {
    return(expr)
  }
  head <- expr[[1L]]
  if (is.symbol(head)) {
    head_name <- as.character(head)
    if (head_name %in% c("::", ":::")) {
      return(expr)
    }
    if (head_name %in% c("$", "@") && length(expr) == 3L) {
      named <- is.symbol(expr[[3L]]) || is.character(expr[[3L]])
      if (identical(expr[[2L]], as.symbol(".env")) && named) {
        value <- .ht_frame_value(as.character(expr[[3L]]), frames)
        if (!is.null(value)) {
          return(.ht_prune_environments(value[[1L]], columns, depth + 1L))
        }
        return(expr)
      }
      expr[2L] <- list(.ht_inline(expr[[2L]], frames, columns, bound, depth))
      return(expr)
    }
    if (identical(head_name, "function")) {
      inner <- c(bound, names(expr[[2L]]))
      if (length(expr) >= 3L) {
        expr[3L] <- list(.ht_inline(expr[[3L]], frames, columns, inner, depth))
      }
      return(expr)
    }
    if (!head_name %in% bound) {
      value <- .ht_frame_value(head_name, frames, fun = TRUE)
      if (!is.null(value)) {
        expr[[1L]] <- .ht_prune_environments(value[[1L]], columns, depth + 1L)
      }
    }
  } else {
    expr[1L] <- list(.ht_inline(head, frames, columns, bound, depth))
  }
  .ht_inline_arguments(expr, frames, columns, bound, depth)
}

# The arguments of the call `expr` inlined (.ht_inline()), an empty argument
# (`x[, 1]`) left as it is, and a value NULL written in, not dropped.
.ht_inline_arguments <- function(expr, frames, columns, bound, depth) {
  for (i in seq_along(expr)[-1L]) {
    if (identical(expr[[i]], quote(expr = )) || is.null(expr[[i]])) {
      next
    }
    expr[i] <- list(.ht_inline(expr[[i]], frames, columns, bound, depth))
  }
  expr
}

#' The environments of a blueprint, pruned to what its expressions read
#'
#' A recipe written inside a function keeps the frame of that function in the
#' expressions of its steps, every object of the frame (the third review of
#' increment 10: 49,347 KB serialized, for a frame that held a 50 MB data
#' frame). Each quosure, formula and function of `x`, a blueprint or a part of
#' it, whose environment is such a frame, is given the shared environment
#' after the frames, the global environment or a namespace, and each variable
#' it reads from the frames is written into its expression as its value
#' (`.ht_inline()`): `step_mutate(x3 = x * k)`, with `k <- 3` in the frame,
#' becomes `x * 3`, `x` being a column of the data, which the step reads
#' first. The result is the same object on every call, so that two results of
#' the runners on the same workflow are `identical()`, which a new
#' environment on each call would not be. A function written in the frame,
#' called by such an expression, is written in pruned in turn; past `depth`
#' 4, as for a function that calls itself, it keeps its environment.
#'
#' @param x A blueprint, or a part of it.
#' @param columns The names of the columns of the data of the recipe, which
#'   its steps read from the data.
#' @param depth The depth of the functions written in.
#' @return `x`, its environments pruned.
#' @keywords internal
#' @noRd
.ht_prune_environments <- function(x, columns = NULL, depth = 0L) {
  if (is.null(x) || is.environment(x) || is.data.frame(x) || isS4(x)) {
    return(x)
  }
  if (is.null(columns) && inherits(x, "recipe_blueprint")) {
    recipe <- x$recipe
    columns <- unique(c(
      recipe$var_info$variable, recipe$term_info$variable,
      recipe$last_term_info$variable
    ))
  }
  if (is.function(x)) {
    env <- environment(x)
    if (is.primitive(x) || is.null(env) || .ht_shared_env(env)) {
      return(x)
    }
    if (depth > 4L) {
      return(x)
    }
    found <- .ht_frames(env)
    arguments <- names(formals(x))
    defaults <- formals(x)
    for (i in seq_along(defaults)) {
      if (!identical(defaults[[i]], quote(expr = )) && !is.null(defaults[[i]])) {
        defaults[i] <- list(.ht_inline(defaults[[i]], found$frames, columns, arguments, depth))
      }
    }
    new <- x
    formals(new) <- defaults
    body(new) <- .ht_inline(body(x), found$frames, columns, arguments, depth)
    environment(new) <- found$shared
    return(new)
  }
  if (inherits(x, "formula")) {
    env <- attr(x, ".Environment")
    if (is.environment(env) && !.ht_shared_env(env)) {
      found <- .ht_frames(env)
      at <- attributes(x)
      plain <- x
      attributes(plain) <- NULL
      plain <- .ht_inline_arguments(plain, found$frames, columns, character(), depth)
      attributes(plain) <- at
      attr(plain, ".Environment") <- found$shared
      return(plain)
    }
    return(x)
  }
  if (is.list(x)) {
    for (i in seq_along(x)) {
      part <- x[[i]]
      if (!is.null(part)) {
        x[[i]] <- .ht_prune_environments(part, columns, depth)
      }
    }
  }
  x
}

# The steps of the recipe of a workflow that it skips on new data
# (`skip = TRUE`), by their position.
.ht_skipped_steps <- function(blueprint) {
  steps <- if (inherits(blueprint, "recipe_blueprint")) blueprint$recipe$steps
  which(vapply(steps, function(s) isTRUE(s$skip), logical(1)))
}

# The advice that ends a refusal of the data of a workflow or a parsnip fit:
# augment() binds the model frame without `data`, and the runners test it.
.ht_null_advice <- function(context) {
  if (identical(context, "augment()")) {
    "Pass `data = NULL`, which binds the model frame of that fit."
  } else {
    "Pass `data = NULL`, which tests the model frame of that fit."
  }
}

# A refusal of the rows of `data`, with the class of the guard, final in the
# runners.
.ht_stop_rows <- function(message) {
  stop(structure(
    class = c("heteroTests_data_rows_error", "error", "condition"),
    list(message = message, call = NULL)
  ))
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
#' fit for the tests. A recipe gives the weights of the rows it keeps, in the
#' order it keeps them (`recipes::bake()`), and a formula keeps every row;
#' weights that cannot be told for each mapped row are refused (from the
#' second review of increment 10: a recipe that dropped rows of new data left
#' them uncompared).
#'
#' With `trace`, the rows are traced to the rows of `data` they were made
#' from, as augment() needs: when the preprocessing kept every row, `data`
#' with its rows rotated by one, and with its first two rows swapped, must be
#' mapped to the mapped rows in that order. A preprocessing that maps each row
#' on its own, in its order, passes; one that reorders the rows, as
#' `step_arrange()` does, or makes a row from other rows, does not (from the
#' second review of increment 10: augment() bound the rows of `data` the
#' values of other rows).
#'
#' @param map The record of the workflow (rtidymodels_map()).
#' @param engine_fit The fit of its engine, an `lm` or a `glm`.
#' @param data A data frame, not grouped.
#' @param lacking The variables of the engine `data` lacks, for the messages.
#' @param context Character scalar naming the calling function.
#' @param trace Trace the mapped rows to the rows of `data`?
#' @param blueprint The blueprint to map by, by default that of the workflow.
#' @return A list: `data`, the mapped data frame; `source`, the row of `data`
#'   each of its rows was made from, or `NULL` when that cannot be told;
#'   `untraced`, why not, `"drops"` (a step it applies to new data dropped or
#'   added rows, such as `step_filter()` with `skip = FALSE`) or `"reorders"`; and
#'   `fitted_rows`, the number of rows the preprocessing kept of the data the
#'   workflow was fitted to.
#' @keywords internal
#' @noRd
.ht_workflow_mapped <- function(map, engine_fit, data, lacking, context, trace = FALSE,
                                blueprint = map$blueprint) {
  if (!requireNamespace("hardhat", quietly = TRUE)) {
    .ht_stop_engine_data(lacking, paste(
      "the package hardhat, whose hardhat::forge() maps `data` to them by the workflow's",
      "preprocessing, is not installed."
    ))
  }
  if (is.null(blueprint)) {
    .ht_stop_engine_data(
      lacking, "the workflow's preprocessing cannot be read, so `data` cannot be mapped to them."
    )
  }
  weights_name <- all.vars(engine_fit$call$weights)
  case_weights <- map$case_weights
  weighted <- length(weights_name) == 1L && length(case_weights) == 1L
  map_rows <- function(rows) {
    forged <- tryCatch(
      suppressWarnings(hardhat::forge(rows, blueprint, outcomes = TRUE)),
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
    # The case weights, under the name of the engine's call (`weights`).
    if (weighted && !weights_name %in% names(mapped)) {
      w <- if (inherits(blueprint, "recipe_blueprint")) {
        baked <- tryCatch(
          suppressWarnings(recipes::bake(blueprint$recipe, new_data = rows)),
          error = function(e) NULL
        )
        baked[[case_weights]]
      } else if (nrow(mapped) == nrow(rows)) {
        rows[[case_weights]]
      }
      if (is.null(w) || length(w) != nrow(mapped)) {
        .ht_stop_rows(paste(
          sprintf(
            paste(
              "%s cannot compare the case weights of the workflow, `%s`, with the weights of",
              "the fit of its engine: `data` mapped by the workflow's preprocessing",
              "(hardhat::forge()) do not give the weight of each row they keep."
            ),
            context, case_weights
          ),
          .ht_null_advice(context)
        ))
      }
      mapped[[weights_name]] <- as.double(unclass(w))
    }
    mapped
  }
  mapped <- map_rows(data)
  n <- nrow(data)
  source <- NULL
  untraced <- NULL
  if (nrow(mapped) != n) {
    untraced <- "drops"
  } else if (!trace || n < 2L) {
    source <- seq_len(n)
  } else {
    same <- function(a, b) {
      identical(names(a), names(b)) && nrow(a) == nrow(b) &&
        all(vapply(names(a), function(k) {
          isTRUE(all.equal(a[[k]], b[[k]], check.attributes = FALSE))
        }, logical(1)))
    }
    orders <- list(c(seq_len(n)[-1L], 1L), c(2L, 1L, seq_len(n)[-(1:2)]))
    traced <- all(vapply(orders, function(o) {
      again <- tryCatch(map_rows(data[o, , drop = FALSE]), error = function(e) NULL)
      !is.null(again) && same(again, mapped[o, , drop = FALSE])
    }, logical(1)))
    if (traced) {
      source <- seq_len(n)
    } else {
      untraced <- "reorders"
    }
  }
  list(data = mapped, source = source, untraced = untraced, fitted_rows = map$fitted_rows)
}

#' The data of a parsnip fit made with fit_xy(), mapped to its engine
#'
#' [parsnip::fit_xy()] fits the engine to the columns of `x` and to the
#' outcome as `..y`, which the data the fit was made from do not hold; before
#' 0.17.0 every test refused them (N5 of the design of the model context). The
#' outcome is mapped to `..y` from the column the fit records
#' (`preproc$y_var`, when `y` was given as a data frame). An outcome given as
#' a vector is not recorded, and such data are refused with a message that
#' says so. Neither are its case weights, which `fit_xy()` takes as a vector
#' (`case_weights`): the weights of `data` cannot be compared with those of
#' the fit, as those of a workflow are, and the data of such a fit are refused
#' (from the third review of increment 10, where other weights were taken).
#'
#' @param map The record of the parsnip fit (rtidymodels_map()).
#' @param engine_fit The fit of its engine.
#' @param data A data frame, not grouped.
#' @param context Character scalar naming the calling function.
#' @return As for `.ht_workflow_mapped()`.
#' @keywords internal
#' @noRd
.ht_parsnip_mapped <- function(map, engine_fit, data, context) {
  y_var <- map$y_var
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
  if (!is.null(engine_fit$call$weights) || !is.null(engine_fit$weights)) {
    .ht_stop_rows(paste(
      sprintf(
        paste(
          "%s cannot compare the case weights of a parsnip fit made with fit_xy() with",
          "those of `data`: fit_xy() takes them as a vector, and the fit does not record",
          "which column of `data` held them, so `data` cannot be confirmed to be the data",
          "the fit was made from."
        ),
        context
      ),
      .ht_null_advice(context)
    ))
  }
  mapped <- as.data.frame(data)
  mapped[["..y"]] <- data[[y_var]]
  list(data = mapped, source = seq_len(nrow(data)), untraced = NULL, fitted_rows = NULL)
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
#' data are refused, with a message that says so. A step skipped on new data
#' that changed the outcome, a predictor or the order of the rows at the fit,
#' as `step_log(y, skip = TRUE)`, the idiom recipes recommends for the
#' outcome, leaves `data` mapped without it: when `data` mapped with the
#' skipped steps applied are confirmed, the refusal says so (from the second
#' review of increment 10, where it advised to pass the data the workflow was
#' fitted to, which they were). A refusal of the rows says what was matched
#' (`lead`), what the alignment found and what to pass, and has the class of
#' the guard, `heteroTests_data_rows_error`, final in the runners: data in
#' another order than the fit, data that lack rows of it ("Missing rows") and
#' data whose rows are not those of the fit alike.
#'
#' @param map The record of the workflow or the parsnip fit
#'   (rtidymodels_map()).
#' @param engine_fit The fit of its engine.
#' @param data The data, collected; `NULL` is returned for data that hold the
#'   variables of the engine already, as its model frame does.
#' @param context Character scalar naming the calling function, as
#'   `"runHeteroTests()"`.
#' @param beyond Does the caller read columns of `data` beyond the model frame
#'   (`augment()`, which binds them)? See [rmodel_context()]. Such a caller
#'   needs the row of `data` of each observation, and the mapped rows are
#'   traced to them.
#' @return `NULL` when `data` hold the variables of the engine; otherwise a
#'   list with `ctx`, the context of the engine's fit matched to the mapped
#'   rows, `source`, the row of `data` of each mapped row (or `NULL`), and
#'   `untraced`, why `source` is `NULL`.
#' @keywords internal
#' @noRd
.ht_engine_match <- function(map, engine_fit, data, context, beyond = FALSE) {
  lacking <- .ht_engine_lacks(engine_fit, data)
  if (length(lacking) == 0L) {
    return(NULL)
  }
  workflow <- identical(map$kind, "workflow")
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
    .ht_workflow_mapped(map, engine_fit, data, lacking, context, trace = beyond)
  } else {
    .ht_parsnip_mapped(map, engine_fit, data, context)
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
      .ht_stop_rows(paste(
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
    # Steps the recipe skips on new data: `data` mapped with them applied, as
    # at the fit, confirmed, say that they stand between them. The steps
    # named are those without which the rows are not confirmed, each left out
    # of the others in turn (from the third review of increment 10, where a
    # step_filter() that kept every row was named with the step_log() that
    # changed the outcome); when none is needed alone, any of them may be.
    skipped <- if (workflow) .ht_skipped_steps(map$blueprint) else integer()
    confirms <- function(steps) {
      applied <- map$blueprint
      for (i in steps) {
        applied$recipe$steps[[i]]$skip <- FALSE
      }
      again <- tryCatch(
        .ht_workflow_mapped(map, engine_fit, data, lacking, context, blueprint = applied)$data,
        error = function(e) NULL
      )
      if (is.null(again)) {
        return(FALSE)
      }
      if (!inherits(attempt(again), "error")) {
        return(TRUE)
      }
      # The rows complete in the variables of the engine, as above, for a
      # step that dropped rows at the fit only and is left skipped.
      if (is.null(fitted_rows) || nrow(again) <= fitted_rows) {
        return(FALSE)
      }
      variables <- intersect(all.vars(stats::terms(engine_fit)), names(again))
      complete <- stats::complete.cases(again[, variables, drop = FALSE])
      if (sum(complete) != fitted_rows) {
        return(FALSE)
      }
      kept <- again[complete, , drop = FALSE]
      rownames(kept) <- NULL
      !inherits(attempt(kept), "error")
    }
    if (length(skipped) > 0L && confirms(skipped)) {
      needed <- skipped[vapply(seq_along(skipped), function(j) !confirms(skipped[-j]), logical(1))]
      class_of <- function(steps) {
        unique(vapply(
          map$blueprint$recipe$steps[steps], function(s) paste0(class(s)[1L], "()"),
          character(1)
        ))
      }
      and <- function(words) {
        if (length(words) > 1L) {
          paste(paste(words[-length(words)], collapse = ", "), "and", words[length(words)])
        } else {
          words
        }
      }
      which_steps <- if (length(needed) == 0L) {
        sprintf(
          "steps it skips on new data, one or more of %s with `skip = TRUE`, which",
          and(class_of(skipped))
        )
      } else if (length(class_of(needed)) == 1L) {
        sprintf("a step it skips on new data, %s with `skip = TRUE`, which", class_of(needed))
      } else {
        sprintf("steps it skips on new data, %s with `skip = TRUE`, which", and(class_of(needed)))
      }
      .ht_stop_rows(paste(
        sprintf(
          paste(
            "%s cannot map `data` to the variables of the fit of the workflow's engine:",
            "the workflow's preprocessing has %s changed the outcome, a predictor or the",
            "order of the rows of the data it was fitted to, so `data`, mapped by that",
            "preprocessing (hardhat::forge()), do not hold the values the engine was fitted to."
          ),
          context, which_steps
        ),
        .ht_null_advice(context)
      ))
    }
    found <- sub(" (Pass|Refit) .*$", "", conditionMessage(ctx))
    .ht_stop_rows(paste(lead, found, advice))
  }
  list(ctx = ctx, source = mapped$source, untraced = mapped$untraced)
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
  if (is.null(.ht_engine_match(rtidymodels_map(workflow), engine_fit, data, context))) {
    return(data)
  }
  NULL
}
.ht_parsnip_data <- .ht_workflow_data

# What augment() needs of the tidymodels object a suite of the runners was
# computed from, a workflow or a parsnip fit (rtidymodels_map()), kept on each
# of its results as attr(, "tidymodels"), so that augment() can map the data
# of that object to the variables of its engine as the runners map them (from
# the review of increment 10).
rsuite_mark_tidymodels <- function(suite, object) {
  map <- rtidymodels_map(object)
  if (is.null(map)) {
    return(suite)
  }
  for (i in seq_along(suite)) {
    if (inherits(suite[[i]], "hetero_test")) {
      attr(suite[[i]], "tidymodels") <- map
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
  # A vector of another length than fitted(), as rstandard() of a fit with a
  # zero weight, which leaves that observation out, is assigned as it is and
  # stops, as it did, whatever the order of `data`.
  at <- function(values) {
    if (is.null(values)) {
      return(rep(NA_real_, nrow(df)))
    }
    if (is.null(bound$positions) || length(values) != length(fitted_values)) {
      return(values)
    }
    unname(values)[bound$positions]
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
#' (`.ht_engine_match()`), by what the runner kept on its results
#' (rtidymodels_map()), and each mapped row is traced to the row of `data` it
#' was made from: a preprocessing that drops rows of new data, or reorders
#' them, does not say which row of `data` each observation comes from, and
#' its data are refused (from the second review of increment 10: on a recipe
#' with `step_arrange()` the rows of `data` were bound the values of other
#' rows). What the alignment refuses is refused, naming augment().
#'
#' @param data The `data` argument of augment(), collected.
#' @param model The model of the result.
#' @param map What the runner kept of the workflow or parsnip fit it was
#'   given (rtidymodels_map()), or `NULL`.
#' @return A list: `df`, the rows of `data` to bind to, a data frame; and
#'   `positions`, for each of its rows, the position of its values in
#'   `fitted()` and `residuals()` of the fit (`NA` for a row the fit dropped),
#'   or `NULL` when they are bound as they are, `data` holding the rows of the
#'   fit in its order.
#' @keywords internal
#' @noRd
raugment_bind <- function(data, model, map = NULL) {
  df <- as.data.frame(data)
  engine <- if (inherits(map, "heteroTests_tidymodels")) {
    .ht_engine_match(map, model, data, "augment()", beyond = TRUE)
  }
  if (is.null(engine)) {
    ctx <- rcontext_data_match(model, df, fn = "augment")
    obs_rows <- ctx$rows
    alignment <- ctx$alignment
  } else {
    if (is.null(engine$source)) {
      why <- if (identical(engine$untraced, "reorders")) {
        paste(
          "the workflow's preprocessing reorders the rows of `data` it maps, or makes a row",
          "from other rows, as step_arrange() does, so the row of `data` each observation",
          "comes from cannot be told."
        )
      } else {
        paste(
          "the workflow's preprocessing drops or adds rows of `data` with a step it applies",
          "to new data, such as step_filter() with skip = FALSE."
        )
      }
      stop(
        paste(
          "augment() cannot tell which rows of `data` hold the observations of the fit:",
          why, "Pass `data = NULL`, which binds the model frame of the fit."
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
