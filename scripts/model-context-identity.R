#!/usr/bin/env Rscript

# Identity harness of the model context (docs/design/model-context.md, "How
# each increment shows identical()").
#
# It runs a corpus of calls on two versions of the package, each in its own R
# process, and lists, call by call, whether the two results are identical().
# A result that differs must be one of the defects the increment closes, in
# the allow-list below; any other difference is unexpected. No value is
# stored in the repository: each run writes an .rds file where it is told.
#
# The corpus:
#   - every recipe of tests/testthat/helper-test-recipes.R (every exported
#     perform*Test() but performBoxMTest(), which tests data, not a model),
#     and the recipes of `identity_extra_recipes` below, under set.seed(7), on
#     every cell of the matrix of tests/testthat/helper-model-context.R;
#   - the default battery of runHeteroTests() on every cell;
#   - the examples of the help pages of the functions built on the context.
#
# Decision 1: on a fit with a poly() term, the value of a function built on
# the context equals, with identical(), what main computes on the fit's
# stored design; against main's rebuilt design, which computes poly() again
# through its predvars, the differences are in the last bits (measured up to
# 1.3e-15 relative on the statistic). Each run therefore also records, for
# those cells, the calls that may change, with main given the stored design:
# the model is refitted with `x = TRUE`, for which model.matrix(model, data)
# returns the stored design, and the five functions built on the context are
# traced so that a model they are given without it, as the refits of a
# bootstrap are, gets its own (`model$x`) first. `compare` checks the branch
# against that value; a poly() cell that is not identical to it is
# unexpected. Where runHeteroTests() falls back on another test on the stored
# design too, as without `data` on a poly() fit before the context, main has
# no stored-design value of the test it names, and those tests are judged by
# the rules of the runner below.
#
# Usage, from the root of the checkout whose corpus is run:
#
#   # main, installed into a private library, never the default one
#   R CMD INSTALL --library=<private library> <checkout of main>
#   Rscript scripts/model-context-identity.R run <private library> main.rds
#   # the checkout, through pkgload::load_all()
#   Rscript scripts/model-context-identity.R run source branch.rds
#   # the list, and the counts
#   Rscript scripts/model-context-identity.R compare main.rds branch.rds [calls.csv]
#
# `compare` exits with status 1 when a difference is unexpected.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L || !args[1L] %in% c("run", "compare")) {
  stop(
    "Usage: model-context-identity.R run <library|source> <out.rds>\n",
    "       model-context-identity.R compare <main.rds> <branch.rds> [calls.csv]",
    call. = FALSE
  )
}

# --- the functions built on the context, and the defects increment 1 closes --

# Recipes beyond those of helper-test-recipes.R: the calls that compute a
# statistic through the functions built on the context.
identity_extra_recipes <- list(
  `performBPTestRobust(studentize = FALSE)` = function(m, d) {
    performBPTestRobust(m, d, studentize = FALSE)
  },
  `bootstrapTestStatistic(performKoenkerTest)` = function(m, d) {
    bootstrapTestStatistic(performKoenkerTest, m, d, B = 19, progress = FALSE)
  },
  `performWhiteTestRobust(bootstrap = TRUE)` = function(m, d) {
    performWhiteTestRobust(m, d, bootstrap = TRUE, B = 19)
  }
)
# The functions whose values may change: those built on the context, the
# deprecated name and the wrappers that compute their statistic through them,
# and the runner, whose default battery is White's and Koenker's tests.
# performWhiteTestBootstrap() is not among them: it keeps the computation of
# 0.15.0 until increment 8 (rwhite_test_rebuilt_design()), so any change of
# it is unexpected.
identity_affected <- c(
  "performWhiteTest", "performBPTest", "performBreuschPaganTest", "performKoenkerTest",
  "performRankPermutationTest", "performHighDimensionalTest", "performWhiteTestRobust",
  names(identity_extra_recipes), "runHeteroTests"
)
# The bootstrap recipes, whose replicates refit the formula until increment 8.
identity_bootstraps <- c(
  "bootstrapTestStatistic(performKoenkerTest)", "performWhiteTestRobust(bootstrap = TRUE)"
)
# The defects of the design whose cells may change, by the kind of change.
identity_allowed <- list(
  error_to_value = c("B12", "L1", "L2", "B14", "B15", "N7"),
  value_change = c("B5")
)
# A cut() term, whose replicates would compute other breaks than the stored
# design, is refused by the bootstraps (rdata_reproduces_fit()), as main
# refused it with another message: only that change is allowed for them.
identity_bootstrap_allowed <- list(error_to_value = c("B12", "L1", "L2", "B15"))
# A value of the runner that fell back on another test, or stopped, where the
# test it names now runs.
identity_runner_allowed <- c("B12", "L1", "L2", "B14", "B15", "N7", "B5")
# A message, of a test or of the runner that runs it, that changes only to the
# test's own documented refusal: M7, a model with more than one response
# (R4/N9). Any other change of a message is unexpected.
identity_messages <- list(R4 = "needs a model with one response")
# The bootstraps: on a cell whose base statistic stopped before (B12, L1, L2,
# N7), the call now reaches the checks of the bootstrap itself, and is refused
# by one of them, as it was refused before by the base statistic: a frame that
# cannot be rebuilt from `data` (a cut() term, or the model frame given as
# `data`), a glm (N8, increment 8), a transformed response under null-imposed
# resampling. Only these messages are allowed.
identity_bootstrap_messages <- c(
  "does not reproduce the fit", "supports Gaussian `lm` models only",
  "Null-imposed resampling needs a plain response variable"
)
identity_bootstrap_defects <- c("B12", "L1", "L2", "N7")

# --- canonical results ---------------------------------------------------------

# A result without the environments, functions and calls it carries, which
# differ between two processes for no reason of value.
identity_strip <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.environment(x) || is.function(x)) {
    return(paste0("<", typeof(x), ">"))
  }
  if (is.language(x) || inherits(x, "formula")) {
    return(paste(deparse(x), collapse = " "))
  }
  at <- attributes(x)
  if (is.list(x)) {
    x <- lapply(unclass(x), identity_strip)
  }
  if (!is.null(at)) {
    at[names(at) == "model"] <- "<model>"
    attributes(x) <- lapply(at, identity_strip)
  }
  x
}

identity_outcome <- function(f) {
  warnings <- character()
  value <- withCallingHandlers(
    tryCatch(
      f(),
      error = function(e) {
        list(.error = TRUE, class = setdiff(class(e), "simpleError"), message = conditionMessage(e))
      },
      skip = function(e) list(.skip = TRUE, message = conditionMessage(e))
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) invokeRestart("muffleMessage")
  )
  if (!(is.list(value) && (isTRUE(value$.error) || isTRUE(value$.skip)))) {
    value <- identity_strip(value)
  }
  list(value = value, warnings = sort(unique(warnings)))
}

# The fields of a runner's results that say what was run and what it gave.
identity_runner_summary <- function(suite) {
  lapply(unclass(suite), function(r) {
    extras <- attr(r, "extras")
    list(
      method = r$method, statistic = unname(r$statistic), parameter = unname(r$parameter),
      p.value = r$p.value, diagnostic = attr(r, "diagnostic"),
      status = extras$status, issue = extras$issue
    )
  })
}

# The five functions built on the context, traced, or untraced, so that a
# model they are given without its design gets its own first: on main,
# model.matrix(model, data) then returns the stored design, also for the
# refits of a bootstrap.
identity_trace <- function(on) {
  fns <- c("performWhiteTest", "performBPTest", "performKoenkerTest",
           "performRankPermutationTest", "performHighDimensionalTest")
  places <- list(asNamespace("heteroTests"), as.environment("package:heteroTests"))
  tracer <- quote(
    if (inherits(model, "lm") && is.null(model[["x"]])) model$x <- stats::model.matrix(model)
  )
  for (where in places) {
    for (fn in fns) {
      if (on) {
        suppressMessages(trace(fn, tracer = tracer, where = where, print = FALSE))
      } else {
        suppressMessages(untrace(fn, where = where))
      }
    }
  }
}

# --- run -----------------------------------------------------------------------

identity_run <- function(where, out, root = ".") {
  if (identical(where, "source")) {
    suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
  } else {
    .libPaths(c(where, .libPaths()))
    suppressPackageStartupMessages(library(heteroTests, lib.loc = where))
  }
  invisible(ht_set_log_level("SILENT"))
  source(file.path(root, "tests", "testthat", "helper-test-recipes.R"), local = globalenv())
  source(file.path(root, "tests", "testthat", "helper-model-context.R"), local = globalenv())
  cells <- context_cells()
  # IDENTITY_CELLS=k runs the first k cells only, to try the harness out.
  first <- as.integer(Sys.getenv("IDENTITY_CELLS", unset = NA))
  if (!is.na(first)) {
    cells <- utils::head(cells, first)
  }
  recipes <- c(contract_recipes[setdiff(names(contract_recipes), "performBoxMTest")],
               identity_extra_recipes)
  records <- list()
  add <- function(kind, fn, cell, outcome) {
    id <- paste(kind, fn, cell$id, sep = " | ")
    records[[id]] <<- c(
      list(id = id, kind = kind, fn = fn, cell = cell$id, defect = cell$defect), outcome
    )
  }
  started <- Sys.time()
  run_cell <- function(cell, built, fns, recipe_kind, runner_kind) {
    for (fn in fns) {
      add(recipe_kind, fn, cell, identity_outcome(function() {
        set.seed(7)
        recipes[[fn]](built$m, built$data)
      }))
    }
    add(runner_kind, "runHeteroTests", cell, identity_outcome(function() {
      set.seed(7)
      identity_runner_summary(
        runHeteroTests(built$m, built$data, use_cache = FALSE, progress = FALSE)
      )
    }))
  }
  for (cell in cells) {
    built <- suppressWarnings(suppressMessages(cell$build()))
    run_cell(cell, built, names(recipes), "recipe", "runner")
  }
  # Decision 1: the cells of a poly() term on the stored design.
  identity_trace(TRUE)
  for (cell in cells) {
    if ("decision1" %in% cell$defect) {
      stored <- suppressWarnings(suppressMessages(cell$build(x = TRUE)))
      run_cell(cell, stored, intersect(names(recipes), identity_affected),
               "stored", "stored_runner")
    }
  }
  identity_trace(FALSE)
  # The examples of the help pages, each expression whose value is a test.
  scratch <- dirname(normalizePath(out, mustWork = FALSE))
  for (topic in c("performWhiteTest", "performBPTest", "performKoenkerTest",
                  "performRankPermutationTest", "performHighDimensionalTest")) {
    example_file <- file.path(scratch, paste0(topic, "-examples.R"))
    tools::Rd2ex(file.path(root, "man", paste0(topic, ".Rd")), example_file)
    expressions <- parse(example_file)
    unlink(example_file)
    env <- new.env(parent = globalenv())
    set.seed(1)
    for (k in seq_along(expressions)) {
      outcome <- identity_outcome(function() eval(expressions[[k]], env))
      if (inherits(outcome$value, "htest") || is.list(outcome$value) &&
            isTRUE(outcome$value$.error)) {
        add("example", topic, list(id = paste("expression", k), defect = character()), outcome)
      }
    }
  }
  meta <- list(
    where = where,
    version = as.character(utils::packageVersion("heteroTests")),
    path = find.package("heteroTests"),
    r = R.version.string,
    seconds = as.numeric(difftime(Sys.time(), started, units = "secs"))
  )
  saveRDS(list(meta = meta, records = records), out)
  message(sprintf("%d calls in %.0f s, heteroTests %s from %s, written to %s",
                  length(records), meta$seconds, meta$version, meta$path, out))
}

# --- compare -------------------------------------------------------------------

identity_numbers <- function(x) {
  if (is.list(x)) {
    return(unlist(lapply(x, identity_numbers), use.names = FALSE))
  }
  if (is.numeric(x)) as.double(x) else numeric()
}

identity_relative <- function(a, b) {
  if (length(a) != length(b) || length(a) == 0L) {
    return(if (length(a) == length(b)) 0 else Inf)
  }
  ok <- is.finite(a) & is.finite(b)
  if (!identical(is.finite(a), is.finite(b)) || !identical(a[!ok], b[!ok])) {
    return(Inf)
  }
  scale <- pmax(abs(a[ok]), abs(b[ok]), .Machine$double.xmin)
  max(c(0, abs(a[ok] - b[ok]) / scale))
}

identity_classify <- function(before, after) {
  a <- before$value
  b <- after$value
  failed <- function(v) is.list(v) && isTRUE(v$.error)
  if (identical(a, b)) {
    return(list(change = "identical", detail = ""))
  }
  if (failed(a) && failed(b)) {
    return(list(change = "message_change", detail = b$message))
  }
  if (failed(a)) {
    return(list(change = "error_to_value", detail = a$message))
  }
  if (failed(b)) {
    return(list(change = "value_to_error", detail = b$message))
  }
  statistic <- identity_relative(identity_numbers(a$statistic), identity_numbers(b$statistic))
  everything <- identity_relative(identity_numbers(a), identity_numbers(b))
  list(change = "value_change", statistic = statistic, everything = everything,
       detail = sprintf("statistic %.2g, all numbers %.2g", statistic, everything))
}

# Decision 1 for one call: "identical" or "allowed" when the branch is main's
# value on the stored design, "unexpected" when it is not, NULL when main has
# no such value for it (it refused the call on the stored design too, or the
# runner fell back on another test) and the other rules decide.
identity_stored_verdict <- function(record, verdict, after, stored) {
  failed <- function(v) is.list(v) && isTRUE(v$.error)
  if (is.null(stored) || failed(stored$value) || failed(after$value)) {
    return(NULL)
  }
  same <- if (identical(record$kind, "runner")) {
    # The tests of the battery that main ran on the stored design, not the
    # fallback it put under their names when one stopped.
    own <- names(stored$value)[vapply(stored$value, function(e) is.null(e$status), logical(1))]
    if (length(own) == 0L) {
      return(NULL)
    }
    identical(after$value[own], stored$value[own]) &&
      (length(own) == length(stored$value) ||
         any(record$defect %in% identity_runner_allowed))
  } else {
    identical(after$value, stored$value)
  }
  if (!isTRUE(same)) {
    return("unexpected")
  }
  if (identical(verdict$change, "identical")) "identical" else "allowed"
}

# `stored` is main's value on the stored design, for a cell of decision 1, or
# NULL.
identity_allow <- function(record, verdict, after, stored = NULL) {
  defects <- record$defect
  # Decision 1: a value on a poly() term must be main's value on the stored
  # design, identical(), whether or not it is main's on its rebuilt one.
  if ("decision1" %in% defects && record$fn %in% identity_affected) {
    decided <- identity_stored_verdict(record, verdict, after, stored)
    if (!is.null(decided)) {
      return(decided)
    }
  }
  if (identical(verdict$change, "identical")) {
    return("identical")
  }
  if (!record$fn %in% identity_affected) {
    return("unexpected")
  }
  bootstrap <- record$fn %in% identity_bootstraps
  # A message that changes only to the function's own documented refusal.
  if (identical(verdict$change, "message_change")) {
    own <- unlist(identity_messages[intersect(defects, names(identity_messages))])
    if (bootstrap && any(defects %in% identity_bootstrap_defects)) {
      own <- c(own, identity_bootstrap_messages)
    }
    if (length(own) > 0L &&
          any(vapply(own, grepl, logical(1), x = after$value$message, fixed = TRUE))) {
      return("allowed")
    }
    return("unexpected")
  }
  if (identical(record$kind, "runner")) {
    if (verdict$change %in% c("value_change", "error_to_value") &&
          any(defects %in% identity_runner_allowed)) {
      return("allowed")
    }
    return("unexpected")
  }
  allowed <- if (bootstrap) identity_bootstrap_allowed else identity_allowed
  if (any(defects %in% allowed[[verdict$change]])) {
    return("allowed")
  }
  "unexpected"
}

identity_compare <- function(main_file, branch_file, csv = NULL) {
  main <- readRDS(main_file)
  branch <- readRDS(branch_file)
  ids <- union(names(main$records), names(branch$records))
  missing_ids <- setdiff(ids, intersect(names(main$records), names(branch$records)))
  if (length(missing_ids) > 0L) {
    stop("The two runs have different corpora: ",
         paste(utils::head(missing_ids, 5L), collapse = "; "), call. = FALSE)
  }
  # The records on the stored design are references, not calls.
  stored_kinds <- c("stored", "stored_runner")
  ids <- ids[!vapply(main$records[ids], function(r) r$kind %in% stored_kinds, logical(1))]
  stored_id <- function(id) {
    sub("^runner \\| ", "stored_runner | ", sub("^recipe \\| ", "stored | ", id))
  }
  rows <- lapply(ids, function(id) {
    before <- main$records[[id]]
    after <- branch$records[[id]]
    stored <- main$records[[stored_id(id)]]
    verdict <- identity_classify(before, after)
    decided <- if ("decision1" %in% before$defect && before$fn %in% identity_affected) {
      identity_stored_verdict(before, verdict, after, stored)
    }
    checked <- !is.null(decided)
    data.frame(
      id = id, kind = before$kind, fn = before$fn, cell = before$cell,
      defect = paste(before$defect, collapse = " "),
      change = verdict$change,
      verdict = identity_allow(before, verdict, after, stored),
      statistic_relative = if (is.null(verdict$statistic)) NA_real_ else verdict$statistic,
      stored_checked = checked,
      stored_identical = checked && !identical(decided, "unexpected"),
      warnings_differ = !identical(before$warnings, after$warnings),
      detail = substr(gsub("\n", " ", verdict$detail), 1L, 160L),
      stringsAsFactors = FALSE
    )
  })
  calls <- do.call(rbind, rows)
  if (!is.null(csv)) {
    utils::write.csv(calls, csv, row.names = FALSE)
  }
  cat(sprintf("main:   heteroTests %s from %s, %d records in %.0f s\n", main$meta$version,
              main$meta$path, length(main$records), main$meta$seconds))
  cat(sprintf("branch: heteroTests %s from %s, %d records in %.0f s\n\n", branch$meta$version,
              branch$meta$path, length(branch$records), branch$meta$seconds))
  cat(sprintf("%-12s %6d\n", c("calls", "identical", "allowed", "unexpected"),
              c(nrow(calls), sum(calls$verdict == "identical"), sum(calls$verdict == "allowed"),
                sum(calls$verdict == "unexpected"))), sep = "")
  cat("\nChanges by kind and defect:\n")
  changed <- calls[calls$verdict != "identical", , drop = FALSE]
  if (nrow(changed) > 0L) {
    changed$calls <- 1L
    print(stats::aggregate(calls ~ verdict + change + defect, data = changed, FUN = sum),
          row.names = FALSE)
    cat("\nCalls by function:\n")
    print(table(changed$fn, changed$verdict))
  }
  checked <- calls[calls$stored_checked, , drop = FALSE]
  if (nrow(checked) > 0L) {
    changed_poly <- checked[checked$change == "value_change", , drop = FALSE]
    largest <- if (nrow(changed_poly) > 0L) max(changed_poly$statistic_relative) else 0
    template <- paste0(
      "\nDecision 1: %d poly() calls with a value checked against main on the stored design, ",
      "%d identical() to it; %d differ from main's rebuilt design, by at most %.2g ",
      "relative on the statistic\n"
    )
    cat(sprintf(template, nrow(checked), sum(checked$stored_identical), nrow(changed_poly),
                largest))
  }
  cat(sprintf("\nCalls whose warnings differ (not counted above): %d\n",
              sum(calls$warnings_differ)))
  unexpected <- calls[calls$verdict == "unexpected", , drop = FALSE]
  if (nrow(unexpected) > 0L) {
    cat("\nUnexpected:\n")
    print(unexpected[, c("id", "change", "detail")], row.names = FALSE)
    quit(status = 1L)
  }
  invisible(calls)
}

if (identical(args[1L], "run")) {
  identity_run(args[2L], args[3L], root = if (length(args) > 3L) args[4L] else ".")
} else {
  identity_compare(args[2L], args[3L], if (length(args) > 3L) args[4L] else NULL)
}
