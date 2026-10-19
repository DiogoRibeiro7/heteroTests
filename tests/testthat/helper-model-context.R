# The test matrix of the model context (R/model_context.R; the design is
# docs/design/model-context.md, section "Test matrix").
#
# A cell is a fitted model and the `data` a test is given with it. The cells
# cross the axes of the design: the fit (ols, weights from a column, weights
# from the workspace, a weighted Gaussian glm, fitWLS()), the offset (none, an
# argument, an offset() term), the terms (none, log, poly(), ns(), I(x1^2),
# I(x1 > median(x1)), cut(x1, quantile(x1))), the form of `data` (NULL, the
# data frame of the fit, shuffled with its own row names, a tibble, a shuffled
# tibble, a shuffled data.table, dplyr::arrange(), the model frame), the rows
# the fit dropped (na.omit with the full data, the complete rows named or
# renumbered, the tibble; na.exclude) and the identifiers (rows in front, rows
# at the end, a fit to repeated rows, repeated row names, character row names
# as a tibble, a repeated (id, time)). test-model-context.R runs the
# functions of `context_migrated` on every cell and checks relational oracles;
# scripts/model-context-identity.R runs every recipe of helper-test-recipes.R
# on the same cells against main.
#
# Only exported functions of the package are used here, so that the identity
# harness can source this file against the released code too.

# The functions built on the model context. Each increment of the migration
# adds the functions it moves; the cells are run for these only, so no
# expectation of a function that is not migrated is weakened. Increment 1:
# the first five; increment 2: the studentized Breusch-Pagan statistic
# (through its deprecated name), RESET, Goldfeld-Quandt and the quantile
# regression test.
context_migrated <- c(
  "performWhiteTest", "performBPTest", "performKoenkerTest",
  "performRankPermutationTest", "performHighDimensionalTest",
  "performStudentizedBPTest", "performRESETTest", "performGQTest",
  "performQuantileRegressionTest"
)

# Fixture B of the design: contract_data() with weights, an offset outside the
# span of the regressors, and a positive response for the log model.
context_data <- function() {
  d <- contract_data()$d
  set.seed(99)
  d$w <- 1 / d$x1^2
  d$z <- stats::rnorm(nrow(d))
  d$o <- 0.5 * d$z
  d$ypos <- exp(0.5 + 0.4 * log(d$x1) + 0.2 * d$x2 + stats::rnorm(nrow(d), sd = 0.3 * d$x1))
  d
}

context_terms <- c("none", "log", "poly", "ns", "square", "median", "cut", "factor")
context_fits <- c("ols", "wls", "wvec", "glm", "fitwls")
context_offsets <- c("none", "argument", "term")

context_formula <- function(terms = "none", offset = "none") {
  f <- switch(terms,
    none = y ~ x1 + x2,
    log = log(ypos) ~ log(x1) + x2,
    poly = y ~ poly(x1, 2) + x2,
    ns = y ~ splines::ns(x1, 3) + x2,
    square = y ~ x1 + I(x1^2) + x2,
    median = y ~ I(x1 > median(x1)) + x2,
    cut = y ~ cut(x1, quantile(x1)) + x2,
    factor = y ~ x1 + group,
    stop("unknown terms: ", terms)
  )
  if (identical(offset, "term")) {
    f <- stats::update(f, . ~ . + offset(o))
  }
  f
}

# The model of a cell, fitted to `d`. `wvec` holds the weights in the
# environment of the formula rather than in `d`.
context_model <- function(d, fit = "ols", offset = "none", terms = "none", ...) {
  f <- context_formula(terms, offset)
  wvec <- d$w
  environment(f) <- environment()
  with_offset <- identical(offset, "argument")
  switch(fit,
    ols = if (with_offset) stats::lm(f, data = d, offset = o, ...) else stats::lm(f, data = d, ...),
    wls = if (with_offset) {
      stats::lm(f, data = d, weights = w, offset = o, ...)
    } else {
      stats::lm(f, data = d, weights = w, ...)
    },
    wvec = if (with_offset) {
      stats::lm(f, data = d, weights = wvec, offset = o, ...)
    } else {
      stats::lm(f, data = d, weights = wvec, ...)
    },
    glm = if (with_offset) {
      stats::glm(f, data = d, weights = w, offset = o, ...)
    } else {
      stats::glm(f, data = d, weights = w, ...)
    },
    fitwls = fitWLS(
      if (with_offset) stats::lm(f, data = d, offset = o, ...) else stats::lm(f, data = d, ...)
    ),
    stop("unknown fit: ", fit)
  )
}

# The columns of the terms computed beforehand, on all the rows of the data,
# as the fit computes them, and the formulas that use them: the reference of a
# term ("a term is the column it computes").
context_precomputed <- function() {
  d <- context_data()
  pre <- d
  pre$ly <- log(d$ypos)
  pre$lx1 <- log(d$x1)
  basis <- stats::poly(d$x1, 2)
  pre$p1 <- basis[, 1]
  pre$p2 <- basis[, 2]
  spline <- splines::ns(d$x1, 3)
  pre$n1 <- spline[, 1]
  pre$n2 <- spline[, 2]
  pre$n3 <- spline[, 3]
  pre$x1sq <- d$x1^2
  pre$high <- d$x1 > stats::median(d$x1)
  pre$cx <- cut(d$x1, stats::quantile(d$x1))
  list(
    data = pre,
    formulas = list(
      log = ly ~ lx1 + x2, poly = y ~ p1 + p2 + x2, ns = y ~ n1 + n2 + n3 + x2,
      square = y ~ x1 + x1sq + x2, median = y ~ high + x2, cut = y ~ cx + x2
    )
  )
}

# The fit of a term cell with its columns computed beforehand, and its data.
context_precomputed_fit <- function(fit, terms) {
  pre <- context_precomputed()
  f <- pre$formulas[[terms]]
  environment(f) <- environment()
  data <- pre$data
  m <- if (identical(fit, "wls")) {
    stats::lm(f, data = data, weights = w)
  } else {
    stats::lm(f, data = data)
  }
  list(m = m, data = data)
}

# The forms of `data` and whether a test that reads `data` accepts them.
context_forms <- c(
  "null", "data_frame", "shuffled", "tibble", "tibble_shuffled",
  "data_table_shuffled", "arranged", "model_frame"
)
context_refused_forms <- c("tibble_shuffled", "data_table_shuffled", "arranged")
context_form_needs <- c(
  tibble = "dplyr", tibble_shuffled = "dplyr", data_table_shuffled = "data.table",
  arranged = "dplyr"
)

context_form <- function(form, m, d) {
  set.seed(3)
  perm <- sample(nrow(d))
  switch(form,
    null = NULL,
    data_frame = d,
    shuffled = d[perm, ],
    tibble = dplyr::as_tibble(d),
    tibble_shuffled = dplyr::as_tibble(d)[perm, ],
    data_table_shuffled = data.table::as.data.table(d[perm, ]),
    arranged = dplyr::arrange(d, x1),
    model_frame = stats::model.frame(m),
    stop("unknown form: ", form)
  )
}

# A cell: an id, the axes it sets, a function that builds the model and the
# data (the cells of the fit and term blocks pass its arguments to the fit),
# and the packages it needs. `reference` names the cell whose value an
# accepted cell must reproduce, `identical()`; `expect` is "value", "guard"
# (refused with the class of the guard) or a regular expression the error
# matches.
context_cell <- function(id, block, build, fit = "ols", offset = "none", terms = "none",
                         form = "data_frame", needs = character(), reference = NULL,
                         expect = "value", defect = character()) {
  list(
    id = id, block = block, build = build, fit = fit, offset = offset, terms = terms,
    form = form, needs = needs, reference = reference, expect = expect, defect = defect
  )
}

# The defects of the design that a cell shows on the code before the context,
# for the allow-list of scripts/model-context-identity.R and the NEWS. B7: an
# offset, which RESET, Goldfeld-Quandt, the quantile regression test and the
# studentized Breusch-Pagan statistic left out of a refit, and refused until
# increment 2. N1: a term that depends on the rows it is computed from, which
# the segments of Goldfeld-Quandt, refitted from the formula on an unweighted
# fit, computed again; for poly(), whose basis spans the same columns on any
# rows, only the last bits changed.
context_cell_defects <- function(fit, offset, terms, form) {
  defects <- character()
  if (!identical(offset, "none")) {
    defects <- c(defects, "B7")
  }
  if (terms %in% c("ns", "median", "cut") || identical(terms, "poly") && identical(fit, "ols")) {
    defects <- c(defects, "N1")
  }
  weighted_column <- fit %in% c("wls", "glm")
  if (form %in% c("null", "model_frame") && (weighted_column || identical(offset, "argument"))) {
    defects <- c(defects, if (identical(form, "null")) "B12" else "L2")
  }
  if (form %in% c("null", "model_frame") &&
        (identical(offset, "term") || terms %in% c("log", "poly", "ns", "median", "cut"))) {
    defects <- c(defects, if (identical(form, "null")) "L1" else "L2")
  }
  if (identical(terms, "cut") && !form %in% context_refused_forms) {
    defects <- c(defects, "N7")
  }
  if (identical(terms, "poly")) {
    defects <- c(defects, "decision1")
  }
  unique(defects)
}

context_cells <- function() {
  d <- context_data()
  cells <- list()
  add <- function(cell) cells[[length(cells) + 1L]] <<- cell
  form_cells <- function(block, fit, offset, terms) {
    for (form in context_forms) {
      local({
        fit <- fit
        offset <- offset
        terms <- terms
        form <- form
        id <- paste(block, fit, offset, terms, form, sep = "/")
        # `...` goes to the fit: `x = TRUE` keeps its design, which
        # scripts/model-context-identity.R uses for the cells of decision 1.
        build <- function(...) {
          m <- context_model(d, fit, offset, terms, ...)
          list(m = m, data = context_form(form, m, d))
        }
        expect <- if (form %in% context_refused_forms) "guard" else "value"
        reference <- if (identical(expect, "value") && !identical(form, "data_frame")) {
          paste(block, fit, offset, terms, "data_frame", sep = "/")
        }
        needs <- unname(context_form_needs[form])
        needs <- needs[!is.na(needs)]
        add(context_cell(id, block, build, fit, offset, terms, form, needs, reference, expect,
                         context_cell_defects(fit, offset, terms, form)))
      })
    }
  }
  # The fit and the offset, without terms, crossed with the forms of data.
  for (fit in context_fits) {
    for (offset in context_offsets) {
      form_cells("fit", fit, offset, "none")
    }
  }
  # The terms, on the unweighted and the weighted fit.
  for (fit in c("ols", "wls")) {
    for (terms in setdiff(context_terms, "none")) {
      form_cells("terms", fit, "none", terms)
    }
  }

  # The rows the fit dropped. The reference is the fit to the complete rows.
  d_na <- d
  d_na$x2[c(3, 7, 11)] <- NA
  complete <- d_na[stats::complete.cases(d_na[, c("y", "x1", "x2")]), ]
  renumbered <- complete
  rownames(renumbered) <- NULL
  missing_cell <- function(id, build, needs = character(), reference = "missing/complete_fit",
                           expect = "value", defect = character(), terms = "none") {
    add(context_cell(paste0("missing/", id), "missing", build, terms = terms, form = id,
                     needs = needs, reference = reference, expect = expect, defect = defect))
  }
  missing_cell("complete_fit", function() {
    list(m = stats::lm(y ~ x1 + x2, data = complete), data = complete)
  }, reference = NULL)
  missing_cell("full", function() list(m = stats::lm(y ~ x1 + x2, data = d_na), data = d_na))
  missing_cell("full_shuffled", function() {
    set.seed(3)
    list(m = stats::lm(y ~ x1 + x2, data = d_na), data = d_na[sample(nrow(d_na)), ])
  })
  missing_cell("complete_named", function() {
    list(m = stats::lm(y ~ x1 + x2, data = d_na), data = complete)
  })
  missing_cell("complete_renumbered", function() {
    list(m = stats::lm(y ~ x1 + x2, data = d_na), data = renumbered)
  }, defect = "B14")
  missing_cell("tibble", function() {
    list(m = stats::lm(y ~ x1 + x2, data = d_na), data = dplyr::as_tibble(d_na))
  }, needs = "dplyr", defect = "B14")
  missing_cell("fit_to_tibble", function() {
    tb <- dplyr::as_tibble(d_na)
    list(m = stats::lm(y ~ x1 + x2, data = tb), data = tb)
  }, needs = "dplyr", defect = "B14")
  missing_cell("null", function() list(m = stats::lm(y ~ x1 + x2, data = d_na), data = NULL))
  # A term computed from all the rows, on a fit that dropped three (B5): the
  # reference is the same fit with the term computed beforehand.
  d_med <- d_na
  d_med$high <- d_med$x1 > stats::median(d_med$x1)
  missing_cell("median_precomputed", function() {
    list(m = stats::lm(y ~ high + x2, data = d_med), data = d_med)
  }, reference = NULL, terms = "median")
  missing_cell("median_full", function() {
    list(m = stats::lm(y ~ I(x1 > median(x1)) + x2, data = d_med), data = d_med)
  }, reference = "missing/median_precomputed", defect = "B5", terms = "median")
  missing_cell("median_null", function() {
    list(m = stats::lm(y ~ I(x1 > median(x1)) + x2, data = d_med), data = NULL)
  }, reference = "missing/median_precomputed", defect = c("B5", "L1"), terms = "median")
  # na.exclude is increment 9: until then the tests refuse its padded residuals.
  missing_cell("na_exclude", function() {
    list(m = stats::lm(y ~ x1 + x2, data = d_na, na.action = stats::na.exclude), data = d_na)
  }, reference = NULL, expect = "Model residuals must be finite")

  # The identifiers of the rows.
  ident_cell <- function(id, build, needs = character(), reference = "fit/ols/none/none/data_frame",
                         expect = "value", defect = character()) {
    add(context_cell(paste0("ident/", id), "ident", build, form = id, needs = needs,
                     reference = reference, expect = expect, defect = defect))
  }
  m_plain <- function() stats::lm(y ~ x1 + x2, data = d)
  ident_cell("front", function() list(m = m_plain(), data = rbind(d[1:5, ], d)), expect = "guard")
  ident_cell("end", function() list(m = m_plain(), data = rbind(d, d[1:5, ])))
  repeated_rows <- d[c(seq_len(nrow(d)), 1:5), ]
  ident_cell("repeated_fit", function() {
    list(m = stats::lm(y ~ x1 + x2, data = repeated_rows), data = repeated_rows)
  }, reference = NULL)
  ident_cell("repeated_fit_tibble", function() {
    list(m = stats::lm(y ~ x1 + x2, data = repeated_rows), data = dplyr::as_tibble(repeated_rows))
  }, needs = "dplyr", reference = "ident/repeated_fit", defect = "B14")
  ident_cell("repeated_names", function() {
    forced <- d
    attr(forced, "row.names") <- c(1L, seq_len(nrow(d) - 1L))
    list(m = m_plain(), data = forced)
  }, expect = "guard")
  named <- d
  rownames(named) <- paste0("obs", seq_len(nrow(d)))
  ident_cell("character_names", function() {
    list(m = stats::lm(y ~ x1 + x2, data = named), data = named)
  })
  ident_cell("character_names_tibble", function() {
    list(m = stats::lm(y ~ x1 + x2, data = named), data = dplyr::as_tibble(named))
  }, needs = "dplyr", defect = "B14")
  ident_cell("repeated_id_time", function() {
    panel <- d
    panel$time[2] <- 1L
    list(m = stats::lm(y ~ x1 + x2, data = panel), data = panel)
  })

  # A fit to a subset given by index (B15): the reference is the fit to the
  # same rows.
  kept <- d[21:120, ]
  add(context_cell("subset/rows", "subset", function() {
    list(m = stats::lm(y ~ x1 + x2, data = kept), data = kept)
  }, form = "rows"))
  add(context_cell("subset/data_frame", "subset", function() {
    list(m = stats::lm(y ~ x1 + x2, data = d, subset = 21:120), data = d)
  }, reference = "subset/rows", defect = "B15"))
  add(context_cell("subset/shuffled", "subset", function() {
    set.seed(3)
    list(m = stats::lm(y ~ x1 + x2, data = d, subset = 21:120), data = d[sample(nrow(d)), ])
  }, form = "shuffled", reference = "subset/rows", defect = "B15"))
  add(context_cell("subset/null", "subset", function() {
    list(m = stats::lm(y ~ x1 + x2, data = d, subset = 21:120), data = NULL)
  }, form = "null", reference = "subset/rows", defect = "B15"))

  # Classes of model. Those whose answer differs between the tests are run with
  # the data frame of the fit, without `data`, and with the same rows shuffled
  # under their own row names (class_forms()).
  set.seed(3)
  class_perm <- sample(nrow(d))
  class_forms <- function(id, build, expect = "value", defect = character(), needs = character(),
                          fit = "ols", expect_null = expect) {
    for (form in c("null", "shuffled")) {
      local({
        form <- form
        cell_expect <- if (identical(form, "null")) expect_null else expect
        built_form <- function() {
          built <- build()
          built$data <- if (identical(form, "shuffled")) built$data[class_perm, , drop = FALSE]
          built
        }
        add(context_cell(paste0("class/", id, "_", form), "class", built_form, fit = fit,
                         form = form, needs = needs, expect = cell_expect, defect = defect,
                         reference = if (identical(cell_expect, "value")) paste0("class/", id)))
      })
    }
  }
  mlm_fit <- function() list(m = stats::lm(cbind(y, ypos) ~ x1 + x2, data = d), data = d)
  add(context_cell("class/mlm", "class", mlm_fit,
                   expect = "needs a model with one response; `model` has 2", defect = "R4"))
  class_forms("mlm", mlm_fit, expect = "needs a model with one response; `model` has 2",
              defect = "R4")
  add(context_cell("class/rank_deficient", "class", function() {
    list(m = stats::lm(y ~ x1 + x2 + I(2 * x2), data = d), data = d)
  }, reference = "fit/ols/none/none/data_frame"))
  add(context_cell("class/rlm", "class", function() {
    list(m = MASS::rlm(y ~ x1 + x2, data = d), data = d)
  }))
  # A Gaussian mgcv::gam() is a glm by class, and is accepted as before 0.16.0;
  # without smooth terms it is the lm.
  add(context_cell("class/gam", "class", function() {
    list(m = mgcv::gam(y ~ x1 + x2, data = d), data = d)
  }, needs = "mgcv", reference = "fit/ols/none/none/data_frame"))
  # A smooth term, usually penalised: the tests of increment 1 test the fit as
  # before, and those of increment 2 refuse it until increment 11 settles which
  # tests take it ("gam"). Goldfeld-Quandt and the quantile regression test, which refitted
  # the formula, stopped on its smooth term outside mgcv.
  gam_smooth_fit <- function() list(m = mgcv::gam(y ~ s(x1) + x2, data = d), data = d)
  add(context_cell("class/gam_smooth", "class", gam_smooth_fit, needs = "mgcv", defect = "gam"))
  class_forms("gam_smooth", gam_smooth_fit, needs = "mgcv", defect = "gam")
  # A zero weight leaves an observation out of the fit with a residual that is
  # not one: refused by every test but RESET, whose two weighted fits both
  # leave it out (decided 2026-10-17), for an lm and a Gaussian glm alike.
  zero <- d
  zero$w0 <- d$w
  zero$w0[1] <- 0
  zero_lm <- function() {
    list(m = stats::lm(y ~ x1 + x2, data = zero, weights = w0), data = zero)
  }
  zero_glm <- function() {
    list(m = stats::glm(y ~ x1 + x2, data = zero, weights = w0), data = zero)
  }
  add(context_cell("class/zero_weight", "class", zero_lm, fit = "wls",
                   expect = "zero or non-finite weights"))
  class_forms("zero_weight", zero_lm, fit = "wls", expect = "zero or non-finite weights")
  add(context_cell("class/zero_weight_glm", "class", zero_glm, fit = "glm",
                   expect = "zero or non-finite weights"))
  class_forms("zero_weight_glm", zero_glm, fit = "glm", expect = "zero or non-finite weights")
  # A fit with model = FALSE whose data are gone, given its data. The tests
  # that read its model frame stopped ("object 'gone' not found"); the context
  # builds the frame from `data` once `data` reproduces the fit ("frame").
  # Without `data` its frame cannot be built, and every test refuses it.
  model_false_fit <- function() {
    env <- new.env()
    assign("gone", d, envir = env)
    m <- eval(quote(stats::lm(y ~ x1 + x2, data = gone, model = FALSE)), env)
    rm("gone", envir = env)
    list(m = m, data = d)
  }
  add(context_cell("class/model_false", "class", model_false_fit,
                   reference = "fit/ols/none/none/data_frame", defect = "frame"))
  class_forms("model_false", model_false_fit, defect = "frame",
              expect_null = "cannot use the model frame of `model`")
  # A fit with model = FALSE whose data were reordered after the fit, without
  # `data`: its model frame, rebuilt from the reordered data, no longer holds
  # its response. RESET read it and returned a value (N6); every test built on
  # the context refuses it.
  add(context_cell("class/model_false_changed", "class", function() {
    env <- new.env()
    assign("changed", d, envir = env)
    m <- eval(quote(stats::lm(y ~ x1 + x2, data = changed, model = FALSE)), env)
    assign("changed", d[order(d$x1), ], envir = env)
    list(m = m, data = NULL)
  }, form = "null", expect = "cannot use the model frame of `model`", defect = "N6"))
  names(cells) <- vapply(cells, `[[`, character(1), "id")
  cells
}

# The cells run on CRAN: no package beyond the hard dependencies, and a few of
# each block.
context_cran_cells <- c(
  "fit/ols/none/none/data_frame", "fit/ols/none/none/null", "fit/ols/none/none/shuffled",
  "fit/ols/none/none/model_frame", "fit/wls/none/none/null", "fit/ols/argument/none/null",
  "terms/ols/none/log/null", "missing/complete_renumbered", "missing/median_full",
  "subset/data_frame", "ident/front", "class/mlm"
)

# The tests of increment 2.
context_increment2 <- c(
  "performStudentizedBPTest", "performRESETTest", "performGQTest",
  "performQuantileRegressionTest"
)

# The functions of `context_migrated` that can run here: the quantile
# regression test needs quantreg, a suggested package.
context_runnable <- function(fns = context_migrated) {
  if (!requireNamespace("quantreg", quietly = TRUE)) {
    fns <- setdiff(fns, "performQuantileRegressionTest")
  }
  fns
}

# The cell as one test sees it: where a migrated test departs from the
# expectation of a cell by design, its own expectation, and the reference of a
# value.
#
# - The tests of increment 2 refuse an mgcv::gam() fit with smooth
#   terms (context_increment2).
# - The quantile regression test refuses a weighted fit, and a MASS::rlm()
#   fit, whose weights it reads, once `data` is aligned.
# - Goldfeld-Quandt orders by `x1`, which the model frame of a fit that
#   transforms `x1` does not hold: without `data` it is refused with the
#   message of the context (M1), and the model frame given as `data` lacks it.
# - RESET reads nothing from `data`. It builds its context without `data` and
#   keeps the check of `data` of the tests computed from the model alone
#   (rcheck_data_argument()), which looks for the row names of the fit and
#   compares no values, until increment 6 moves those tests. So it gives the
#   value of the fit on the forms of data the guard refuses, refuses data
#   whose row names lack those of the fit, refuses a model = FALSE fit whose
#   data are gone, stops on the padded fitted values of na.exclude
#   (increment 9), and accepts a zero weight.
context_cell_for <- function(fn, cell) {
  if (fn %in% context_increment2 && startsWith(cell$id, "class/gam_smooth")) {
    cell$expect <- "cannot test an mgcv::gam() fit with smooth terms (s(x1))"
    return(cell)
  }
  weighted <- cell$fit %in% c("wls", "wvec", "glm", "fitwls") || identical(cell$id, "class/rlm")
  if (identical(fn, "performQuantileRegressionTest") && weighted &&
        !identical(cell$expect, "guard")) {
    cell$expect <- "does not support weighted fits"
    cell$reference <- NULL
  }
  transformed <- cell$terms %in% c("log", "poly", "ns", "median", "cut")
  # The cells without `data`: the form "null", and the fit that dropped rows.
  no_data <- identical(cell$form, "null") || identical(cell$id, "missing/median_null")
  if (identical(fn, "performGQTest") && transformed &&
        (no_data || identical(cell$form, "model_frame")) && identical(cell$expect, "value")) {
    cell$expect <- if (no_data) {
      "The following variables are missing from the model frame of `model`: x1."
    } else {
      "The following variables are missing from input data: x1."
    }
    cell$reference <- NULL
  }
  if (identical(fn, "performRESETTest")) {
    rows_lacking <- c(
      "missing/complete_renumbered", "ident/repeated_fit_tibble",
      "ident/character_names_tibble", "ident/repeated_names"
    )
    if (cell$id %in% rows_lacking) {
      cell$expect <- "RESET requires `data` to contain the rows used to fit the model."
      cell$reference <- NULL
    } else if (identical(cell$expect, "guard")) {
      cell$expect <- "value"
      cell$reference <- if (cell$block %in% c("fit", "terms")) {
        paste(cell$block, cell$fit, cell$offset, cell$terms, "data_frame", sep = "/")
      } else {
        "fit/ols/none/none/data_frame"
      }
    } else if (cell$id %in% c("class/model_false", "class/model_false_shuffled")) {
      cell$expect <- "cannot use the model frame of `model`"
      cell$reference <- NULL
    } else if (identical(cell$id, "missing/na_exclude")) {
      cell$expect <- "NA/NaN/Inf in 'x'"
    } else if (startsWith(cell$id, "class/zero_weight")) {
      cell$expect <- "value"
    }
  }
  cell
}

# A cell run through one test: the result, or the error as a condition.
context_run <- function(fn, cell, recipes = contract_recipes) {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  built <- cell$build()
  set.seed(7)
  tryCatch(
    suppressMessages(suppressWarnings(recipes[[fn]](built$m, built$data))),
    error = function(e) e
  )
}
