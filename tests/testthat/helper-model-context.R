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
# renumbered, the tibble; na.exclude, against its twin with na.omit, in the
# block exclude/), the identifiers (rows in front, rows
# at the end, a fit to repeated rows, repeated row names, character row names
# as a tibble, a repeated (id, time)) and the panels of increment 5 (a fit
# that drops every row of a unit, by an lm, a weighted lm, a weighted Gaussian
# glm and a gam, in fit order, shuffled and as a tibble; a repeated
# (id, time), shuffled). test-model-context.R runs the
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
# regression test; increment 3: Glejser, Park, Szroeter, Harrison-McCabe,
# Hartley and the spatial test; increment 4: the five group tests, Levene,
# Brown-Forsythe, Fligner-Killeen, Bartlett and O'Brien; increment 5: the two
# panel tests, the scatter diagnostic and runPanelTests(), which runs the
# panel tests; increment 6: the tests computed from the model alone,
# Spearman's, Harvey's, the ARCH LM, McLeod-Li, Cook-Weisberg and NCV tests,
# the spread-level, Davidian-Carroll and variance form tests (and RESET, of
# increment 2).
context_migrated <- c(
  "performWhiteTest", "performBPTest", "performKoenkerTest",
  "performRankPermutationTest", "performHighDimensionalTest",
  "performStudentizedBPTest", "performRESETTest", "performGQTest",
  "performQuantileRegressionTest",
  "performGlejserTest", "performParkTest", "performSzroeterTest",
  "performHarrisonMcCabeTest", "performHartleyFmaxTest", "performSpatialHeteroTest",
  "performLeveneTest", "performBrownForsytheTest", "performFlignerKilleenTest",
  "performBartlettTest", "performOBrienTest",
  "performPesaranTest", "performBPRandomEffectsTest", "performScatterDiagnostic",
  "runPanelTests",
  "performSpearmanTest", "performHarveyTest", "performArchLMTest", "performMcLeodLiTest",
  "performCookWeisbergTest", "performNCVTest", "performSpreadLevelTest",
  "performDavidianCarrollTest", "performVarianceFormTest"
)

# The calls of the matrix: a recipe for every function of `context_migrated`.
# Those of helper-test-recipes.R, which test-return-contract.R holds to an
# htest, and the two functions of increment 5 that are not tests: the scatter
# diagnostic, which returns a correlation for each variable, and
# runPanelTests(), which returns a list of the two panel tests. And the calls
# of increment 6 that evaluate a second formula (`context_second_formula`),
# fitWLS() among them, as the vector of its coefficients and estimated
# weights. A function, since testthat sources helper-test-recipes.R after this
# file.
context_recipes <- function() {
  # Unnamed: the reference of a term computed beforehand names its coefficient
  # after its column.
  wls_numbers <- function(fit) unname(c(stats::coef(fit), stats::weights(fit)))
  c(contract_recipes, list(
    performScatterDiagnostic = function(m, d) performScatterDiagnostic(m, d, c("x1", "x2")),
    runPanelTests = function(m, d) runPanelTests(m, d, id = "id", time = "time"),
    `performCookWeisbergTest(var_formula = ~ z)` = function(m, d) {
      performCookWeisbergTest(m, d, var_formula = ~ z)
    },
    `performVarianceFormTest(var_formula = ~ z)` = function(m, d) {
      performVarianceFormTest(m, d, var_formula = ~ z)
    },
    `performVarianceFormTest(against = ~ z)` = function(m, d) {
      performVarianceFormTest(m, d, against = ~ z)
    },
    `fitWLS(var_formula = ~ z)` = function(m, d) wls_numbers(fitWLS(m, d, var_formula = ~ z)),
    fitWLS = function(m, d) wls_numbers(fitWLS(m, d))
  ))
}

# The tests computed from the model alone (increment 6), with RESET
# (increment 2), and the label each names itself with in the messages on the
# rows of `data`.
context_model_alone <- c(
  performSpearmanTest = "Spearman rank", performHarveyTest = "Harvey",
  performArchLMTest = "ARCH LM", performMcLeodLiTest = "McLeod-Li",
  performCookWeisbergTest = "Cook-Weisberg", performNCVTest = "NCV",
  performSpreadLevelTest = "Spread-level", performDavidianCarrollTest = "Davidian-Carroll",
  performVarianceFormTest = "Variance form test", performRESETTest = "RESET"
)

# The calls of increment 6 that evaluate a second formula of z, a variable no
# model frame of the matrix holds, on the rows of `data`; and fitWLS()
# without one, which reads nothing from `data`. They are run on every cell by
# a test of their own (test-model-context.R), as the cell is seen by
# context_cell_for_formula().
context_second_formula <- c(
  "performCookWeisbergTest(var_formula = ~ z)", "performVarianceFormTest(var_formula = ~ z)",
  "performVarianceFormTest(against = ~ z)", "fitWLS(var_formula = ~ z)", "fitWLS"
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

context_terms <- c("none", "log", "poly", "ns", "square", "median", "cut", "factor", "log_factor")
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
    log_factor = log(ypos) ~ log(x1) + group,
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
                         expect = "value", defect = character(), base = NULL,
                         base_expect = NULL, twin = NULL) {
  list(
    id = id, block = block, build = build, fit = fit, offset = offset, terms = terms,
    form = form, needs = needs, reference = reference, expect = expect, defect = defect,
    base = base, base_expect = base_expect, twin = twin
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
  transformed <- terms %in% c("log", "poly", "ns", "median", "cut", "log_factor")
  if (form %in% c("null", "model_frame") && (identical(offset, "term") || transformed)) {
    defects <- c(defects, if (identical(form, "null")) "L1" else "L2")
  }
  if (identical(terms, "cut") && !form %in% context_refused_forms) {
    defects <- c(defects, "N7")
  }
  if (identical(terms, "poly")) {
    defects <- c(defects, "decision1")
  }
  # Increment 5. B3 and B4: the panel tests and the scatter diagnostic read
  # `data` by position, and refused data with row names of their own in
  # another order than the fit. B16: the panel tests on a fit that dropped
  # rows, as cut(x1, quantile(x1)) drops the row at the minimum of x1, which
  # they refused for its number of rows.
  if (identical(form, "shuffled")) {
    defects <- c(defects, "B3", "B4")
  }
  if (identical(terms, "cut") && !form %in% c("null", "model_frame", context_refused_forms)) {
    defects <- c(defects, "B16")
  }
  # Increment 7. B6: the streaming tests built the design of each chunk from
  # the formula, on its rows of `data`, so a term computed from the rows it is
  # given took other values in each chunk than in the fit.
  if (terms %in% c("median", "cut")) {
    defects <- c(defects, "B6")
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
  # The terms, on the unweighted and the weighted fit. The model with the
  # factor `group` holds the variable the group tests read in its model frame,
  # also with log() terms (increment 4) and with an offset() term, which the
  # model frame holds as `offset(o)` and not as `o`.
  for (fit in c("ols", "wls")) {
    for (terms in setdiff(context_terms, "none")) {
      form_cells("terms", fit, "none", terms)
    }
    form_cells("terms", fit, "term", "factor")
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
  # The panel tests and the scatter diagnostic refused the data of a fit that
  # dropped rows, for its number of rows (B16), and data with row names of
  # their own in another order than the fit (B3, B4).
  missing_cell("full", function() list(m = stats::lm(y ~ x1 + x2, data = d_na), data = d_na),
               defect = "B16")
  missing_cell("full_shuffled", function() {
    set.seed(3)
    list(m = stats::lm(y ~ x1 + x2, data = d_na), data = d_na[sample(nrow(d_na)), ])
  }, defect = c("B3", "B4", "B16"))
  missing_cell("complete_named", function() {
    list(m = stats::lm(y ~ x1 + x2, data = d_na), data = complete)
  })
  missing_cell("complete_renumbered", function() {
    list(m = stats::lm(y ~ x1 + x2, data = d_na), data = renumbered)
  }, defect = c("B14", "B16"))
  missing_cell("tibble", function() {
    list(m = stats::lm(y ~ x1 + x2, data = d_na), data = dplyr::as_tibble(d_na))
  }, needs = "dplyr", defect = c("B14", "B16"))
  missing_cell("fit_to_tibble", function() {
    tb <- dplyr::as_tibble(d_na)
    list(m = stats::lm(y ~ x1 + x2, data = tb), data = tb)
  }, needs = "dplyr", defect = c("B14", "B16"))
  missing_cell("null", function() list(m = stats::lm(y ~ x1 + x2, data = d_na), data = NULL))
  # A term computed from all the rows, on a fit that dropped three (B5): the
  # reference is the same fit with the term computed beforehand. Its fit to
  # the complete rows is the reference of the panel tests and the scatter
  # diagnostic, which refused the full data (B16, context_cell_for()).
  d_med <- d_na
  d_med$high <- d_med$x1 > stats::median(d_med$x1)
  med_complete <- d_med[stats::complete.cases(d_med[, c("y", "high", "x2")]), ]
  missing_cell("median_complete", function() {
    list(m = stats::lm(y ~ high + x2, data = med_complete), data = med_complete)
  }, reference = NULL, terms = "median")
  missing_cell("median_precomputed", function() {
    list(m = stats::lm(y ~ high + x2, data = d_med), data = d_med)
  }, reference = NULL, terms = "median", defect = "B16")
  missing_cell("median_full", function() {
    list(m = stats::lm(y ~ I(x1 > median(x1)) + x2, data = d_med), data = d_med)
  }, reference = "missing/median_precomputed", defect = c("B5", "B16", "B6"), terms = "median")
  missing_cell("median_null", function() {
    list(m = stats::lm(y ~ I(x1 > median(x1)) + x2, data = d_med), data = NULL)
  }, reference = "missing/median_precomputed", defect = c("B5", "L1"), terms = "median")
  # A fit with na.exclude (increment 9, B13), whose twin is the same fit with
  # na.omit given the same data (the block `exclude/` below). The tests
  # refused it ("Model residuals must be finite"), RESET stopped on it, and the
  # panel tests and the scatter diagnostic read its padded residuals against
  # the rows of `data` by position.
  add(context_cell("missing/na_exclude", "missing", function() {
    list(m = stats::lm(y ~ x1 + x2, data = d_na, na.action = stats::na.exclude), data = d_na)
  }, form = "na_exclude", reference = "missing/full", defect = "B13", twin = "missing/full"))

  # A group that is missing at observations of the fit (increment 4). The
  # group tests leave those observations out, with the warning "Removed N
  # observations due to missing values in group", as before 0.16.0; Hartley's
  # test refuses them (decision 8). Each form of data is compared with the data
  # frame of the fit. `na_dropped`: the fit drops rows 3, 7 and 11, the group
  # is missing at row 5, which the fit holds, and at row 7, which it dropped.
  # `na_factor`: a model of the group, whose fit drops the rows with a missing
  # group, without and with `data`.
  set.seed(3)
  group_perm <- sample(nrow(d))
  d_g <- d
  d_g$group[c(5, 50)] <- NA
  d_gn <- d_na
  d_gn$group[c(5, 7)] <- NA
  gn_complete <- d_gn[stats::complete.cases(d_gn[, c("y", "x1", "x2")]), ]
  rownames(gn_complete) <- NULL
  group_cell <- function(id, build, form, needs = character(), reference = NULL,
                         expect = "value", defect = character(), terms = "none") {
    add(context_cell(paste0("group/", id), "group", build, terms = terms, form = form,
                     needs = needs, reference = reference, expect = expect, defect = defect))
  }
  group_cell("na", function() list(m = stats::lm(y ~ x1 + x2, data = d_g), data = d_g),
             form = "data_frame")
  group_cell("na_shuffled", function() {
    list(m = stats::lm(y ~ x1 + x2, data = d_g), data = d_g[group_perm, ])
  }, form = "shuffled", reference = "group/na", defect = c("B3", "B4"))
  group_cell("na_tibble", function() {
    list(m = stats::lm(y ~ x1 + x2, data = d_g), data = dplyr::as_tibble(d_g))
  }, form = "tibble", needs = "dplyr", reference = "group/na")
  group_cell("na_tibble_shuffled", function() {
    list(m = stats::lm(y ~ x1 + x2, data = d_g), data = dplyr::as_tibble(d_g)[group_perm, ])
  }, form = "tibble_shuffled", needs = "dplyr", expect = "guard")
  group_cell("na_dropped", function() list(m = stats::lm(y ~ x1 + x2, data = d_gn), data = d_gn),
             form = "data_frame", defect = "B16")
  group_cell("na_dropped_shuffled", function() {
    list(m = stats::lm(y ~ x1 + x2, data = d_gn), data = d_gn[group_perm, ])
  }, form = "shuffled", reference = "group/na_dropped", defect = c("B3", "B4", "B16"))
  group_cell("na_dropped_renumbered", function() {
    list(m = stats::lm(y ~ x1 + x2, data = d_gn), data = gn_complete)
  }, form = "complete_renumbered", reference = "group/na_dropped", defect = c("B14", "B16"))
  group_cell("na_factor", function() {
    list(m = stats::lm(y ~ x1 + group, data = d_g), data = d_g)
  }, form = "data_frame", terms = "factor", defect = "B16")
  # The fit of the model of the group to its complete rows: the reference of
  # the panel tests and the scatter diagnostic on the cell above, whose data
  # they refused (B16, context_cell_for()).
  g_complete <- d_g[!is.na(d_g$group), ]
  group_cell("na_factor_complete", function() {
    list(m = stats::lm(y ~ x1 + group, data = g_complete), data = g_complete)
  }, form = "data_frame", terms = "factor")
  group_cell("na_factor_null", function() {
    list(m = stats::lm(y ~ x1 + group, data = d_g), data = NULL)
  }, form = "null", terms = "factor", reference = "group/na_factor")

  # The identifiers of the rows.
  ident_cell <- function(id, build, needs = character(), reference = "fit/ols/none/none/data_frame",
                         expect = "value", defect = character()) {
    add(context_cell(paste0("ident/", id), "ident", build, form = id, needs = needs,
                     reference = reference, expect = expect, defect = defect))
  }
  m_plain <- function() stats::lm(y ~ x1 + x2, data = d)
  ident_cell("front", function() list(m = m_plain(), data = rbind(d[1:5, ], d)), expect = "guard")
  # The panel tests and the scatter diagnostic refused rows after those of the
  # fit, for their number (B3, B4).
  ident_cell("end", function() list(m = m_plain(), data = rbind(d, d[1:5, ])),
             defect = c("B3", "B4"))
  # A fit to repeated rows repeats their pairs of `id` and `time` too, which
  # Pesaran's test refuses (R2, decision 13).
  repeated_rows <- d[c(seq_len(nrow(d)), 1:5), ]
  ident_cell("repeated_fit", function() {
    list(m = stats::lm(y ~ x1 + x2, data = repeated_rows), data = repeated_rows)
  }, reference = NULL, defect = "R2")
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
  }, defect = "R2")

  # The panels of increment 5. A fit that drops every row of a unit, here the
  # twelfth (rows 111 to 120, whose x2 is missing), keeps a balanced panel of
  # eleven units, on which Pesaran's test is defined; the panel tests and the
  # scatter diagnostic refused its data, for their number of rows (B16), and
  # in another order (B3, B4). It is fitted by an lm, a weighted lm, a weighted
  # Gaussian glm and a gam, whose residuals carry no names (N17); the
  # reference of each is the same fit to the complete rows. And a panel whose
  # second row repeats the period of its first, shuffled (R2).
  d_unit <- d
  d_unit$x2[111:120] <- NA
  unit_complete <- d_unit[1:110, ]
  set.seed(3)
  unit_perm <- sample(nrow(d_unit))
  unit_fit <- function(kind, data) {
    switch(kind,
      lm = stats::lm(y ~ x1 + x2, data = data),
      wls = stats::lm(y ~ x1 + x2, data = data, weights = w),
      glm = stats::glm(y ~ x1 + x2, data = data, weights = w),
      gam = mgcv::gam(y ~ x1 + x2, data = data)
    )
  }
  panel_cell <- function(id, build, form, fit = "ols", needs = character(), reference = NULL,
                         defect = character()) {
    add(context_cell(paste0("panel/", id), "panel", build, fit = fit, form = form, needs = needs,
                     reference = reference, defect = defect))
  }
  for (kind in c("lm", "wls", "glm", "gam")) {
    local({
      kind <- kind
      fit <- switch(kind, lm = "ols", gam = "ols", kind)
      needs <- if (identical(kind, "gam")) "mgcv"
      complete_id <- paste0("panel/unit_", kind, "_complete")
      panel_cell(paste0("unit_", kind, "_complete"), function() {
        list(m = unit_fit(kind, unit_complete), data = unit_complete)
      }, form = "data_frame", fit = fit, needs = needs)
      panel_cell(paste0("unit_", kind, "_full"), function() {
        list(m = unit_fit(kind, d_unit), data = d_unit)
      }, form = "data_frame", fit = fit, needs = needs, reference = complete_id, defect = "B16")
      panel_cell(paste0("unit_", kind, "_shuffled"), function() {
        list(m = unit_fit(kind, d_unit), data = d_unit[unit_perm, ])
      }, form = "shuffled", fit = fit, needs = needs, reference = complete_id,
      defect = c("B3", "B4", "B16", if (identical(kind, "gam")) "N17"))
    })
  }
  panel_cell("unit_lm_tibble", function() {
    list(m = unit_fit("lm", d_unit), data = dplyr::as_tibble(d_unit))
  }, form = "tibble", needs = "dplyr", reference = "panel/unit_lm_complete", defect = "B16")
  # An unbalanced panel (N12): unit 1 observed at 5 of the 10 periods, fitted
  # to those rows, whose residuals Pesaran's test recycled over the periods
  # the unit lacks and returned a wrong value; and the fit that drops those
  # rows from the full data, which it refused for their number of rows.
  # Increment 5 refused both (decided 2026-10-24); since the pull request of
  # N12 Pesaran's test gives the statistic computed from the periods each pair
  # of units shares, -0.373, as plm::pcdtest() does, and like the other tests
  # the value of the fit to the rows it keeps.
  half_unit <- d[-(1:5), ]
  panel_cell("half_unit", function() {
    list(m = stats::lm(y ~ x1 + x2, data = half_unit), data = half_unit)
  }, form = "data_frame")
  d_half <- d
  d_half$x2[1:5] <- NA
  panel_cell("half_unit_full", function() {
    list(m = stats::lm(y ~ x1 + x2, data = d_half), data = d_half)
  }, form = "data_frame", reference = "panel/half_unit", defect = "B16")
  repeated_pair <- d
  repeated_pair$time[2] <- 5L
  panel_cell("repeated_id_time", function() {
    list(m = stats::lm(y ~ x1 + x2, data = repeated_pair), data = repeated_pair)
  }, form = "data_frame", reference = "fit/ols/none/none/data_frame", defect = "R2")
  panel_cell("repeated_id_time_shuffled", function() {
    list(m = stats::lm(y ~ x1 + x2, data = repeated_pair), data = repeated_pair[unit_perm, ])
  }, form = "shuffled", reference = "fit/ols/none/none/data_frame",
  defect = c("B3", "B4", "R2"))

  # A variable of a second formula, z, missing at an observation of the fit,
  # and at a row the fit dropped (increment 6). Cook-Weisberg's test with
  # `var_formula` dropped the incomplete rows of `data`, with the warning
  # "Removed N observations due to missing values in z", and then lacked the
  # row of the fit; fitWLS() and the variance form test evaluated the formula
  # on the rows of the fit and refused the missing value. A test that does not
  # read z gives the value of its data without z.
  d_z <- d
  d_z$z[5] <- NA
  add(context_cell("variance/z_missing", "variance", function() {
    list(m = stats::lm(y ~ x1 + x2, data = d_z), data = d_z)
  }, reference = "fit/ols/none/none/data_frame"))
  d_zn <- d_na
  d_zn$z[7] <- NA
  add(context_cell("variance/z_missing_dropped", "variance", function() {
    list(m = stats::lm(y ~ x1 + x2, data = d_zn), data = d_zn)
  }, reference = "missing/complete_fit", defect = "B16"))

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

  # A fit with na.action = na.exclude (increment 9, B13). It drops the rows
  # na.omit drops, and pads residuals(), fitted(), weights() and hatvalues()
  # with NA at them (naresid()): the tests refused it ("Model residuals must be
  # finite"), RESET stopped on it ("NA/NaN/Inf in 'x'"), and the panel tests
  # and the scatter diagnostic read its padded residuals against the rows of
  # `data` by position. Each cell has a twin, the same fit with na.omit given
  # the same data, and a test gives on it what it gives on its twin, its value
  # or its refusal, and an output with one value per row padded as naresid()
  # pads it (test-model-context.R). Besides missing/na_exclude, a regressor
  # missing, in the other forms of its data, the values are missing in the
  # response, in x1, the variable and the ordering of the recipes, in the group,
  # in the id and the time of the panel tests at rows the fit dropped, and in
  # z, the variable of a second formula; with weights and an offset, a Gaussian
  # glm, a fitWLS() fit, a log model, a subset, data with row names of their
  # own, and a balanced and an unbalanced panel. `fit_with(na_action)` fits the
  # model, `data_of(m)` gives the data the test is given; `twin` names an
  # existing cell, or the twin is the cell `exclude/<id>_omit`.
  exclude_pair <- function(id, fit_with, data_of, twin = NULL, form = "data_frame",
                           needs = character(), fit = "ols", offset = "none", terms = "none",
                           expect = "value") {
    twin_id <- if (is.null(twin)) paste0("exclude/", id, "_omit") else twin
    if (is.null(twin)) {
      add(context_cell(twin_id, "exclude", function() {
        m <- fit_with(stats::na.omit)
        list(m = m, data = data_of(m))
      }, fit = fit, offset = offset, terms = terms, form = form, needs = needs,
      expect = expect))
    }
    add(context_cell(paste0("exclude/", id), "exclude", function() {
      m <- fit_with(stats::na.exclude)
      list(m = m, data = data_of(m))
    }, fit = fit, offset = offset, terms = terms, form = form, needs = needs,
    reference = twin_id, defect = "B13", twin = twin_id))
  }
  na_fit <- function(data) {
    force(data)
    function(na_action) stats::lm(y ~ x1 + x2, data = data, na.action = na_action)
  }
  # A regressor missing, x2 at rows 3, 7 and 11, in the other forms of data:
  # the twins are the cells of the block of missing rows.
  exclude_pair("shuffled", na_fit(d_na), function(m) {
    set.seed(3)
    d_na[sample(nrow(d_na)), ]
  }, twin = "missing/full_shuffled", form = "shuffled")
  exclude_pair("complete_named", na_fit(d_na), function(m) complete,
               twin = "missing/complete_named", form = "complete_named")
  exclude_pair("complete_renumbered", na_fit(d_na), function(m) renumbered,
               twin = "missing/complete_renumbered", form = "complete_renumbered")
  exclude_pair("tibble", na_fit(d_na), function(m) dplyr::as_tibble(d_na),
               twin = "missing/tibble", form = "tibble", needs = "dplyr")
  exclude_pair("tibble_shuffled", na_fit(d_na), function(m) {
    set.seed(3)
    dplyr::as_tibble(d_na)[sample(nrow(d_na)), ]
  }, form = "tibble_shuffled", needs = "dplyr", expect = "guard")
  exclude_pair("null", na_fit(d_na), function(m) NULL, twin = "missing/null", form = "null")
  exclude_pair("model_frame", na_fit(d_na), function(m) stats::model.frame(m),
               form = "model_frame")
  # The response missing, and x1, a regressor and the variable or ordering of
  # the recipes of Goldfeld-Quandt, Glejser, Park, Szroeter and Harrison-McCabe.
  d_y <- d
  d_y$y[c(3, 7, 11)] <- NA
  exclude_pair("response", na_fit(d_y), function(m) d_y)
  d_x1 <- d
  d_x1$x1[c(3, 7, 11)] <- NA
  exclude_pair("x1", na_fit(d_x1), function(m) d_x1)
  # Data with row names of their own, in the order of the fit and shuffled.
  named_na <- d_na
  rownames(named_na) <- paste0("obs", seq_len(nrow(d_na)))
  exclude_pair("named", na_fit(named_na), function(m) named_na)
  exclude_pair("named_shuffled", na_fit(named_na), function(m) {
    set.seed(3)
    named_na[sample(nrow(named_na)), ]
  }, form = "shuffled")
  # Weights and an offset, from columns of the data, with and without `data`.
  wls_offset_fit <- function(na_action) {
    stats::lm(y ~ x1 + x2, data = d_na, weights = w, offset = o, na.action = na_action)
  }
  exclude_pair("wls_offset", wls_offset_fit, function(m) d_na, fit = "wls", offset = "argument")
  exclude_pair("wls_offset_null", wls_offset_fit, function(m) NULL, form = "null", fit = "wls",
               offset = "argument")
  # A weighted Gaussian glm, and a fitWLS() fit of the lm.
  exclude_pair("glm", function(na_action) {
    stats::glm(y ~ x1 + x2, data = d_na, weights = w, na.action = na_action)
  }, function(m) d_na, fit = "glm")
  exclude_pair("fitwls", function(na_action) fitWLS(na_fit(d_na)(na_action)),
               function(m) d_na, fit = "fitwls")
  # A Gaussian mgcv::gam() fit, without and with a smooth term: its design,
  # model.matrix.gam(), is padded with NA rows at the rows the fit dropped,
  # which the context took for parts of different lengths, refusing every
  # such fit (from the review of increment 9). The tests of increment 2 and
  # the White bootstrap refuse the smooth term, as for its twin (decision 23),
  # and the wild bootstrap every gam.
  exclude_pair("gam", function(na_action) {
    mgcv::gam(y ~ x1 + x2, data = d_na, na.action = na_action)
  }, function(m) d_na, needs = "mgcv")
  exclude_pair("gam_smooth", function(na_action) {
    mgcv::gam(y ~ s(x1) + x2, data = d_na, na.action = na_action)
  }, function(m) d_na, needs = "mgcv")
  # A log model, whose model frame holds the terms, with and without `data`.
  log_fit <- function(na_action) {
    stats::lm(log(ypos) ~ log(x1) + x2, data = d_na, na.action = na_action)
  }
  exclude_pair("log", log_fit, function(m) d_na, terms = "log")
  exclude_pair("log_null", log_fit, function(m) NULL, form = "null", terms = "log")
  # A subset given by index, whose rows the fit drops before na.exclude does:
  # the padding is of the rows of the subset.
  d_sub <- d
  d_sub$x2[c(30, 45)] <- NA
  exclude_pair("subset", function(na_action) {
    stats::lm(y ~ x1 + x2, data = d_sub, subset = 21:120, na.action = na_action)
  }, function(m) d_sub)
  # A group missing at an observation of the fit and at a row it dropped, as
  # the cells of the block of missing groups, whose fits are the twins; and a
  # model of the group, whose fit drops the rows of the missing group.
  exclude_pair("group", na_fit(d_gn), function(m) d_gn, twin = "group/na_dropped")
  exclude_pair("group_shuffled", na_fit(d_gn), function(m) d_gn[group_perm, ],
               twin = "group/na_dropped_shuffled", form = "shuffled")
  exclude_pair("group_renumbered", na_fit(d_gn), function(m) gn_complete,
               twin = "group/na_dropped_renumbered", form = "complete_renumbered")
  exclude_pair("group_factor", function(na_action) {
    stats::lm(y ~ x1 + group, data = d_g, na.action = na_action)
  }, function(m) d_g, twin = "group/na_factor", terms = "factor")
  # The id and the time of the panel tests missing at rows the fit dropped
  # only, which the tests do not read.
  d_it <- d_na
  d_it$id[3] <- NA
  d_it$time[7] <- NA
  exclude_pair("id_time", na_fit(d_it), function(m) d_it)
  # z, the variable of a second formula, missing at a row the fit dropped.
  exclude_pair("z", na_fit(d_zn), function(m) d_zn, twin = "variance/z_missing_dropped")
  # A balanced panel, the fit dropping every row of a unit, in fit order and
  # shuffled, and an unbalanced one, unit 1 at 5 of its 10 periods (N12).
  exclude_pair("unit", na_fit(d_unit), function(m) d_unit, twin = "panel/unit_lm_full")
  exclude_pair("unit_shuffled", na_fit(d_unit), function(m) d_unit[unit_perm, ],
               twin = "panel/unit_lm_shuffled", form = "shuffled")
  exclude_pair("half_unit", na_fit(d_half), function(m) d_half, twin = "panel/half_unit_full")

  # Classes of model. Those whose answer differs between the tests are run with
  # the data frame of the fit and in the other forms of data (class_forms()):
  # without `data`, with the same rows shuffled under their own row names, as a
  # shuffled tibble, which the guard refuses unless the fit is refused first,
  # and as the model frame (increment 4). `shuffled_defect` names the defects
  # of the shuffled cell alone; `base` is the cell of the data frame of the
  # fit, the reference of fitWLS(), which refits a zero weight and an edited
  # fit (context_cell_for_formula()), and `base_expect` its expectation.
  set.seed(3)
  class_perm <- sample(nrow(d))
  class_forms <- function(id, build, expect = "value", defect = character(), needs = character(),
                          fit = "ols", expect_null = expect, shuffled_defect = character(),
                          forms = c("null", "shuffled", "tibble_shuffled", "model_frame"),
                          expect_tibble = "guard") {
    for (form in forms) {
      local({
        form <- form
        cell_expect <- switch(form, null = expect_null, tibble_shuffled = expect_tibble, expect)
        # The rows shuffled under their own row names were refused by the
        # panel tests and the scatter diagnostic (B3, B4).
        defect <- if (identical(form, "shuffled")) {
          c(defect, shuffled_defect, "B3", "B4")
        } else {
          defect
        }
        built_form <- function() {
          built <- build()
          built$data <- switch(form,
            null = NULL,
            shuffled = built$data[class_perm, , drop = FALSE],
            tibble_shuffled = dplyr::as_tibble(built$data)[class_perm, ],
            model_frame = stats::model.frame(built$m)
          )
          built
        }
        add(context_cell(paste0("class/", id, "_", form), "class", built_form, fit = fit,
                         form = form,
                         needs = c(needs, if (identical(form, "tibble_shuffled")) "dplyr"),
                         expect = cell_expect, defect = defect,
                         reference = if (identical(cell_expect, "value")) paste0("class/", id),
                         base = paste0("class/", id), base_expect = expect))
      })
    }
  }
  mlm_fit <- function() list(m = stats::lm(cbind(y, ypos) ~ x1 + x2, data = d), data = d)
  add(context_cell("class/mlm", "class", mlm_fit,
                   expect = "needs a model with one response; `model` has 2", defect = "R4"))
  # A multivariate lm is refused before `data` is matched, on every form.
  class_forms("mlm", mlm_fit, expect = "needs a model with one response; `model` has 2",
              defect = "R4", expect_tibble = "needs a model with one response; `model` has 2")
  add(context_cell("class/rank_deficient", "class", function() {
    list(m = stats::lm(y ~ x1 + x2 + I(2 * x2), data = d), data = d)
  }, reference = "fit/ols/none/none/data_frame"))
  add(context_cell("class/rlm", "class", function() {
    list(m = MASS::rlm(y ~ x1 + x2, data = d), data = d)
  }))
  # A Gaussian mgcv::gam() is a glm by class, and is accepted as before 0.16.0;
  # without smooth terms it is the lm.
  gam_fit <- function() list(m = mgcv::gam(y ~ x1 + x2, data = d), data = d)
  add(context_cell("class/gam", "class", gam_fit, needs = "mgcv",
                   reference = "fit/ols/none/none/data_frame"))
  # The residuals of a gam carry no names, and the tests that read a column of
  # `data` matched `data` to them by position: given the rows of the fit in
  # another order under their own names, they returned a wrong value (N17).
  # The context matches them by the row names of the model frame.
  add(context_cell("class/gam_shuffled", "class", function() {
    built <- gam_fit()
    built$data <- built$data[class_perm, , drop = FALSE]
    built
  }, form = "shuffled", needs = "mgcv", reference = "class/gam", defect = c("N17", "B3", "B4")))
  # A gam with an offset given as an argument, which predict.gam() leaves out
  # of its predictions: the registered Szroeter test, which ordered by
  # predict() before 0.16.0, ordered such a fit by X b without the offset
  # ("gam_offset"); it orders by the fitted values with the offset, those of
  # the same gam with an offset() term.
  add(context_cell("class/gam_offset_term", "class", function() {
    list(m = mgcv::gam(y ~ x1 + x2 + offset(o), data = d), data = d)
  }, needs = "mgcv", reference = "fit/ols/term/none/data_frame"))
  gam_offset_fit <- function() list(m = mgcv::gam(y ~ x1 + x2, data = d, offset = o), data = d)
  add(context_cell("class/gam_offset", "class", gam_offset_fit, needs = "mgcv",
                   reference = "class/gam_offset_term", defect = "gam_offset"))
  # In the other forms of data (increment 4): its residuals carry no names
  # either, and the group tests matched it by position too (N17).
  class_forms("gam_offset", gam_offset_fit, needs = "mgcv", defect = "gam_offset",
              shuffled_defect = "N17")
  # A smooth term, usually penalised: the tests of increments 1 and 3 test the
  # fit as before, and those of increment 2 refuse it until increment 11
  # settles which tests take it ("gam"). Goldfeld-Quandt and the quantile
  # regression test, which refitted the formula, stopped on its smooth term
  # outside mgcv.
  gam_smooth_fit <- function() list(m = mgcv::gam(y ~ s(x1) + x2, data = d), data = d)
  add(context_cell("class/gam_smooth", "class", gam_smooth_fit, needs = "mgcv", defect = "gam"))
  class_forms("gam_smooth", gam_smooth_fit, needs = "mgcv", defect = "gam",
              shuffled_defect = "N17")
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
  # Its model frame cannot be built, so it has no model frame to give as
  # `data`; its shuffled tibble is the cell below.
  class_forms("model_false", model_false_fit, defect = "frame",
              expect_null = "cannot use the model frame of `model`",
              forms = c("null", "shuffled"))
  # The same fit given its data as a tibble in another order. Numbered data in
  # another order than the fit are refused (decision 3): the frame built from
  # them does not give the response of the fit, and the tests built on the
  # context refuse the fit. The tests of increment 3 returned a value before,
  # the wrong one for those that read a column of `data` ("frame").
  add(context_cell("class/model_false_tibble", "class", function() {
    built <- model_false_fit()
    built$data <- dplyr::as_tibble(built$data)[class_perm, ]
    built
  }, form = "tibble_shuffled", needs = "dplyr",
  expect = "cannot use the model frame of `model`", defect = "frame"))
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
  # Increment 6. The same fit given its data: the frame it rebuilds is stale,
  # whatever `data` holds, and every test built on the context refuses it. The
  # tests computed from the model alone read it or ignored it, and returned a
  # value, Harvey's test and the variance form test on the stale design (N6).
  # And a fit with model = FALSE whose data are kept as they were: its frame
  # is rebuilt, reproduces the fit, and is used.
  model_false_with <- function(changed) {
    env <- new.env()
    assign("kept", d, envir = env)
    m <- eval(quote(stats::lm(y ~ x1 + x2, data = kept, model = FALSE)), env)
    if (changed) assign("kept", d[order(d$x1), ], envir = env)
    m
  }
  add(context_cell("class/model_false_changed_data", "class", function() {
    list(m = model_false_with(TRUE), data = d)
  }, expect = "cannot use the model frame of `model`", defect = "N6"))
  add(context_cell("class/model_false_kept", "class", function() {
    list(m = model_false_with(FALSE), data = d)
  }, reference = "fit/ols/none/none/data_frame"))
  add(context_cell("class/model_false_kept_null", "class", function() {
    list(m = model_false_with(FALSE), data = NULL)
  }, form = "null", reference = "fit/ols/none/none/null"))
  # A fit whose coefficient was changed after the fit (M8): refused by every
  # test built on the context once `data` is matched. The group tests returned
  # a value before increment 4, computed from the residuals the edit leaves
  # unchanged ("M8").
  edited_fit <- function() {
    m <- stats::lm(y ~ x1 + x2, data = d)
    m$coefficients[["x1"]] <- m$coefficients[["x1"]] + 0.5
    list(m = m, data = d)
  }
  edited <- "does not reproduce its residuals from its design and coefficients"
  add(context_cell("class/edited", "class", edited_fit, expect = edited, defect = "M8"))
  class_forms("edited", edited_fit, expect = edited, defect = "M8")
  # N12: the panels Pesaran's test finds unbalanced, which it stopped on or
  # computed with recycled residuals, refused in increment 5, and computes
  # from the periods each pair of units shares since the pull request of N12.
  cells <- lapply(cells, function(cell) {
    if (context_unbalanced(cell)) cell$defect <- unique(c(cell$defect, "N12"))
    cell
  })
  names(cells) <- vapply(cells, `[[`, character(1), "id")
  cells
}

# Harrison-McCabe's statistic and its lower tail written out with the rank of
# the fit, independently of the package (N4; decided 2026-10-18), from the
# decomposition the fit itself made: the Pearson residuals of the fit, which
# are those the test reads; the residual-maker of the first `rank` columns of
# the orthonormal basis of the fit's own QR decomposition (`m$qr`, rows in the
# order of the fit, of the weighted design, at the fit's tolerance), so that
# the null distribution and the residuals have the same rank; both put in the
# order of `order_by`; the eigenvalues of the quadratic form of the statistic
# on the full n x n matrices; and Imhof's formula integrated over u, as in
# test-harrison-mccabe.R. The package decomposes the design again, in the
# order of `order_by`. Before 0.16.0 it did so at 1e-7, which on a Gaussian
# glm (1e-11) or an lm fitted with a smaller `tol` can give a smaller rank
# than the fit's.
context_hmc_by_hand <- function(m, d, order_by = "x1", point = 0.5) {
  e <- stats::residuals(m, type = "pearson")
  ord <- order(d[[order_by]][match(names(e), rownames(d))])
  e <- unname(e)[ord]
  n <- length(e)
  first <- floor(point * n)
  basis <- qr.Q(m$qr)[, seq_len(m$rank), drop = FALSE]
  resid_maker <- (diag(n) - tcrossprod(basis))[ord, ord]
  statistic <- sum(e[seq_len(first)]^2) / sum(e^2)
  selector <- diag(c(rep(1, first), rep(0, n - first)))
  form <- resid_maker %*% (selector - statistic * diag(n)) %*% resid_maker
  lambda <- eigen((form + t(form)) / 2, symmetric = TRUE, only.values = TRUE)$values
  lambda <- lambda[abs(lambda) > 1e-12 * max(abs(lambda))]
  lambda <- lambda / sqrt(sum(lambda^2))
  integrand <- function(u) {
    vapply(u, function(v) {
      sin(0.5 * sum(atan(lambda * v))) / (v * prod((1 + lambda^2 * v^2)^0.25))
    }, numeric(1))
  }
  lower <- 0.5 - stats::integrate(integrand, 0, Inf, rel.tol = 1e-12, abs.tol = 1e-15,
                                  subdivisions = 5000L)$value / pi
  list(statistic = c(HMC = statistic), parameter = c(m = first, n = n), p.value = lower)
}

# The cells of Harrison-McCabe's rank (N4): a design with a column that is
# nearly a combination of the others, `x3 = x1 + 1e-9 z` (seed 5), fitted by
# a Gaussian glm, unweighted and weighted, which decide their rank at 1e-11,
# by an lm with `tol = 1e-11`, and by an lm at its default 1e-7, which aliases
# x3; and the well-conditioned Gaussian glm, which keeps its value.
context_rank_cells <- function() {
  d <- context_data()
  set.seed(5)
  d$x3 <- d$x1 + 1e-9 * stats::rnorm(nrow(d))
  f <- y ~ x1 + x2 + x3
  fits <- list(
    glm = function() stats::glm(f, data = d),
    glm_weighted = function() stats::glm(f, data = d, weights = w),
    lm_tol = function() stats::lm(f, data = d, tol = 1e-11),
    lm = function() stats::lm(f, data = d),
    glm_conditioned = function() stats::glm(y ~ x1 + x2, data = d)
  )
  lapply(stats::setNames(names(fits), paste0("rank/", names(fits))), function(id) {
    fit <- fits[[id]]
    list(id = paste0("rank/", id), form = "data_frame", defect = "N4",
         build = function() list(m = fit(), data = d))
  })
}

# The cells whose reference is another fit of the same model, which equals
# them up to rounding: the rank-deficient fit and the gam fits, against the lm.
context_rounded_cells <- c(
  "class/rank_deficient", "class/gam", "class/gam_offset_term", "class/gam_offset"
)

# Increment 10, B11 (decision 10): a grouped run, a grouped tibble given to
# runHeteroTests(), refits the model within each group by evaluating its call
# on the rows of the group. The call of a fitWLS() fit is that of the model it
# refitted, without the weights it estimated, so each group was tested as the
# unweighted fit; such a run is refused. The fits of the grouped matrix: an
# lm, a weighted lm, whose weights are a column the refit of each group reads,
# the fitWLS() fit of the lm, and the model as a formula; the groupings: one
# column, `group` (four groups of 30), and two, `group` and `half` (eight of
# 15).
context_grouped_data <- function() {
  d <- context_data()
  d$half <- rep(1:2, each = nrow(d) / 2)
  d
}
context_grouped_fits <- list(
  lm = function(d) stats::lm(y ~ x1 + x2, data = d),
  wls = function(d) stats::lm(y ~ x1 + x2, data = d, weights = w),
  fitwls = function(d) fitWLS(stats::lm(y ~ x1 + x2, data = d), d),
  formula = function(d) y ~ x1 + x2
)
context_groupings <- list(one = "group", two = c("group", "half"))
context_grouped <- function(d, grouping) {
  dplyr::group_by(dplyr::as_tibble(d), dplyr::across(dplyr::all_of(context_groupings[[grouping]])))
}

# Increment 10, N5: the runners refused the data a tidymodels workflow was
# fitted to, whose engine names its response `..y` and its predictors as the
# workflow's preprocessing made them, and the data of a parsnip fit made with
# fit_xy(). They map them to the variables of the engine, by the workflow's
# preprocessing, and test its model frame once the rows are confirmed, as
# with `data = NULL`. The models of the matrix: workflows with a formula, with
# transformed predictors and a factor, with a transformed outcome (a recipe
# with step_log() of it, and a formula of log(y), decision 35), with a recipe
# that transforms a predictor, fitted to data with missing values, with
# variables, with case weights, and with a recipe that drops rows, by
# step_naomit() and step_filter(), each skipped on new data (the default) or
# not (from the review of increment 10); with a recipe that reorders the rows,
# step_arrange() by a predictor and by another in descending order, which it
# applies to new data; with a step skipped on new data that transformed the
# outcome, step_log(ypos, skip = TRUE), the idiom recipes recommends; and
# with case weights and a step_filter() applied to new data (from the second
# review of increment 10); with a recipe written in a function, one of its
# steps reading a variable of the function's frame, and a parsnip fit of
# fit_xy() with case weights (from the third review); and parsnip fits of fit() (whose
# data the runners took already) and of fit_xy(), with the outcome as a data
# frame and as a vector (refused: the fit does not record its column).
# `outcome` names the outcome of the data. The forms of data: none, the data
# of the fit, as a tibble, shuffled under their own row names (refused for a
# workflow: the engine's fit numbers its rows), and without the outcome
# (refused).
context_tidy_models <- function() {
  d <- context_data()
  d_na <- d
  d_na$x2[c(3, 7, 11)] <- NA
  d_cw <- d
  d_cw$cw <- hardhat::importance_weights(d$w)
  spec <- function() parsnip::linear_reg()
  wf <- function(pre, data) workflows::fit(workflows::add_model(pre, spec()), data = data)
  with_recipe <- function(rec, data) wf(workflows::add_recipe(workflows::workflow(), rec), data)
  list(
    wf_formula = list(data = d, outcome = "y", fit = function(data) {
      wf(workflows::add_formula(workflows::workflow(), y ~ x1 + x2), data)
    }),
    wf_terms = list(data = d, outcome = "y", fit = function(data) {
      wf(workflows::add_formula(workflows::workflow(), y ~ log(x1) + group), data)
    }),
    wf_recipe = list(data = d, outcome = "y", fit = function(data) {
      with_recipe(recipes::step_log(recipes::recipe(y ~ x1 + x2, data = data), x1), data)
    }),
    wf_recipe_outcome = list(data = d, outcome = "ypos", fit = function(data) {
      with_recipe(recipes::step_log(recipes::recipe(ypos ~ x1 + x2, data = data), ypos), data)
    }),
    wf_formula_outcome = list(data = d, outcome = "ypos", fit = function(data) {
      wf(workflows::add_formula(workflows::workflow(), log(ypos) ~ x1 + x2), data)
    }),
    wf_missing = list(data = d_na, outcome = "y", fit = function(data) {
      wf(workflows::add_formula(workflows::workflow(), y ~ x1 + x2), data)
    }),
    wf_variables = list(data = d, outcome = "y", fit = function(data) {
      wf(workflows::add_variables(workflows::workflow(), outcomes = y, predictors = c(x1, x2)),
         data)
    }),
    wf_case_weights = list(data = d_cw, outcome = "y", fit = function(data) {
      pre <- workflows::add_case_weights(
        workflows::add_formula(workflows::workflow(), y ~ x1 + x2), cw
      )
      wf(pre, data)
    }),
    wf_naomit = list(data = d_na, outcome = "y", fit = function(data) {
      rec <- recipes::step_naomit(recipes::recipe(y ~ x1 + x2, data = data),
                                  recipes::all_predictors())
      with_recipe(rec, data)
    }),
    wf_naomit_applied = list(data = d_na, outcome = "y", fit = function(data) {
      rec <- recipes::step_naomit(recipes::recipe(y ~ x1 + x2, data = data),
                                  recipes::all_predictors(), skip = FALSE)
      with_recipe(rec, data)
    }),
    wf_filter = list(data = d, outcome = "y", fit = function(data) {
      with_recipe(recipes::step_filter(recipes::recipe(y ~ x1 + x2, data = data), x1 > 0.7), data)
    }),
    wf_filter_applied = list(data = d, outcome = "y", fit = function(data) {
      rec <- recipes::step_filter(recipes::recipe(y ~ x1 + x2, data = data), x1 > 0.7,
                                  skip = FALSE)
      with_recipe(rec, data)
    }),
    wf_arrange = list(data = d, outcome = "y", fit = function(data) {
      with_recipe(recipes::step_arrange(recipes::recipe(y ~ x1 + x2, data = data), x1), data)
    }),
    wf_arrange_desc = list(data = d, outcome = "y", fit = function(data) {
      rec <- recipes::step_arrange(recipes::recipe(y ~ x1 + x2, data = data), dplyr::desc(x2))
      with_recipe(rec, data)
    }),
    wf_outcome_skipped = list(data = d, outcome = "ypos", fit = function(data) {
      rec <- recipes::step_log(recipes::recipe(ypos ~ x1 + x2, data = data), ypos, skip = TRUE)
      with_recipe(rec, data)
    }),
    wf_filter_case_weights = list(data = d_cw, outcome = "y", fit = function(data) {
      rec <- recipes::step_filter(recipes::recipe(y ~ x1 + x2 + cw, data = data), x1 > 0.7,
                                  skip = FALSE)
      pre <- workflows::add_case_weights(workflows::add_recipe(workflows::workflow(), rec), cw)
      wf(pre, data)
    }),
    # A recipe written in a function whose frame holds other objects, a step
    # reading a variable of it (the third review of increment 10).
    wf_local_variable = list(data = d, outcome = "y", fit = function(data) {
      k <- 3
      unused <- seq_len(1e5) + 0.5
      rec <- recipes::step_mutate(recipes::recipe(y ~ x1 + x2, data = data), x3 = x1 * k)
      with_recipe(rec, data)
    }),
    parsnip_formula = list(data = d, outcome = "y", fit = function(data) {
      parsnip::fit(spec(), y ~ x1 + x2, data = data)
    }),
    parsnip_xy = list(data = d, outcome = "y", fit = function(data) {
      parsnip::fit_xy(spec(), x = data[, c("x1", "x2")], y = data[, "y", drop = FALSE])
    }),
    parsnip_xy_vector = list(data = d, outcome = "y", fit = function(data) {
      parsnip::fit_xy(spec(), x = data[, c("x1", "x2")], y = data$y)
    }),
    parsnip_xy_weights = list(data = d, outcome = "y", fit = function(data) {
      parsnip::fit_xy(spec(), x = data[, c("x1", "x2")], y = data[, "y", drop = FALSE],
                      case_weights = hardhat::importance_weights(data$w))
    })
  )
}
context_tidy_forms <- c("null", "data_frame", "tibble", "shuffled", "lacking")
context_tidy_data <- function(model, form) {
  set.seed(3)
  switch(form,
    null = NULL,
    data_frame = model$data,
    tibble = dplyr::as_tibble(model$data),
    shuffled = model$data[sample(nrow(model$data)), ],
    lacking = model$data[, setdiff(names(model$data), model$outcome)],
    stop("unknown form: ", form)
  )
}
# What each model of the tidy matrix gives on a form of data with `data`: the
# value of the call without `data`, or a refusal: "guard" (the class of the
# guard), or a regular expression of the message.
context_tidy_expect <- function(name, form) {
  if (identical(form, "null")) {
    return("value")
  }
  if (identical(name, "parsnip_xy_vector")) {
    return("the fit does not record the column of its outcome")
  }
  if (identical(form, "lacking")) {
    return(if (identical(name, "parsnip_formula")) {
      "missing from input data"
    } else if (startsWith(name, "parsnip")) {
      "`data` lacks the outcome `y` of the fit too"
    } else {
      "the workflow's preprocessing cannot map `data` to them"
    })
  }
  # A step that drops rows at the fit only numbers the rows it kept afresh;
  # those of step_naomit() are found again, the rows of `data` complete in the
  # variables of the engine, and those of step_filter() are not.
  if (identical(name, "wf_filter")) {
    return("dropped rows at the fit only")
  }
  # fit_xy() takes its case weights as a vector and does not record their
  # column (the third review of increment 10).
  if (identical(name, "parsnip_xy_weights")) {
    return("cannot compare the case weights of a parsnip fit made with fit_xy()")
  }
  # A recipe that sorts the rows of new data sorts them as it sorted the data
  # it was fitted to, whatever their order: the rows are those of the fit.
  if (name %in% c("wf_arrange", "wf_arrange_desc")) {
    return("value")
  }
  if (identical(form, "shuffled") && !identical(name, "parsnip_formula") &&
        !identical(name, "parsnip_xy")) {
    return("guard")
  }
  # The outcome a skipped step transformed at the fit (from the second review
  # of increment 10, where the refusal advised to pass these very data).
  if (identical(name, "wf_outcome_skipped")) {
    return("the workflow's preprocessing has a step it skips on new data, step_log()")
  }
  "value"
}

# What augment() of the result of the runner without `data` gives on a form
# of data: what the runner gives, but for a preprocessing that does not say
# which row of `data` each observation comes from, one that drops rows of new
# data or reorders them, and for the data of a parsnip fit of fit() without
# its outcome, whose refusal names augment().
context_tidy_augment_expect <- function(name, form) {
  expect <- context_tidy_expect(name, form)
  if (identical(expect, "value") && !identical(form, "null")) {
    if (name %in% c("wf_naomit_applied", "wf_filter_applied", "wf_filter_case_weights")) {
      return("augment() cannot tell which rows of `data` hold the observations")
    }
    if (name %in% c("wf_arrange", "wf_arrange_desc")) {
      return("reorders the rows of `data` it maps")
    }
  }
  if (identical(name, "parsnip_formula") && identical(form, "lacking")) {
    return("augment() matches the rows of `data` to the observations of the fit")
  }
  expect
}

# Does augment() of a tidymodels object bind each row of `data` its own
# values? For each row of `bound` at the positions `at`, the row of `data` it
# is named after, alone: for a workflow, mapped by its preprocessing
# (hardhat::forge() of that row) and predicted by its engine; for a parsnip
# fit, predicted by the fit. Its .fitted must be that prediction, and its
# .resid its outcome, as the preprocessing makes it, less that prediction, to
# 1e-8 (from the second review of increment 10: the checks compared the
# values bound with those of the model frame in their order, and a recipe that
# reorders the rows passed them). The prediction of a workflow itself
# (predict.workflow()) maps the predictors without the outcome, which a
# recipe that transforms the outcome on new data cannot bake. A row whose
# prediction or outcome cannot be computed, as in data without the outcome,
# is not its own: FALSE, so that the identity harness records the value
# augment() gave, and not the error of this check.
context_tidy_own <- function(object, data, bound, at = seq_len(nrow(bound))) {
  plain <- as.data.frame(data)
  index <- match(rownames(bound), rownames(plain))
  if (anyNA(index[at])) {
    return(FALSE)
  }
  workflow <- inherits(object, "workflow")
  engine <- if (workflow) workflows::extract_fit_engine(object) else object$fit
  blueprint <- if (workflow) workflows::extract_mold(object)$blueprint
  close <- function(a, b) length(a) == 1L && length(b) == 1L && isTRUE(all.equal(
    unname(as.numeric(a)), unname(as.numeric(b)), tolerance = 1e-8
  ))
  all(vapply(at, function(k) tryCatch({
    row <- data[index[k], , drop = FALSE]
    if (workflow) {
      forged <- hardhat::forge(row, blueprint, outcomes = TRUE)
      predictors <- as.data.frame(forged$predictors)
      predictors[["(Intercept)"]] <- NULL
      prediction <- stats::predict(engine, newdata = predictors)
      outcome <- forged$outcomes[[1L]]
    } else {
      prediction <- stats::predict(object, new_data = row)$.pred
      outcome <- if (is.character(object$preproc$y_var) && length(object$preproc$y_var) == 1L) {
        row[[object$preproc$y_var]]
      } else {
        stats::model.response(stats::model.frame(stats::formula(engine), as.data.frame(row)))
      }
    }
    close(bound$.fitted[k], prediction) && close(bound$.resid[k], outcome - prediction)
  }, error = function(e) FALSE), logical(1)))
}
# A few rows of `n`, for the tests: the first three, the middle and the last
# two.
context_tidy_rows <- function(n) unique(pmin(c(1L, 2L, 3L, n %/% 2L, n - 1L, n), n))
# Does `x` hold a fitted model or a fitted workflow anywhere in its lists?
# The record the runners keep for augment() (rtidymodels_map()) holds the
# blueprint of the workflow and no fit (decision 36, the fourth review of
# increment 10).
context_holds_fit <- function(x, depth = 0L) {
  if (inherits(x, c("workflow", "model_fit", "lm", "glm"))) {
    return(TRUE)
  }
  if (depth > 12L || !is.list(x) || is.data.frame(x)) {
    return(FALSE)
  }
  any(vapply(x, context_holds_fit, logical(1), depth = depth + 1L))
}

# The cells run on CRAN: no package beyond the hard dependencies, and a few of
# each block.
context_cran_cells <- c(
  "fit/ols/none/none/data_frame", "fit/ols/none/none/null", "fit/ols/none/none/shuffled",
  "fit/ols/none/none/model_frame", "fit/wls/none/none/null", "fit/ols/argument/none/null",
  "terms/ols/none/log/null", "missing/complete_renumbered", "missing/median_full",
  "subset/data_frame", "ident/front", "class/mlm", "group/na_shuffled",
  "panel/unit_lm_shuffled", "panel/repeated_id_time"
)

# Whether the full matrix runs, or only the cells CRAN runs. The full matrix
# is about 30,000 expectations, the largest part of the suite's time, so CI
# runs it in one job rather than in every job and again under covr. The
# environment variable HETEROTESTS_FULL_MATRIX decides:
#
#   "true"   the full matrix;
#   "false"  the CRAN cells only (context_cran_cells);
#   unset    the full matrix off CRAN, that is with NOT_CRAN=true, which
#            devtools::test(), testthat::test_local() and the CI jobs set, and
#            the CRAN cells on CRAN.
#
# A developer running the tests therefore gets the full matrix unless they set
# it to false. CI sets it to false in every job except ubuntu-latest (R
# release) in R-CMD-check.yml, where it is true (docs/development.md). The two
# tests of test-cache-contract.R that run every resampling test at its default
# replications, the next largest cost, are gated the same way.
context_full_matrix <- function() {
  value <- tolower(Sys.getenv("HETEROTESTS_FULL_MATRIX"))
  if (identical(value, "")) {
    return(identical(Sys.getenv("NOT_CRAN"), "true"))
  }
  if (!value %in% c("true", "false")) {
    stop("HETEROTESTS_FULL_MATRIX must be \"true\" or \"false\", or unset; it is \"",
         Sys.getenv("HETEROTESTS_FULL_MATRIX"), "\".", call. = FALSE)
  }
  identical(value, "true")
}

# Skips a test of the full matrix when context_full_matrix() is FALSE.
skip_unless_full_matrix <- function() {
  if (!context_full_matrix()) {
    testthat::skip("the full matrix is off (HETEROTESTS_FULL_MATRIX, helper-model-context.R)")
  }
  invisible(TRUE)
}

# The tests of increment 2.
context_increment2 <- c(
  "performStudentizedBPTest", "performRESETTest", "performGQTest",
  "performQuantileRegressionTest"
)

# The tests of increment 3.
context_increment3 <- c(
  "performGlejserTest", "performParkTest", "performSzroeterTest",
  "performHarrisonMcCabeTest", "performHartleyFmaxTest", "performSpatialHeteroTest"
)

# The tests of increment 4, the five group tests.
context_increment4 <- c(
  "performLeveneTest", "performBrownForsytheTest", "performFlignerKilleenTest",
  "performBartlettTest", "performOBrienTest"
)

# The variable of `data` each test reads besides the model, in its recipe
# (helper-test-recipes.R): the variable it orders by, regresses on or groups
# by.
context_reads <- c(
  performGQTest = "x1", performGlejserTest = "x1", performParkTest = "x1",
  performSzroeterTest = "x1", performHarrisonMcCabeTest = "x1",
  performHartleyFmaxTest = "group", performLeveneTest = "group",
  performBrownForsytheTest = "group", performFlignerKilleenTest = "group",
  performBartlettTest = "group", performOBrienTest = "group"
)

# The migrated tests that group by `group`, which the model frame of
# `y ~ x1 + x2` does not hold.
context_group_readers <- function() {
  names(context_reads)[context_reads == "group"]
}

# The functions of increment 5: the two panel tests, the scatter diagnostic
# and runPanelTests(), which runs the two panel tests.
context_increment5 <- c(
  "performPesaranTest", "performBPRandomEffectsTest", "performScatterDiagnostic",
  "runPanelTests"
)

# Increment 7: the three streaming tests, which read the stored design chunk
# by chunk. Their recipes of helper-test-recipes.R read chunks of 10,000
# observations, one chunk on the cells of the matrix, and join
# `context_migrated`; those of context_streaming_recipes() read chunks of 7
# observations, 18 chunks on 120, and are run by a test of their own
# (test-model-context.R). Up to 0.16.0 each chunk built its own design from
# the formula, on its rows of `data` (B6).
context_streaming <- c(
  "performWhiteTestStreaming", "performBPTestStreaming", "performKoenkerTestStreaming"
)
context_migrated <- c(context_migrated, context_streaming)
# Increment 8: the resampling tests, the White bootstrap, whose observed
# statistic and replicates are computed on the stored design, and the wild
# bootstrap, whose replicates already were, with `B = 19`; and the robust White
# and Breusch-Pagan tests, which prepared `data` themselves until then. Their
# replicates are drawn in the order of the fit, so their value on every form of
# data they accept is that of the data frame of the fit. bootstrapTestStatistic()
# draws them in the order of `data`, and has tests of its own
# (test-model-context.R).
context_resampling <- c(
  "performWhiteTestBootstrap", "performWildBootstrapTest", "performWhiteTestRobust",
  "performBPTestRobust"
)
context_migrated <- c(context_migrated, context_resampling)
context_streaming_recipes <- function() {
  list(
    `performWhiteTestStreaming(chunk_size = 7)` = function(m, d) {
      performWhiteTestStreaming(m, d, chunk_size = 7, progress = FALSE)
    },
    `performBPTestStreaming(chunk_size = 7)` = function(m, d) {
      performBPTestStreaming(m, d, chunk_size = 7, progress = FALSE)
    },
    `performKoenkerTestStreaming(chunk_size = 7)` = function(m, d) {
      performKoenkerTestStreaming(m, d, chunk_size = 7, progress = FALSE)
    }
  )
}
context_streaming_chunked <- names(context_streaming_recipes())

# The terms whose squares, or whose products with each other, are collinear
# with the terms of White's auxiliary regression: the square of a dummy is the
# dummy, that of the first column of an orthogonal poly() basis a combination
# of the intercept and the basis, and that of x1 is I(x1^2). Up to 0.16.0
# White's streaming test stopped on them ("Auxiliary regression became rank
# deficient"), where performWhiteTest() drops the collinear columns; since the
# pull request of N3 (0.17.0, decision 22) it drops them too, and on these
# cells gives the value of the reference of the cell, as on the others, and
# that of performWhiteTest() (test-model-context.R).
context_white_streaming_collinear <- c("poly", "square", "median", "cut", "factor", "log_factor")

# The cell as the streaming tests see it. They read no column of `data`
# beyond the model frame, so the generic rules of the cell apply, White's
# test on the collinear terms of its auxiliary regression among them (N3).
context_cell_for_streaming <- function(fn, cell) {
  cell
}

# The columns the panel tests read besides the model, `id` and `time`, which
# no model of the matrix uses, so that without `data` they are refused (M1).
# runPanelTests() and the scatter diagnostic need `data`.
context_panel_reads <- c(
  performPesaranTest = "id, time", performBPRandomEffectsTest = "id", runPanelTests = "id"
)

# The functions that need `data`, or a column the model frame does not hold:
# without `data` they are refused, whatever the cell.
context_data_readers <- function() {
  c(context_group_readers(), context_increment5)
}

# The variables of the recipe of the scatter diagnostic, x1 and x2, that the
# model frame of each model of the term block holds under their own names: it
# is refused as `data` by the others ("Variables not found").
context_frame_variables <- list(
  none = c("x1", "x2"), log = "x2", poly = "x2", ns = "x2", square = c("x1", "x2"),
  median = "x2", cut = "x2", factor = "x1", log_factor = character()
)

# The cells whose panel is unbalanced, a fit that dropped rows of some units
# and kept others, or a fit to such rows: Pesaran's test arranged its
# residuals in a matrix of periods by units, and before increment 5 stopped
# there ("number of items to replace is not a multiple of replacement
# length"), or recycled the residuals of a unit (N12). Increment 5 refused
# such a panel ("needs a balanced panel", decided 2026-10-24); since the pull
# request of N12 the test computes its statistic from the periods each pair of
# units shares, as plm::pcdtest() does, and the identity harness checks each
# of these cells against plm (scripts/model-context-identity.R).
context_unbalanced <- function(cell) {
  missing_rows <- identical(cell$block, "missing") &&
    !cell$id %in% c("missing/null", "missing/median_null", "missing/na_exclude")
  missing_rows || identical(cell$block, "terms") && identical(cell$terms, "cut") ||
    cell$id %in% c("group/na_dropped", "group/na_dropped_shuffled", "group/na_dropped_renumbered",
                   "group/na_factor", "group/na_factor_complete", "panel/half_unit",
                   "panel/half_unit_full", "variance/z_missing_dropped")
}

# The reference of the panel tests and the scatter diagnostic on a cell of a
# fit that dropped rows, whose data they refused before 0.16.0 (B16), where
# the cell has none or one they refused too: the fit to the complete rows.
context_dropped_references <- c(
  "missing/median_precomputed" = "missing/median_complete",
  "group/na_dropped" = "missing/complete_fit",
  "group/na_factor" = "group/na_factor_complete"
)

# The cells whose pairs of `id` and `time` repeat, which Pesaran's test
# refuses (R2, decision 13).
context_repeated_pairs <- c(
  "ident/repeated_id_time", "ident/repeated_fit", "panel/repeated_id_time",
  "panel/repeated_id_time_shuffled"
)

# The cell as the functions of increment 5 see it (context_cell_for()).
context_cell_for_panel <- function(fn, cell) {
  scatter <- identical(fn, "performScatterDiagnostic")
  runner <- identical(fn, "runPanelTests")
  pesaran <- fn %in% c("performPesaranTest", "runPanelTests")
  value_like <- function(expect) {
    expect %in% c("value", "zero or non-finite weights", "guard") ||
      grepl("does not reproduce its residuals", expect, fixed = TRUE)
  }
  no_data <- identical(cell$form, "null") || identical(cell$id, "missing/median_null")
  if (no_data) {
    # The scatter diagnostic and the runner need a data frame, which they
    # check first; the panel tests read `id` and `time` from the model frame,
    # which does not hold them (M1), after the refusals of the context and
    # the check of an edited fit.
    if (scatter) {
      cell$expect <- "`data` must be a data frame."
    } else if (runner) {
      cell$expect <- "Input data must be a data.frame"
    } else if (identical(cell$expect, "value") ||
                 identical(cell$expect, "zero or non-finite weights")) {
      cell$expect <- sprintf(
        "The following variables are missing from the model frame of `model`: %s.",
        context_panel_reads[[fn]]
      )
    }
    cell$reference <- NULL
    return(cell)
  }
  if (identical(cell$form, "model_frame")) {
    # The model frame does not hold `id` or `time`, which the runner checks
    # first and the panel tests after the refusals of the context; the scatter
    # diagnostic reads x1 and x2, which it holds unless the model transforms
    # them.
    if (scatter) {
      absent <- setdiff(c("x1", "x2"), context_frame_variables[[cell$terms]])
      if (length(absent) > 0L) {
        cell$expect <- paste("Variables not found:", paste(absent, collapse = ", "))
        cell$reference <- NULL
      }
    } else if (runner || value_like(cell$expect)) {
      cell$expect <- if (identical(fn, "performPesaranTest")) {
        "`id` and `time` must be columns in `data`."
      } else {
        "`id` must be a column in `data`."
      }
      cell$reference <- NULL
    }
    return(cell)
  }
  # The scatter diagnostic refuses a zero weight before it matches `data`, as
  # before 0.16.0.
  if (scatter && startsWith(cell$id, "class/zero_weight") && identical(cell$expect, "guard")) {
    cell$expect <- "zero or non-finite weights"
    return(cell)
  }
  # `id` and `time` are not columns of the model frame: the tibble of a fit to
  # repeated rows, whose rows tie, is refused (the guard). The scatter
  # diagnostic reads x1 and x2, columns of the frame, and takes it.
  if (!scatter && identical(cell$id, "ident/repeated_fit_tibble")) {
    cell$expect <- "guard"
    cell$reference <- NULL
    return(cell)
  }
  if (!identical(cell$expect, "value")) {
    return(cell)
  }
  # An unbalanced panel (N12), which the fifth step refused, is tested by
  # Pesaran's test as by the random-effects test, against the same reference.
  if (pesaran && cell$id %in% context_repeated_pairs) {
    cell$expect <- "needs one observation of the fit for each pair of `id` and `time`"
    cell$reference <- NULL
  } else if (cell$id %in% names(context_dropped_references)) {
    cell$reference <- context_dropped_references[[cell$id]]
  }
  cell
}

# Fligner-Killeen's statistic ranks the absolute deviations of the residuals
# from their group medians. In a group of even size the two residuals in the
# middle lie at the same distance from the median, and before 0.16.0 whether
# rounding left that tie exact differed from one fit of the model to another,
# such as the lm and the gam or the Gaussian glm: on the data of the matrix,
# two of the four ties were exact for the lm and all four for the gam. The
# statistic then moved by up to 2.6e-6 of its value and the p-value by up to
# 1.1e-5 (measured on Windows, the gam against the lm), and which ties were
# exact depended on the platform too: the Gaussian glm with an offset and the
# glm of y - o, equal on Windows to 1e-10, differed on CI by 1.5e-7 of the
# statistic on Linux (1.98307876 against 1.98307847) and by 1.9e-6 on macOS
# (1.9830772 against 1.9830809); this tolerance was 1e-4. Since 0.16.0 (N15)
# the residuals are computed observation by observation and deviations that
# only rounding tells apart are ranked as tied (rfligner_residuals() and
# rfligner_rounding_ties(), R/performFlignerKilleenTest.R), so the fits of
# the same model give the same ranks, and the statistic, which depends on the
# ranks alone, is the same: identical() on the matrix, measured on Windows.
# Where its reference is another fit of the same model, its value is compared
# to the tolerance of the other tests.
context_fligner_tolerance <- 1e-10

# The functions of `context_migrated` that can run here: the quantile
# regression test needs quantreg, and the spatial test spdep, suggested
# packages.
context_runnable <- function(fns = context_migrated) {
  if (!requireNamespace("quantreg", quietly = TRUE)) {
    fns <- setdiff(fns, "performQuantileRegressionTest")
  }
  if (!requireNamespace("spdep", quietly = TRUE)) {
    fns <- setdiff(fns, "performSpatialHeteroTest")
  }
  fns
}

# The cell as one test sees it: where a migrated test departs from the
# expectation of a cell by design, its own expectation, and the reference of a
# value.
#
# - The tests of increment 2 refuse an mgcv::gam() fit with smooth
#   terms (context_increment2), once `data` is matched: a shuffled tibble is
#   refused by the guard first, by RESET too since it matches `data`
#   (decision 31).
# - The quantile regression test refuses a weighted fit, and a MASS::rlm()
#   fit, whose weights it reads, once `data` is aligned.
# - Goldfeld-Quandt, Glejser, Park, Szroeter and Harrison-McCabe read `x1`,
#   which the model frame of a fit that transforms `x1` does not hold, and
#   Hartley's test and the five group tests `group`, which only the models
#   with the factor hold: without `data` they are refused with the message of
#   the context (M1), also on a fit with a zero weight, which it refuses next,
#   and the model frame given as `data` lacks the variable, also on a fit with
#   a zero weight. Data in the order of the fit whose rows tie on every
#   compared column are refused (the guard) when the variable is not a column
#   of the model frame, as for the fit to repeated rows given as a tibble.
# - A group missing at observations of the fit: Hartley's test refuses them
#   (decision 8); the five group tests leave them out, with a warning, as
#   before.
# - RESET and the tests computed from the model alone read nothing from
#   `data`, which they match as the others do (decision 31), and keep their
#   own refusals (context_cell_for_model_alone()): RESET accepts a zero
#   weight.
# - The functions of increment 5 read `id`, `time` or the variables of the
#   scatter diagnostic beside the model (context_cell_for_panel()): without
#   `data` the panel tests are refused (M1) and the others need a data frame;
#   the model frame lacks `id` and `time`; Pesaran's test refuses a repeated
#   pair of `id` and `time` (R2), and since the pull request of N12 gives a
#   value on an unbalanced panel, as that of a fit that dropped rows of some
#   units, which the fifth step refused.
# - A fit with na.exclude gives what its twin gives (increment 9, B13).
context_cell_for <- function(fn, cell) {
  # A fit with na.exclude (increment 9, B13) gives what its twin, the same fit
  # with na.omit, gives, value or refusal (context_check_matrix()): its
  # reference is the twin, whatever the test.
  if (!is.null(cell$twin)) {
    return(cell)
  }
  # The streaming tests (increment 7), in chunks of 10,000 or of 7.
  if (fn %in% c(context_streaming, context_streaming_chunked)) {
    return(context_cell_for_streaming(fn, cell))
  }
  if (fn %in% context_increment5) {
    return(context_cell_for_panel(fn, cell))
  }
  if (fn %in% context_second_formula) {
    return(context_cell_for_formula(fn, cell))
  }
  # The wild bootstrap reads the leverages of the fit with hatvalues(), which
  # an mgcv::gam() fit, whose qr component is not that of an lm, does not
  # give: it stops on every gam, as before increment 8 (outside the model
  # context; ROADMAP.md).
  if (identical(fn, "performWildBootstrapTest") && identical(cell$expect, "value") &&
        grepl("^class/gam|^panel/unit_gam|^exclude/gam", cell$id)) {
    cell$expect <- "lm object does not have a proper 'qr' component"
    cell$reference <- NULL
    return(cell)
  }
  # The White bootstrap (increment 8) refits the design in each replicate, and
  # the robust Breusch-Pagan test studentizes by default, as the tests of
  # increment 2 do.
  refits_design <- c(context_increment2, "performWhiteTestBootstrap", "performBPTestRobust")
  if (fn %in% refits_design && grepl("^class/gam_smooth|^exclude/gam_smooth", cell$id) &&
        !identical(cell$expect, "guard")) {
    cell$expect <- "cannot test an mgcv::gam() fit with smooth terms (s(x1))"
    return(cell)
  }
  weighted <- cell$fit %in% c("wls", "wvec", "glm", "fitwls") || identical(cell$id, "class/rlm")
  if (identical(fn, "performQuantileRegressionTest") && weighted &&
        !identical(cell$expect, "guard")) {
    cell$expect <- "does not support weighted fits"
    cell$reference <- NULL
  }
  transformed <- cell$terms %in% c("log", "poly", "ns", "median", "cut", "log_factor")
  # The cells without `data`: the form "null", and the fit that dropped rows.
  no_data <- identical(cell$form, "null") || identical(cell$id, "missing/median_null")
  model_frame <- identical(cell$form, "model_frame")
  if (fn %in% names(context_reads)) {
    variable <- context_reads[[fn]]
    held <- if (identical(variable, "group")) {
      cell$terms %in% c("factor", "log_factor")
    } else {
      !transformed
    }
    first <- identical(cell$expect, "value") ||
      (no_data || model_frame) && identical(cell$expect, "zero or non-finite weights")
    if (!held && first && (no_data || model_frame)) {
      cell$expect <- sprintf(
        if (no_data) {
          "The following variables are missing from the model frame of `model`: %s."
        } else {
          "The following variables are missing from input data: %s."
        },
        variable
      )
      cell$reference <- NULL
    }
    if (!held && identical(cell$id, "ident/repeated_fit_tibble")) {
      cell$expect <- "guard"
      cell$reference <- NULL
    }
  }
  if (identical(fn, "performHartleyFmaxTest") && identical(cell$block, "group") &&
        !identical(cell$terms, "factor") && identical(cell$expect, "value")) {
    cell$expect <- "where group, the `group` variable, is missing."
    cell$reference <- NULL
  }
  if (fn %in% names(context_model_alone)) {
    return(context_cell_for_model_alone(fn, cell))
  }
  cell
}

# The cell as the calls of increment 6 that evaluate a second formula of z see
# it (context_second_formula), and fitWLS() without one.
#
# - fitWLS() without `var_formula` reads nothing from `data`, which it
#   matches as the tests do (decision 31; it did not check it before), and it
#   is not a test, so it neither checks that the fit reproduces itself nor
#   refuses a zero weight.
# - With `var_formula = ~ z` or `against = ~ z`, z is read at the rows the
#   alignment confirmed (B9): the forms of data the guard refuses are refused,
#   and so is the tibble of a fit to repeated rows, whose rows tie, z being
#   beyond the model frame; numbered data with one row per observation are
#   taken in the order of the fit. No model frame holds z: without `data` it is
#   refused (M1, R3), and with the model frame as `data` by the message of
#   `data`, after the refusals that come first in each: Cook-Weisberg's test
#   refuses a zero weight before it reads z, and the variance form test
#   weights that did not come from fitWLS(), and a variance formula on a
#   fitWLS() fit. A missing z at an observation of the fit is refused,
#   Cook-Weisberg's test naming it (decision 8).
context_cell_for_formula <- function(fn, cell) {
  wls <- startsWith(fn, "fitWLS")
  variance_form <- startsWith(fn, "performVarianceFormTest")
  edited <- grepl("does not reproduce its residuals", cell$expect, fixed = TRUE)
  zero_weight <- identical(cell$expect, "zero or non-finite weights")
  # fitWLS() refits a rank-deficient fit with its aliased coefficient: a value
  # of its own.
  if (wls && identical(cell$id, "class/rank_deficient")) {
    cell$expect <- "value"
    cell$reference <- NULL
    return(cell)
  }
  if (identical(fn, "fitWLS")) {
    if (zero_weight || edited) {
      cell$expect <- "value"
      cell$reference <- cell$base
    }
    return(cell)
  }
  if (wls && (zero_weight || edited)) {
    cell$expect <- "value"
    cell$reference <- if (identical(cell$form, "shuffled")) cell$base
  }
  weighted <- cell$fit %in% c("wls", "wvec", "glm") || identical(cell$id, "class/rlm") ||
    startsWith(cell$id, "class/zero_weight")
  if (variance_form && !identical(cell$expect, "guard")) {
    value_like <- cell$expect %in% c("value", "zero or non-finite weights")
    if (weighted && value_like) {
      cell$expect <- "was fitted with weights that did not come from fitWLS()"
      cell$reference <- NULL
      return(cell)
    }
    if (identical(fn, "performVarianceFormTest(var_formula = ~ z)") &&
          identical(cell$fit, "fitwls") && identical(cell$expect, "value")) {
      cell$expect <- "is a fitWLS() fit and carries the variance function to test"
      cell$reference <- NULL
      return(cell)
    }
  }
  no_data <- identical(cell$form, "null") || identical(cell$id, "missing/median_null")
  model_frame <- identical(cell$form, "model_frame")
  if ((no_data || model_frame) && identical(cell$expect, "value")) {
    cell$expect <- if (no_data) {
      "The following variables are missing from the model frame of `model`: z."
    } else {
      "The following variables are missing from input data: z."
    }
    cell$reference <- NULL
    return(cell)
  }
  if (identical(cell$id, "ident/repeated_fit_tibble")) {
    cell$expect <- "guard"
    cell$reference <- NULL
  }
  if (identical(cell$id, "variance/z_missing")) {
    cell$expect <- if (startsWith(fn, "performCookWeisbergTest")) {
      "where z, the `var_formula` variable, is missing."
    } else {
      "produced missing or non-finite values on the fitted rows."
    }
    cell$reference <- NULL
  }
  cell
}

# The cell as a test computed from the model alone sees it (increment 6), and
# RESET. They read nothing from `data`, which they match to the observations
# of the fit as every test built on the context does (decision 31): the guard
# refuses what it refuses, numbered data with one row per observation are
# taken in the order of the fit, and, since they read no column beyond the
# model frame, data in that order whose rows tie are taken too. Until decision
# 31 they kept the check of 0.15.0, the variables of the model and the row
# names of the fit, and gave the value of the fit on the data the guard
# refuses. Each keeps the refusals of its own, once `data` is matched: RESET
# accepts a zero weight, which the others refuse with the Pearson residuals,
# and refuses a gam with smooth terms (decision 23); the variance form test
# refuses weights that did not come from fitWLS(). Until increment 9 RESET
# stopped on the padded fitted values of a fit with na.exclude, which the
# spread-level and Davidian-Carroll tests took, and the others refused with
# the checks of the model; such a fit now gives what its twin gives.
context_cell_for_model_alone <- function(fn, cell) {
  if (identical(cell$expect, "guard")) {
    return(cell)
  }
  zero_weight <- startsWith(cell$id, "class/zero_weight")
  if (zero_weight && identical(fn, "performRESETTest")) {
    cell$expect <- "value"
    return(cell)
  }
  weighted <- cell$fit %in% c("wls", "wvec", "glm") || identical(cell$id, "class/rlm")
  if (identical(fn, "performVarianceFormTest") && weighted &&
        (identical(cell$expect, "value") || zero_weight)) {
    cell$expect <- "was fitted with weights that did not come from fitWLS()"
    cell$reference <- NULL
  }
  cell
}

# What a call gives on a fit with na.exclude, from what it gives on its twin,
# the same fit with na.omit (increment 9, B13): the same value, but for an
# output with one value per row, padded with NA at the rows the fit dropped, as
# naresid() pads residuals() and weights(). Of the calls of the matrix, the
# fitWLS() recipes return one, the vector of the coefficients and the weights
# of the fit (context_recipes()); the tests return none.
context_twin_value <- function(fn, twin_value, cell) {
  if (!startsWith(fn, "fitWLS") || !is.numeric(twin_value)) {
    return(twin_value)
  }
  m <- suppressWarnings(suppressMessages(cell$build()))$m
  n <- length(m$residuals)
  k <- length(twin_value) - n
  c(twin_value[seq_len(k)], unname(stats::naresid(m$na.action, twin_value[k + seq_len(n)])))
}

# A cell run through one test: the result, or the error as a condition.
context_run <- function(fn, cell, recipes = context_recipes()) {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  built <- cell$build()
  set.seed(7)
  tryCatch(
    suppressMessages(suppressWarnings(recipes[[fn]](built$m, built$data))),
    error = function(e) e
  )
}
