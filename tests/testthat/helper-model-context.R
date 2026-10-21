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
# regression test; increment 3: Glejser, Park, Szroeter, Harrison-McCabe,
# Hartley and the spatial test; increment 4: the five group tests, Levene,
# Brown-Forsythe, Fligner-Killeen, Bartlett and O'Brien.
context_migrated <- c(
  "performWhiteTest", "performBPTest", "performKoenkerTest",
  "performRankPermutationTest", "performHighDimensionalTest",
  "performStudentizedBPTest", "performRESETTest", "performGQTest",
  "performQuantileRegressionTest",
  "performGlejserTest", "performParkTest", "performSzroeterTest",
  "performHarrisonMcCabeTest", "performHartleyFmaxTest", "performSpatialHeteroTest",
  "performLeveneTest", "performBrownForsytheTest", "performFlignerKilleenTest",
  "performBartlettTest", "performOBrienTest"
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
                         base_expect = NULL) {
  list(
    id = id, block = block, build = build, fit = fit, offset = offset, terms = terms,
    form = form, needs = needs, reference = reference, expect = expect, defect = defect,
    base = base, base_expect = base_expect
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
  }, form = "shuffled", reference = "group/na")
  group_cell("na_tibble", function() {
    list(m = stats::lm(y ~ x1 + x2, data = d_g), data = dplyr::as_tibble(d_g))
  }, form = "tibble", needs = "dplyr", reference = "group/na")
  group_cell("na_tibble_shuffled", function() {
    list(m = stats::lm(y ~ x1 + x2, data = d_g), data = dplyr::as_tibble(d_g)[group_perm, ])
  }, form = "tibble_shuffled", needs = "dplyr", expect = "guard")
  group_cell("na_dropped", function() list(m = stats::lm(y ~ x1 + x2, data = d_gn), data = d_gn),
             form = "data_frame")
  group_cell("na_dropped_shuffled", function() {
    list(m = stats::lm(y ~ x1 + x2, data = d_gn), data = d_gn[group_perm, ])
  }, form = "shuffled", reference = "group/na_dropped")
  group_cell("na_dropped_renumbered", function() {
    list(m = stats::lm(y ~ x1 + x2, data = d_gn), data = gn_complete)
  }, form = "complete_renumbered", reference = "group/na_dropped", defect = "B14")
  group_cell("na_factor", function() {
    list(m = stats::lm(y ~ x1 + group, data = d_g), data = d_g)
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
  # the data frame of the fit and in the other forms of data (class_forms()):
  # without `data`, with the same rows shuffled under their own row names, as a
  # shuffled tibble, which the guard refuses unless the fit is refused first,
  # and as the model frame (increment 4). `shuffled_defect` names the defects
  # of the shuffled cell alone; `base` is the cell of the data frame of the
  # fit, and `base_expect` its expectation, for RESET, which reads nothing
  # from `data` (context_cell_for()).
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
        defect <- if (identical(form, "shuffled")) c(defect, shuffled_defect) else defect
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
  }, form = "shuffled", needs = "mgcv", reference = "class/gam", defect = "N17"))
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

# The cells run on CRAN: no package beyond the hard dependencies, and a few of
# each block.
context_cran_cells <- c(
  "fit/ols/none/none/data_frame", "fit/ols/none/none/null", "fit/ols/none/none/shuffled",
  "fit/ols/none/none/model_frame", "fit/wls/none/none/null", "fit/ols/argument/none/null",
  "terms/ols/none/log/null", "missing/complete_renumbered", "missing/median_full",
  "subset/data_frame", "ident/front", "class/mlm", "group/na_shuffled"
)

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

# Fligner-Killeen's statistic ranks the absolute deviations of the residuals
# from their group medians. In a group of even size the two residuals in the
# middle lie at the same distance from the median, and whether rounding leaves
# that tie exact differs from one fit of the model to another, such as the lm
# and the gam or the Gaussian glm (N15, a separate pull request): on the data
# of the matrix, two of the four ties are exact for the lm and all four for the
# gam. The statistic then moves by up to 2.6e-6 of its value and the p-value
# by up to 1.1e-5 (measured, the gam against the lm), and which ties are exact
# depends on the platform too: on Linux the Gaussian glm with an offset and
# the glm of y - o differ by 1.5e-7 of the statistic. Where its reference is
# another fit of the same model, its value is compared to this tolerance.
context_fligner_tolerance <- 1e-4

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
#   refused by the guard first, but by RESET, which matches no `data`.
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
# - RESET reads nothing from `data`. It builds its context without `data` and
#   keeps the check of `data` of the tests computed from the model alone
#   (rcheck_data_argument()), which looks for the row names of the fit and
#   compares no values, until increment 6 moves those tests. So it gives the
#   value of the fit on the forms of data the guard refuses, and on a class of
#   model what it gives on the data frame of the fit, refuses data whose row
#   names lack those of the fit, refuses a model = FALSE fit whose data are
#   gone, stops on the padded fitted values of na.exclude (increment 9), and
#   accepts a zero weight.
context_cell_for <- function(fn, cell) {
  if (fn %in% context_increment2 && startsWith(cell$id, "class/gam_smooth") &&
        (!identical(cell$expect, "guard") || identical(fn, "performRESETTest"))) {
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
  if (identical(fn, "performRESETTest")) {
    rows_lacking <- c(
      "missing/complete_renumbered", "ident/repeated_fit_tibble",
      "ident/character_names_tibble", "ident/repeated_names", "group/na_dropped_renumbered"
    )
    if (cell$id %in% rows_lacking) {
      cell$expect <- "RESET requires `data` to contain the rows used to fit the model."
      cell$reference <- NULL
    } else if (identical(cell$expect, "guard") && !is.null(cell$base)) {
      # A class of model given data the guard refuses: what RESET gives on the
      # data frame of the fit.
      base <- cell
      base$id <- cell$base
      base$form <- "data_frame"
      base$expect <- cell$base_expect
      base$base <- NULL
      seen <- context_cell_for(fn, base)
      cell$expect <- seen$expect
      cell$reference <- if (identical(seen$expect, "value")) cell$base
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
