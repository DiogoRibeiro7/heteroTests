library(testthat)
library(heteroTests)

test_that("White test works on homoscedastic data", {
  set.seed(1)
  n <- 200
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = 1)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performWhiteTest(model, df)
  expect_s3_class(res, "htest")
  expect_gt(res$p.value, 0.05)
})

test_that("White test detects heteroscedasticity", {
  set.seed(1)
  n <- 200
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = 1 + 5 * x)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performWhiteTest(model, df)
  expect_lt(res$p.value, 0.05)
})

test_that("Cross products can be disabled", {
  set.seed(1)
  n <- 100
  x1 <- runif(n)
  x2 <- runif(n)
  y <- 1 + 2 * x1 + 3 * x2 + rnorm(n)
  df <- data.frame(x1 = x1, x2 = x2, y = y)
  model <- lm(y ~ x1 + x2, data = df)
  res_cp <- performWhiteTest(model, df, cross_products = TRUE)
  res_no <- performWhiteTest(model, df, cross_products = FALSE)
  expect_true(res_cp$statistic >= res_no$statistic)
})

test_that("cross_products must be logical", {
  data(mtcars)
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  expect_error(performWhiteTest(m, mtcars, cross_products = "yes"),
               "cross_products")
})

test_that("performWhiteTest enforces sample size minimum", {
  set.seed(42)
  df <- data.frame(
    y = rnorm(18),
    x1 = rnorm(18),
    x2 = rnorm(18)
  )
  model <- lm(y ~ x1 + x2, data = df)
  expect_error(
    performWhiteTest(model, df),
    "requires at least 20"
  )
})

test_that("performWhiteTest warns about missing values and succeeds", {
  set.seed(123)
  n <- 40
  df <- data.frame(
    y = 1 + rnorm(n),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  df$x1[1] <- NA
  df$x2[5] <- NA
  model <- lm(y ~ x1 + x2, data = df)
  expect_warning(
    res <- performWhiteTest(model, df),
    "Removed 2 observations due to missing values"
  )
  expect_s3_class(res, "htest")
})

test_that("performWhiteTest drops collinear auxiliary terms instead of aborting", {
  # A 0/1 indicator squared equals itself, so its "_sq" auxiliary column is
  # perfectly collinear. The test should drop the redundant columns and report a
  # degrees-of-freedom value equal to the realised rank, rather than erroring.
  set.seed(321)
  n <- 40
  x1 <- rnorm(n)
  d <- factor(sample(c("a", "b", "c"), n, replace = TRUE))
  df <- data.frame(y = rnorm(n), x1 = x1, d = d)
  model <- lm(y ~ x1 + d, data = df)

  result <- performWhiteTest(model, df, cross_products = TRUE)
  expect_s3_class(result, "htest")
  # df must be the number of independent auxiliary regressors actually used
  expect_lt(result$parameter[["df"]], attr(result, "original_regressors")^2)
  expect_gt(result$parameter[["df"]], 0)
})

# --- Degrees of freedom ---------------------------------------------------------
#
# The auxiliary regression of e^2 on Z, the regressors, their squares and their
# cross-products, has an intercept, so the degrees of freedom of n R^2 are the
# rank of [1, Z] minus one (N2 of docs/design/model-context.md). In 0.15.0
# they were the rank of Z, one more when the columns of Z span the constant,
# with the same statistic. The designs are on the data of contract_data()
# (helper-test-recipes.R); `spans` says, with and without the cross products,
# whether the auxiliary columns span the constant.
white_df_designs <- function() {
  d <- contract_data()$d
  n <- nrow(d)
  d$half <- factor(ifelse(seq_len(n) %% 2 == 0, "even", "odd"))
  d$s <- ifelse(d$half == "even", 1, -1)
  d$h <- factor(rep(c("p", "q", "r"), each = n / 3))
  d$sub <- factor(rep(1:2, each = 4, length.out = n))
  d$v <- 1 / d$x1
  # Ordered factors, whose default contrasts are contr.poly.
  d$o4 <- factor(d$group, ordered = TRUE)
  d$o3 <- factor(d$h, ordered = TRUE)
  # No observation in the cell of the baseline levels, (even, p), or in (odd, r).
  empty <- d[!(d$half == "even" & d$h == "p"), ]
  other_empty <- d[!(d$half == "odd" & d$h == "r"), ]
  sum_group <- list(group = "contr.sum")
  list(
    sum2 = list(m = lm(y ~ x1 + half, data = d, contrasts = list(half = "contr.sum")),
                d = d, spans = c(TRUE, TRUE)),
    sum4 = list(m = lm(y ~ x1 + group, data = d, contrasts = sum_group), d = d,
                spans = c(TRUE, TRUE)),
    helmert4 = list(m = lm(y ~ x1 + group, data = d, contrasts = list(group = "contr.helmert")),
                    d = d, spans = c(TRUE, TRUE)),
    nested_helmert = list(
      m = lm(y ~ x1 + group / sub, data = d,
             contrasts = list(group = "contr.helmert", sub = "contr.helmert")),
      d = d, spans = c(TRUE, TRUE)
    ),
    empty_cell = list(m = lm(y ~ x1 + half * h, data = empty), d = empty,
                      spans = c(TRUE, FALSE)),
    no_intercept = list(m = lm(y ~ 0 + group + x1, data = d), d = d, spans = c(TRUE, TRUE)),
    poly = list(m = lm(y ~ poly(x1, 2) + x2, data = d), d = d, spans = c(TRUE, TRUE)),
    plus_minus_one = list(m = lm(y ~ x1 + s, data = d), d = d, spans = c(TRUE, TRUE)),
    sum4_weighted = list(m = lm(y ~ x1 + group, data = d, weights = v, contrasts = sum_group),
                         d = d, spans = c(TRUE, TRUE)),
    ordered4 = list(m = lm(y ~ x1 + o4, data = d), d = d, spans = c(TRUE, TRUE)),
    ordered3_alone = list(m = lm(y ~ o3, data = d), d = d, spans = c(TRUE, TRUE)),
    continuous = list(m = lm(y ~ x1 + x2, data = d), d = d, spans = c(FALSE, FALSE)),
    treatment = list(m = lm(y ~ x1 + group, data = d), d = d, spans = c(FALSE, FALSE)),
    other_empty_cell = list(m = lm(y ~ x1 + half * h, data = other_empty), d = other_empty,
                            spans = c(FALSE, FALSE)),
    no_intercept_continuous = list(m = lm(y ~ 0 + x1 + x2, data = d), d = d,
                                   spans = c(FALSE, FALSE)),
    continuous_weighted = list(m = lm(y ~ x1 + x2, data = d, weights = v), d = d,
                               spans = c(FALSE, FALSE)),
    treatment_weighted = list(m = lm(y ~ x1 + group, data = d, weights = v), d = d,
                              spans = c(FALSE, FALSE))
  )
}

# The auxiliary design built from the fit, without the package: the columns the
# fit estimated, without the intercept, their squares and, with
# `cross_products` and at most `max_interactions` columns, their products;
# with `centre`, built from the centred columns, which span the same space
# with the intercept and whose rank does not depend on where they are located.
white_df_aux <- function(model, cross_products = TRUE, max_interactions = 10, centre = FALSE) {
  X <- stats::model.matrix(model)
  X <- X[, !is.na(stats::coef(model)), drop = FALSE]
  X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  if (centre) {
    X <- scale(X, scale = FALSE)
  }
  p <- ncol(X)
  Z <- cbind(X, X^2)
  if (cross_products && p > 1 && p <= max_interactions) {
    for (i in seq_len(p - 1L)) {
      for (j in seq.int(i + 1L, p)) Z <- cbind(Z, X[, i] * X[, j])
    }
  }
  colnames(Z) <- paste0("z", seq_len(ncol(Z)))
  rownames(Z) <- rownames(X)
  Z
}

# The Pearson residuals sqrt(w) e that the test reads.
white_df_residuals <- function(model) {
  w <- stats::weights(model)
  e <- stats::residuals(model)
  if (is.null(w)) e else sqrt(w) * e
}

# White's test written out: lm(e^2 ~ Z_c) on the design of the centred
# columns, n R^2, the rank of lm() minus one; with the ranks of the design of
# the columns as they are, Z, and of [1, Z].
white_df_by_hand <- function(model, cross_products = TRUE, max_interactions = 10) {
  Z <- white_df_aux(model, cross_products, max_interactions)
  Zc <- white_df_aux(model, cross_products, max_interactions, centre = TRUE)
  e2 <- white_df_residuals(model)^2
  aux <- stats::lm(e2 ~ Zc)
  statistic <- length(e2) * summary(aux)$r.squared
  df <- aux$rank - 1L
  list(statistic = statistic, df = df,
       p.value = stats::pchisq(statistic, df, lower.tail = FALSE),
       rank_z = qr(Z)$rank, rank_1z = qr(cbind(1, Z))$rank)
}

# lmtest::bptest() with the same squares and cross-products as `varformula`
# (of the centred columns with `centre`): its studentized statistic is n R^2 of
# the regression of e^2 on them, with an intercept, on the rank of that
# regression minus one. A weighted fit is given as the unweighted regression of
# sqrt(w) y on sqrt(w) X, whose residuals are the Pearson residuals White's
# test reads; bptest() of the weighted fit itself would test its raw residuals.
white_df_bptest <- function(model, cross_products = TRUE, max_interactions = 10, centre = FALSE) {
  Z <- as.data.frame(white_df_aux(model, cross_products, max_interactions, centre))
  varformula <- stats::reformulate(names(Z))
  w <- stats::weights(model)
  if (is.null(w)) {
    return(lmtest::bptest(model, varformula = varformula, data = Z))
  }
  sw <- sqrt(w)
  sy <- sw * stats::model.response(stats::model.frame(model))
  sX <- sw * stats::model.matrix(model)
  lmtest::bptest(stats::lm(sy ~ 0 + sX), varformula = varformula, data = Z)
}

test_that("White's degrees of freedom are those of lm(e^2 ~ Z), with or without cross products", {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  designs <- white_df_designs()
  for (name in names(designs)) {
    des <- designs[[name]]
    for (k in 1:2) {
      cross_products <- k == 1L
      info <- paste(name, if (cross_products) "with" else "without", "cross products")
      ours <- performWhiteTest(des$m, des$d, cross_products = cross_products)
      ref <- white_df_by_hand(des$m, cross_products)
      # The design spans the constant, or does not, as it says.
      expect_identical(ref$rank_z == ref$rank_1z, des$spans[k], info = info)
      expect_equal(unname(ours$statistic), ref$statistic, tolerance = 1e-10, info = info)
      expect_identical(unname(ours$parameter[["df"]]), ref$df, info = info)
      expect_identical(attr(ours, "auxiliary_regressors"), ref$df, info = info)
      expect_equal(ours$p.value, ref$p.value, tolerance = 1e-10, info = info)
      # The degrees of freedom are the rank of [1, Z] minus one, which is the
      # rank of Z when Z does not span the constant and one less when it does.
      expect_identical(ref$df, ref$rank_1z - 1L, info = info)
    }
  }
})

test_that("White's test is lmtest::bptest() with its squares and cross products", {
  skip_if_not_installed("lmtest")
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  designs <- white_df_designs()
  for (name in names(designs)) {
    des <- designs[[name]]
    for (cross_products in c(TRUE, FALSE)) {
      info <- paste(name, if (cross_products) "with" else "without", "cross products")
      ours <- performWhiteTest(des$m, des$d, cross_products = cross_products)
      bp <- white_df_bptest(des$m, cross_products)
      expect_equal(unname(ours$statistic), unname(bp$statistic), tolerance = 1e-10, info = info)
      expect_identical(unname(ours$parameter[["df"]]), as.integer(bp$parameter), info = info)
      expect_equal(ours$p.value, unname(bp$p.value), tolerance = 1e-10, info = info)
    }
  }
})

test_that("White's p-values change where the auxiliary columns span the constant, and only there", {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  designs <- white_df_designs()
  # The statistic, which does not change, the degrees of freedom and p-value of
  # 0.15.0, and those of now. Under the default treatment contrasts the four
  # levels of `group` give the model of contr.sum, of contr.helmert and of the
  # model without an intercept, and its 8 degrees of freedom.
  pins <- data.frame(
    design = c("sum2", "sum4", "helmert4", "no_intercept", "nested_helmert", "empty_cell",
               "poly", "sum4_weighted", "sum4", "treatment", "continuous", "empty_cell",
               "ordered4", "ordered3_alone"),
    cross_products = c(rep(TRUE, 8), FALSE, TRUE, TRUE, FALSE, TRUE, TRUE),
    statistic = c(6.190502, 17.830176, 17.830176, 17.830176, 19.407384, 17.682087,
                  11.950445, 16.280548, 13.788111, 17.830176, 11.144838, 12.248134,
                  17.830176, 1.695572),
    df_before = c(5L, 9L, 9L, 9L, 17L, 11L, 9L, 9L, 6L, 8L, 5L, 6L, 9L, 3L),
    p_before = c(0.28812116, 0.03719571, 0.03719571, 0.03719571, 0.30565583, 0.08925654,
                 0.21611740, 0.06124917, 0.03209475, 0.02253635, 0.04858271, 0.05665671,
                 0.03719571, 0.63791878),
    df = c(4L, 8L, 8L, 8L, 16L, 10L, 8L, 8L, 5L, 8L, 5L, 6L, 8L, 2L),
    p = c(0.18536600, 0.02253635, 0.02253635, 0.02253635, 0.24810447, 0.06056910,
          0.15342862, 0.03853617, 0.01701289, 0.02253635, 0.04858271, 0.05665671,
          0.02253635, 0.42836237),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(pins))) {
    pin <- pins[i, ]
    des <- designs[[pin$design]]
    info <- paste(pin$design, if (pin$cross_products) "with" else "without", "cross products")
    ours <- performWhiteTest(des$m, des$d, cross_products = pin$cross_products)
    expect_equal(unname(ours$statistic), pin$statistic, tolerance = 1e-6, info = info)
    expect_identical(unname(ours$parameter[["df"]]), pin$df, info = info)
    expect_equal(ours$p.value, pin$p, tolerance = 1e-6, info = info)
    # The count of 0.15.0 was the rank of the auxiliary columns alone.
    Z <- white_df_aux(des$m, pin$cross_products)
    expect_identical(qr(Z)$rank, pin$df_before, info = info)
    expect_equal(stats::pchisq(pin$statistic, pin$df_before, lower.tail = FALSE), pin$p_before,
                 tolerance = 1e-6, info = info)
  }
})

test_that("every route to White's test takes the degrees of freedom of the exact test", {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  designs <- white_df_designs()
  df_of <- function(x) unname(x$parameter[["df"]])
  for (name in c("sum4", "poly", "empty_cell", "continuous")) {
    des <- designs[[name]]
    m <- des$m
    d <- des$d
    full <- white_df_by_hand(m, cross_products = TRUE)
    reduced <- white_df_by_hand(m, cross_products = FALSE)
    expect_identical(df_of(performWhiteTest(m)), full$df, info = name)
    # More regressors than max_interactions: no cross products.
    suppressMessages(expect_warning(capped <- performWhiteTest(m, d, max_interactions = 1)))
    expect_identical(df_of(capped), reduced$df, info = name)
    robust <- performWhiteTestRobust(m, d)
    expect_identical(df_of(robust), full$df, info = name)
    expect_equal(unname(robust$robust_details$confidence_interval$estimate),
                 stats::qchisq(c(0.025, 0.975), full$df), info = name)
    expect_identical(df_of(performWhiteTestRobust(m, d, method = "reduced")), reduced$df,
                     info = name)
    registered <- get("white", envir = heteroTests:::.diagnostic_registry)
    expect_identical(df_of(registered(m, d)), full$df, info = name)
    expect_identical(df_of(heteroTests:::.ht_custom_fallbacks$white_simplified(m, d)),
                     reduced$df, info = name)
    suite <- runHeteroTests(m, d, tests = "white", use_cache = FALSE, progress = FALSE)
    expect_identical(df_of(suite$white), full$df, info = name)
    expect_equal(suite$white$p.value, full$p.value, tolerance = 1e-10, info = name)
    expect_identical(df_of(heteroTests:::rwhite_test_rebuilt_design(m, d)), full$df, info = name)
    # The bootstrap's p-value depends on its observed statistic, White's, alone.
    set.seed(1)
    boot <- performWhiteTestBootstrap(m, d, B = 9)
    expect_equal(unname(boot$statistic), full$statistic, tolerance = 1e-10, info = name)
  }
})

test_that("the White streaming test gives the degrees of freedom of the exact test", {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  designs <- white_df_designs()
  # It stops when terms of its auxiliary regression are collinear, as the
  # squares of dummies are, and so on every design whose auxiliary columns span
  # the constant, which are collinear with its intercept. Where it gives a
  # value, its degrees of freedom are those of performWhiteTest(). Its checks
  # of the model warn about the collinearity of the dummies on the way.
  values <- 0L
  for (name in names(designs)) {
    des <- designs[[name]]
    for (cross_products in c(TRUE, FALSE)) {
      info <- paste(name, if (cross_products) "with" else "without", "cross products")
      streaming <- suppressWarnings(tryCatch(
        performWhiteTestStreaming(des$m, des$d, chunk_size = 50,
                                  cross_products = cross_products, progress = FALSE),
        error = function(e) NULL
      ))
      if (is.null(streaming)) next
      values <- values + 1L
      exact <- performWhiteTest(des$m, des$d, cross_products = cross_products)
      expect_identical(unname(streaming$parameter[["df"]]), unname(exact$parameter[["df"]]),
                       info = info)
      expect_equal(unname(streaming$statistic), unname(exact$statistic), tolerance = 1e-10,
                   info = info)
    }
  }
  # The designs with continuous regressors only.
  expect_identical(values, 6L)
})

test_that("the log names the auxiliary terms White's test drops and its degrees of freedom", {
  old <- ht_set_log_level("INFO")
  on.exit(ht_set_log_level(old), add = TRUE)
  des <- white_df_designs()$sum2
  suppressMessages(expect_message(
    performWhiteTest(des$m, des$d),
    "dropped 1 auxiliary term(s) collinear with the intercept and the other terms; using df = 4.",
    fixed = TRUE
  ))
})

test_that("White's test does not depend on where the regressors are located", {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- contract_data()$d
  base <- performWhiteTest(lm(y ~ x1 + x2, data = d), d)
  for (shift in c(1e2, 1e4, 1e6)) {
    moved <- d
    moved$x1 <- d$x1 + shift
    m <- lm(y ~ x1 + x2, data = moved)
    info <- paste("x1 +", shift)
    for (cross_products in c(TRUE, FALSE)) {
      ours <- performWhiteTest(m, moved, cross_products = cross_products)
      ref <- white_df_by_hand(m, cross_products)
      expect_equal(unname(ours$statistic), ref$statistic, tolerance = 1e-10, info = info)
      expect_identical(unname(ours$parameter[["df"]]), ref$df, info = info)
      expect_equal(ours$p.value, ref$p.value, tolerance = 1e-10, info = info)
    }
    ours <- performWhiteTest(m, moved)
    expect_equal(unname(ours$statistic), unname(base$statistic), tolerance = 1e-8, info = info)
    expect_identical(ours$parameter, base$parameter, info = info)
    expect_equal(ours$p.value, base$p.value, tolerance = 1e-8, info = info)
    # The routes.
    expect_identical(performWhiteTestRobust(m, moved)$parameter, base$parameter, info = info)
    suite <- runHeteroTests(m, moved, tests = "white", use_cache = FALSE, progress = FALSE)
    expect_equal(unname(suite$white$statistic), unname(base$statistic), tolerance = 1e-8,
                 info = info)
  }
  # Before, the rank of [1, Z] took the square of x1 + 1e4 for a combination of
  # the intercept and x1: 10.341282 on 4 degrees of freedom, against 11.144838
  # on 5 for the same model at zero.
  moved <- d
  moved$x1 <- d$x1 + 1e4
  ours <- performWhiteTest(lm(y ~ x1 + x2, data = moved), moved)
  expect_equal(unname(ours$statistic), 11.144838, tolerance = 1e-6)
  expect_identical(unname(ours$parameter[["df"]]), 5L)
  expect_equal(unname(base$statistic), 11.144838, tolerance = 1e-6)
  # A date written as YYYYMMDD.
  set.seed(8)
  dates <- data.frame(ymd = 20240101 + sample(0:27, 150, TRUE), z = rnorm(150))
  dates$y <- dates$z + rnorm(150, sd = exp(dates$z / 2))
  from_zero <- dates
  from_zero$ymd <- dates$ymd - 20240101
  on_dates <- performWhiteTest(lm(y ~ ymd + z, data = dates), dates)
  on_days <- performWhiteTest(lm(y ~ ymd + z, data = from_zero), from_zero)
  expect_identical(unname(on_dates$parameter[["df"]]), 5L)
  expect_equal(unname(on_dates$statistic), unname(on_days$statistic), tolerance = 1e-8)
})

test_that("White's located test is lmtest::bptest() on the centred terms", {
  skip_if_not_installed("lmtest")
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- contract_data()$d
  for (shift in c(1e4, 1e6)) {
    moved <- d
    moved$x1 <- d$x1 + shift
    m <- lm(y ~ x1 + x2, data = moved)
    for (cross_products in c(TRUE, FALSE)) {
      info <- paste("x1 +", shift, if (cross_products) "with" else "without", "cross products")
      ours <- performWhiteTest(m, moved, cross_products = cross_products)
      bp <- white_df_bptest(m, cross_products, centre = TRUE)
      expect_equal(unname(ours$statistic), unname(bp$statistic), tolerance = 1e-10, info = info)
      expect_identical(unname(ours$parameter[["df"]]), as.integer(bp$parameter), info = info)
      expect_equal(ours$p.value, unname(bp$p.value), tolerance = 1e-10, info = info)
    }
  }
})

test_that("White's test refuses an auxiliary regression with as many terms as observations", {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  # Six regressors, 27 squares and products, 28 terms with the intercept, for
  # 22 observations: R^2 was 1 and the statistic 22, whatever the data.
  set.seed(22)
  small <- data.frame(matrix(rnorm(22 * 6), 22, 6))
  names(small) <- paste0("v", 1:6)
  small$y <- rnorm(22, sd = exp(small$v1 / 2))
  m <- lm(y ~ v1 + v2 + v3 + v4 + v5 + v6, data = small)
  refusal <- "There are too few observations for White's test with these regressors"
  expect_error(performWhiteTest(m, small), refusal, fixed = TRUE)
  expect_error(performWhiteTest(m, small), "has 22 independent terms with its intercept for 22",
               fixed = TRUE)
  expect_error(performWhiteTestRobust(m, small), refusal, fixed = TRUE)
  expect_error(heteroTests:::rwhite_test_rebuilt_design(m, small), refusal, fixed = TRUE)
  expect_error(get("white", envir = heteroTests:::.diagnostic_registry)(m, small), refusal,
               fixed = TRUE)
  expect_error(suppressWarnings(performWhiteTestStreaming(m, small, progress = FALSE)), refusal,
               fixed = TRUE)
  # Without the products, 13 terms: the test gives a value.
  reduced <- white_df_by_hand(m, cross_products = FALSE)
  ours <- performWhiteTest(m, small, cross_products = FALSE)
  expect_equal(unname(ours$statistic), reduced$statistic, tolerance = 1e-10)
  expect_identical(unname(ours$parameter[["df"]]), reduced$df)
  # The runner falls back on it, as on any refusal of White's test.
  suite <- suppressWarnings(runHeteroTests(m, small, tests = "white", use_cache = FALSE,
                                           progress = FALSE))
  expect_identical(attr(suite$white, "extras")$status, "fallback")
  expect_identical(attr(suite$white, "extras")$fallback_strategy, "white_simplified")
  expect_equal(unname(suite$white$statistic), reduced$statistic, tolerance = 1e-10)
  # One observation more than terms: a value.
  set.seed(29)
  enough <- data.frame(matrix(rnorm(29 * 6), 29, 6))
  names(enough) <- paste0("v", 1:6)
  enough$y <- rnorm(29)
  m29 <- lm(y ~ v1 + v2 + v3 + v4 + v5 + v6, data = enough)
  ours <- performWhiteTest(m29, enough)
  expect_identical(unname(ours$parameter[["df"]]), 27L)
  expect_lt(unname(ours$statistic), 29)
})

test_that("White's test refuses an auxiliary regression that fits the squared residuals exactly", {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  refusal <- "There are too few observations for White's test with these regressors"
  # Two observations per subject: the residuals of each pair sum to zero, the
  # squared residuals are a combination of the subject dummies, and R^2 was 1:
  # 60 on 30 degrees of freedom (p = 0.0009) on homoscedastic data.
  set.seed(30)
  d <- data.frame(subject = factor(rep(1:30, each = 2)), treat = rep(0:1, 30))
  d$y <- 2 + 0.5 * d$treat + rnorm(30)[as.integer(d$subject)] + rnorm(60)
  m <- lm(y ~ treat + subject, data = d)
  e2 <- residuals(m)^2
  expect_equal(unname(e2[c(TRUE, FALSE)]), unname(e2[c(FALSE, TRUE)]), tolerance = 1e-10)
  quiet <- function(expr) suppressMessages(suppressWarnings(expr))
  expect_error(quiet(performWhiteTest(m, d)), refusal, fixed = TRUE)
  expect_error(quiet(performWhiteTest(m, d)), "as when each level of a factor holds two",
               fixed = TRUE)
  expect_error(quiet(performWhiteTest(m, d, cross_products = FALSE)), refusal, fixed = TRUE)
  expect_error(quiet(performWhiteTestRobust(m, d)), refusal, fixed = TRUE)
  expect_error(quiet(get("white", envir = heteroTests:::.diagnostic_registry)(m, d)), refusal,
               fixed = TRUE)
  expect_error(quiet(performWhiteTestStreaming(m, d, progress = FALSE)), refusal, fixed = TRUE)
  set.seed(1)
  expect_error(quiet(performWhiteTestBootstrap(m, d, B = 9)), refusal, fixed = TRUE)
  # The regressors alone fit the squared residuals exactly, so the Breusch-Pagan
  # and Koenker tests of them would too: the runner has no fallback and stops.
  expect_error(quiet(runHeteroTests(m, d, tests = "white", use_cache = FALSE, progress = FALSE)),
               refusal, fixed = TRUE)
  # Neither the products nor Koenker's test help: the advice is more data.
  err <- tryCatch(quiet(performWhiteTest(m, d)), error = function(e) e)
  expect_s3_class(err, "heteroTests_white_exact_fit")
  expect_true(err$regressors_exact)
  expect_match(conditionMessage(err), "use more data.", fixed = TRUE)
  expect_no_match(conditionMessage(err), "cross_products = FALSE", fixed = TRUE)
  expect_no_match(conditionMessage(err), "Koenker", fixed = TRUE)

  # A factor of 10 levels, two observations each, beside x1 (n = 20): with the
  # products, as many independent terms as observations; without them, an
  # exact fit on 11 degrees of freedom.
  set.seed(9)
  d10 <- data.frame(x1 = rnorm(20), f = factor(rep(letters[1:10], 2)))
  d10$y <- rnorm(20)
  m10 <- lm(y ~ x1 + f, data = d10)
  expect_error(quiet(performWhiteTest(m10, d10)), "at least as many as there are", fixed = TRUE)
  expect_error(quiet(performWhiteTest(m10, d10, cross_products = FALSE)), refusal, fixed = TRUE)
  expect_error(quiet(runHeteroTests(m10, d10, tests = "white", use_cache = FALSE,
                                    progress = FALSE)), refusal, fixed = TRUE)
})

test_that("White's test gives a value on a near-exact but genuine fit", {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  # Pairs with the same x and residuals of opposite sign, (1 + x) s: e^2 is
  # (1 + x)^2, an exact fit of the auxiliary regression; with noise of relative
  # size delta its residual sum of squares is about delta^2 of the total.
  set.seed(4)
  x <- rep(runif(40, 0, 2), each = 2)
  s <- rep(c(1, -1), 40)
  fit_with <- function(delta) {
    d <- data.frame(x = x, y = 1 + x + (1 + x) * s * (1 + delta * rnorm(80)))
    list(m = lm(y ~ x, data = d), d = d)
  }
  exact <- fit_with(0)
  expect_error(performWhiteTest(exact$m, exact$d), "fits the squared residuals exactly",
               fixed = TRUE)
  expect_error(performWhiteTestStreaming(exact$m, exact$d, progress = FALSE),
               "fits the squared residuals exactly", fixed = TRUE)
  # The regressor alone does not fit (1 + x)^2: Koenker's test is advised.
  err <- tryCatch(performWhiteTest(exact$m, exact$d), error = function(e) e)
  expect_false(err$regressors_exact)
  expect_match(conditionMessage(err), "Koenker's test", fixed = TRUE)
  # Squared residuals that depart from the exact fit by 2e-5 of their size
  # leave about 1.9e-9 of the total, below the threshold of 1.5e-8: refused.
  closer <- fit_with(1e-5)
  expect_error(performWhiteTest(closer$m, closer$d), "fits the squared residuals exactly",
               fixed = TRUE)
  # By 2e-4 and 2e-3, about 1.9e-7 and 1.9e-5: a genuine fit, tested.
  for (delta in c(1e-4, 1e-3)) {
    near <- fit_with(delta)
    ours <- performWhiteTest(near$m, near$d)
    ref <- white_df_by_hand(near$m)
    expect_equal(unname(ours$statistic), ref$statistic, tolerance = 1e-10, info = delta)
    expect_lt(unname(ours$statistic), 80)
    expect_gt(unname(ours$statistic), 79.9)
    streaming <- performWhiteTestStreaming(near$m, near$d, progress = FALSE)
    expect_equal(unname(streaming$statistic), ref$statistic, tolerance = 1e-8, info = delta)
  }
})

test_that("the advice of the refusal depends on the call", {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  # Eleven regressors, more than max_interactions, so no products: 23 terms
  # with the intercept for 22 observations.
  set.seed(7)
  d <- data.frame(matrix(rnorm(22 * 11), 22, 11))
  names(d) <- paste0("v", 1:11)
  d$y <- rnorm(22, sd = exp(d$v1 / 2))
  m <- lm(stats::reformulate(paste0("v", 1:11), "y"), data = d)
  err <- tryCatch(suppressMessages(suppressWarnings(performWhiteTest(m, d))),
                  error = function(e) e)
  expect_s3_class(err, "heteroTests_white_exact_fit")
  expect_no_match(conditionMessage(err), "cross_products = FALSE", fixed = TRUE)
  expect_match(conditionMessage(err), "use Koenker's test (performKoenkerTest()) or more data.",
               fixed = TRUE)
  # Six regressors with their products, for 22 observations.
  set.seed(22)
  small <- data.frame(matrix(rnorm(22 * 6), 22, 6))
  names(small) <- paste0("v", 1:6)
  small$y <- rnorm(22, sd = exp(small$v1 / 2))
  err <- tryCatch(performWhiteTest(lm(y ~ v1 + v2 + v3 + v4 + v5 + v6, data = small), small),
                  error = function(e) e)
  expect_match(conditionMessage(err),
               "use `cross_products = FALSE`, Koenker's test (performKoenkerTest()) or more data.",
               fixed = TRUE)
})

test_that("a regressor named like a column White's test generates is tested", {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  set.seed(3)
  d <- data.frame(a = runif(100, 1, 3))
  d$a_sq <- log(d$a) + rnorm(100, sd = 0.3)
  d$y <- d$a + d$a_sq + rnorm(100, sd = d$a)
  renamed <- d
  names(renamed)[names(renamed) == "a_sq"] <- "b"
  for (cross_products in c(TRUE, FALSE)) {
    ours <- performWhiteTest(lm(y ~ a + a_sq, data = d), d, cross_products = cross_products)
    other <- performWhiteTest(lm(y ~ a + b, data = renamed), renamed,
                              cross_products = cross_products)
    expect_identical(ours[c("statistic", "parameter", "p.value")],
                     other[c("statistic", "parameter", "p.value")])
    ref <- white_df_by_hand(lm(y ~ a + a_sq, data = d), cross_products)
    expect_equal(unname(ours$statistic), ref$statistic, tolerance = 1e-10)
  }
})

test_that("performWhiteTest aborts for perfect fits", {
  df <- data.frame(
    x = seq_len(25),
    y = 3 + 2 * seq_len(25)
  )
  model <- lm(y ~ x, data = df)
  expect_error(
    performWhiteTest(model, df),
    "Residual variance is too small to evaluate White",
    fixed = FALSE
  )
})

