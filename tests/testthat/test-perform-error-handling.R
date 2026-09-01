library(testthat)


test_that("core perform*Test functions reject invalid model inputs", {
  set.seed(2024)
  n <- 40
  base_data <- data.frame(
    y = rnorm(n),
    x1 = rnorm(n),
    x2 = rnorm(n),
    group = factor(rep(letters[1:4], length.out = n)),
    order_var = rnorm(n)
  )
  base_data$positive <- abs(base_data$x1) + 0.5
  base_data$id <- rep(seq_len(10), length.out = n)
  base_data$time <- rep(seq_len(4), each = 10)[seq_len(n)]
  coords <- data.frame(x = rnorm(n), y = rnorm(n))

  invalid_model <- list()

  msg_glm <- "Provide an object fitted with stats::lm() or stats::glm()"
  msg_glm_plain <- "Provide a model fitted with stats::lm() or stats::glm()."
  msg_lm_only <- "`model` must be an object of class 'lm'."
  msg_terms <- "no terms component nor attribute"

  cases <- list(
    list(name = "white", fun = performWhiteTest, args = list(data = base_data), expected = c(msg_glm, msg_glm_plain)),
    list(name = "bp", fun = performBPTest, args = list(data = base_data), expected = c(msg_glm, msg_glm_plain)),
    list(name = "studentized", fun = performStudentizedBPTest, args = list(data = base_data), expected = c(msg_terms, msg_glm, msg_glm_plain)),
    list(name = "koenker", fun = performKoenkerTest, args = list(data = base_data), expected = c(msg_glm, msg_glm_plain)),
    list(name = "gq", fun = performGQTest, args = list(data = base_data, order_by = "order_var"), expected = c(msg_glm, msg_glm_plain)),
    list(name = "ncv", fun = performNCVTest, args = list(), expected = c(msg_lm_only, msg_glm, msg_glm_plain)),
    list(name = "harvey", fun = performHarveyTest, args = list(), expected = c(msg_glm, msg_glm_plain)),
    list(name = "park", fun = performParkTest, args = list(data = base_data, variable = "positive"), expected = c(msg_glm, msg_glm_plain)),
    list(name = "glejser", fun = performGlejserTest, args = list(data = base_data, variable = "positive"), expected = c(msg_glm, msg_glm_plain)),
    list(name = "cameron", fun = performCameronTrivediTest, args = list(), expected = c(msg_glm, msg_glm_plain)),
    list(name = "arch", fun = performArchLMTest, args = list(lags = 2), expected = c(msg_glm, msg_glm_plain)),
    list(name = "mcleod", fun = performMcLeodLiTest, args = list(lags = 2), expected = c(msg_glm, msg_glm_plain)),
    list(name = "spread", fun = performSpreadLevelTest, args = list(), expected = c(msg_glm_plain, msg_glm)),
    list(name = "spearman", fun = performSpearmanTest, args = list(), expected = c(msg_glm, msg_glm_plain)),
    list(name = "cook", fun = performCookWeisbergTest, args = list(), expected = c(msg_lm_only, msg_glm, msg_glm_plain)),
    list(name = "hartley", fun = performHartleyFmaxTest, args = list(data = base_data, group = "group"), expected = c(msg_glm, msg_glm_plain)),
    list(name = "levene", fun = performLeveneTest, args = list(data = base_data, group = "group"), expected = c(msg_glm, msg_glm_plain)),
    list(name = "brown_forsythe", fun = performBrownForsytheTest, args = list(data = base_data, group = "group"), expected = c(msg_glm, msg_glm_plain)),
    list(name = "obrien", fun = performOBrienTest, args = list(data = base_data, group = "group"), expected = c(msg_lm_only, msg_glm, msg_glm_plain)),
    list(name = "fligner", fun = performFlignerKilleenTest, args = list(data = base_data, group = "group"), expected = c(msg_glm, msg_glm_plain)),
    list(name = "modified_bartlett", fun = performModifiedBartlettTest, args = list(data = base_data, group = "group"), expected = c(msg_lm_only, msg_glm, msg_glm_plain)),
    list(name = "bartlett", fun = performBartlettTest, args = list(data = base_data, group = "group"), expected = c(msg_glm, msg_glm_plain)),
    list(name = "rice", fun = performRiceTest, args = list(), expected = "no power against heteroscedasticity"),
    list(name = "pesaran", fun = performPesaranTest, args = list(data = base_data, id = "id", time = "time"), expected = c(msg_lm_only, msg_glm, msg_glm_plain)),
    list(name = "ordered", fun = performOrderedLMTest, args = list(data = base_data, order_by = "order_var"), expected = c(msg_glm, msg_glm_plain)),
    list(name = "curry_walsh", fun = performCurryWalshTest, args = list(coords = coords), expected = "performSpatialHeteroTest"),
    list(name = "white_bootstrap", fun = performWhiteTestBootstrap, args = list(data = base_data, B = 10, parallel = FALSE), expected = c(msg_terms, msg_glm, msg_glm_plain)),
    list(name = "szroeter", fun = performSzroeterTest, args = list(data = base_data, order_by = "order_var"), expected = c(msg_terms, msg_glm, msg_glm_plain)),
    list(name = "davidian", fun = performDavidianCarrollTest, args = list(), expected = c(msg_lm_only, msg_glm, msg_glm_plain)),
    list(name = "bp_re", fun = performBPRandomEffectsTest, args = list(data = base_data, id = "id"), expected = c(msg_lm_only, msg_glm, msg_glm_plain))
  )

  message_matches <- function(message, patterns) {
    any(vapply(patterns, function(p) grepl(p, message, fixed = TRUE), logical(1)))
  }

  for (case in cases) {
    result <- tryCatch(
      do.call(case$fun, c(list(model = invalid_model), case$args)),
      error = function(e) e
    )
    expect_true(inherits(result, "error"), info = paste(case$name, "should error on invalid model"))
    expect_true(
      message_matches(conditionMessage(result), case$expected),
      info = paste(case$name, "returned unexpected error message:", conditionMessage(result))
    )
  }
})

test_that("performBoxMTest validates data inputs", {
  expect_error(
    performBoxMTest(list(1:5), "group"),
    "data must be a data.frame or matrix",
    fixed = TRUE
  )
})
