# Names deprecated in 0.14.0 keep working, warn with base .Deprecated() and
# name their replacement, and return what the replacement returns. The match
# is on the message rather than the class, because R 4.1 is supported.

test_that("performBreuschPaganTest() warns and returns what performBPTest() returns", {
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  expect_warning(
    old <- performBreuschPaganTest(m, mtcars),
    "'performBreuschPaganTest' is deprecated.*Use 'performBPTest' instead"
  )
  expect_identical_numbers(old, performBPTest(m, mtcars))
})
