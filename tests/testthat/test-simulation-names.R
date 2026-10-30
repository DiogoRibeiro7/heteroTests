# 0.14.0 renames the two simulation one-offs into the documented snake_case
# family. The old names warn and return what the new names return.

quick_test <- function(model, data) performBPTest(model, data)

test_that("simulate_type_I_errors() warns and returns what simulate_size() returns", {
  old_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old_level), add = TRUE)
  expect_warning(
    old <- simulate_type_I_errors(quick_test, n_sims = 5, n_obs = 30, seed = 7),
    "'simulate_type_I_errors' is deprecated.*Use 'simulate_size' instead",
    class = "deprecatedWarning"
  )
  expect_identical_numbers(old, simulate_size(quick_test, n_sims = 5, n_obs = 30, seed = 7))
})

test_that("simulate_power_analysis() warns and returns what simulate_power() returns", {
  old_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old_level), add = TRUE)
  set.seed(11)
  expect_warning(
    old <- simulate_power_analysis(quick_test, effect_sizes = 0.5, n_sims = 5, n_obs = 30),
    "'simulate_power_analysis' is deprecated.*Use 'simulate_power' instead",
    class = "deprecatedWarning"
  )
  set.seed(11)
  expect_identical_numbers(old,
                           simulate_power(quick_test, effect_sizes = 0.5, n_sims = 5, n_obs = 30))
})
