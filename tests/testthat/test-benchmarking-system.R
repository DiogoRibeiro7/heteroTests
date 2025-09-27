skip_on_cran()

test_that("benchmark suite produces structured output", {
  skip_if_not_installed("lmtest")
  skip_if_not_installed("car")

  results <- run_benchmark_suite(
    sample_sizes = c(100L, 250L),
    replicates = c(1L, 1L),
    hetero_patterns = c("none", "linear"),
    baseline_packages = c("lmtest", "car"),
    profile_memory = FALSE,
    progress = FALSE
  )

  expect_type(results, "list")
  expect_true(all(c("performance", "accuracy", "summary", "metadata") %in% names(results)))
  expect_s3_class(results$performance, "data.frame")
  expect_true(all(c("test", "package", "time", "p_value") %in% names(results$performance)))
  expect_true(all(results$performance$sample_size %in% c(100L, 250L)))

  hetero_perf <- subset(results$performance, package == "heteroTests" & status == "success")
  expect_gt(nrow(hetero_perf), 0)

  time_by_size <- stats::aggregate(
    time ~ sample_size + test,
    data = hetero_perf,
    FUN = function(x) stats::median(x, na.rm = TRUE)
  )
  if (nrow(time_by_size) > 1) {
    smallest <- min(time_by_size$sample_size)
    largest <- max(time_by_size$sample_size)
    small_time <- time_by_size$time[time_by_size$sample_size == smallest]
    large_time <- time_by_size$time[time_by_size$sample_size == largest]
    if (length(small_time) > 0 && length(large_time) > 0) {
      expect_true(all(large_time <= small_time * 50))
    }
  }

  accuracy_success <- subset(results$accuracy, status == "success")
  if (nrow(accuracy_success) > 0) {
    diffs <- accuracy_success$p_value_diff
    diffs <- diffs[is.finite(diffs)]
    if (length(diffs) > 0) {
      expect_lt(max(diffs), 1e-4)
    }
  }

  expect_s3_class(results$summary$speed, "data.frame")
  expect_true(all(c("test", "package", "sample_size", "median_value") %in% names(results$summary$speed)))

  report <- generate_benchmark_report(results, accuracy_tolerance = 1e-4)
  expect_type(report, "list")
  expect_true(all(c("speed", "memory", "accuracy", "recommendations", "scalability") %in% names(report)))
  expect_s3_class(report$recommendations, "data.frame")
  if (nrow(report$recommendations) > 0) {
    expect_true(all(report$recommendations$fastest_package %in% unique(results$performance$package)))
  }
})
