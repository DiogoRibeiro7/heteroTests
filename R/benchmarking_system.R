
#' Benchmark heteroTests implementations against baseline packages
#'
#' These helpers provide a reproducible benchmarking workflow covering execution
#' time, memory footprint, and statistical agreement between \pkg{heteroTests}
#' diagnostics and reference implementations from the \pkg{lmtest} and \pkg{car}
#' packages. Benchmarks can be executed across a wide range of sample sizes (up
#' to one million observations) and heterogeneous variance patterns. Results are
#' returned as tidy data frames suitable for regression testing and automated
#' reporting.
#'
#' @param sample_sizes Integer vector of observation counts to benchmark. Values
#'   may range from small samples (e.g. 100) up to very large inputs
#'   (e.g. 1e6).
#' @param tests Optional character vector restricting the benchmark to a subset
#'   of supported diagnostics (currently "breusch_pagan", "koenker", and "ncv").
#' @param replicates Either a scalar integer applied to every sample size or an
#'   integer vector the same length as `sample_sizes` giving the number of
#'   replications per size.
#' @param hetero_patterns Character vector of variance patterns recycled across
#'   replications. Supported values are "none", "linear", "group", and
#'   "exponential".
#' @param hetero_strength Positive numeric multiplier controlling the severity of
#'   heteroscedasticity.
#' @param baseline_packages Character vector of external packages to compare
#'   against. Defaults to both "lmtest" and "car"; missing packages are skipped
#'   gracefully and recorded in the metadata.
#' @param seed Integer seed for reproducibility.
#' @param n_predictors Number of regressors (excluding the intercept) used when
#'   generating synthetic benchmark datasets.
#' @param profile_memory Logical. If `TRUE` and the \pkg{bench} package is
#'   installed, memory allocations are recorded alongside execution time.
#' @param progress Logical. Display a textual progress bar while running the
#'   benchmarks.
#'
#' @return A list containing four elements: `performance` (raw timing and
#'   memory results), `accuracy` (p-value/statistic differences versus the
#'   baseline packages), `summary` (aggregated medians for quick inspection), and
#'   `metadata` describing the benchmark configuration.
#' @examples
#' \donttest{
#' if (requireNamespace("lmtest", quietly = TRUE) &&
#'     requireNamespace("car", quietly = TRUE)) {
#'   results <- run_benchmark_suite(
#'     sample_sizes = c(100L, 250L),
#'     replicates = 1,
#'     profile_memory = FALSE,
#'     progress = FALSE
#'   )
#'   report <- generate_benchmark_report(results)
#'   head(report$recommendations)
#' }
#' }
#' @export
run_benchmark_suite <- function(sample_sizes = c(100L, 500L, 1000L, 10000L, 100000L, 1000000L),
                                tests = NULL,
                                replicates = 3L,
                                hetero_patterns = c("none", "linear", "group"),
                                hetero_strength = 1,
                                baseline_packages = c("lmtest", "car"),
                                seed = 123L,
                                n_predictors = 4L,
                                profile_memory = TRUE,
                                progress = interactive()) {
  if (!is.numeric(sample_sizes) || any(!is.finite(sample_sizes))) {
    stop("`sample_sizes` must be a numeric vector of finite values.", call. = FALSE)
  }
  sample_sizes <- unique(as.integer(sample_sizes))
  if (any(sample_sizes <= 0L)) {
    stop("`sample_sizes` must contain positive integers only.", call. = FALSE)
  }
  if (!is.null(tests) && !is.character(tests)) {
    stop("`tests` must be NULL or a character vector of test identifiers.", call. = FALSE)
  }
  if (!is.numeric(replicates) || any(!is.finite(replicates))) {
    stop("`replicates` must be numeric and finite.", call. = FALSE)
  }
  if (length(replicates) == 1L) {
    replicates <- rep(as.integer(replicates), length(sample_sizes))
  } else if (length(replicates) != length(sample_sizes)) {
    stop("Length of `replicates` must equal length of `sample_sizes` or be scalar.", call. = FALSE)
  } else {
    replicates <- as.integer(replicates)
  }
  if (any(replicates <= 0L)) {
    stop("`replicates` must contain positive integers only.", call. = FALSE)
  }
  if (!is.numeric(hetero_strength) || length(hetero_strength) != 1L || hetero_strength <= 0) {
    stop("`hetero_strength` must be a positive numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(n_predictors) || length(n_predictors) != 1L || n_predictors < 1L) {
    stop("`n_predictors` must be a positive integer.", call. = FALSE)
  }
  hetero_patterns <- unique(hetero_patterns)
  if (length(hetero_patterns) == 0L) {
    hetero_patterns <- "linear"
  }
  supported_patterns <- c("none", "linear", "group", "exponential")
  invalid_patterns <- setdiff(hetero_patterns, supported_patterns)
  if (length(invalid_patterns) > 0L) {
    stop("Unsupported hetero_patterns: ", paste(invalid_patterns, collapse = ", "), call. = FALSE)
  }

  config <- ht_default_benchmark_tests()
  if (!is.null(tests)) {
    missing_tests <- setdiff(tests, names(config))
    if (length(missing_tests) > 0L) {
      stop("Unsupported tests requested: ", paste(missing_tests, collapse = ", "), call. = FALSE)
    }
    config <- config[tests]
  }
  if (length(config) == 0L) {
    stop("No diagnostics selected for benchmarking.", call. = FALSE)
  }

  baseline_packages <- unique(baseline_packages)
  bench_available <- profile_memory && requireNamespace("bench", quietly = TRUE)
  if (profile_memory && !bench_available) {
    warning("`bench` package not available; memory profiling disabled.", call. = FALSE)
  }

  missing_dependencies <- list()
  for (test_name in names(config)) {
    baselines <- config[[test_name]]$baselines
    baselines <- baselines[names(baselines) %in% baseline_packages]
    if (length(baselines) == 0L) {
      config[[test_name]]$baselines <- baselines
      next
    }
    availability <- vapply(baselines, function(x) requireNamespace(x$package, quietly = TRUE), logical(1))
    if (any(!availability)) {
      missing_dependencies[[test_name]] <- names(availability)[!availability]
    }
    config[[test_name]]$baselines <- baselines[availability]
  }

  total_tasks <- sum(replicates) * length(config)
  pb <- NULL
  if (progress && total_tasks > 0L) {
    pb <- utils::txtProgressBar(min = 0, max = total_tasks, style = 3)
    on.exit(close(pb), add = TRUE)
  }

  performance_records <- list()
  accuracy_records <- list()
  completed <- 0L

  for (size_index in seq_along(sample_sizes)) {
    n <- sample_sizes[size_index]
    reps <- replicates[size_index]
    for (rep in seq_len(reps)) {
      pattern <- hetero_patterns[((rep - 1L) %% length(hetero_patterns)) + 1L]
      seed_offset <- abs(as.integer(seed)) + size_index * 1000L + rep
      case <- ht_generate_benchmark_case(
        n = n,
        n_predictors = n_predictors,
        hetero_pattern = pattern,
        hetero_strength = hetero_strength,
        seed = seed_offset
      )
      model <- case$model
      data <- case$data

      for (test_name in names(config)) {
        entry <- config[[test_name]]
        hetero_run <- ht_run_single_benchmark(
          run_fun = function() entry$hetero$fun(model, data),
          use_bench = bench_available
        )
        hetero_metrics <- if (identical(hetero_run$status, "success")) {
          ht_extract_metrics(hetero_run$result)
        } else {
          ht_empty_metrics()
        }

        performance_records[[length(performance_records) + 1L]] <- ht_build_performance_row(
          test = test_name,
          entry = entry,
          implementation = entry$hetero,
          sample_size = n,
          replicate = rep,
          pattern = pattern,
          run_info = hetero_run,
          metrics = hetero_metrics
        )

        if (length(entry$baselines) > 0L) {
          for (baseline_name in names(entry$baselines)) {
            baseline_entry <- entry$baselines[[baseline_name]]
            baseline_run <- ht_run_single_benchmark(
              run_fun = function() baseline_entry$fun(model, data),
              use_bench = bench_available
            )
            baseline_metrics <- if (identical(baseline_run$status, "success")) {
              ht_extract_metrics(baseline_run$result)
            } else {
              ht_empty_metrics()
            }

            performance_records[[length(performance_records) + 1L]] <- ht_build_performance_row(
              test = test_name,
              entry = entry,
              implementation = baseline_entry,
              sample_size = n,
              replicate = rep,
              pattern = pattern,
              run_info = baseline_run,
              metrics = baseline_metrics
            )

            accuracy_records[[length(accuracy_records) + 1L]] <- ht_build_accuracy_row(
              test = test_name,
              entry = entry,
              pattern = pattern,
              sample_size = n,
              replicate = rep,
              hetero_metrics = hetero_metrics,
              baseline_metrics = baseline_metrics,
              baseline_entry = baseline_entry,
              hetero_status = hetero_run$status,
              baseline_status = baseline_run$status
            )
          }
        }

        completed <- completed + 1L
        if (!is.null(pb)) {
          utils::setTxtProgressBar(pb, completed)
        }
      }
    }
  }

  performance_df <- if (length(performance_records) > 0L) {
    do.call(rbind, performance_records)
  } else {
    data.frame()
  }
  accuracy_df <- if (length(accuracy_records) > 0L) {
    do.call(rbind, accuracy_records)
  } else {
    data.frame()
  }

  summary <- ht_compute_benchmark_summary(performance_df, accuracy_df)
  metadata <- list(
    sample_sizes = sample_sizes,
    replicates = replicates,
    hetero_patterns = hetero_patterns,
    hetero_strength = hetero_strength,
    tests = names(config),
    baseline_packages = baseline_packages,
    missing_dependencies = missing_dependencies,
    bench_available = bench_available,
    timestamp = Sys.time()
  )

  list(
    performance = performance_df,
    accuracy = accuracy_df,
    summary = summary,
    metadata = metadata
  )
}

#' Generate a structured benchmark report
#'
#' Converts the raw output of [run_benchmark_suite()] into aggregated tables and
#' recommendations describing when each diagnostic is preferable from a runtime
#' and accuracy perspective.
#'
#' @param benchmark_results Output list returned by [run_benchmark_suite()].
#' @param accuracy_tolerance Numeric tolerance for declaring statistical
#'   equivalence (based on absolute p-value differences).
#'
#' @return A list with elements `speed`, `memory`, `accuracy`,
#'   `recommendations`, `scalability`, and `metadata` (augmented with the chosen
#'   tolerance).
#' @export
#' @noRd
generate_benchmark_report <- function(benchmark_results, accuracy_tolerance = 1e-4) {
  if (!is.list(benchmark_results) || !all(c("performance", "accuracy") %in% names(benchmark_results))) {
    stop("`benchmark_results` must be the output of run_benchmark_suite().", call. = FALSE)
  }
  if (!is.numeric(accuracy_tolerance) || length(accuracy_tolerance) != 1L || accuracy_tolerance < 0) {
    stop("`accuracy_tolerance` must be a non-negative numeric scalar.", call. = FALSE)
  }

  performance <- benchmark_results$performance
  accuracy <- benchmark_results$accuracy

  perf_success <- performance[performance$status == "success", , drop = FALSE]
  if (nrow(perf_success) > 0L) {
    speed <- ht_group_median(perf_success, value_col = "time")
    names(speed)[names(speed) == "median_value"] <- "median_time"
    speed$time_per_observation <- speed$median_time / speed$sample_size

    memory <- ht_group_median(perf_success, value_col = "memory")
    names(memory)[names(memory) == "median_value"] <- "median_memory_mb"
  } else {
    speed <- data.frame()
    memory <- data.frame()
  }

  accuracy_success <- accuracy[accuracy$status == "success", , drop = FALSE]
  if (nrow(accuracy_success) > 0L) {
    accuracy_summary <- ht_group_median_accuracy(accuracy_success)
  } else {
    accuracy_summary <- data.frame()
  }

  recommendations <- ht_build_recommendations(speed, memory, accuracy_summary, accuracy_tolerance)
  scalability <- ht_compute_scalability(speed)

  metadata <- benchmark_results$metadata
  metadata$accuracy_tolerance <- accuracy_tolerance

  list(
    speed = speed,
    memory = memory,
    accuracy = accuracy_summary,
    recommendations = recommendations,
    scalability = scalability,
    metadata = metadata
  )
}

ht_default_benchmark_tests <- function() {
  list(
    breusch_pagan = list(
      label = "Breusch-Pagan LM",
      hetero = list(
        package = "heteroTests",
        method = "performBPTest",
        fun = function(model, data) performBPTest(model, data)
      ),
      baselines = list(
        lmtest = list(
          package = "lmtest",
          # performBPTest() is the classical statistic, so the baseline must
          # disable studentizing; bptest() defaults to studentize = TRUE, which
          # is the Koenker form compared under the `koenker` entry below.
          method = "lmtest::bptest (classical)",
          fun = function(model, data) lmtest::bptest(model, studentize = FALSE)
        )
      )
    ),
    koenker = list(
      label = "Koenker studentized BP",
      hetero = list(
        package = "heteroTests",
        method = "performKoenkerTest",
        fun = function(model, data) performKoenkerTest(model, data)
      ),
      baselines = list(
        lmtest = list(
          package = "lmtest",
          method = "lmtest::bptest (studentized)",
          fun = function(model, data) lmtest::bptest(model, studentize = TRUE)
        )
      )
    ),
    ncv = list(
      label = "Non-constant variance score",
      hetero = list(
        package = "heteroTests",
        method = "performNCVTest",
        fun = function(model, data) performNCVTest(model)
      ),
      baselines = list(
        car = list(
          package = "car",
          method = "car::ncvTest",
          fun = function(model, data) car::ncvTest(model)
        )
      )
    )
  )
}

ht_generate_benchmark_case <- function(n, n_predictors, hetero_pattern, hetero_strength, seed) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  predictors <- matrix(rnorm(n * n_predictors), nrow = n, ncol = n_predictors)
  colnames(predictors) <- paste0("x", seq_len(n_predictors))
  predictors_df <- as.data.frame(predictors)
  linear_part <- 1 + predictors %*% seq(0.5, length.out = n_predictors, by = 0.5 / max(1, n_predictors - 1))
  hetero_pattern <- match.arg(hetero_pattern, c("none", "linear", "group", "exponential"))
  base_sigma <- switch(
    hetero_pattern,
    none = rep(1, n),
    linear = 0.75 + hetero_strength * abs(predictors_df[[1]]),
    group = 0.6 + hetero_strength * ifelse(predictors_df[[1]] > 0, 1, 0.3),
    exponential = exp(0.1 * predictors_df[[1]]) * hetero_strength
  )
  noise <- rnorm(n, sd = base_sigma)
  y <- as.numeric(linear_part) + noise
  data <- predictors_df
  data$y <- y
  list(
    data = data,
    model = stats::lm(y ~ ., data = data)
  )
}

ht_run_single_benchmark <- function(run_fun, use_bench) {
  status <- "success"
  message <- NULL
  result_container <- NULL
  time_value <- NA_real_
  memory_value <- NA_real_

  if (use_bench) {
    bench_res <- tryCatch({
      tmp_result <- NULL
      res <- bench::mark(
        tmp_result <- run_fun(),
        iterations = 1,
        memory = TRUE,
        check = FALSE
      )
      list(result = tmp_result, bench = res)
    }, error = function(e) {
      status <<- "error"
      message <<- conditionMessage(e)
      NULL
    })
    if (!is.null(bench_res)) {
      result_container <- bench_res$result
      time_value <- ht_convert_bench_time(bench_res$bench$median)
      memory_value <- ht_convert_bench_memory(bench_res$bench$mem_alloc)
    }
  } else {
    start <- proc.time()
    result_container <- tryCatch(run_fun(), error = function(e) {
      status <<- "error"
      message <<- conditionMessage(e)
      NULL
    })
    elapsed <- proc.time() - start
    if (identical(status, "success")) {
      time_value <- ht_convert_elapsed_time(elapsed)
    }
  }

  if (!identical(status, "success")) {
    result_container <- NULL
    time_value <- NA_real_
    memory_value <- NA_real_
  }

  list(
    result = result_container,
    time = time_value,
    memory = memory_value,
    status = status,
    message = message
  )
}

ht_convert_elapsed_time <- function(x) {
  if (is.null(x)) {
    return(NA_real_)
  }
  if (is.numeric(x)) {
    if (!is.null(names(x)) && "elapsed" %in% names(x)) {
      return(as.numeric(x[["elapsed"]]))
    }
    return(as.numeric(x[length(x)]))
  }
  if (inherits(x, "proc_time")) {
    return(as.numeric(x[["elapsed"]]))
  }
  NA_real_
}

ht_convert_bench_time <- function(x) {
  if (is.null(x)) {
    return(NA_real_)
  }
  tryCatch({
    as.numeric(x / bench::as_bench_time("1s"))
  }, error = function(e) {
    tryCatch(as.numeric(x), error = function(e2) NA_real_)
  })
}

ht_convert_bench_memory <- function(x) {
  if (is.null(x)) {
    return(NA_real_)
  }
  tryCatch({
    as.numeric(x) / (1024^2)
  }, error = function(e) NA_real_)
}

ht_extract_metrics <- function(result) {
  if (inherits(result, "htest")) {
    stat <- result$statistic
    stat_value <- if (length(stat) > 0L) unname(stat[[1]]) else NA_real_
    stat_name <- if (length(stat) > 0L) names(stat)[1] else NA_character_
    df_value <- if (!is.null(result$parameter) && length(result$parameter) > 0L) {
      unname(result$parameter[[1]])
    } else {
      NA_real_
    }
    list(
      statistic = stat_value,
      statistic_name = stat_name,
      p_value = if (!is.null(result$p.value)) as.numeric(result$p.value) else NA_real_,
      df = df_value,
      method = if (!is.null(result$method)) result$method else NA_character_
    )
  } else if (inherits(result, "anova")) {
    df_res <- as.data.frame(result)
    stat_col <- intersect(c("Chisq", "Chisq.", "Chisquare", "Statistic"), names(df_res))
    p_col <- intersect(c("Pr(>Chisq)", "Pr(>Chi)", "Pr(>F)", "Pr(>t)", "p.value"), names(df_res))
    stat_value <- if (length(stat_col) > 0L) tail(df_res[[stat_col[1]]], 1) else NA_real_
    p_value <- if (length(p_col) > 0L) tail(df_res[[p_col[1]]], 1) else NA_real_
    heading <- attr(result, "heading")
    method <- if (is.null(heading)) NA_character_ else paste(heading, collapse = " ")
    list(
      statistic = as.numeric(stat_value),
      statistic_name = if (length(stat_col) > 0L) stat_col[1] else NA_character_,
      p_value = as.numeric(p_value),
      df = NA_real_,
      method = method
    )
  } else if (inherits(result, "chisqTest")) {
    # car::ncvTest() returns ChiSquare/Df/p rather than the htest field names,
    # so without this branch the car baseline never produced a comparable
    # p-value and every accuracy row against it was NA. The statistic is named
    # to match ours so the difference is actually computed.
    list(
      statistic = as.numeric(result$ChiSquare)[1],
      statistic_name = "X-squared",
      p_value = as.numeric(result$p)[1],
      df = as.numeric(result$Df)[1],
      method = "car::ncvTest"
    )
  } else if (is.list(result) && !is.null(result$p.value)) {
    list(
      statistic = if (!is.null(result$statistic)) as.numeric(result$statistic)[1] else NA_real_,
      statistic_name = NA_character_,
      p_value = as.numeric(result$p.value)[1],
      df = if (!is.null(result$parameter)) as.numeric(result$parameter)[1] else NA_real_,
      method = if (!is.null(result$method)) result$method else NA_character_
    )
  } else {
    ht_empty_metrics()
  }
}

ht_empty_metrics <- function() {
  list(
    statistic = NA_real_,
    statistic_name = NA_character_,
    p_value = NA_real_,
    df = NA_real_,
    method = NA_character_
  )
}

ht_build_performance_row <- function(test, entry, implementation, sample_size, replicate, pattern, run_info, metrics) {
  data.frame(
    test = test,
    label = entry$label,
    package = implementation$package,
    implementation = implementation$method,
    sample_size = sample_size,
    replicate = replicate,
    hetero_pattern = pattern,
    time = run_info$time,
    memory = run_info$memory,
    statistic = metrics$statistic,
    statistic_name = metrics$statistic_name,
    p_value = metrics$p_value,
    df = metrics$df,
    method = metrics$method,
    status = run_info$status,
    message = if (is.null(run_info$message)) NA_character_ else run_info$message,
    stringsAsFactors = FALSE
  )
}

ht_build_accuracy_row <- function(test, entry, pattern, sample_size, replicate, hetero_metrics, baseline_metrics, baseline_entry, hetero_status, baseline_status) {
  status <- if (identical(hetero_status, "success") && identical(baseline_status, "success")) {
    "success"
  } else {
    paste0(hetero_status, "/", baseline_status)
  }
  p_diff <- if (identical(status, "success")) {
    if (is.na(hetero_metrics$p_value) || is.na(baseline_metrics$p_value)) NA_real_ else abs(hetero_metrics$p_value - baseline_metrics$p_value)
  } else {
    NA_real_
  }
  stat_diff <- if (identical(status, "success") &&
    !is.na(hetero_metrics$statistic) &&
    !is.na(baseline_metrics$statistic) &&
    identical(hetero_metrics$statistic_name, baseline_metrics$statistic_name)) {
    abs(hetero_metrics$statistic - baseline_metrics$statistic)
  } else {
    NA_real_
  }
  data.frame(
    test = test,
    label = entry$label,
    sample_size = sample_size,
    replicate = replicate,
    hetero_pattern = pattern,
    baseline_package = baseline_entry$package,
    baseline_implementation = baseline_entry$method,
    p_value_diff = p_diff,
    statistic_diff = stat_diff,
    statistic_reference = hetero_metrics$statistic_name,
    status = status,
    stringsAsFactors = FALSE
  )
}

ht_compute_benchmark_summary <- function(performance_df, accuracy_df) {
  if (!is.data.frame(performance_df)) {
    performance_df <- data.frame()
  }
  if (!is.data.frame(accuracy_df)) {
    accuracy_df <- data.frame()
  }
  speed <- if (nrow(performance_df) > 0L) ht_group_median(performance_df, value_col = "time") else data.frame()
  memory <- if (nrow(performance_df) > 0L) ht_group_median(performance_df, value_col = "memory") else data.frame()
  accuracy <- if (nrow(accuracy_df) > 0L) ht_group_median_accuracy(accuracy_df) else data.frame()
  list(speed = speed, memory = memory, accuracy = accuracy)
}

ht_safe_median <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(NA_real_)
  }
  stats::median(x)
}

ht_group_median <- function(performance_df, value_col) {
  data <- performance_df[performance_df$status == "success" & is.finite(performance_df[[value_col]]), , drop = FALSE]
  if (nrow(data) == 0L) {
    return(data.frame())
  }
  labels <- unique(data[, c("test", "label")])
  formula <- stats::as.formula(paste(value_col, "~ test + package + sample_size"))
  aggregated <- stats::aggregate(formula, data = data, FUN = ht_safe_median)
  names(aggregated)[names(aggregated) == value_col] <- "median_value"
  aggregated <- merge(aggregated, labels, by = "test", all.x = TRUE)
  aggregated[order(aggregated$test, aggregated$sample_size, aggregated$package), , drop = FALSE]
}

ht_group_median_accuracy <- function(accuracy_df) {
  data <- accuracy_df[accuracy_df$status == "success", , drop = FALSE]
  # aggregate() drops rows whose response is NA, so a frame with rows but no
  # finite differences would otherwise reach aggregate() empty and error.
  data <- data[is.finite(data$p_value_diff) | is.finite(data$statistic_diff), , drop = FALSE]
  if (nrow(data) == 0L) {
    return(data.frame())
  }
  labels <- unique(data[, c("test", "label")])
  p_data <- data[is.finite(data$p_value_diff), , drop = FALSE]
  s_data <- data[is.finite(data$statistic_diff), , drop = FALSE]
  agg <- function(d, col) {
    if (nrow(d) == 0L) {
      return(NULL)
    }
    stats::aggregate(
      stats::as.formula(paste(col, "~ test + baseline_package + sample_size")),
      data = d, FUN = ht_safe_median
    )
  }
  p_val <- agg(p_data, "p_value_diff")
  stat <- agg(s_data, "statistic_diff")
  if (is.null(p_val) && is.null(stat)) {
    return(data.frame())
  }
  keys <- c("test", "baseline_package", "sample_size")
  if (is.null(p_val)) {
    p_val <- cbind(unique(stat[, keys, drop = FALSE]), p_value_diff = NA_real_)
  }
  if (is.null(stat)) {
    stat <- cbind(unique(p_val[, keys, drop = FALSE]), statistic_diff = NA_real_)
  }
  names(p_val)[names(p_val) == "p_value_diff"] <- "median_p_value_diff"
  names(stat)[names(stat) == "statistic_diff"] <- "median_statistic_diff"
  merged <- merge(p_val, stat, by = c("test", "baseline_package", "sample_size"), all = TRUE)
  merged <- merge(merged, labels, by = "test", all.x = TRUE)
  merged[order(merged$test, merged$sample_size, merged$baseline_package), , drop = FALSE]
}

ht_build_recommendations <- function(speed, memory, accuracy_summary, tolerance) {
  if (nrow(speed) == 0L) {
    return(data.frame())
  }
  key <- interaction(speed$test, speed$sample_size, drop = TRUE)
  recs <- lapply(split(seq_len(nrow(speed)), key), function(idx) {
    df <- speed[idx, , drop = FALSE]
    df <- df[order(df$median_time), , drop = FALSE]
    fastest <- df[1, ]
    # With profile_memory = FALSE the memory table is an empty data.frame, so
    # memory$test is NULL and order() would be handed NULL rather than a vector.
    mem_subset <- if (nrow(memory) > 0L && all(c("test", "sample_size", "median_memory_mb") %in% names(memory))) {
      hits <- memory[memory$test == fastest$test & memory$sample_size == fastest$sample_size, , drop = FALSE]
      hits[order(hits$median_memory_mb), , drop = FALSE]
    } else {
      data.frame()
    }
    memory_leader <- if (nrow(mem_subset) > 0L) {
      mem_subset[1, ]
    } else {
      data.frame(package = NA_character_, median_memory_mb = NA_real_, label = NA_character_)
    }
    acc_subset <- if (nrow(accuracy_summary) > 0L && all(c("test", "sample_size") %in% names(accuracy_summary))) {
      accuracy_summary[accuracy_summary$test == fastest$test & accuracy_summary$sample_size == fastest$sample_size, , drop = FALSE]
    } else {
      data.frame()
    }
    accuracy_note <- ht_interpret_accuracy(acc_subset, tolerance)
    data.frame(
      test = fastest$test,
      label = fastest$label,
      sample_size = fastest$sample_size,
      fastest_package = fastest$package,
      fastest_median_time = fastest$median_time,
      time_per_observation = fastest$time_per_observation,
      memory_leader = if (is.null(memory_leader$package)) NA_character_ else memory_leader$package,
      memory_median_mb = if (is.null(memory_leader$median_memory_mb)) NA_real_ else memory_leader$median_memory_mb,
      accuracy_note = accuracy_note,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, recs)
}

ht_interpret_accuracy <- function(accuracy_subset, tolerance) {
  if (nrow(accuracy_subset) == 0L) {
    return("No baseline comparison available")
  }
  diffs <- accuracy_subset$median_p_value_diff
  diffs <- diffs[is.finite(diffs)]
  if (length(diffs) == 0L) {
    return("No valid accuracy measurements")
  }
  problematic <- accuracy_subset$baseline_package[accuracy_subset$median_p_value_diff > tolerance]
  if (length(problematic) == 0L) {
    return(sprintf("Matches baseline within %.1e tolerance", tolerance))
  }
  sprintf("Differences exceed tolerance vs %s", paste(unique(problematic), collapse = ", "))
}

ht_compute_scalability <- function(speed) {
  if (nrow(speed) == 0L) {
    return(data.frame())
  }
  combos <- split(seq_len(nrow(speed)), interaction(speed$test, speed$package, drop = TRUE))
  scal <- lapply(combos, function(idx) {
    df <- speed[idx, , drop = FALSE]
    df <- df[order(df$sample_size), , drop = FALSE]
    if (nrow(df) < 2L) {
      predicted <- if (any(df$sample_size == 1000000L)) df$median_time[df$sample_size == 1000000L][1] else NA_real_
      return(data.frame(
        test = df$test[1],
        label = df$label[1],
        package = df$package[1],
        log_log_slope = NA_real_,
        predicted_time_1e6 = predicted,
        largest_sample_size = df$sample_size[nrow(df)],
        time_per_observation = df$time_per_observation[nrow(df)],
        stringsAsFactors = FALSE
      ))
    }
    log_time <- log(pmax(df$median_time, .Machine$double.eps))
    log_size <- log(df$sample_size)
    fit <- tryCatch(stats::lm(log_time ~ log_size), error = function(e) NULL)
    slope <- if (!is.null(fit)) unname(stats::coef(fit)[2]) else NA_real_
    predicted <- NA_real_
    if (!is.null(fit)) {
      predicted <- exp(stats::predict(fit, newdata = data.frame(log_size = log(1e6))))
    } else if (any(df$sample_size == 1000000L)) {
      predicted <- df$median_time[df$sample_size == 1000000L][1]
    }
    data.frame(
      test = df$test[1],
      label = df$label[1],
      package = df$package[1],
      log_log_slope = slope,
      predicted_time_1e6 = predicted,
      largest_sample_size = max(df$sample_size),
      time_per_observation = df$time_per_observation[nrow(df)],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, scal)
}
