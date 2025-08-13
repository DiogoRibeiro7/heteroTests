#' Advanced simulation framework for test validation
#'
#' Provides tools for validating heteroscedasticity tests under
#' controlled scenarios and estimating statistical power.
#'
#' @param test_function Function taking a model and data and returning an
#'   object with a numeric `p.value` component.
#' @param n_sims Number of simulation replications.
#' @param alpha Significance level for Type I error or power calculations.
#' @param n_obs Number of observations for generated datasets.
#' @param seed Optional integer seed for reproducibility.
#' @return A list summarising Type I error rate or a data frame of power
#'   estimates.
#' @name simulation_framework
NULL

#' Simulate Type I error rates for a heteroscedasticity test
#' @rdname simulation_framework
#' @export
simulate_type_I_errors <- function(test_function, n_sims = 1000,
                                   alpha = 0.05, n_obs = 100,
                                   seed = 123) {
  set.seed(seed)

  p_values <- replicate(n_sims, {
    x <- rnorm(n_obs)
    y <- 1 + 2 * x + rnorm(n_obs)
    data <- data.frame(x = x, y = y)
    model <- lm(y ~ x, data = data)

    tryCatch(
      {
        result <- test_function(model, data)
        result$p.value
      },
      error = function(e) NA
    )
  })

  type_I_rate <- mean(p_values < alpha, na.rm = TRUE)

  list(
    type_I_rate = type_I_rate,
    expected_rate = alpha,
    difference = type_I_rate - alpha,
    p_values = p_values,
    n_valid = sum(!is.na(p_values))
  )
}

#' Simulate statistical power under various heteroscedastic patterns
#' @rdname simulation_framework
#' @param sigma_functions List of variance functions generating heteroscedastic patterns.
#' @param effect_sizes Numeric vector of effect size multipliers.
#' @export
simulate_power_analysis <- function(test_function,
                                    sigma_functions = list(sigma_linear),
                                    effect_sizes = c(0.1, 0.2, 0.5, 1.0),
                                    n_sims = 500,
                                    n_obs = 100,
                                    alpha = 0.05) {
  results <- expand.grid(
    sigma_func = seq_along(sigma_functions),
    effect_size = effect_sizes,
    stringsAsFactors = FALSE
  )
  results$power <- NA_real_

  for (i in seq_len(nrow(results))) {
    sigma_func <- sigma_functions[[results$sigma_func[i]]]
    effect_size <- results$effect_size[i]

    p_values <- replicate(n_sims, {
      sim_data <- simulate_hetero(
        n = n_obs,
        beta0 = 1,
        beta1 = 2,
        sigma_func = function(x) effect_size * sigma_func(x)
      )

      model <- lm(y ~ x, data = sim_data)

      tryCatch(
        {
          result <- test_function(model, sim_data)
          result$p.value
        },
        error = function(e) NA
      )
    })

    results$power[i] <- mean(p_values < alpha, na.rm = TRUE)
  }

  results$sigma_func_name <- sapply(results$sigma_func, function(j) {
    nm <- names(sigma_functions)[j]
    if (!is.null(nm) && nzchar(nm)) nm else paste0("func", j)
  })

  class(results) <- c("power_analysis", "data.frame")
  results
}

#' Plot method for power analysis results
#' @param x Object returned by `simulate_power_analysis()`.
#' @export
plot.power_analysis <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for plotting power analysis")
  }

  ggplot2::ggplot(x, ggplot2::aes(
    x = effect_size, y = power,
    color = sigma_func_name
  )) +
    ggplot2::geom_line(size = 1.2) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_hline(
      yintercept = 0.8, linetype = "dashed",
      color = "red", alpha = 0.7
    ) +
    ggplot2::labs(
      x = "Effect Size",
      y = "Statistical Power",
      color = "Variance Pattern",
      title = "Power Analysis for Heteroscedasticity Test",
      subtitle = "Dashed line shows conventional 80% power threshold"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}
