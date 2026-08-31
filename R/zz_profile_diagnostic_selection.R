# Keep profile-based diagnostic selection consistent with the validated registry.
# This late-loaded definition supersedes the historical implementation while the
# recommendation module is being decomposed into smaller units.
suggestDiagnosticsForProfile <- function(profile) {
  stopifnot(is.list(profile))

  recommendations <- list(
    list(test = "breusch_pagan", rationale = "Baseline regression-based heteroscedasticity check."),
    list(test = "white", rationale = "Detects general forms of non-constant variance.")
  )

  if (identical(profile$size_bucket, "small")) {
    recommendations <- c(recommendations, list(
      list(test = "wild_bootstrap", rationale = "Bootstrap inference performs well in small samples."),
      list(test = "rank_permutation", rationale = "Rank-based test is robust when asymptotics are unreliable.")
    ))
  }

  if (identical(profile$size_bucket, "very_large")) {
    recommendations <- c(recommendations, list(
      list(test = "quantile_regression", rationale = "Quantile-based checks can reveal distributional heterogeneity."),
      list(test = "koenker", rationale = "Studentized Breusch-Pagan inference avoids the classical normality scaling assumption.")
    ))
  }

  if (isTRUE(profile$high_dimensional)) {
    recommendations <- c(recommendations, list(
      list(test = "high_dimensional", rationale = "Experimental projection diagnostic for scenarios where predictors outnumber observations.")
    ))
  }

  if (isTRUE(profile$detected_spatial_fields)) {
    recommendations <- c(recommendations, list(
      list(test = "spatial_hetero", rationale = "Spatial diagnostic for geographic residual-scale structure."),
      list(test = "wild_bootstrap", rationale = "Bootstrap-based inference can complement spatial diagnostics when its assumptions are appropriate.")
    ))
  }

  if (!is.null(profile$missingness) && profile$missingness$overall_rate >= 0.05) {
    recommendations <- c(recommendations, list(
      list(test = "quantile_regression", rationale = "Quantile slope comparisons provide a complementary distributional check after missing-data handling."),
      list(test = "wild_bootstrap", rationale = "Bootstrap diagnostics can provide a complementary finite-sample check after missing-data handling.")
    ))
  }

  if (nrow(profile$numeric_distribution) > 0) {
    skewed <- profile$numeric_distribution$variable[
      profile$numeric_distribution$shape %in% c("strong_right_skew", "strong_left_skew")
    ]
    if (length(skewed) > 0) {
      recommendations <- c(recommendations, list(
        list(
          test = "quantile_regression",
          rationale = sprintf(
            "Variables %s show strong skewness; quantile slope comparisons provide a complementary distributional check.",
            paste(skewed, collapse = ", ")
          )
        )
      ))
    }
  }

  unique_tests <- unique(vapply(recommendations, `[[`, character(1), "test"))
  df <- data.frame(
    test = unique_tests,
    rationale = vapply(unique_tests, function(tst) {
      reasons <- vapply(recommendations, function(rec) {
        if (rec$test == tst) rec$rationale else NA_character_
      }, character(1))
      paste(stats::na.omit(reasons), collapse = " ")
    }, character(1)),
    stringsAsFactors = FALSE
  )

  df[order(df$test), , drop = FALSE]
}
