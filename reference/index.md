# Package index

## Tests of the variance

Each test returns an `htest` object, whose fields `test_results`
documents. The tests are grouped by the family they belong to.

### What every test returns

- [`test_results`](https://diogoribeiro7.github.io/heteroTests/reference/test_results.md)
  : What every test returns

### Auxiliary-regression tests

- [`performWhiteTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTest.md)
  : Perform White's test for heteroscedasticity
- [`performWhiteTestRobust()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTestRobust.md)
  : Robust White test with bootstrap and effect sizes
- [`performWhiteTestStreaming()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTestStreaming.md)
  : Memory-efficient White test for large datasets
- [`performBPTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTest.md)
  [`performBreuschPaganTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTest.md)
  : Perform Breusch-Pagan test for heteroscedasticity
- [`performBPTestRobust()`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTestRobust.md)
  : Robust Breusch–Pagan test with bootstrap and effect sizes
- [`performBPTestStreaming()`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTestStreaming.md)
  : Streaming Breusch\\Pagan test for large datasets
- [`performKoenkerTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTest.md)
  : Perform Koenker studentized Breusch-Pagan test
- [`performKoenkerTestStreaming()`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTestStreaming.md)
  : Streaming Koenker studentized Breusch\\Pagan test
- [`performStudentizedBPTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performStudentizedBPTest.md)
  : Studentized Breusch–Pagan test (deprecated)
- [`performCookWeisbergTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performCookWeisbergTest.md)
  : Perform Cook-Weisberg test for heteroscedasticity
- [`performNCVTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performNCVTest.md)
  : Perform the non-constant variance (NCV) score test (deprecated)
- [`performHarveyTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performHarveyTest.md)
  : Perform Harvey test for multiplicative heteroscedasticity
- [`performGlejserTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performGlejserTest.md)
  : Perform Glejser test for heteroscedasticity
- [`performParkTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performParkTest.md)
  : Perform Park test for heteroscedasticity
- [`performVarianceFormTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performVarianceFormTest.md)
  : Test the functional form of the error variance
- [`performDavidianCarrollTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performDavidianCarrollTest.md)
  : Perform Davidian-Carroll test
- [`performSpearmanTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performSpearmanTest.md)
  : Perform Spearman rank correlation test
- [`performSpreadLevelTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performSpreadLevelTest.md)
  : Perform Spread-Level test
- [`performHighDimensionalTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performHighDimensionalTest.md)
  : High-dimensional projection heteroscedasticity test

### Tests on ordered observations

- [`performGQTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performGQTest.md)
  : Perform Goldfeld-Quandt test for heteroscedasticity
- [`performSzroeterTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performSzroeterTest.md)
  : Szroeter test for ordered alternatives
- [`performHarrisonMcCabeTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performHarrisonMcCabeTest.md)
  : Harrison-McCabe test for heteroscedasticity

### Group-wise tests

- [`performLeveneTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performLeveneTest.md)
  : Perform Levene's test for equality of variances
- [`performBrownForsytheTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBrownForsytheTest.md)
  : Perform Brown-Forsythe test for equality of variances
- [`performFlignerKilleenTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performFlignerKilleenTest.md)
  : Perform Fligner-Killeen test for homogeneity of variances
- [`performBartlettTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBartlettTest.md)
  : Perform Bartlett's test for equality of variances
- [`performHartleyFmaxTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performHartleyFmaxTest.md)
  : Perform Hartley's Fmax test
- [`performOBrienTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performOBrienTest.md)
  : Perform O'Brien test for equality of variances
- [`performBoxMTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBoxMTest.md)
  : Box's M Test for Equality of Covariance Matrices

### Time series, panel and spatial tests

- [`performArchLMTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performArchLMTest.md)
  : Perform Engle's ARCH LM test
- [`performMcLeodLiTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performMcLeodLiTest.md)
  : Perform McLeod-Li test
- [`performBPRandomEffectsTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBPRandomEffectsTest.md)
  : Breusch-Pagan LM test for random effects
- [`performPesaranTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performPesaranTest.md)
  : Perform Pesaran CD test
- [`performSpatialHeteroTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performSpatialHeteroTest.md)
  : Spatial heteroscedasticity test

### Resampling and robust tests

- [`performWhiteTestBootstrap()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTestBootstrap.md)
  : Bootstrap White test
- [`performWildBootstrapTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performWildBootstrapTest.md)
  : Wild bootstrap heteroscedasticity test
- [`performRankPermutationTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performRankPermutationTest.md)
  : Rank-based permutation heteroscedasticity test
- [`performQuantileRegressionTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performQuantileRegressionTest.md)
  : Quantile regression heteroscedasticity test
- [`bootstrapTestStatistic()`](https://diogoribeiro7.github.io/heteroTests/reference/rbootstrap_test_statistic.md)
  [`rbootstrap_test_statistic()`](https://diogoribeiro7.github.io/heteroTests/reference/rbootstrap_test_statistic.md)
  : Bootstrap a test statistic
- [`bootstrap_methods`](https://diogoribeiro7.github.io/heteroTests/reference/bootstrap_methods.md)
  : Bootstrap utilities for heteroscedasticity diagnostics
- [`robust_implementations`](https://diogoribeiro7.github.io/heteroTests/reference/robust_implementations.md)
  : Robust heteroscedasticity diagnostics
- [`modern_diagnostics`](https://diogoribeiro7.github.io/heteroTests/reference/modern_diagnostics.md)
  : Modern heteroscedasticity diagnostics
- [`additional_tests`](https://diogoribeiro7.github.io/heteroTests/reference/additional_tests.md)
  : Additional heteroscedasticity tests

## Model checks

Checks of the fitted model that are not tests of the variance: the
functional form, collinearity, influence and the fit itself. They are
listed apart so that they are not mistaken for heteroscedasticity tests.

- [`performRESETTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performRESETTest.md)
  : Ramsey's RESET test for nonlinearity
- [`performVIFDiagnostic()`](https://diogoribeiro7.github.io/heteroTests/reference/performVIFDiagnostic.md)
  : Variance inflation factors
- [`performInfluenceDiagnostics()`](https://diogoribeiro7.github.io/heteroTests/reference/performInfluenceDiagnostics.md)
  : Identify influential observations
- [`performScatterDiagnostic()`](https://diogoribeiro7.github.io/heteroTests/reference/performScatterDiagnostic.md)
  : Scatter-plot diagnostics for heteroscedasticity
- [`checkModel()`](https://diogoribeiro7.github.io/heteroTests/reference/checkModel.md)
  : Validate model argument
- [`checkModelEnhanced()`](https://diogoribeiro7.github.io/heteroTests/reference/checkModelEnhanced.md)
  : Enhanced model diagnostics (deprecated)

## Running several diagnostics

- [`runHeteroTests()`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md)
  : Run multiple heteroscedasticity diagnostics
- [`runHeteroTestsParallel()`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTestsParallel.md)
  : Parallel test execution helper
- [`runDiagnostics()`](https://diogoribeiro7.github.io/heteroTests/reference/runDiagnostics.md)
  : Run a suite of model diagnostics
- [`runDiagnosticPlots()`](https://diogoribeiro7.github.io/heteroTests/reference/runDiagnosticPlots.md)
  : Run registered diagnostic plots
- [`runMultivariateTests()`](https://diogoribeiro7.github.io/heteroTests/reference/runMultivariateTests.md)
  : Run multivariate heteroscedasticity tests
- [`runTimeSeriesTests()`](https://diogoribeiro7.github.io/heteroTests/reference/runTimeSeriesTests.md)
  : Run time-series heteroscedasticity tests
- [`runPanelTests()`](https://diogoribeiro7.github.io/heteroTests/reference/runPanelTests.md)
  : Run panel-data heteroscedasticity tests
- [`runSurveyHeteroTests()`](https://diogoribeiro7.github.io/heteroTests/reference/runSurveyHeteroTests.md)
  : Run heteroscedasticity diagnostics on the data of a survey design
- [`runAdvancedDiagnostics()`](https://diogoribeiro7.github.io/heteroTests/reference/runAdvancedDiagnostics.md)
  [`rrunAdvancedDiagnostics()`](https://diogoribeiro7.github.io/heteroTests/reference/runAdvancedDiagnostics.md)
  : Run enhanced heteroscedasticity diagnostics
- [`compareModelDiagnostics()`](https://diogoribeiro7.github.io/heteroTests/reference/compareModelDiagnostics.md)
  : Compare diagnostics across models
- [`compareTestResults()`](https://diogoribeiro7.github.io/heteroTests/reference/compareTestResults.md)
  : Summarise heteroscedasticity test results
- [`analyzeMLResiduals()`](https://diogoribeiro7.github.io/heteroTests/reference/analyzeMLResiduals.md)
  : Machine-learning residual analysis
- [`registerDiagnostic()`](https://diogoribeiro7.github.io/heteroTests/reference/registerDiagnostic.md)
  : Register a custom diagnostic
- [`registerPlot()`](https://diogoribeiro7.github.io/heteroTests/reference/registerPlot.md)
  : Register a diagnostic plot
- [`listDiagnostics()`](https://diogoribeiro7.github.io/heteroTests/reference/listDiagnostics.md)
  : List the registered diagnostics
- [`HeteroDiagnostic()`](https://diogoribeiro7.github.io/heteroTests/reference/HeteroDiagnostic.md)
  : Create a diagnostic object
- [`test()`](https://diogoribeiro7.github.io/heteroTests/reference/test.HeteroDiagnostic.md)
  : Run diagnostic tests
- [`summary(`*`<HeteroDiagnostic>`*`)`](https://diogoribeiro7.github.io/heteroTests/reference/summary.HeteroDiagnostic.md)
  : Summarize diagnostics
- [`plot(`*`<HeteroDiagnostic>`*`)`](https://diogoribeiro7.github.io/heteroTests/reference/plot.HeteroDiagnostic.md)
  : Plot diagnostics
- [`hetero_test_suite`](https://diogoribeiro7.github.io/heteroTests/reference/hetero_test_suite.md)
  [`hetero_grouped_suite`](https://diogoribeiro7.github.io/heteroTests/reference/hetero_test_suite.md)
  : S3 containers for heteroscedasticity diagnostics
- [`tidy(`*`<hetero_test>`*`)`](https://diogoribeiro7.github.io/heteroTests/reference/tidy.hetero_test.md)
  [`glance(`*`<hetero_test>`*`)`](https://diogoribeiro7.github.io/heteroTests/reference/tidy.hetero_test.md)
  [`augment(`*`<hetero_test>`*`)`](https://diogoribeiro7.github.io/heteroTests/reference/tidy.hetero_test.md)
  [`tidy(`*`<hetero_test_suite>`*`)`](https://diogoribeiro7.github.io/heteroTests/reference/tidy.hetero_test.md)
  [`glance(`*`<hetero_test_suite>`*`)`](https://diogoribeiro7.github.io/heteroTests/reference/tidy.hetero_test.md)
  [`augment(`*`<hetero_test_suite>`*`)`](https://diogoribeiro7.github.io/heteroTests/reference/tidy.hetero_test.md)
  [`tidy(`*`<hetero_grouped_suite>`*`)`](https://diogoribeiro7.github.io/heteroTests/reference/tidy.hetero_test.md)
  : broom methods for heteroscedasticity diagnostics
- [`cachedTest()`](https://diogoribeiro7.github.io/heteroTests/reference/cachedTest.md)
  : Cache results of a heteroscedasticity test
- [`clearTestCache()`](https://diogoribeiro7.github.io/heteroTests/reference/clearTestCache.md)
  : Clear cached test results
- [`clearAnalysisCache()`](https://diogoribeiro7.github.io/heteroTests/reference/clearAnalysisCache.md)
  : Clear cached diagnostic run results
- [`TestFactory`](https://diogoribeiro7.github.io/heteroTests/reference/TestFactory.md)
  : Heteroscedasticity Test Factory
- [`test_factory`](https://diogoribeiro7.github.io/heteroTests/reference/test_factory.md)
  : Shared instance of the test factory

## Remediation

- [`fitWLS()`](https://diogoribeiro7.github.io/heteroTests/reference/fitWLS.md)
  : Weighted Least Squares wrapper
- [`fitRobust()`](https://diogoribeiro7.github.io/heteroTests/reference/fitRobust.md)
  : Robust regression wrapper
- [`autoTransform()`](https://diogoribeiro7.github.io/heteroTests/reference/autoTransform.md)
  : Automatic transformation helper
- [`suggestRemediation()`](https://diogoribeiro7.github.io/heteroTests/reference/suggestRemediation.md)
  [`print(`*`<remediation_suggestions>`*`)`](https://diogoribeiro7.github.io/heteroTests/reference/suggestRemediation.md)
  : Suggest remediation actions for heteroscedasticity
- [`autoCompareRemediations()`](https://diogoribeiro7.github.io/heteroTests/reference/autoCompareRemediations.md)
  : Automatically compare remedial models

## Recommendations

- [`analyseDatasetCharacteristics()`](https://diogoribeiro7.github.io/heteroTests/reference/generateHeteroRecommendations.md)
  [`suggestDiagnosticsForProfile()`](https://diogoribeiro7.github.io/heteroTests/reference/generateHeteroRecommendations.md)
  [`interpretHeteroTestResults()`](https://diogoribeiro7.github.io/heteroTests/reference/generateHeteroRecommendations.md)
  [`recommendRemediationStrategies()`](https://diogoribeiro7.github.io/heteroTests/reference/generateHeteroRecommendations.md)
  [`warnDiagnosticAssumptions()`](https://diogoribeiro7.github.io/heteroTests/reference/generateHeteroRecommendations.md)
  [`buildDiagnosticDecisionTree()`](https://diogoribeiro7.github.io/heteroTests/reference/generateHeteroRecommendations.md)
  [`composeDiagnosticNarrative()`](https://diogoribeiro7.github.io/heteroTests/reference/generateHeteroRecommendations.md)
  [`generateHeteroRecommendations()`](https://diogoribeiro7.github.io/heteroTests/reference/generateHeteroRecommendations.md)
  : Intelligent recommendations for heteroscedasticity diagnostics

## Simulation

The simulation functions are named in snake_case on purpose; the rest of
the package uses camelCase.

- [`simulate_hetero()`](https://diogoribeiro7.github.io/heteroTests/reference/simulate_hetero.md)
  [`simulate_arch1()`](https://diogoribeiro7.github.io/heteroTests/reference/simulate_hetero.md)
  [`sigma_linear()`](https://diogoribeiro7.github.io/heteroTests/reference/simulate_hetero.md)
  [`sigma_exponential()`](https://diogoribeiro7.github.io/heteroTests/reference/simulate_hetero.md)
  [`sigma_group()`](https://diogoribeiro7.github.io/heteroTests/reference/simulate_hetero.md)
  [`sigma_piecewise()`](https://diogoribeiro7.github.io/heteroTests/reference/simulate_hetero.md)
  [`sigma_poly()`](https://diogoribeiro7.github.io/heteroTests/reference/simulate_hetero.md)
  [`sigma_sin()`](https://diogoribeiro7.github.io/heteroTests/reference/simulate_hetero.md)
  [`sigma_multiplicative()`](https://diogoribeiro7.github.io/heteroTests/reference/simulate_hetero.md)
  [`sigma_spatial()`](https://diogoribeiro7.github.io/heteroTests/reference/simulate_hetero.md)
  [`sigma_logistic()`](https://diogoribeiro7.github.io/heteroTests/reference/simulate_hetero.md)
  [`sigma_inverse()`](https://diogoribeiro7.github.io/heteroTests/reference/simulate_hetero.md)
  [`sigma_power()`](https://diogoribeiro7.github.io/heteroTests/reference/simulate_hetero.md)
  [`sigma_step()`](https://diogoribeiro7.github.io/heteroTests/reference/simulate_hetero.md)
  [`sigma_u_shape()`](https://diogoribeiro7.github.io/heteroTests/reference/simulate_hetero.md)
  [`sigma_exp_decay()`](https://diogoribeiro7.github.io/heteroTests/reference/simulate_hetero.md)
  [`sigma_gaussian_peak()`](https://diogoribeiro7.github.io/heteroTests/reference/simulate_hetero.md)
  [`sigma_piecewise_linear()`](https://diogoribeiro7.github.io/heteroTests/reference/simulate_hetero.md)
  : Simulate heteroscedastic data
- [`simulate_size()`](https://diogoribeiro7.github.io/heteroTests/reference/simulation_framework.md)
  [`simulate_power()`](https://diogoribeiro7.github.io/heteroTests/reference/simulation_framework.md)
  [`simulate_type_I_errors()`](https://diogoribeiro7.github.io/heteroTests/reference/simulation_framework.md)
  [`simulate_power_analysis()`](https://diogoribeiro7.github.io/heteroTests/reference/simulation_framework.md)
  : Advanced simulation framework for test validation
- [`plot(`*`<power_analysis>`*`)`](https://diogoribeiro7.github.io/heteroTests/reference/plot.power_analysis.md)
  : Plot method for power analysis results

## Plotting

- [`plotResidualsFitted()`](https://diogoribeiro7.github.io/heteroTests/reference/plotResidualsFitted.md)
  : Plot residuals vs fitted values
- [`plotResidualsFittedEnhanced()`](https://diogoribeiro7.github.io/heteroTests/reference/plotResidualsFittedEnhanced.md)
  : Enhanced residuals vs fitted plot (deprecated)
- [`plotSpreadLevel()`](https://diogoribeiro7.github.io/heteroTests/reference/plotSpreadLevel.md)
  : Spread-Level plot for variance diagnostics
- [`plotResidualDensity()`](https://diogoribeiro7.github.io/heteroTests/reference/plotResidualDensity.md)
  : Density plot of residuals
- [`plotResidualQQ()`](https://diogoribeiro7.github.io/heteroTests/reference/plotResidualQQ.md)
  : QQ plot of residuals
- [`plotBubbleVariance()`](https://diogoribeiro7.github.io/heteroTests/reference/plotBubbleVariance.md)
  : Bubble plot of residual variance by covariate
- [`plotDiagnosticSuite()`](https://diogoribeiro7.github.io/heteroTests/reference/plotDiagnosticSuite.md)
  : Generate a suite of diagnostic plots
- [`plotDiagnosticSuiteEnhanced()`](https://diogoribeiro7.github.io/heteroTests/reference/plotDiagnosticSuiteEnhanced.md)
  : Enhanced diagnostic plot suite (deprecated)
- [`plotBeforeAfter()`](https://diogoribeiro7.github.io/heteroTests/reference/plotBeforeAfter.md)
  : Compare residuals before and after remediation
- [`autoplot(`*`<hetero_test_suite>`*`)`](https://diogoribeiro7.github.io/heteroTests/reference/autoplot.hetero_test_suite.md)
  [`autoplot(`*`<hetero_grouped_suite>`*`)`](https://diogoribeiro7.github.io/heteroTests/reference/autoplot.hetero_test_suite.md)
  : Autoplot heteroscedasticity diagnostics
- [`theme_hetero()`](https://diogoribeiro7.github.io/heteroTests/reference/theme_hetero.md)
  : Heteroscedasticity diagnostic theme
- [`scale_colour_hetero_diagnostic()`](https://diogoribeiro7.github.io/heteroTests/reference/scale_colour_hetero_diagnostic.md)
  [`scale_fill_hetero_diagnostic()`](https://diogoribeiro7.github.io/heteroTests/reference/scale_colour_hetero_diagnostic.md)
  : Colour scale for heteroscedasticity diagnostics

## Reporting and data

- [`generateDiagnosticReport()`](https://diogoribeiro7.github.io/heteroTests/reference/generateDiagnosticReport.md)
  : Automated diagnostic report generation
- [`downloadTeachingData()`](https://diogoribeiro7.github.io/heteroTests/reference/downloadTeachingData.md)
  : Download example teaching dataset
- [`diagnostic_data`](https://diogoribeiro7.github.io/heteroTests/reference/diagnostic_data.md)
  : Simulated dataset with collinearity and mild nonlinearity
- [`hetero_data`](https://diogoribeiro7.github.io/heteroTests/reference/hetero_data.md)
  : Simulated dataset with heteroscedastic errors
- [`print(`*`<htest_enhanced>`*`)`](https://diogoribeiro7.github.io/heteroTests/reference/print.htest_enhanced.md)
  : Print method for enhanced White test results

## Dashboard and benchmarks

To move to a companion package, once it has a name.

- [`launchDiagnosticDashboard()`](https://diogoribeiro7.github.io/heteroTests/reference/launchDiagnosticDashboard.md)
  : Interactive diagnostic dashboard

- [`run_benchmark_suite()`](https://diogoribeiro7.github.io/heteroTests/reference/run_benchmark_suite.md)
  [`generate_benchmark_report()`](https://diogoribeiro7.github.io/heteroTests/reference/run_benchmark_suite.md)
  :

  Benchmark heteroTests against reference implementations

## Package infrastructure

- [`ht_set_log_level()`](https://diogoribeiro7.github.io/heteroTests/reference/ht_logging.md)
  [`ht_enable_log_capture()`](https://diogoribeiro7.github.io/heteroTests/reference/ht_logging.md)
  [`ht_log_history()`](https://diogoribeiro7.github.io/heteroTests/reference/ht_logging.md)
  [`ht_clear_log_history()`](https://diogoribeiro7.github.io/heteroTests/reference/ht_logging.md)
  : Control logging for heteroTests diagnostics
- [`std_error()`](https://diogoribeiro7.github.io/heteroTests/reference/std_error.md)
  : Generate standardized error message (deprecated)
- [`std_warning()`](https://diogoribeiro7.github.io/heteroTests/reference/std_warning.md)
  : Generate standardized warning message (deprecated)
- [`validateTestInputs()`](https://diogoribeiro7.github.io/heteroTests/reference/validateTestInputs.md)
  : Validate model and data inputs before running diagnostics
  (deprecated)
- [`checkData()`](https://diogoribeiro7.github.io/heteroTests/reference/checkData.md)
  : Validate data argument (deprecated)
- [`rcalculateEffectSize()`](https://diogoribeiro7.github.io/heteroTests/reference/rcalculateEffectSize.md)
  : Effect size calculations for heteroscedasticity diagnostics
- [`restimate_test_power()`](https://diogoribeiro7.github.io/heteroTests/reference/restimate_test_power.md)
  : Estimate test power from observed effect size
- [`rvalidate_against_reference()`](https://diogoribeiro7.github.io/heteroTests/reference/rvalidate_against_reference.md)
  : Validate diagnostics against reference implementations

## Internal

- [`.diagnostic_registry`](https://diogoribeiro7.github.io/heteroTests/reference/dot-diagnostic_registry.md)
  : Diagnostics registry
- [`algorithms_bibliography`](https://diogoribeiro7.github.io/heteroTests/reference/algorithms_bibliography.md)
  : Bibliography of Diagnostic Algorithms
- [`bootstrap_white_test()`](https://diogoribeiro7.github.io/heteroTests/reference/bootstrap_white_test.md)
  : Bootstrap implementation for White test
- [`checkNumericVector()`](https://diogoribeiro7.github.io/heteroTests/reference/checkNumericVector.md)
  : Validate numeric vector
- [`check_memory_usage()`](https://diogoribeiro7.github.io/heteroTests/reference/check_memory_usage.md)
  : Check memory usage and warn for large datasets
- [`clearValidationCache()`](https://diogoribeiro7.github.io/heteroTests/reference/clearValidationCache.md)
  : Reset cached validation results
- [`.ht_null`](https://diogoribeiro7.github.io/heteroTests/reference/dot-ht_null.md)
  : Ecosystem integration helpers
- [`ht_log()`](https://diogoribeiro7.github.io/heteroTests/reference/ht_log.md)
  : Log a formatted message
- [`interpret_white_test()`](https://diogoribeiro7.github.io/heteroTests/reference/interpret_white_test.md)
  : Interpret White test results
- [`messages`](https://diogoribeiro7.github.io/heteroTests/reference/messages.md)
  : Standardized error and warning messages
- [`rTEST_REQUIREMENTS`](https://diogoribeiro7.github.io/heteroTests/reference/rTEST_REQUIREMENTS.md)
  : Test-specific sample size requirements
- [`rhandleMissingValues()`](https://diogoribeiro7.github.io/heteroTests/reference/rhandleMissingValues.md)
  : Handle missing values according to a specified strategy
- [`rprocessValidationResult()`](https://diogoribeiro7.github.io/heteroTests/reference/rprocessValidationResult.md)
  : Process validation helper outputs
- [`rvalidateDataInputs()`](https://diogoribeiro7.github.io/heteroTests/reference/rvalidateDataInputs.md)
  : Validate data inputs for heteroscedasticity tests
- [`rvalidateDistributionalAssumptions()`](https://diogoribeiro7.github.io/heteroTests/reference/rvalidateDistributionalAssumptions.md)
  : Validate distributional assumptions
- [`rvalidateGroupingVariable()`](https://diogoribeiro7.github.io/heteroTests/reference/rvalidateGroupingVariable.md)
  : Validate grouping variable structure
- [`rvalidateModelInputs()`](https://diogoribeiro7.github.io/heteroTests/reference/rvalidateModelInputs.md)
  : Validate regression model inputs
- [`rvalidateSampleSize()`](https://diogoribeiro7.github.io/heteroTests/reference/rvalidateSampleSize.md)
  : Validate test-specific sample size requirements
- [`rvalidateTestRequirements()`](https://diogoribeiro7.github.io/heteroTests/reference/rvalidateTestRequirements.md)
  : Validate test-specific requirements
- [`safe_lm()`](https://diogoribeiro7.github.io/heteroTests/reference/safe_lm.md)
  : Safely fit a linear model
- [`safe_var()`](https://diogoribeiro7.github.io/heteroTests/reference/safe_var.md)
  : Safe variance calculation
