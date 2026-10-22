# The sections of the API reference, in the order the site lists them.
#
# tools/build-docs.R reads this file to write docs/reference/index.md and the
# API reference part of the nav in mkdocs.yml. It stops if a help topic in
# man/ is in no section, and tests/testthat/test-reference-index.R checks that
# every exported object is in one, so a new topic has to be placed here on
# purpose.
#
# Each entry is a section (`title`) or a subsection of the section above it
# (`subtitle`), with an optional description (`desc`, markdown) and the topics
# it lists (`contents`: a topic name or alias, as in `?name`). `keyword` adds
# every topic that has that \keyword, in file name order.

list(
  list(
    title = "Tests of the variance",
    desc = paste(
      "Each test returns an `htest` object, whose fields `test_results`",
      "documents. The tests are grouped by the family they belong to."
    )
  ),
  list(
    subtitle = "What every test returns",
    contents = "test_results"
  ),
  list(
    subtitle = "Auxiliary-regression tests",
    contents = c(
      "performWhiteTest", "performWhiteTestRobust", "performWhiteTestStreaming",
      "performBPTest", "performBPTestRobust", "performBPTestStreaming",
      "performKoenkerTest", "performKoenkerTestStreaming",
      "performStudentizedBPTest", "performCookWeisbergTest", "performNCVTest",
      "performHarveyTest", "performGlejserTest", "performParkTest",
      "performVarianceFormTest", "performDavidianCarrollTest",
      "performSpearmanTest", "performSpreadLevelTest",
      "performHighDimensionalTest"
    )
  ),
  list(
    subtitle = "Tests on ordered observations",
    contents = c("performGQTest", "performSzroeterTest", "performHarrisonMcCabeTest")
  ),
  list(
    subtitle = "Group-wise tests",
    contents = c(
      "performLeveneTest", "performBrownForsytheTest", "performFlignerKilleenTest",
      "performBartlettTest", "performHartleyFmaxTest", "performOBrienTest",
      "performBoxMTest"
    )
  ),
  list(
    subtitle = "Time series, panel and spatial tests",
    contents = c(
      "performArchLMTest", "performMcLeodLiTest", "performBPRandomEffectsTest",
      "performPesaranTest", "performSpatialHeteroTest"
    )
  ),
  list(
    subtitle = "Resampling and robust tests",
    contents = c(
      "performWhiteTestBootstrap", "performWildBootstrapTest",
      "performRankPermutationTest", "performQuantileRegressionTest",
      "rbootstrap_test_statistic", "bootstrap_methods", "robust_implementations",
      "modern_diagnostics", "additional_tests"
    )
  ),
  list(
    title = "Model checks",
    desc = paste(
      "Checks of the fitted model that are not tests of the variance: the",
      "functional form, collinearity, influence and the fit itself. They are",
      "listed apart so that they are not mistaken for heteroscedasticity tests."
    ),
    contents = c(
      "performRESETTest", "performVIFDiagnostic", "performInfluenceDiagnostics",
      "performScatterDiagnostic", "checkModel", "checkModelEnhanced"
    )
  ),
  list(
    title = "Running several diagnostics",
    contents = c(
      "runHeteroTests", "runHeteroTestsParallel", "runDiagnostics",
      "runDiagnosticPlots", "runMultivariateTests", "runTimeSeriesTests",
      "runPanelTests", "runSurveyHeteroTests", "runAdvancedDiagnostics",
      "compareModelDiagnostics", "compareTestResults", "analyzeMLResiduals",
      "registerDiagnostic", "registerPlot", "listDiagnostics",
      "HeteroDiagnostic", "test.HeteroDiagnostic", "summary.HeteroDiagnostic",
      "plot.HeteroDiagnostic", "hetero_test_suite", "summary.hetero_test_suite",
      "tidy.hetero_test",
      "cachedTest", "clearTestCache", "clearAnalysisCache", "TestFactory",
      "test_factory"
    )
  ),
  list(
    title = "Remediation",
    contents = c(
      "fitWLS", "fitRobust", "autoTransform", "suggestRemediation",
      "autoCompareRemediations"
    )
  ),
  list(
    title = "Recommendations",
    contents = "generateHeteroRecommendations"
  ),
  list(
    title = "Simulation",
    desc = paste(
      "The simulation functions are named in snake_case on purpose; the rest",
      "of the package uses camelCase."
    ),
    contents = c("simulate_hetero", "simulation_framework", "plot.power_analysis")
  ),
  list(
    title = "Plotting",
    contents = c(
      "plotResidualsFitted", "plotResidualsFittedEnhanced", "plotSpreadLevel",
      "plotResidualDensity", "plotResidualQQ", "plotBubbleVariance",
      "plotDiagnosticSuite", "plotDiagnosticSuiteEnhanced", "plotBeforeAfter",
      "autoplot.hetero_test_suite", "theme_hetero", "scale_colour_hetero_diagnostic"
    )
  ),
  list(
    title = "Reporting and data",
    contents = c(
      "generateDiagnosticReport", "downloadTeachingData", "diagnostic_data",
      "hetero_data", "print.htest_enhanced"
    )
  ),
  list(
    title = "Dashboard and benchmarks",
    desc = "To move to a companion package, once it has a name.",
    contents = c("launchDiagnosticDashboard", "run_benchmark_suite")
  ),
  list(
    title = "Package infrastructure",
    contents = c(
      "ht_set_log_level", "std_error", "std_warning", "validateTestInputs",
      "checkData", "rcalculateEffectSize", "restimate_test_power",
      "rvalidate_against_reference"
    )
  ),
  list(
    title = "Internal",
    contents = ".diagnostic_registry",
    keyword = "internal"
  )
)
