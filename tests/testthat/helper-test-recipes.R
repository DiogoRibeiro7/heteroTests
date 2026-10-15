# One recipe per exported perform*Test(): how to call it on the design of
# contract_data(). test-return-contract.R checks what each one returns and
# test-input-validation.R what each one does with a bad input.

contract_data <- function() {
  set.seed(20261015)
  n <- 120
  d <- data.frame(
    x1 = runif(n, 0.5, 2), x2 = rnorm(n),
    group = factor(rep(letters[1:4], length.out = n)),
    id = rep(1:12, each = 10), time = rep(1:10, 12)
  )
  d$y <- 1 + d$x1 - d$x2 + rnorm(n, sd = d$x1)
  list(d = d, m = lm(y ~ x1 + x2, data = d))
}

contract_recipes <- list(
  performWhiteTest = function(m, d) performWhiteTest(m, d),
  performWhiteTestStreaming = function(m, d) performWhiteTestStreaming(m, d, progress = FALSE),
  performWhiteTestRobust = function(m, d) performWhiteTestRobust(m, d),
  performBPTest = function(m, d) performBPTest(m, d),
  performBPTestStreaming = function(m, d) performBPTestStreaming(m, d, progress = FALSE),
  performBreuschPaganTest = function(m, d) performBreuschPaganTest(m, d),
  performBPTestRobust = function(m, d) performBPTestRobust(m, d),
  performGQTest = function(m, d) performGQTest(m, d, order_by = "x1"),
  performKoenkerTest = function(m, d) performKoenkerTest(m, d),
  performKoenkerTestStreaming = function(m, d) performKoenkerTestStreaming(m, d, progress = FALSE),
  performParkTest = function(m, d) performParkTest(m, d, "x1"),
  performStudentizedBPTest = function(m, d) performStudentizedBPTest(m, d),
  performWhiteTestBootstrap = function(m, d) performWhiteTestBootstrap(m, d, B = 19),
  performSzroeterTest = function(m, d) performSzroeterTest(m, d, order_by = "x1"),
  performHarrisonMcCabeTest = function(m, d) performHarrisonMcCabeTest(m, d, order_by = "x1"),
  performWildBootstrapTest = function(m, d) {
    performWildBootstrapTest(m, d, B = 19, progress = FALSE)
  },
  performQuantileRegressionTest = function(m, d) {
    testthat::skip_if_not_installed("quantreg")
    performQuantileRegressionTest(m, d)
  },
  performRankPermutationTest = function(m, d) {
    performRankPermutationTest(m, d, B = 19, progress = FALSE)
  },
  performHighDimensionalTest = function(m, d) performHighDimensionalTest(m, d),
  performSpatialHeteroTest = function(m, d) {
    testthat::skip_if_not_installed("spdep")
    set.seed(1)
    coords <- cbind(runif(nrow(d)), runif(nrow(d)))
    lw <- suppressWarnings(spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(coords, k = 4))))
    performSpatialHeteroTest(m, d, listw = lw, permutations = 19)
  },
  performSpearmanTest = function(m, d) performSpearmanTest(m, d),
  performLeveneTest = function(m, d) performLeveneTest(m, d, "group"),
  performGlejserTest = function(m, d) performGlejserTest(m, d, "x1"),
  performBrownForsytheTest = function(m, d) performBrownForsytheTest(m, d, "group"),
  performFlignerKilleenTest = function(m, d) performFlignerKilleenTest(m, d, "group"),
  performBartlettTest = function(m, d) performBartlettTest(m, d, "group"),
  performHarveyTest = function(m, d) performHarveyTest(m, d),
  performArchLMTest = function(m, d) performArchLMTest(m, d, lags = 2),
  performMcLeodLiTest = function(m, d) performMcLeodLiTest(m, d, lags = 5),
  performHartleyFmaxTest = function(m, d) performHartleyFmaxTest(m, d, "group"),
  performOBrienTest = function(m, d) performOBrienTest(m, d, "group"),
  performCookWeisbergTest = function(m, d) performCookWeisbergTest(m, d),
  performNCVTest = function(m, d) performNCVTest(m, d),
  performSpreadLevelTest = function(m, d) performSpreadLevelTest(m, d),
  performPesaranTest = function(m, d) performPesaranTest(m, d, "id", "time"),
  performBPRandomEffectsTest = function(m, d) performBPRandomEffectsTest(m, d, "id"),
  performDavidianCarrollTest = function(m, d) performDavidianCarrollTest(m, d),
  performVarianceFormTest = function(m, d) performVarianceFormTest(m, d),
  performRESETTest = function(m, d) performRESETTest(m),
  performBoxMTest = function(m, d) performBoxMTest(d[, c("y", "x2")], d$group)
)
