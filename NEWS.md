# heteroTests News

## 0.6.4
- Added "Comprehensive Guide to Heteroscedasticity Testing" and "Troubleshooting Common Issues" vignettes.
- Implemented `performWhiteTestStreaming` for large datasets.
- Added wild bootstrap, HC covariance, quantile regression, permutation, high-dimensional, and spatial heteroscedasticity diagnostics.
- Integrated broom tidiers, ggplot2 theming/autoplot support, tidymodels workflows, survey designs, grouped data pipelines, and
  data.table/dtplyr compatibility for diagnostic workflows.

## 0.6.3
- Added automated report generation via `generateDiagnosticReport`.

## 0.6.2
- Added studentized Breusch-Pagan, bootstrap White and Szroeter tests.

## 0.6.1
- Introduced `TestFactory` R6 class with metadata support.
- Added exported `test_factory` instance registering core tests.
- DESCRIPTION now lists R6 in Suggests.
