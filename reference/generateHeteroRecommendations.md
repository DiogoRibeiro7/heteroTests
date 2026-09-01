# Intelligent recommendations for heteroscedasticity diagnostics

These helpers assemble an end-to-end recommendation workflow that
profiles the input dataset, selects appropriate heteroscedasticity
diagnostics, interprets results with qualitative confidence labels, and
suggests remediation strategies with decision-tree style guidance.

## Usage

``` r
analyseDatasetCharacteristics(data, response = NULL)

suggestDiagnosticsForProfile(profile)

interpretHeteroTestResults(test_results, alpha = 0.05)

recommendRemediationStrategies(test_results, model, data, profile = NULL)

warnDiagnosticAssumptions(profile, interpretations, model, data, test_results)

buildDiagnosticDecisionTree(profile, recommendations)

composeDiagnosticNarrative(profile, interpretations, remediation,
  assumption_warnings, decision_tree)

generateHeteroRecommendations(model, data = NULL, test_results = NULL,
  alpha = 0.05, include_report = TRUE)
```

## Arguments

- data:

  A `data.frame` containing the modelling variables.

- response:

  Optional response variable name used for labelling.

- profile:

  Dataset profile produced by `analyseDatasetCharacteristics()`.

- test_results:

  Named list of `htest` objects, typically returned by
  [`runHeteroTests()`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md).

- alpha:

  Significance level applied when interpreting p-values.

- model:

  Fitted `lm`/`glm` object or model formula.

- interpretations:

  Output from `interpretHeteroTestResults()`.

- recommendations:

  Output from `suggestDiagnosticsForProfile()`, used to build the
  decision tree.

- remediation:

  Output from `recommendRemediationStrategies()`.

- assumption_warnings:

  Character vector of generated warnings.

- decision_tree:

  Data frame returned by `buildDiagnosticDecisionTree()`.

- include_report:

  Logical; include a plain-language narrative in the output.

## Value

`analyseDatasetCharacteristics()` returns a list describing dataset
size, missingness, skewness, and risk factors.
`suggestDiagnosticsForProfile()` returns a data frame of recommended
diagnostics with rationales. `interpretHeteroTestResults()` yields a
tidy data frame of interpretations and an overall summary.
`recommendRemediationStrategies()` produces a structured remediation
plan. `warnDiagnosticAssumptions()` returns warnings about assumption
violations. `buildDiagnosticDecisionTree()` constructs a tidy decision
tree. `composeDiagnosticNarrative()` outputs a plain-language character
vector, and `generateHeteroRecommendations()` returns an object of class
`hetero_recommendation_report` bundling all components.

## Details

The recommendation engine analyses dataset characteristics such as
sample size, missingness, and predictor skewness to propose diagnostics
tailored to the context. It automatically interprets test outcomes,
attaches qualitative confidence labels, warns when assumptions are at
risk, and suggests remediation strategies matched to detected variance
patterns. The generated decision tree and narrative target
non-specialist audiences who need actionable guidance.

## See also

[`runHeteroTests()`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md),
[`suggestRemediation()`](https://diogoribeiro7.github.io/heteroTests/reference/suggestRemediation.md),
[`generateDiagnosticReport()`](https://diogoribeiro7.github.io/heteroTests/reference/generateDiagnosticReport.md)

## Examples

``` r
if (FALSE) { # \dontrun{
model <- lm(mpg ~ wt + hp, data = mtcars)
recs <- generateHeteroRecommendations(model, mtcars)
print(recs)
} # }
```
