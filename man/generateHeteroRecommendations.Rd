% Generated manually to describe the intelligent recommendation system helpers
\name{generateHeteroRecommendations}
\alias{generateHeteroRecommendations}
\alias{analyseDatasetCharacteristics}
\alias{suggestDiagnosticsForProfile}
\alias{interpretHeteroTestResults}
\alias{recommendRemediationStrategies}
\alias{warnDiagnosticAssumptions}
\alias{buildDiagnosticDecisionTree}
\alias{composeDiagnosticNarrative}
\alias{print.hetero_recommendation_report}
\title{Intelligent recommendations for heteroscedasticity diagnostics}
\description{
These helpers assemble an end-to-end recommendation workflow that profiles the
input dataset, selects appropriate heteroscedasticity diagnostics, interprets
results with qualitative confidence labels, and suggests remediation strategies
with decision-tree style guidance.
}
\usage{
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
}
\arguments{
  \item{data}{A \code{data.frame} containing the modelling variables.}
  \item{response}{Optional response variable name used for labelling.}
  \item{profile}{Dataset profile produced by
    \code{analyseDatasetCharacteristics()}.}
  \item{test_results}{Named list of \code{htest} objects, typically returned by
    \code{runHeteroTests()}.}
  \item{alpha}{Significance level applied when interpreting p-values.}
  \item{model}{Fitted \code{lm}/\code{glm} object or model formula.}
  \item{interpretations}{Output from \code{interpretHeteroTestResults()}.}
  \item{remediation}{Output from \code{recommendRemediationStrategies()}.}
  \item{assumption_warnings}{Character vector of generated warnings.}
  \item{decision_tree}{Data frame returned by
    \code{buildDiagnosticDecisionTree()}.}
  \item{include_report}{Logical; include a plain-language narrative in the
    output.}
}
\value{
\code{analyseDatasetCharacteristics()} returns a list describing dataset size,
missingness, skewness, and risk factors. \code{suggestDiagnosticsForProfile()}
returns a data frame of recommended diagnostics with rationales.
\code{interpretHeteroTestResults()} yields a tidy data frame of interpretations
and an overall summary. \code{recommendRemediationStrategies()} produces a
structured remediation plan. \code{warnDiagnosticAssumptions()} returns
warnings about assumption violations. \code{buildDiagnosticDecisionTree()}
constructs a tidy decision tree. \code{composeDiagnosticNarrative()} outputs a
plain-language character vector, and \code{generateHeteroRecommendations()}
returns an object of class \code{hetero_recommendation_report} bundling all
components.
}
\details{
The recommendation engine analyses dataset characteristics such as sample size,
missingness, and predictor skewness to propose diagnostics tailored to the
context. It automatically interprets test outcomes, attaches qualitative
confidence labels, warns when assumptions are at risk, and suggests remediation
strategies matched to detected variance patterns. The generated decision tree
and narrative target non-specialist audiences who need actionable guidance.
}
\examples{
\dontrun{
model <- lm(mpg ~ wt + hp, data = mtcars)
recs <- generateHeteroRecommendations(model, mtcars)
print(recs)
}
}
\seealso{\code{runHeteroTests()}, \code{suggestRemediation()},
\code{generateDiagnosticReport()}}
