% Generated manually
\name{runSurveyHeteroTests}
\alias{runSurveyHeteroTests}
\title{Run heteroscedasticity diagnostics on the data of a survey design}
\description{
A convenience wrapper that takes the data out of a
\code{survey::svydesign()} object, fits the model by ordinary least squares and
runs \code{\link{runHeteroTests}}. \strong{It does not use the sampling
weights, strata or clusters of the design}, and warns when the design has
unequal weights or clusters.
}
\usage{
runSurveyHeteroTests(formula, design, tests = c("white", "breusch_pagan"), ...)
}
\arguments{
  \item{formula}{Model formula specifying the mean structure.}
  \item{design}{A \code{survey::svydesign()} object. Only its data are used.}
  \item{tests}{Diagnostic names passed on to \code{runHeteroTests}.}
  \item{...}{Additional arguments forwarded to \code{runHeteroTests}.}
}
\details{
The tests are the ones \code{\link{runHeteroTests}} runs on any \code{lm}: they
treat the observations as an independent sample with equal weights. They hold
their level on survey data when the design can be ignored given the regressors,
that is when the chance of being sampled depends only on variables in the
model, and the sample has no clusters. When selection depends on the response,
or observations are clustered, they reject a model with constant error variance
far more often than their nominal level.

The package has no design-based test of constant variance. Sampling weights are
not precision weights: they say how many population units an observation stands
for and nothing about its error variance. Passing them to
\code{lm(weights = )}, or testing a \code{survey::svyglm()} fit, makes the
tests examine residuals multiplied by the square roots of the weights, and a
homoscedastic model is then rejected whenever the weights vary with the
regressors. \code{svyglm} fits are refused for that reason.

\code{inst/validation/survey-designs-size.R} measures all of this, together
with two design-based tests that were tried and not adopted because they
reject too often at the sample sizes examined.
}
\section{Earlier versions}{
Up to 0.12.0 this page described a survey-weighted fit. The weights did not
reach the fit, which was by ordinary least squares, as it is now, so the
reported values are unchanged.
}
\value{
An object of class \code{hetero_test_suite}.
}
\seealso{\code{\link{runHeteroTests}}}
\examples{
\donttest{
if (requireNamespace("survey", quietly = TRUE)) {
  data(api, package = "survey")
  design <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat)
  # Warns: the weights of this design are unequal and are not used.
  res <- runSurveyHeteroTests(api00 ~ api99 + ell, design)
  generics::tidy(res)
}
}
}
