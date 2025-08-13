\name{diagnostic_data}
\alias{diagnostic_data}
\docType{data}
\title{Simulated dataset with collinearity and mild nonlinearity}
\description{This dataset contains two predictors with correlation and a response including a quadratic term. It is useful for testing diagnostics beyond heteroscedasticity.}
\usage{data(diagnostic_data)}
\format{A data frame with 150 rows and 3 variables:
  \describe{
    \item{x1}{first predictor}
    \item{x2}{second predictor, correlated with \code{x1}}
    \item{y}{response}
  }}
\source{Simulated via \code{set.seed(123)}.}
\examples{
data(diagnostic_data)
head(diagnostic_data)
}
