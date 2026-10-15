\name{HeteroDiagnostic}
\alias{HeteroDiagnostic}
\title{Create a diagnostic object}
\usage{HeteroDiagnostic(model, data = NULL)}
\arguments{
  \item{model}{A fitted \code{lm}, or a \code{glm} with the Gaussian family and the identity link, or a formula.
  The tests refuse a \code{glm} of any other family or link since 0.15.0.}
  \item{data}{Data used to fit the model when \code{model} is a formula.}
}
\value{An object of class \code{HeteroDiagnostic}.}
\description{Create a diagnostic object that exposes \code{test()}, \code{plot()} and \code{summary()} methods. Formulas are accepted for convenience.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 d <- HeteroDiagnostic(m, mtcars)
 runDiagnostics(d$model, d$data)
}
