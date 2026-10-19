\name{performRESETTest}
\alias{performRESETTest}
\title{Ramsey's RESET test for nonlinearity}
\description{Adds powers of the fitted values and performs an F-test to check for neglected nonlinearity.}
\details{On a fit with weights the augmented model is fitted by weighted least squares and both residual sums of squares are weighted. Releases before 0.12.0 refitted the augmented model without the weights, so the two models were not nested.

On a fit with an offset the fitted values are the design times the coefficients plus the offset, and the test is the nested F test of the model with its offset against the model augmented with the powers of those fitted values, fitted with the same offset and weights (since 0.16.0, decided 2026-10-18). It is not RESET of the model fitted to the response minus the offset, which augments that model with the powers of the design times the coefficients alone. Before 0.16.0 the augmented model was fitted without the offset, so the two models were not nested.}
\usage{performRESETTest(model, data = NULL, power = 2:3)}
\arguments{\item{model}{A fitted \code{lm} model.}\item{data}{optional data frame the model was fitted to, or \code{NULL}, the default. The statistic is computed from the response, fitted values, design, weights and offset of the fit itself, so \code{data} does not change the result; it is accepted so that every test takes \code{(model, data, ...)}, and must hold the variables of the model and the row names of the fit. Before 0.15.0 the second argument was \code{power}: a number passed there by position is still read as \code{power} in 0.15.x, with a warning that shows the new call, and stops working in 1.0.0.}\item{power}{Numeric vector of powers to include.}}
\value{An object of class \code{htest}.}
\section{Supported models}{
It accepts an \code{lm} and a \code{glm} with the Gaussian family and the identity link, weighted or
not, a \code{fitWLS()} fit and, since 0.16.0, a rank-deficient fit, which it tests on the estimable
part of its design: it gives the value of the same model fitted without the aliased columns. A fit
with a zero weight is accepted too: the test compares two weighted least-squares fits, which both
leave the observation out, and gives the value of the fit to the observations with positive weight
(kept so, decided on 2026-10-17). It accepts a model with an offset, given as an argument or as an
\code{offset()} term, weighted or not, and tests the model with its offset (since 0.16.0; see
\emph{Details}). It refuses a \code{glm} of another family or link, a \code{survey::svyglm()} fit,
a coefficient that is not finite for another reason, \code{data} that lack rows or variables of
the fit, and, since 0.16.0, a fit with \code{model = FALSE} whose model frame, rebuilt from the
data it was fitted to, no longer holds its response, because those data changed after the fit:
it read that frame and returned a value.
It refuses a multivariate \code{lm} ("needs a model with one response"), which it tested without
\code{data} before 0.16.0, and, since 0.16.0, an \code{mgcv::gam()} fit with smooth terms: a
smooth term is usually penalised, and a penalised fit is not the least-squares fit of the design
the test fits again. A later step settles which tests take such a fit; a \code{gam} without smooth
terms is tested as the Gaussian \code{glm} it is.
Which model each test accepts, and why, is set out in \code{\link{test_results}}, under \emph{Supported
models}.
}
\examples{data(mtcars)
m <- lm(mpg ~ wt + qsec, data = mtcars)
performRESETTest(m)}
