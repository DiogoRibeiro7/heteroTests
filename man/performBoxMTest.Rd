\name{performBoxMTest}
\alias{performBoxMTest}
\title{Box's M Test for Equality of Covariance Matrices}
\description{Test whether multiple groups have equal covariance matrices, by Box's (1949) chi-square approximation to his M statistic.}
\details{\code{performBoxMTest(data, group)} is the one test that does not take \code{(model, data, ...)}: it tests the covariance matrices of the data themselves, not the residuals of a fitted model.

With \eqn{g} groups of \eqn{n_i} observations of \eqn{p} variables, \eqn{\nu_i = n_i - 1}{nu_i = n_i - 1}, \eqn{\nu = \sum_i \nu_i}{nu = sum nu_i}, \eqn{S_i} the covariance matrix of group \eqn{i} and \eqn{S} the pooled one, Box's statistic is
\deqn{M = \nu \log|S| - \sum_i \nu_i \log|S_i|,}{M = nu log|S| - sum_i nu_i log|S_i|,}
his correction factor is
\deqn{c_1 = \left(\sum_i \frac{1}{\nu_i} - \frac{1}{\nu}\right) \frac{2p^2 + 3p - 1}{6 (p + 1) (g - 1)},}{c1 = (sum_i 1/nu_i - 1/nu) (2 p^2 + 3 p - 1) / (6 (p + 1) (g - 1)),}
and \eqn{M (1 - c_1)}{M (1 - c1)} is referred to a chi-square distribution with \eqn{f_1 = (g - 1) p (p + 1) / 2}{f1 = (g - 1) p (p + 1) / 2} degrees of freedom: Box's (1949) chi-square approximation. The test assumes that each group is multivariate normal. It gives the values of \code{biotools::boxM()}, \code{heplots::boxM()} and \code{rstatix::box_m()}: 140.9430 on 20 degrees of freedom for the example on \code{iris}.

Before 0.16.0 the correction factor was \eqn{(g - 1) c_1}{(g - 1) c1}: the same for two groups, whose values do not change, and too large for three or more, which made the statistic too small and the test conservative. The example on \code{iris} gave 135.2229.

The groups are the levels of \code{group} that have observations: a level with none, as \code{subset()} leaves on a factor, is dropped (since 0.16.0; before, it made the result \code{NA}). A group of one observation has no covariance matrix, and gives \code{NA}.}
\section{Size}{
In \code{inst/validation/box-m-approximations-size.R}, at 5000 replications per cell and the 5\% level, with
\eqn{g} = 2, 3 or 5 groups of 10 to 100 observations of \eqn{p} = 1 to 10 variables, the test held its level
under Gaussian data in 60 of 67 cells, inside the release gate's interval, 0.042 to 0.058.
\itemize{
  \item \strong{It is liberal when the groups are small for the number of variables.} With groups of 10 and
    4 or 6 variables it rejected a true null hypothesis 0.055 to 0.106 of the time, outside the gate in four
    of those six cells (0.062 to 0.106, the most with five groups of 10 and six variables), and with groups
    of 20 and 10 variables 0.071 to 0.087. Box's F approximation, which the package does not compute, was
    closer to the level in each of those nine cells (0.051 to 0.070); in the other Gaussian cells the two
    differed by at most 0.005.
  \item \strong{It is not robust to heavy tails.} With independent \eqn{t_5} coordinates, three groups of 20,
    50 or 100 and two or four variables, it rejected 0.26 to 0.48 of the time (0.18 to 0.46 with the factor
    of 0.15.0), more with larger groups: the test then detects the tails as well as unequal covariance
    matrices.
  \item \strong{It is conservative with light tails.} On the data of the full sweep
    (\code{inst/validation/full-sweep-size-power.R}), 150 observations of \code{(y, x)} with \code{x}
    uniform and grouped by \code{cut(x, 3)}, it rejected 0.017 of the time, as Box's F did. The sweep's
    0.012, at 400 replications, was measured with the factor of 0.15.0, which gives 0.016 in that design at
    5000 replications: the low size comes from the uniform, light-tailed data, not from the approximation.
}
When the groups are small for the number of variables, or the data may have heavy tails, a rejection is
weak evidence of unequal covariance matrices.
}
\section{Validation}{
\code{tests/testthat/test-boxm.R} checks the statistic against Box's approximation written out for two to
five groups and one to four variables, and against the values of \code{biotools::boxM()} 4.3 and
\code{heplots::boxM()} 1.8.6 on \code{iris} and on \code{mtcars}, and that two groups give the values of
0.15.0. \code{inst/validation/box-m-approximations-size.R} compares the chi-square and the F approximation
with \code{biotools}, \code{heplots} and \code{DFA.CANCOR} on ten data sets, and measures the sizes above.
}
\usage{performBoxMTest(data, group)}
\arguments{
  \item{data}{A numeric data frame or matrix.}
  \item{group}{A factor indicating group membership.}
}
\value{An object of class \code{htest}: \code{statistic}, named \code{M}, is Box's corrected statistic
\eqn{M (1 - c_1)}{M (1 - c1)}; \code{parameter}, named \code{df}, its degrees of freedom \eqn{f_1}{f1};
\code{p.value} the upper tail of the chi-square distribution; and \code{method} and \code{data.name}.}
\references{
Box, G. E. P. (1949). A general distribution theory for a class of likelihood criteria. \emph{Biometrika}, 36(3/4), 317--346. \doi{10.1093/biomet/36.3-4.317}
}
\examples{
data(iris)
performBoxMTest(iris[,1:4], iris$Species)

# Two species: with two groups the values are those of 0.15.0
two <- droplevels(subset(iris, Species != "virginica"))
performBoxMTest(two[, 1:4], two$Species)
}
