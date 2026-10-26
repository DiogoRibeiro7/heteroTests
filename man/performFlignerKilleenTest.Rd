\name{performFlignerKilleenTest}
\alias{performFlignerKilleenTest}
\title{Perform Fligner-Killeen test for homogeneity of variances}
\description{The median-centred Fligner-Killeen test of homogeneity of variances, on the Pearson residuals of the fit, computed with \code{\link[stats]{fligner.test}()}: it ranks the absolute deviations of the residuals from their group medians and scores the ranks by quantiles of the normal distribution, which makes it robust to outliers and to errors that are not normal.}
\details{The test is the median-centred version of the test of Fligner and Killeen (1976), the "F-K:med \eqn{X^2}" of Conover, Johnson and Johnson (1981), whose simulation study found it among the tests of homogeneity of variances most robust to departures from normality; it is the test \code{\link[stats]{fligner.test}()} computes. The residuals of each group are centred at the median of the group, the absolute values of the centred residuals are ranked over all \eqn{n} observations, and the rank \eqn{r_i} is given the score \eqn{a_i = \Phi^{-1}\{(1 + r_i / (n + 1)) / 2\}}. With \eqn{\bar a_j} the mean score of group \eqn{j}, of size \eqn{n_j}, \eqn{\bar a} the mean of all the scores and \eqn{s_a^2 = \sum_i (a_i - \bar a)^2 / (n - 1)}, the statistic
\deqn{X^2 = \sum_j n_j (\bar a_j - \bar a)^2 / s_a^2}{X^2 = sum_j n_j (abar_j - abar)^2 / s_a^2}
is referred to a chi-squared distribution with \eqn{k-1} degrees of freedom, for \eqn{k} groups. No logarithm is taken. The observations whose group is missing are left out before the statistic is computed.

Tied values are given the mean of their ranks. Since 0.16.0 the deviations are those of the residuals \eqn{r_i = \sqrt{w_i}\,(y_i - x_i^\top \hat\beta - o_i)}{r_i = sqrt(w_i) (y_i - x_i'b - o_i)} computed observation by observation, as the Gaussian \code{glm} computes them, and not the residuals an \code{lm} stores, which its QR decomposition rounds differently from one observation to the next; and observations with the same values of the model's variables, weight and offset, such as repeated values of an integer response or of a response recorded with few digits, are given the same residual, as in exact arithmetic, even where a basis such as \code{poly()} gives them design rows that differ in their last bits. For an \code{mgcv::gam()} fit \eqn{x_i} is the row of its linear predictor matrix. And absolute deviations closer than \eqn{8 \epsilon s_j}{8 eps s_j}, with \eqn{\epsilon}{eps} = \code{.Machine$double.eps} and \eqn{s_j} the largest \eqn{|r_i|} of group \eqn{j} (the smaller of the two for deviations of different groups), are ranked as tied: in a group of even size the two residuals in the middle lie at the same distance from the median of the group, half their difference, a tie that the centring breaks by up to \eqn{\epsilon s_j}{eps s_j}. On data with repeated observations the statistic can differ much from before: on \code{lm(extra ~ group, data = sleep)} it was 0.154 and is 0.213, the value of exact ties. On continuous data it moves by much less (see \code{NEWS}). Where neither changes a rank, the value is that of \code{\link[stats]{fligner.test}()} on the residuals of the fit, as before. Two limits remain. A tie between different observations that exact decimal arithmetic would give, as when two pairs of observations of a regressor recorded with few digits differ by the same amounts, is restored only when rounding leaves it within the tolerance. And in a group whose residuals are far from zero, \eqn{10^8} times their spread, the tolerance may also tie two deviations that differ by a few units in the last place of the residuals.}
\usage{performFlignerKilleenTest(model, data = NULL, group)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{The data frame \code{model} was fitted to, or \code{NULL}, the default. The residuals and weights are read from the fit itself, and \code{group} from the rows of \code{data} that hold the observations of the fit; the observations whose group is missing are left out, with a warning. Without \code{data} it is read from the model frame of \code{model}, which holds it only when it is a variable of the model under its own name (\code{x}, not \code{log(x)}); a grouping variable the model does not use needs \code{data}. Since 0.16.0, data that are given are matched to the observations of the fit: rows with row names of their own by name, in any order; rows that only number themselves (a tibble, a data.table, a data frame whose row names were reset) by the row numbers of the fit, or in the order of the fit when they hold one row per observation. The rows the fit dropped may be there or not. Data whose rows cannot be shown to hold the observations of the fit are refused; see \code{\link{test_results}}.}
  \item{group}{name of the grouping variable.}
}
\value{An object of class \code{htest} containing the chi-squared statistic, p-value and degrees of freedom.}
\section{Supported models}{
It accepts an \code{lm} and a \code{glm} with the Gaussian family and the identity link, unweighted
or with positive weights, a \code{fitWLS()} fit and, since 0.16.0, a rank-deficient fit, which it
tests on the estimable part of its design: it gives the value of the same model fitted without the
aliased columns. It refuses a \code{glm} of another family or link, a \code{survey::svyglm()} fit, a
fit with a zero weight, a coefficient that is not finite for another reason, and \code{data} that
lack rows or variables of the fit.
It refuses a multivariate \code{lm} ("needs a model with one response"), since 0.16.0; before, it stopped with a coercion error. An \code{mgcv::gam()} fit is tested as before 0.16.0, with smooth terms too, until a later step settles which tests take such a fit; since 0.16.0 its observations are matched to the rows of \code{data} by the row names of its model frame. Before, they were matched by position, since the residuals of a \code{gam} carry no names, and \code{data} with the rows of the fit in another order gave a wrong value.
Which model each test accepts, and why, is set out in
\code{\link{test_results}}, under \emph{Supported models}.
}
\examples{
 data(mtcars)
 mtcars$cyl <- factor(mtcars$cyl)
 m <- lm(mpg ~ wt, data = mtcars)
 performFlignerKilleenTest(m, mtcars, "cyl")
}
\references{Conover, W. J., Johnson, M. E., & Johnson, M. M. (1981). A comparative study of tests for homogeneity of variances, with applications to the outer continental shelf bidding data. \emph{Technometrics}, 23(4), 351--361. \doi{10.1080/00401706.1981.10487680}

Fligner, M. A., & Killeen, T. J. (1976). Distribution-free two-sample tests for scale. \emph{Journal of the American Statistical Association}, 71(353), 210--213. \doi{10.1080/01621459.1976.10481517}}
