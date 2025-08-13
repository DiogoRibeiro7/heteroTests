\name{simulate_hetero}
\alias{simulate_hetero}
\alias{simulate_arch1}
\alias{sigma_linear}
\alias{sigma_exponential}
\alias{sigma_group}
\alias{sigma_piecewise}
\alias{sigma_poly}
\alias{sigma_sin}
\alias{sigma_multiplicative}
\alias{sigma_spatial}
\alias{sigma_logistic}
\alias{sigma_inverse}
\alias{sigma_power}
\alias{sigma_step}
\alias{sigma_u_shape}
\alias{sigma_exp_decay}
\alias{sigma_gaussian_peak}
\alias{sigma_piecewise_linear}
\title{Simulate heteroscedastic data}
\description{Utilities to generate data with non-constant variance patterns. \code{simulate_hetero} simulates a linear regression model with heteroscedastic errors using a user-supplied variance function. The helper functions prefixed by \code{sigma_} implement common variance patterns and can be passed to \code{sigma_func}. \code{simulate_arch1} creates a simple ARCH(1) time series.}
\usage{
simulate_hetero(n, beta0, beta1, sigma_func, seed = NULL)

simulate_arch1(n, mu = 0, alpha0 = 0.5, alpha1 = 0.3, seed = NULL)

sigma_linear(x)

sigma_exponential(x)

sigma_group(x)

sigma_piecewise(x)

sigma_poly(x, a = 0.5, b = 0.1, c = 0.02)

sigma_sin(x, A = 1, B = 0.5, omega = 2 * pi/10, phi = 0)

sigma_multiplicative(x, mu_func, p = 1)

sigma_spatial(coords, gamma0 = 0.5, gamma1 = 0.1)

sigma_logistic(x, L = 2, k = 1, x0 = 5)

sigma_inverse(x, a = 1, b = 1)

sigma_power(x, a = 0.5, p = 0.5)

sigma_step(x, thr = 5, low = 0.5, high = 2)

sigma_u_shape(x, a = 0.1, b = 0.05, center = 5)

sigma_exp_decay(x, a = 2, b = 0.2)

sigma_gaussian_peak(x, base = 0.5, height = 1, mu = 5, sd = 1)

sigma_piecewise_linear(x, thr = 5, slope1 = 0.1, slope2 = 0.3)
}
\arguments{
\item{n}{Number of observations for \code{simulate_hetero}.}
\item{beta0}{Intercept of the regression.}
\item{beta1}{Slope of the regression.}
\item{sigma_func}{Function returning the standard deviation for each \code{x}.}
\item{seed}{Optional integer seed for reproducibility.}
\item{n}{Length of the time series for \code{simulate_arch1}.}
\item{mu}{Mean of the ARCH(1) process.}
\item{alpha0}{Constant term in the ARCH variance equation.}
\item{alpha1}{ARCH coefficient (must be in [0,1)).}
\item{x}{Numeric vector used by the \code{sigma_*} functions.}
\item{coords}{Matrix or data frame with columns \code{x} and \code{y} for \code{sigma_spatial}.}
\item{mu_func}{Function returning mean values for \code{sigma_multiplicative}.}
\item{p}{Exponent for \code{sigma_multiplicative} or \code{sigma_power}.}
\item{a,b,L,k,x0,thr,low,high,center,base,height,mu,sd,slope1,slope2,gamma0,gamma1}{Additional numeric parameters controlling the shape of the variance patterns.}
}
\value{For \code{simulate_hetero} a data frame with columns \code{x} and \code{y}. For \code{simulate_arch1} a data frame with columns \code{time}, \code{y} and \code{sigma}. The \code{sigma_*} helpers return numeric vectors of standard deviations.}
\examples{
set.seed(1)
hetero <- simulate_hetero(200, 1, 2, sigma_linear)
head(hetero)

# Other variance patterns
simulate_hetero(100, 0, 1, sigma_logistic)
simulate_arch1(50)
}
