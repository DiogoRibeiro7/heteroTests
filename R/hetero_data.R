#' Simulated dataset with heteroscedastic errors
#'
#' This dataset is generated from the model
#' \deqn{y_i = 1 + 2 x_i + \varepsilon_i}
#' where \eqn{\varepsilon_i \sim N(0, (0.5 + 2 x_i)^2)}. A fixed seed is used so
#' that \code{data(hetero_data)} returns the same values every time.
#'
#' @format A data frame with 100 rows and 2 variables:
#' \describe{
#'   \item{x}{predictor}
#'   \item{y}{response}
#' }
#'
#' @source Simulated with \code{set.seed(42); runif()} and \code{rnorm()}.
#' @usage data(hetero_data)
#' @examples
#' data(hetero_data)
#' plot(hetero_data$x, hetero_data$y)
#'
#' @name hetero_data
NULL
