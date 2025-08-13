\name{runPanelTests}
\alias{runPanelTests}
\title{Run panel-data heteroscedasticity tests}
\description{Breusch-Pagan LM and Pesaran CD tests for panel models.}
\usage{runPanelTests(model, data, id, time = NULL,
          tests = c("bp_random", "pesaran"))}
\arguments{
  \item{model}{A fitted \code{lm} object.}
  \item{data}{Data frame used to fit \code{model}.}
  \item{id}{Column identifying individuals.}
  \item{time}{Column identifying time periods for the Pesaran test.}
  \item{tests}{Character vector of test names.}
}
\value{A named list of \code{htest} objects.}
\examples{
 df <- data.frame(id = rep(1:3, each = 4), time = rep(1:4, 3),
                  x = runif(12), y = rnorm(12))
 m <- lm(y ~ x, data = df)
 runPanelTests(m, df, id = "id", time = "time")
}
\seealso{\code{\link{runTimeSeriesTests}}}
