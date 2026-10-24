\name{runPanelTests}
\alias{runPanelTests}
\title{Run panel-data heteroscedasticity tests}
\description{Breusch-Pagan LM and Pesaran CD tests for panel models.}
\usage{runPanelTests(model, data, id, time = NULL,
          tests = c("bp_random", "pesaran"))}
\arguments{
  \item{model}{A fitted \code{lm} object.}
  \item{data}{Data frame used to fit \code{model}. The two tests read \code{id} and \code{time} from the rows of \code{data} that hold the observations of the fit: since 0.16.0, data that are given are matched to the observations of the fit, by row name in any order, or by the row numbers or in the order of the fit for rows that only number themselves, as described under \code{data} in \code{\link{performPesaranTest}()}.}
  \item{id}{Column identifying individuals.}
  \item{time}{Column identifying time periods for the Pesaran test, whose pairs of \code{id} and \code{time} must each hold one observation of the fit.}
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
