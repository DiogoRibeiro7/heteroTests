#' Download example teaching dataset
#'
#' Provides convenient access to example datasets hosted online. Currently
#' supports the 'tips' dataset used in various tutorials.
#'
#' @param name Name of the dataset to download. Only "tips" is currently
#'   supported.
#' @param destfile Optional path to save the downloaded dataset. Defaults to a
#'   temporary file.
#' @return Path to the downloaded dataset.
#' @examples
#' path <- downloadTeachingData("tips")
#' read.csv(path)
#' @export
downloadTeachingData <- function(name = "tips", destfile = tempfile(fileext = ".csv")) {
  url <- switch(name,
    tips = "https://raw.githubusercontent.com/mwaskom/seaborn-data/master/tips.csv",
    stop("Unknown dataset name")
  )
  utils::download.file(url, destfile, quiet = TRUE)
  destfile
}
