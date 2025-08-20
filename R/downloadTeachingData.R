#' Download example teaching dataset
#'
#' Provides convenient access to example datasets hosted online. Currently
#' supports the 'tips' dataset used in various tutorials.
#'
#' @param name Name of the dataset to download. Only "tips" is currently
#'   supported.
#' @param destfile Optional path to save the downloaded dataset. Defaults to a
#'   temporary file.
#' @param quiet Logical; if FALSE, informative messages are printed during the
#'   download process.
#' @return Path to the downloaded dataset on success, or `NULL` invisibly if the
#'   download fails or no internet connection is available.
#' @examples
#' \donttest{
#' # Online example (requires internet)
#' if (curl::has_internet()) {
#'   data_path <- downloadTeachingData(quiet = TRUE)
#'   if (!is.null(data_path)) {
#'     # Use downloaded data
#'   }
#' }
#' }
#'
#' # Offline example using built-in data
#' data(mtcars)
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' result <- performWhiteTest(model, mtcars)
#' print(result)
#' @export
downloadTeachingData <- function(name = "tips", destfile = tempfile(fileext = ".csv"), quiet = FALSE) {
  url <- switch(name,
    tips = "https://raw.githubusercontent.com/mwaskom/seaborn-data/master/tips.csv",
    stop("Unknown dataset name")
  )

  if (!curl::has_internet()) {
    if (!quiet) {
      message("No internet connection available. Skipping data download.")
    }
    return(invisible(NULL))
  }

  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = 30)

  tryCatch({
    utils::download.file(url, destfile, quiet = quiet, mode = "wb")
    if (!quiet) {
      message("Data downloaded successfully to: ", destfile)
    }
    destfile
  }, error = function(e) {
    if (!quiet) {
      message("Download failed: ", e$message)
      message("Please check your internet connection and try again.")
    }
    invisible(NULL)
  })
}
