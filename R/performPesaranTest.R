#' Perform Pesaran test for cross-sectional dependence
#'
#' This implementation approximates Pesaran's CD test using residuals from a
#' panel model.
#'
#' @param model A fitted model of class `lm`.
#' @param data Data frame used to fit `model`.
#' @param id Character. Column name identifying individuals.
#' @param time Character. Column name identifying time periods.
#'
#' @return An object of class \code{htest} with the Z statistic and p-value.
#' @examples
#' df <- data.frame(
#'   id = rep(1:3, each = 5),
#'   time = rep(1:5, 3),
#'   x = runif(15),
#'   y = rnorm(15)
#' )
#' m <- lm(y ~ x, data = df)
#' performPesaranTest(m, df, "id", "time")
performPesaranTest <- function(model, data, id, time) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class 'lm'.")
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!all(c(id, time) %in% names(data))) {
    stop("`id` and `time` must be columns in `data`.")
  }

  res <- residuals(model)
  df <- data.frame(id = data[[id]], time = data[[time]], res = res)
  df <- df[order(df$time, df$id), ]
  ids <- unique(df$id)
  times <- unique(df$time)
  N <- length(ids)
  T <- length(times)
  mat <- matrix(NA, nrow = T, ncol = N)
  for (i in seq_along(ids)) {
    mat[, i] <- df$res[df$id == ids[i]]
  }
  cor_vals <- c()
  for (i in 1:(N - 1)) {
    for (j in (i + 1):N) {
      cor_vals <- c(cor_vals, cor(mat[, i], mat[, j]))
    }
  }
  CD <- sqrt(N * (N - 1) / (2 * T)) * mean(cor_vals, na.rm = TRUE)
  p_value <- 2 * (1 - pnorm(abs(CD)))

  structure(
    list(
      statistic = c(z = CD),
      parameter = NULL,
      p.value = p_value,
      method = "Pesaran CD test for cross-sectional dependence",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
