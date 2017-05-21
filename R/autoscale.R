#' Center and Scale
#'
#' Center and scale predictors using the same methods
#' as in \pkg{rstanarm}.
#'
#' @param x object
#' @param center If \code{TRUE}, then set the mean of the predictor
#'   to zero.
#' @param scale If \code{TRUE}, then scale the predictor.
#' @param ... Other arguments (not used)
#'
#' @details
#' Variables are standardized using the following rules, based on the
#' number of unique values:
#' \itemize{
#' \item{one: nothing}
#' \item{two: subtract the mean and divide by the range}
#' \item{three+: subtract the mean and divide by the standard deviation}
#' }
#' @export
#' @importFrom stats sd
autoscale <- function(x, ...) {
  UseMethod("autoscale")
}

#' @rdname autoscale
#' @export
autoscale.numeric <- function(x, center = TRUE, scale = TRUE, ...) {
  nvals <- length(unique(x))
  if (nvals <= 1) {
    out <- x
  } else if (nvals == 2) {
    out <- if (scale) {
      (x - min(x, na.rm = TRUE)) / diff(range(x, finite = TRUE))
    } else x
    if (center) {
      out <- x - mean(x)
    }
  } else {
    out <- if (center) {
      x - mean(x, na.rm = TRUE)
    } else x
    out <- if (scale) out / sd(out, na.rm = TRUE)
  }
  out
}

#' @rdname autoscale
#' @export
autoscale.logical <- function(x, center = TRUE, scale = TRUE, ...) {
  x <- as.numeric(x)
  out <- if (scale) {
    (x - min(x, na.rm = TRUE)) / diff(range(x, finite = TRUE))
  } else x
  if (center) {
    out <- x - mean(x)
  }
  out
}

#' @rdname autoscale
#' @export
autoscale.matrix <- function(x, center = TRUE, scale = TRUE, ...) {
  apply(x, 2, autoscale, center = center, scale = scale)
}

#' @rdname autoscale
#' @export
autoscale.data.frame <- function(x, center = TRUE, scale = TRUE, ...) {
  dplyr::mutate_if(x, is.numeric, autoscale.numeric, 
                   center = center, scale = scale)
}
