#' loo tidiers
#'
#' Methods for tidying the results from from WAIC and LOO cross-validation
#' methods in the \pkg{loo} package.
#'
#' @param x an \code{\link[loo]{loo}} object
#'
#' @param data Data frame with the original data
#'
#' @param ... Not used currently
#'
#' @return \code{augment.loo} returns a data frame, with one row per observation
#' and the pointwise elpd, elpd standard error, and estimated number of observations.
#'
#' @export
#'
#' @importFrom tibble as_tibble
augment.loo <- function(x, data = NULL, ...) {
  out <- as_tibble(x$pointwise)
  names(out) <- paste0(".", names(out))
  out$.pareto_k <- x$pareto_k
  if (!is.null(data)) {
    dplyr::bind_cols(as.data.frame(data), out)
  } else {
    out
  }
}

#' @rdname augment.loo
#' @export
#' @return \code{glance.loo} returns a one row data frame with the columns
#'   \item{\code{elpd_loo},\code{elpd_waic}}{Expected log pointwise predictive density}
#'   \item{\code{se_elpd_loo},\code{se_elpd_waic}}{Standard error of the log pointwise predictive density}
#'   \item{\code{se_looic}, \code{see_waic}}{LOO information criterion (\code{-2 * elpd_loo}), or WAIC, i.e. converted to the deviance scale}
#'   \item{\code{p_loo}, \code{p_waic}}{Estimated effective number of parameters}
#'   \item{\code{se_p_loo}, \code{se_p_waic}}{Standard error of the estimated effective number of parameters}
#'   \item{\code{n}}{Number of observations}
#'   \item{\code{n_sims}}{Number of MCMC sims}
#' @export
glance.loo <- function(x, ...) {
  out <- as_tibble(unclass(x[setdiff(names(x), c("pointwise", "pareto_k"))]))
  out$n <- attr(x, "log_lik_dim")[2]
  out$n_sims <- attr(x, "log_lik_dim")[1]
  out
}

#' @rdname augment.loo
#' @return A data frame with columns:
#'    \item{\code{parameter}}{Parameter: elpd_loo or elpd_waic, p_loo or p_waic, or waic or looic}
#'    \item{\code{estimate}}{Parameter value}
#'    \item{\code{std.error}}{Standard error of the parameter}
#' @importFrom tibble tibble
#' @export
tidy.loo <- function(x, ...) {
  elpd <- grep("^elpd_(loo|waic)$", names(x), value = TRUE)
  p <- grep("^p_(loo|waic)$", names(x), value = TRUE)
  ic <- grep("^(waic|looic)$", names(x), value = TRUE)
  tibble(
    parameter = c(elpd, p, ic),
    estimate = c(x[[elpd]], x[[p]], x[[ic]]),
    std.err = c(x[[paste0("se_", elpd)]],
                x[[paste0("se_", p)]],
                x[[paste0("se_", ic)]]))
}

# From rstan::is.stanreg
is.stanreg <- function(x) inherits(x, "stanreg")
