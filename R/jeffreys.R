#' Jeffreys' Variance Reference Prior
#'
#' The Jeffreys' improper prior for variance is
#' \eqn{p(x) \propto 1 / x}. This is not a proper probability distribution.
#' However, "density" and random sampling functions are provided.
#'
#' The Jeffreys' improper prior can be approximated by a Gamma distribution
#' as the shape and rate go to zero.
#'
#' @param x A numeric vector of quantiles
#' @param n The number of samples to draw
#' @export
djeffreys <- function(x)  1 / x

#' @rdname djeffreys
#' @export
rjeffreys <- function(n) {
  stats::rgamma(n, shape = .Machine$double.eps,
              rate = .Machine$double.eps)
}