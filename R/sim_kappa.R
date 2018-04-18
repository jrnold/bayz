fun_or_rep <- function(f, n) {
  if (is.function(f)) f(n)
  else rep(f, n)
}

#' Simulate priors on shrinkage and number of effective parameters
#'
#' Draw samples from the implied priors on the shrinkage parameters (\code{sim_kappa}) and
#' number of effective parameters (\code{sim_meff}) in hierarchical scale-mixture of normal
#' shrinkage priors.
#'
#' @param lambda Either a number or a function which samples from the
#'   local scale, \eqn{\tau}.
#' @param tau Either a number or a function which samples from the
#'   global scales, \eqn{\lambda}.
#' @param sigma Either a number or a function which samples from the observation
#'   scale, \eqn{\sigma}.
#' @param n The number of observations in the data
#' @param D The number of parameters
#' @param sims The number of samples to draw.
#' @return A numeric vector of samples
#' @export
sim_meff <- function(lambda, tau = 1, sigma = 1, D = 1, n = 1,
                     sims = 1000) {
  f <- function(tau, precision) {
    kappa <- 1 / (1 + n * precision * tau ^ 2 * fun_or_rep(lambda, D) ^ 2)
    sum(1 - kappa)
  }
  purrr::map2_dbl(fun_or_rep(tau), 1 / fun_or_rep(sigma) ^ 2, f)
}

#' @rdname sim_meff
#' @export
sim_kappa <- function(lambda, tau = 1, sigma = 1,
                      n = 1, sims = 1000) {
  precision <- 1 / fun_or_rep(sigma, sims) ^ 2
  1 / (1 + n * precision * fun_or_rep(tau, sims) ^ 2 *
         fun_or_rep(lambda, sims) ^ 2)
}


