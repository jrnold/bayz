#' Global scale prior scale choice
#'
#'
#' Piironen and Vehtari (2017) suggest the following choice of the scale for the
#' global hyperparameter in a regression shrinkage problem with a global-local
#' scale mixture of normal prior distribution,
#' \deqn{
#' \tau_0 = \frac{p}{D - p} \frac{\sigma}{\sqrt{n}}
#' }
#' where \eqn{p} is the expected number of non-zero parameters, \code{sigma}
#' is the scale of the observation disturbances, \code{D} is the number of
#' parameters, and \code{n} is the number of observations.
#'
#' @param p Expected number of non-zero parameters
#' @param D Number of parameters
#' @param sigma Scale of the observation disturbances
#' @param n Number of observations
#' @return A numeric vector
#' @references Piironen, J., Vehtari, A. 2017.
#'    \url{https://arxiv.org/abs/1610.05559}.
#'
#' @export
tau_prior_scale <- function(p, D = 1, n = D, sigma = 1) {
  p / (D - p) * sigma / sqrt(n)
}