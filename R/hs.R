#' The Horseshoe Prior Distribution
#'
#' @details The Horseshoe Prior Distribution is defined hierarchically as,
#' \deqn{
#' x_i | \lambda_i \sim N(0, \lambda_i^2)
#' \lambda_i | \eta_i \sim C^{+}(0, \eta_i)
#' }
#' The density of \eqn{\lambda}{lambda} is a half-Cauchy,
#' \deqn{
#' p(\lambda_i) = \frac{2}{\pi^2} \frac{\log(\lambda_i)^2}{\lambda_i^2 - 1} .
#' }
#'
#' There is no closed form representation of the density, but it is bounded by,
#' \deqn{
#' \frac{K}{2} \log \left( 1 + \frac{4}{\theta^2} \right)
#' < p_{HS}(\theta)
#' \leq K \log \left(1 + \frac{2}{\theta^2}} \right)
#' }{
#' (K / 2) * log(1 + 4 / theta^2)
#' < p_HS (theta)
#' < K log (1 + 2 / theta^2)
#' }
#'
#' @references
#' Carvalho, C. M., Polson, N. G., Scott, J. G. (2010)
#'    \dQuote{The horseshoe estimator for sparse signals}. \emph{Biometrika}
#'    \href{https://dx.doi.org/10.1093/biomet/asq017}{doi:10.1093/biomet/asq017}.
#'
#' @param x A numeric vector of quantiles
#' @param n Number of samples to draw
#' @param tau numeric vector. The scale parameter
#' @param location numeric vector. The location parameter.
#' @return A numeric vector
#' @export
dhs <- function(x, tau = 1, location = 0) {
  x <- (x - location) / tau
  K <- 1 / sqrt(2 * base::pi ^ 3)
  lb <- 0.5 * K * log(1 + 4 / x ^ 2)
  ub <- K * log(1 + 2 / x ^ 2)
  0.5 * (lb + ub)
}

#' @export
#' @rdname dhs
#' @importFrom stats rcauchy rnorm
rhs <- function(n, tau = 1, location = 0) {
  lambda <- abs(rcauchy(n))
  rnorm(n, mean = location, sd = lambda * tau)
}
