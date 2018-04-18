#' The Horseshoe+ Prior Distribution
#'
#' @details There is no exact closed form for the density of the horseshoe
#' distribution.
#'
#' The Horseshoe+ Prior Distribution is defined hierarchically as,
#' \deqn{
#' x_i | \lambda_i \sim N(0, \lambda_i^2)
#' \lambda_i | \eta_i \sim C^{+}(0, \eta_i)
#' \eta_i | \eta_i \sim C^{+}(0, 1)
#' }
#' Integrating over \eqn{\eta_i}, the density of \eqn{\lambda_i} is,
#' \deqn{
#' p(\lambda_i) = \frac{4}{\pi^2} \frac{\log(\lambda_i)}{\lambda_i^2 - 1} .
#' }
#'
#' There is no closed form representation of the density, but it is bounded by,
#' \deqn{
#' \frac{1}{\pi^2 \sqrt{2 \pi}} \log \left( 1 + \frac{4}{\theta^2} \right)
#' < p_{HS+}(\theta)
#' \leq \frac{1}{\pi^2 | |\theta|}
#' }{
#' 1 / (pi^2 * sqrt(2 pi)) * log(1 + 4 / theta^2)
#' < p_HS+ (theta)
#' <=  1 / (pi^2 * abs(theta))
#' }
#'
#' @references
#' Bhadra, A., Datta, J., Polson, N. G., Willard, B. (2016)
#'    \dQuote{The Horseshoe+ Estimator of Ultra-Sparse Signals}.
#'    \emph{Bayesian Analysis},
#'    \href{https://dx.doi.org/10.1214/16-BA1028}{doi:10.1214/16-BA1028}
#' @param x A numeric vector of quantiles
#' @param n Number of samples to draw
#' @param tau numeric vector. The scale parameter
#' @param location numeric vector. The location parameter.
#' @return A numeric vector
#' @export
dhsplus <- function(x, tau = 1, location = 0) {
  x <- (x - location) / tau
  if (x == 0) {
    Inf
  } else {
    lb <- 1 / (base::pi ^ 2 * sqrt(2 * base::pi)) * log(1 + 4 / x ^ 2)
    ub <- 1 / (base::pi ^ 2 * abs(x))
    0.5 * (lb + ub)
  }
}

#' @export
#' @rdname dhsplus
rhsplus <- function(n, tau = 1, location = 0) {
  eta <- abs(rcauchy(n))
  lambda <- abs(rcauchy(n, scale = eta))
  rnorm(n, mean = location, sd = lambda * tau)
}
