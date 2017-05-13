#' Moments for shrinkage and number of effective parameters
#'
#' Means and variances for the shinkage parameter, \eqn{kappa}, and
#' the number of effective parameters, \eqn{m_{eff}}, in regression
#' problems using scale-mixture of normal shrinkage priors.
#'
#' @details
#' The mean and variance for the shrinkage parameter \eqn{\kappa} given the global scale
#' \eqn{\tau}, and observation disturbance scale \eqn{\sigma} are:
#' \eqn{
#' \begin{aligned}[t]\
#' E(\kappa_j | \tau, \sigma) &= \frac{1}{1 + \sigma^{-1} \tau \sqrt{n}} , \\
#' Var(\kappa_j | \tau, \sigma) &= \frac{\sigma^{-1} \tau \sqrt{n}}{2(1 + \sigma^{-1} \tau \sqrt{n})^2} \\
#' \end{aligned}
#' }
#' Likewise, the mean and variance for the number of effective parameters \eqn{m_{eff}}
#' for \eqn{D} parameters is,
#' \eqn{
#' \begin{aligned}[t]
#' E(m_{eff} | \tau, \sigma) &= \frac{1}{1 + \sigma^{-1} \tau \sqrt{n}} D, \\
#' Var(m_{eff} | \tau, \sigma) &= \frac{\sigma^{-1} \tau \sqrt{n}}{2(1 + \sigma^{-1} \tau \sqrt{n})^2} D . \\
#' \end{aligned}
#' }
#'
#' @param tau Numeric. The global scale parameter.
#' @param sigma Numeric. The observation disturbance scale paramter.
#' @param n Integer. The number of observations.
#' @param D Integer. The number of parameters.
#' @return
#' \describe{
#' \item{kappa_moments}{A data frame with columns: \code{tau}, \code{sigma}, \code{n},
#'       \code{mean}, and \code{var}.}
#' \item{meff_moments}{A data frame with columns: \code{tau}, \code{sigma}, \code{n},
#'       \code{D}, \code{mean}, and \code{var}.}
#' }
#' @export
kappa_moments <- function(tau, sigma = 1, n = 1) {
  stn <- sigma ^ -1 * tau * sqrt(n)
  tibble(tau = tau,
         sigma = sigma,
         n = n,
         mean = 1 / (1 + stn),
         var = stn / (2 * (1 + stn) ^ 2))
}

#' @export
#' @rdname kappa_moments
meff_moments <- function(tau, sigma = 1, n = 1, D = 1) {
  stn <- sigma ^ -1 * tau * sqrt(n)
  list(mean = 1 / (1 + stn) * D,
       var = stn / (2 * (1 + stn) ^ 2) * D)
}
