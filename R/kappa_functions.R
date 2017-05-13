#' Shinkage parameter functions
#'
#' Generate functions of the shrinkage parameter,
#' \deqn{
#' \kappa_j = \frac{1}{1 + n \sigma^{-1} \tau^2 \lambda_j^2} ,
#' }
#' with respect to lambda, sigma, or tau, as well as the inverse functions
#' and derivatives of the inverse functions (with respect to scale, variance, or precisition).
#' These functions can be used in a transformation of variables, to generate
#' the density of the probability of distribution of kappa given the probability
#' distributuion of lambda, sigma, or tau.
#'
#' @param tau numeric. The global scale parameter.
#' @param sigma numeric. The observation disturbance scale parameter.
#' @param lambda numeric. The local scale parameter.
#' @param n integer. The number of observations.
#' @return A list of functions
#' @export
kappa_lambda_funs <- function(tau = 1, sigma = 1, n = 1) {
  nprec <- n * sigma ^ -2
  tau2 <- tau ^ 2
  xx <- 1 + nprec * tau2
  list(
    # kappa(lambda)
    fun = function(lambda) {
      1 / (1 + nprec * tau2 * lambda ^ 2)
    },
    # lambda(kappa)
    inv_scale = function(kappa) {
      sqrt(1 / (xx * kappa))
    },
    # d(lambda(kappa)) / d kappa
    dinv_scale = function(kappa) {
      -0.5 * xx ^ (-0.5) * kappa ^ (-1.5)
    },
    # lambda^2(kappa)
    inv_var = function(kappa) {
      1 / (xx * kappa)
    },
    # d(lambda^2(kappa)) / d kappa
    dinv_var = function(kappa) {
      -xx ^ (-1) * kappa ^ (-2)
    },
    # lambda^{-2}(kappa)
    inv_prec = function(kappa) {
      xx * kappa
    },
    # d(lambda^{-2}(kappa)) / d kappa
    dinv_prec = function(kappa) {
      xx
    }
  )
}

#' @rdname kappa_lambda_funs
#' @export
kappa_tau_funs <- function(lambda = 1, sigma = 1, n = 1) {
  nprec <- n * sigma ^ -2
  lambda2 <- lambda ^ 2
  xx <- 1 + nprec * lambda2
  list(
    # kappa(lambda)
    fun = function(tau) {
      1 / (1 + nprec * tau ^ 2 * lambda2)
    },
    # lambda(kappa)
    inv_scale = function(kappa) {
      sqrt(1 / (xx * kappa))
    },
    # d(lambda(kappa)) / d kappa
    dinv_scale = function(kappa) {
      -0.5 * xx ^ (-0.5) * kappa ^ (-1.5)
    },
    # lambda^2(kappa)
    inv_var = function(kappa) {
      1 / (xx * kappa)
    },
    # d(lambda^2(kappa)) / d kappa
    dinv_var = function(kappa) {
      -xx ^ (-1) * kappa ^ (-2)
    },
    # lambda^{-2}(kappa)
    inv_prec = function(kappa) {
      xx * kappa
    },
    # d(lambda^{-2}(kappa)) / d kappa
    dinv_prec = function(kappa) {
      xx
    }
  )
}


#' @rdname kappa_lambda_funs
#' @export
kappa_sigma_funs <- function(lambda = 1, tau = 1, n = 1) {
  tau2 <- tau ^ 2
  lambda2 <- lambda ^ 2
  list(
    fun = function(sigma) {
      1 / (1 + n * sigma ^ (-2) * tau2 * lambda2)
    },
    inv_scale = function(kappa) {
      tau * lambda * sqrt(n / (1 - kappa))
    },
    dinv_scale = function(kappa) {
      -0.5 * tau * lambda * sqrt(n) * (1 - kappa) ^ (-3 / 2)
    },
    inv_var = function(kappa) {
      (tau2 * lambda2 * n) / (1 - kappa)
    },
    dinv_var = function(kappa) {
      -(tau2 * lambda2 * n) * kappa ^ (-2)
    },
    inv_prec = function(kappa) {
      (1 - kappa) / (n * tau2 * lambda2)
    },
    jacobian_prec = function(kappa) {
      -1 / (n * tau2 * lambda2)
    }
  )
}

# Distribution over Kappa from distribution over lambda
# kappa_dist <- function(dlambda, tau = 1, sigma = 1, n = 1,
#                        type = c("scale", "variance", "precision"), ...) {
#   type <- match.arg(type)
#   fkappa <- kappa_lambda_funs(tau = tau, sigma = sigma, n = n)
#   if (type == "scale") {
#     finv <- fkappa$inv_scale
#     jacobian <- fkappa$jacobian_scale
#   } else if (type == "variance") {
#     finv <- fkappa$inv_var
#     jacobian <- fkappa$jacobian_var
#   } else {
#     finv <- fkappa$inv_prec
#     jacobian <- fkappa$jacobian_prec
#   }
#   function(x) {
#     dlambda(finv(x), ...) * jacobian(x)
#   }
# }





