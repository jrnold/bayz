# Adapted from this
# Copyright: Michael Betancourt <https://betanalpha.github.io/writing/>
# License: BSD (3 clause)
# See also http://mc-stan.org/users/documentation/case-studies/rstan_workflow.html

#' Robust Stan Workflow Functions
#'
#' These functions are derived from from Michael Betancourt's \href{https://github.com/betanalpha/knitr_case_studies/blob/master/rstan_workflow/stan_utility.R}{Robust Stan Workflow},
#' Stan case study.
#'
#' \describe{
#' \item{check_div}{Check transitions that ended with a divergence}
#' \item{check_energy}{Check chains for low E-BFI.}
#' \item{check_treedepth}{}
#' \item{partition_div}{Partitions parameters into divergent and non-divergent iterations.}
#' }
#'
#' @param fit A \code{stanfit} object.
#' @param pars A character vector of parameters to include.
#' @param threshhold If any chain has an E-BFI below \code{threshshold}, a
#'   warning is printed.
#' @return These functions are largely run interactively in order to
#' \describe{
#'   \item{check_div}{A list with two elements: \code{n_div} is an integer scalar
#'     with the total number of divergences, and \code{n_iter} is an integer scalar
#'     with the total number of iterations.}
#'   \item{check_treedepth}{A list with three elements:
#'    \describe{
#'    \item{\code{max_depth}}{The maximum treedepth used in the sampler.}
#'    \item{\code{n_max}}{The number of iterations reaching the maximum depth.}
#'    \item{\code{n_iter}}{The total number of elements.}
#'    }}
#'   \item{check_energy}{A numeric vector with the E-BFI for each chain.}
#'   \item{partition_div}{A data frame with a row for each (sample, parameter),
#'     and columns,
#'     \describe{
#'     \item{\code{.iter}}{Iteration number}
#'     \item{\code{.chain}}{Chain number}
#'     \item{\code{.div}}{Was this iteration a divergence?}
#'     \item{\code{parameter}}{parameter name}
#'     \item{\code{value}}{parameter value}
#'     }}
#' }
#' @export
#' @name stan_workflow
#' @importFrom purrr flatten_int map
#' @importFrom rstan get_sampler_params
#' @importFrom glue glue
check_div <- function(fit) {
  sampler_params <- get_sampler_params(fit, inc_warmup=FALSE)
  divergent <- flatten_int(map(sampler_params,
                               ~ as_integer(.x[, "divergent__"])))
  n = sum(divergent)
  N = length(divergent)
  message(glue("{n} of {N} iterations ended with a divergence ({100 * n / N}%)"))
  if (n > 0) {
    fit_args <- attr(fit@sim$samples[[1]], "args")
    adapt_delta <- fit_args[["control"]][["adapt_delta"]]
    warning(glue("Try sampling with a larger adapt_delta ",
                 "`adapt_delta` > {adapt_delta}) to ",
                 "remove the divergences"))
  }
  list(n_div = n, n_iter = N)
}

#' @export
#' @rdname stan_workflow
check_treedepth <- function(fit) {
  sampler_params <- get_sampler_params(fit, inc_warmup=FALSE)
  # all max treedepths should be the same
  max_depth <- attr(fit@sim$samples[[1]], "args")[["control"]][["max_treedepth"]]
  treedepths <- flatten_int(map(sampler_params, ~ as_integer(.x[, "treedepth__"])))
  n = sum(treedepths >= max_depth)
  N = length(treedepths)
  message(glue("{n} of {N} iterations saturated the maximum tree depth of {max_depth} ({100 * n / N}%)"))
  if (n > 0) {
    warning(glue("Run again with `max_depth > {max_depth}` to avoid saturation"))
  }
  invisible(list(max_depth = max_depth, n_max = n, iter = N))
}

#' @rdname stan_workflow
#' @export
#' @importFrom purrr map_dbl
#' @importFrom stats var
check_energy <- function(fit, threshhold = 0.2) {
  sampler_params <- get_sampler_params(fit, inc_warmup=FALSE)
  low_ebfi <- FALSE
  chain_ebfis <- map_dbl(sampler_params, function(.x) {
    energies <- .x[,"energy__"]
    numer <- sum(diff(energies) ^ 2) / length(energies)
    denom <- var(energies)
    numer / denom
  })
  for (i in seq_along(chain_ebfis)) {
    ebfi <- chain_ebfis[[i]]
    message(glue("Chain {i}: E-BFMI = {round(ebfi, 2)}"))
  }
  n_low <- sum(chain_ebfis < threshhold)
  if (n_low) {
    warning(glue("Warning: {n_low} chains had an E-BFMI below {threshhold}. ",
                 "This indicates you may need to reparameterize your model"))
  }
  chain_ebfis
}

#' @export
#' @rdname stan_workflow
#' @importFrom purrr pmap_df array_branch
#' @importFrom dplyr one_of
#' @importFrom tidyr gather
partition_div <- function(fit, pars = NULL) {
  nom_params <- rstan::extract(fit, pars = pars, permuted = FALSE)
  n_chains <- dim(nom_params)[[2]]
  parnames <- dimnames(nom_params)[[3]]
  sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)
  divergent <- map(sampler_params, ~ .x[, "divergent__"])

  par2df <- function(x, div, chain) {
    # bound vars: parnames
    colnames(x) <- parnames
    out <- tibble(x)
    out[[".chain"]] <- chain
    out[[".iter"]] <- seq_len(nrow(out))
    out[[".div"]] <- div
    gather(chain, -one_of(c(".chain", ".iter", ".div")))
  }
  pmap_df(list(x = array_branch(nom_params, 2),
               div = divergent,
               chain = seq_along(n_chains)), par2df)
}

