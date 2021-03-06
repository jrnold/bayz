#' Evaluate expression within a Stan iteration
#'
#' Evaluate an expression on each iteration of \link[=stan]{Stan} output.
#' This makes it easier to post-process Stan output because the expression is
#' evaluated with the parameters having the same dimensions as they do in the
#' model.
#'
#' @inheritParams rstan::extract
#' @inheritParams rlang::eval_tidy
#' @param object An object.
#' @param expr An expression to evaluate.
#'   This uses \code{\link[rlang]{eval_tidy}}
#'   to evaluate the expression with the contents of \code{data} and the
#'   current iteration's parameters as a \link[=as_data_mask]{data mask}.
#' @param data A list or environment with additional data to have in scope
#'    when evaluating \code{expr}.
#' @param ... Passed to other methods
#'
#' @return A list with the output of \code{expr} evaluated with each expression.
#'
#' @export
with_mcmc_iter <- function(object, ...) {
  UseMethod("with_mcmc_iter")
}

#' @importFrom rlang eval_tidy enquo base_env as_environment as_data_pronoun
#' @importFrom rlang new_data_mask
#' @rdname with_mcmc_iter
#' @export
with_mcmc_iter.stanfit <- function(object, expr,
                                   inc_warmup = FALSE, data = list(),
                                   env = rlang::base_env(), ...) {
  data <- as_environment(data)
  pars <- rstan::extract(object, permuted = FALSE, inc_warmup = inc_warmup)
  expr <- enquo(expr)
  unflattener <- unflatten_pars(object)
  map(purrr::array_branch(pars, 1:2), function(x) {
    pars <- as_environment(unflattener(x))
    parent.env(data) <- pars
    mask <- new_data_mask(data, parent.env(data))
    mask$.data <- as_data_pronoun(data)
    mask$.pars <- as_data_pronoun(pars)
    eval_tidy(expr, data = mask)
  })
}

#' @rdname with_mcmc_iter
#' @export
with_mcmc_iter.stanreg <- function(object, ...) {
  with_mcmc_iter.stanfit(object[["stanfit"]], ...)
}