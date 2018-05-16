#' Create a skeleton for a stanfit object
#'
#' @param object A \code{stanfit} object
#' @return A named list where the elements are the parameters of \code{object},
#'   and the elements are numeric vectors or matrices with the proper dimensions.
#'   This is intended to be used with \code{\link{relist}} to convert a vector.
#'
#' @noRd
create_skeleton <- function(object) {
  pars <- object@model_pars
  dims <- object@par_dims
  lst <- purrr::map(dims, function(d) {
    if (length(d) < 1) {
      0
    } else {
      array(0, dim = d)
    }
  })
  lst
}


#' Unflatten Stan parameters
#'
#' This function returns a function that will convert of vector of flattened
#' parameters from the output of a Stan model into a list with the original
#' parameters with their original values.
#'
#' @param object A \code{"\link[=stanfit-class]{stanfit}"} object.
#'
#' @return A function that takes a single argument \code{x} which
#'  is a expected to be a numeric vector.
#'
#' @export
unflatten_pars <-  function(object) {
  skeleton <- create_skeleton(object)
  function(x) {
    lst <- utils::relist(x, skeleton)
    for (i in seq_along(skeleton)) {
      dim(lst[[i]]) <- dim(skeleton[[i]])
    }
    lst
  }
}
