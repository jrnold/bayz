#' Generate indices for all dimensions
#'
#' Create matrix if all indices for a given
#' dimension vector.
#'
#' @param d Array dimensions
#' @param colmajor Generate indexes using column-major (or row-major) order.b
#' @return \code{data.frame} with dimensions \code{c(prod(dim), dim)}.
#' @importFrom purrr cross_df map
#' @importFrom stats setNames
#' @keywords internal
expand_grid_dim <- function(d, colmajor = TRUE) {
  dim_names <- paste0("dim_", seq_len(length(d)))
  if (!colmajor) {
    d <- rev(d)
    dim_names <- rev(dim_names)
  }
  idx <- cross_df(setNames(map(as.integer(d), seq_len), dim_names))
  if (!colmajor) {
    idx <- idx[ , rev(dim_names)]
  }
  idx
}


#' Create Stan or BUGS parameter names
#'
#' Given a parameter name and a matrix of index values, generate
#' names for the unlisted parameters in the style of Stan or BUGS.
#'
#' @param x \code{character} Parameter name.
#' @param d \code{integer} Dimension of the array.
#' @param pre \code{character} String to put before indices.
#' @param sep \code{character} String used to seperate indices.
#' @param post \code{character} String to put after indices.
#' @param colmajor \code{logical} Order indexes by column
#'   (\code{TRUE}) or row (\code{FALSE}).
#' @return \code{character} vector of flat parameter names.
#'
#' @examples
#' create_stan_parnames("alpha", c(1, 2))
#' create_parnames("alpha", c(1, 2), "_", "_", "")
#' @export
create_parnames <- function(x, d, pre="[", sep=",", post="]",
                            colmajor = TRUE) {
  if (is.null(d) || is.na(d[1]) || !length(d)) {
    x
  } else {
    idxstr <- apply(expand_grid_dim(d, colmajor = colmajor), 1, paste,
                    collapse = sep)
    paste0(x, pre, idxstr, post)
  }
}

#' @rdname create_parnames
#' @export
create_stan_parnames <- function(x, d) {
  create_parnames(x, d, "[", ",", "]", colmajor = TRUE)
}

#' @rdname create_parnames
#' @export
create_bugs_parnames <- function(x, d) {
  create_parnames(x, d, ".", ".", "", colmajor = TRUE)
}

#' Parse Stan parameter names
#'
#' Functions that parse a vector of flatted parameter names
#' and returns a data frame with the parameter names and indexes.
#'
#' @param x \code{character} vector with flat parameter names.
#' @param pre \code{character} Pattern between parameter name and indices.
#' @param sep \code{character} Pattern seperating each index.
#' @param post \code{character} Pattern following the indices.
#' @return A data frame with columns: `\code{paramname}`, `\code{parameter}`, and
#'  `\code{dim_1}`, `\code{dim_2}`, ....
#'
#' @examples
#' parse_stan_parnames(c("alpha", "beta[1]", "gamma[1,1]", "gamma[1, 2]"))
#' parse_parnames(c("beta.1.1", "beta.1.2"))
#' @export
#' @importFrom stringr str_split str_match
#' @importFrom purrr map_int map_df
parse_parnames <- function(x, pre="\\W+", sep="\\W+", post="\\W+") {
  # Remove post
  pattern <- sprintf("^(.*?)(%s(\\d+(%s\\d+)*)(%s)?)?$", pre, sep, post)
  parsed <- str_match(x, pattern)
  idx <- map(str_split(parsed[ , 4], sep), as.integer)
  tibble(
    par = x,
    name = parsed[ , 2],
    dim = idx
  )
}


#' @rdname parse_parnames
#' @export
parse_stan_parnames <- function(x) {
  parse_parnames(x, pre = "\\[", sep = ",", post = "\\]")
}

#' @rdname parse_parnames
#' @export
pars_bugs_parnames <- function(x) {
  parse_parnames(x, "\\.", "\\.", "")
}
