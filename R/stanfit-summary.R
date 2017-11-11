#' Class for \code{stanfit} summary objects
#'
#' @param x For \code{stanfit_summary}, the result of the \code{stanfit}
#'  \code{summary} method. For methods, a \code{stanfit_summary} object.
#'
#' @return An object of class \code{stanfit_summary}
#' @export
stanfit_summary <- function(x) {
  structure(x, class = "stanfit_summary")
}

#' @describeIn stanfit_summary Print the summary object
#' @param n Number of rows to print
#' @param width Width of text output to generate.
#' @param stats character vector with the names of the stats to include.
#' @param n_extra Number of extra columns for which to print abbreviated
#'   information.
#' @param ... Not used.
#' @export
print.stanfit_summary <- function(x, ..., n = NULL, width = NULL,
                                  n_extra = NULL,
                                  stats = c("n_eff", "Rhat", "mean",
                                            "se_mean", "sd")) {
  x <- as.data.frame(x$summary)
  x$parameter <- rownames(x)
  print(tibble::trunc_mat(x[ , c("parameter", stats)], n = n, width = width,
                          n_extra = n_extra))
  invisible(x)
}

#' @export
#' @param summary If \code{TRUE}, then return the summary of all chains.
#' @param chains If \code{TRUE}, then return the individual chains. One of
#'    \code{summary} or \code{chains} must be \code{TRUE}. They will be appended
#'    together.
#' @describeIn stanfit_summary Converts the summary object to a data frame.
#' @importFrom assertthat assert_that is.flag
#' @importFrom dplyr select bind_rows as_data_frame everything
#' @importFrom tibble rownames_to_column as_data_frame
#' @importFrom rlang UQ expr sym
as_data_frame.stanfit_summary <- function(x, summary = TRUE,
                                          chains = FALSE, ...) {
  assert_that(is.flag(summary))
  assert_that(is.flag(chains))
  assert_that(summary || chains)
  out <- vector("list", 2)
  if (summary) {
    tbl <- as.data.frame(x$summary)
    tbl <- rownames_to_column(tbl, "parameter")
    res <- as_tibble(tbl)
    tbl[["chain"]] <- "summary"
    out[[1]] <- tbl
  }
  if (chains) {
    nm <- dimnames(x$c_summary)
    f <- function(.chain, x) {
      colnames(x) <- nm$stats
      res <- as_data_frame(x)
      res$parameter <- nm$parameter
      res$chain <- .chain
      res
    }
    out[[2]] <- purrr::map2_df(nm$chains,
                               purrr::array_branch(x$c_summary, 3), f)
  }
  select(bind_rows(out), UQ(sym("chain")), UQ(sym("parameter")),
         everything())
}
