#' View the model code of compiled Stan model
#'
#' This can be useful when debugging and the model contains include statements
#' that makes it difficult to find the problematic line in the original file.
#'
#' @param mod A Stan model
#' @param start Start line
#' @param end End line
#' @export
stan_model_view_code <- function(mod, start = NULL, end = NULL) {
  code <- mod@model_code
  lines <- str_split(code, "\n")[[1]]
  if (is.null(start)) start <- 1L
  if (is.null(end)) end <- length(lines)
  lines[start:end]
}
