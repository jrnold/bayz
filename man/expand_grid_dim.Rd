% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parparser.R
\name{expand_grid_dim}
\alias{expand_grid_dim}
\title{Generate indices for all dimensions}
\usage{
expand_grid_dim(d, colmajor = TRUE)
}
\arguments{
\item{d}{Array dimensions}

\item{colmajor}{Generate indexes using column-major (or row-major) order.b}
}
\value{
\code{data.frame} with dimensions \code{c(prod(dim), dim)}.
}
\description{
Create matrix if all indices for a given
dimension vector.
}
\keyword{internal}
