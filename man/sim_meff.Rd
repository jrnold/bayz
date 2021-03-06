% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_kappa.R
\name{sim_meff}
\alias{sim_meff}
\alias{sim_kappa}
\title{Simulate priors on shrinkage and number of effective parameters}
\usage{
sim_meff(lambda, tau = 1, sigma = 1, D = 1, n = 1, sims = 1000)

sim_kappa(lambda, tau = 1, sigma = 1, n = 1, sims = 1000)
}
\arguments{
\item{lambda}{Either a number or a function which samples from the
local scale, \eqn{\tau}.}

\item{tau}{Either a number or a function which samples from the
global scales, \eqn{\lambda}.}

\item{sigma}{Either a number or a function which samples from the observation
scale, \eqn{\sigma}.}

\item{D}{The number of parameters}

\item{n}{The number of observations in the data}

\item{sims}{The number of samples to draw.}
}
\value{
A numeric vector of samples
}
\description{
Draw samples from the implied priors on the shrinkage parameters (\code{sim_kappa}) and
number of effective parameters (\code{sim_meff}) in hierarchical scale-mixture of normal
shrinkage priors.
}
