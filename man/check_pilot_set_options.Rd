% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_pilot_set.R
\name{check_pilot_set_options}
\alias{check_pilot_set_options}
\title{Check Pilot set options}
\usage{
check_pilot_set_options(
  pilot_fraction,
  pilot_size,
  group_by_covariates,
  data,
  n_c
)
}
\arguments{
\item{pilot_fraction}{numeric between 0 and 1 giving the proportion of
controls to be allotted for building the prognostic score (default = 0.1)}

\item{pilot_size}{alternative to pilot_fraction. Approximate number of
observations to be used in pilot set. Note that the actual pilot set size
returned may not be exactly \code{pilot_size} if \code{group_by_covariates}
is specified because balancing by covariates may result in deviations from
desired size. If \code{pilot_size} is specified, \code{pilot_fraction} is
ignored.}

\item{group_by_covariates}{character vector giving the names of covariates to
be grouped by (optional). If specified, the pilot set will be sampled in a
stratified manner, so that the composition of the pilot set reflects the
composition of the whole data set in terms of these covariates.  The
specified covariates must be categorical.}

\item{data}{\code{data.frame} with observations as rows, features as columns}

\item{n_c}{number of control observations in \code{data}}
}
\value{
nothing
}
\description{
Check Pilot set options
}
\keyword{internal}
