% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manual_stratify.R
\name{check_inputs_manual_stratify}
\alias{check_inputs_manual_stratify}
\title{Check inputs to manual_stratify}
\usage{
check_inputs_manual_stratify(data, strata_formula, force)
}
\arguments{
\item{data}{data.frame with observations as rows, features as columns}

\item{strata_formula}{the formula to be used for stratification.  (e.g. \code{treat
~ X1}) the variable on the left is taken to be the name of the treatment
assignment column, and the variables on the left are taken to be the
variables by which the data should be stratified}

\item{force}{a boolean. If true, run even if a variable appears continuous.
(default = FALSE)}
}
\value{
nothing; produces errors and warnings if anything is wrong
}
\description{
Not meant to be called externally.  Checks validity of formula, types of all inputs to manual stratify, and warns if covariates are continuous.
}
\keyword{internal}
