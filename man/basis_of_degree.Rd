% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_basis.R
\name{basis_of_degree}
\alias{basis_of_degree}
\title{Compute Degree of Basis Functions}
\usage{
basis_of_degree(x, degree)
}
\arguments{
\item{x}{An input \code{matrix} containing observations and covariates
following standard conventions in problems of statistical learning.}

\item{degree}{The highest order of interaction terms for which the basis
functions ought to be generated. The default (\code{NULL}) corresponds to
generating basis functions for the full dimensionality of the input matrix.}
}
\description{
Find the full list of basis functions up to a particular degree
}
